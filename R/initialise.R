
#' Initialises the optimisation for a jwmodel with all parameters defined
#' 
#' Once a jwmodel object has been loaded with all the necessary parameter
#' information, it must be initialised with this function to ready it for 
#' optimisation.
#' 
#' @param obj jwmodel object to be initialised
#' @return Returns an object of type \code{jwmodel}
#' @export
#' @examples 
#' \dontrun{
#' mymodel <- initialise(mymodel)
#' }
initialise <- function(obj) {
  UseMethod("initialise")
}

#' @export
initialise.default <- function(obj) {
  cat("'initialise' function can only be used on jwmodel objects")
}

# function to initialise optimisation: derives and creates objective function
# and all constraints; loads them into an lpSolveAPI object ready to optimise
#' @importFrom rlang .data
#' @importFrom  utils head
#' @export
initialise.jwmodel <- function(obj) {
  
  n_years <- nrow(obj$years)
  n_jurisdictions <- nrow(obj$jurisdictions)
  n_types <- nrow(obj$judge_types)
  if (is.null(obj$regions)) {
    n_regions <- 1
  } else {
    n_regions <- nrow(obj$regions)
  }
  
  n_vars <- (n_regions * n_years * n_jurisdictions * (n_types + 1)) +
    (n_regions * n_years * n_types * 3)
  
  # identify user-selected demand scenario (from passed metadata),
  # default = baseline = 3 (3rd column in 'baseline demand' table)
  if (is.numeric(obj$metadata$demand_scenario)) {
    selected_demand_scenario <- obj$metadata$demand_scenario + 2
  } else {
    selected_demand_scenario <- 3
  }
  
  # identify user-selected max recruitment limit scenario (from passed metadata),
  # default = column 3 in obj$recruit_limits tibble
  if (is.numeric(obj$metadata$recruit_scenario)) {
    selected_recruitment_scenario <- obj$metadata$recruit_scenario + 2
  } else {
    selected_recruitment_scenario <- 3
  }
  
  # generates correctly ordered unique combos of (Region)/Year/Jurisdiction/Judge Type
  # for "Allocation Variables", for use in dplyr joins
  allocation_vars <- allocation_vars_template(obj)
  
  # generates correctly ordered unique combos of (Region)/Year/Judge Type/In-Out Status
  # for "Resource Variables", for use in dplyr joins
  # NB for In-Out Status (io), "E" = existing / in post, "I" = incoming,
  # "O" = outgoing
  resource_vars <- resource_vars_template(obj)
  
  
  # add bounds to prevent any variable taking a value less than zero
  bounds <- list(coefficients = rep(0, n_vars), indices = 1:n_vars)
  obj$bounds$lower <- append(obj$bounds$lower, list(bounds))
  
  
  ##### EQ-000 set Objective Function #####
  # Minimise total 'cost': ref EQ000
  
  # define variable cost (fees) coefficients: NB includes unallocated penalty costs
  
  # coefficient = avg sitting days (capacity) x avg per-sitting-day cost
  # (result ordered by Year > Jurisdiction > Judge Type)
  df <- allocation_vars %>%
    dplyr::left_join(obj$variable_costs, by = c("Judge", "Jurisdiction", "Region")) %>%
    dplyr::left_join(obj$sitting_days, by = c("Judge" = "Judge Type", "Year", "Region")) %>%
    dplyr::left_join(obj$penalty_costs, by = c("Judge", "Jurisdiction")) %>%
    dplyr::mutate(`Avg Sitting Day Cost` = dplyr::if_else(.data$Judge == "U", 
                                                          .data$`Penalty Cost`,
                                                          .data$`Avg Sitting Day Cost`)) %>%
    tidyr::replace_na(list(`Avg Sitting Day Cost` = 0, `Avg Sitting Days` = 1)) %>%
    dplyr::mutate(coeff = .data$`Avg Sitting Day Cost` * .data$`Avg Sitting Days`)
  
  f_costs_yjt <- df$coeff 
  
  # fixed cost (salary) coeffiecients
  # create ordered coefficients for 'in-post' resource variables (= salary cost)
  
  df <- resource_vars %>%
    dplyr::left_join(obj$fixed_costs, by = c("Judge" = "Judge Type", "Region")) %>%
    tidyr::replace_na(list("Avg Annual Cost" = 0)) %>%
    dplyr::mutate(coeff = dplyr::if_else(.data$io == "E", .data$`Avg Annual Cost`, 0))
  
  
  # interleve with penalty cost coefficients for 'income' and zero cost for
  # 'outgoing' resource variables
  # s_costs_yt <- c(rbind(df$coeff, 0, 0))
  s_costs_yt <- df$coeff
  
  # combine coefficients into a single vector in the correct order
  coeffs <- c(f_costs_yjt, s_costs_yt)
  n_cols <- length(coeffs)
  indices <- 1:n_cols
  
  # create objective function using calculated coefficients
  obj$constraints$obj <- create_objective_function(n_cols, coeffs, indices)
  
  ##### EQ-002 Demand must be satisfied constraint #####
  # See Ref EQ-002b
  
  df <- allocation_vars %>%
    dplyr::left_join(
      obj$sitting_days, 
      by = c("Judge" = "Judge Type", "Year", "Region")
    ) %>%
    dplyr::select(
      .data$Region, .data$Year, .data$Jurisdiction, .data$Judge, 
      coeff = .data$`Avg Sitting Days`
    ) %>%
    dplyr::mutate(coeff = tidyr::replace_na(.data$coeff, 1))
  
  obj$constraints$demand <- list()
  
  # create constraint for each Region / Year / Jurisdiction combo
  for (r in levels(obj$regions$Region)) {
    
    for (y in levels(obj$years$Years)) {
      
      for (j in levels(obj$jurisdictions$Jurisdiction)) {
        
        indices <- which(df$Region == r & df$Year == y & df$Jurisdiction == j)
        coeffs <- df$coeff[indices]
        
        RHS <- obj$demand[
          obj$demand$Region == r & obj$demand$Year == y & obj$demand$Jurisdiction == j,
          selected_demand_scenario # user selected, default = baseline = 3
          ] 
        RHS <- as.numeric(RHS)
        
        constraint_name <- paste(
          "EQ002|Demand", as.character(r), as.character(y), as.character(j), 
          sep = "|"
        )
        
        # add constraint to jwmodel object in list format
        constraint <- create_constraint(n_cols, coeffs, indices, ">=", RHS, constraint_name)
        obj$constraints$demand <- append(obj$constraints$demand, list(constraint))
        
      }
      
    }
    
  }
  
  ##### EQ-004 Cannot allocate more judges than available constraint #####
  
  df <- allocation_vars %>%
    dplyr::mutate(coeff = dplyr::if_else(.data$Judge == "U", 0, 1))
  
  df2 <- resource_vars %>%
    dplyr::mutate(coeff = dplyr::if_else(.data$io == "E", -1, 0))
  
  obj$constraints$allocate <- list()
  
  for (r in levels(obj$regions$Region)) {
    for (y in levels(obj$years$Years)) {
      for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
        
        indices <- which(df$Region == r & df$Year == y & df$Judge == t)
        coeffs <- df$coeff[indices]
        
        indices <- c(
          indices, 
          which(df2$Region == r & df2$Year == y & df2$Judge == t) + nrow(allocation_vars)
        )
        coeffs <- c(coeffs, df2$coeff[df2$Region == r & df2$Year == y & df2$Judge == t])
        
        constraint_name <- paste(
          "EQ004|Allocate", as.character(r), as.character(y), as.character(t), 
          sep = "|"
        )
        
        # add constraint to jwmodel object in list format
        constraint <- create_constraint(n_cols, coeffs, indices, "<=", rhs = 0, constraint_name)
        obj$constraints$allocate <- append(obj$constraints$allocate, list(constraint))
        
      }
    }
  }
  
  ##### EQ-001 In Post Judges constraint #####
  
  # constraint such that volume of judges in one year equals volume in previous
  # plus number recruited minus number leaving
  
  obj$constraints$inpost <- list()
  
  for (i_year in 1:n_years) {
    
    y <- levels(obj$years$Years)[i_year]
    
    if (i_year > 1) {
      y_prev <- levels(obj$years$Years)[i_year - 1]
    } else {
      y_prev <- NULL
    }
    
    for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
      
      for (r in levels(obj$regions$Region)) {
        
        if (i_year == 1) {
          # In first year the preceding year is a known constant (the number of
          # judges currently in post) rather than another variable. This requires
          # us to rearrange the equation, putting this constant on the RHS.
          # See Ref EQ-001b
          indices <- which(resource_vars$Region == r & resource_vars$Year == y & resource_vars$Judge == t)
          indices <- indices + nrow(allocation_vars)
          
          coeffs <- c(1, -1, 1)
          
          RHS <- obj$n_judges$`Number of Judges`[obj$n_judges$`Judge Type` == t & obj$n_judges$Region == r]
          
        } else {
          # Ref EQ-001a
          indices <- which(
            resource_vars$Year == y_prev & resource_vars$Region == r & 
            resource_vars$Judge == t & resource_vars$io == "E"
            )
          indices <- c(
            indices,
            which(resource_vars$Year == y & resource_vars$Region == r & resource_vars$Judge == t))
          indices <- indices + nrow(allocation_vars)
          
          coeffs <- c(1, -1, 1, -1)
          
          RHS <- 0
        }
        
        constraint_name <- paste(
          "EQ001|InPost", as.character(r), as.character(y), as.character(t), 
          sep = "|"
        )
        
        # add constraint to jwmodel object in list format
        constraint <- create_constraint(n_cols, coeffs, indices, "=", RHS, constraint_name)
        obj$constraints$inpost <- append(obj$constraints$inpost, list(constraint))
        
      }
    }
    
  }
  
  ##### EQ-003 Outgoing Judges constraint #####
  
  # Number of judges leaving post in a given year is equal to the number leaving 
  # the profession + the number who are moving to a different role.
  
  df_jp <- obj$judge_progression
  
  obj$constraints$outgoing <- list()
  
  for (y in levels(obj$years$Years)) { # for each year
    
    for (f in head(levels(obj$judge_types$`Judge Type`), -1)) { # for each judge type (from)
      
      coeffs <- NULL
      
      for (i in head(levels(obj$judge_types$`Judge Type`), -1)) { # for each judge type (into)
        
        i_coeff <- -df_jp$Proportion[df_jp$`Recruited Into` == i & 
                                       df_jp$`Recruited From` == f]
        
        if (length(i_coeff) == 0) { i_coeff <- 0 }
        
        if (i == f) {
          o_coeff <- 1
        } else {
          o_coeff <- 0
        }
        
        coeffs <- c(coeffs,
                    c(0, i_coeff, o_coeff))
        
      }
      
      indices <- which(resource_vars$Year == y) + 
        n_years * n_jurisdictions * (n_types + 1)
      
      # TODO refactor for speed? (I think this runs very slowly)
      RHS <- obj$judge_departures$`Expected Departures`[
        obj$judge_departures$`Judge Type` == f &
          obj$judge_departures$Year == y
        ] 
      
      constraint_name <- paste("EQ003-Outgoing", as.character(y), as.character(f), sep = "-")
      
      # add constraint to jwmodel object in list format
      constraint <- create_constraint(n_cols, coeffs, indices, "=", RHS, constraint_name)
      obj$constraints$outgoing <- append(obj$constraints$outgoing, list(constraint))
      
    }
    
  }
  
  ##### EQ-006 Judges only work in certain jurisdictions constraint ####
  
  # Ensure allocation variables for invalid judge-jurisdiction combos = zero.
  # Uses set.bounds; could have also set as a single constraint (working in 
  # conjunction with existing lower bounds = 0)
  
  df <- allocation_vars %>%
    dplyr::left_join(obj$alloc_limits,
                     by = c("Judge", "Jurisdiction")) %>%
    tidyr::replace_na(list(MaxPct = 0)) %>%
    dplyr::mutate(exclude = dplyr::case_when(
      .data$Judge == "U" ~ FALSE,
      .data$MaxPct <= 0 ~ TRUE,
      TRUE ~ FALSE
    ))
  
  indices <- which(df$exclude)
  
  ubounds <- rep(0, length(indices))
  
  # define as upper bounds of zero (lower bounds of zero defined elsewhere)
  bounds <- list(coefficients = ubounds, indices = indices)
  obj$bounds$upper <- append(obj$bounds$upper, list(bounds))
  
  ##### EQ-007 Judges must work a minimum number of sitting days each #####
  
  # This effectively ensures *all* judges are allocated to jurisdictions, 
  # lending their "avg sitting days" full quota to that jurisdiction's capacity
  
  df <- allocation_vars %>%
    dplyr::left_join(obj$sitting_days, 
                     by = c("Judge" = "Judge Type", "Year" = "Year")) %>%
    dplyr::select(.data$Year, .data$Jurisdiction, .data$Judge, coeff = .data$`Avg Sitting Days`) %>%
    dplyr::mutate(coeff = tidyr::replace_na(.data$coeff, 0))
  
  df2 <- resource_vars %>%
    dplyr::left_join(obj$sitting_days, by = c("Judge" = "Judge Type", "Year")) %>%
    dplyr::select(.data$Year, .data$Judge, .data$io, coeff = .data$`Avg Sitting Days`) %>%
    dplyr::mutate(coeff = dplyr::if_else(.data$io == "E", -.data$coeff, 0))
  
  obj$constraints$mindays <- list()
  
  for (y in levels(obj$years$Years)) {
    
    for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
      
      indices <- which(df$Year == y & df$Judge == t)
      coeffs <- df$coeff[indices]
      
      indices2 <- which(df2$Year == y & df2$Judge == t)
      coeffs2 <- df2$coeff[indices2]
      indices2 <- indices2 + n_years * n_jurisdictions * (n_types + 1)
      
      constraint_name <- paste("EQ007-MinDays", as.character(y), as.character(t), sep = "-")
      
      # add constraint to jwmodel object in list format
      constraint <- create_constraint(
        n_cols = n_cols,
        coeffs = c(coeffs, coeffs2),
        indices = c(indices, indices2),
        constraint_type = ">=",
        rhs = RHS,
        constraint_name
      )
      obj$constraints$mindays <- append(obj$constraints$mindays, list(constraint))
      
    }
    
  }
  
  ##### EQ-008 Constrain the maximum number of Judges recruited in one year #####
  
  df <- resource_vars
  
  obj$constraints$recruitcap <- list()
  
  for (y in levels(obj$years$Years)) {
    for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
      
      indices <- which(df$Year == y & df$Judge == t) + nrow(allocation_vars)
      coeffs <- c(0, 1, 0)
      
      RHS <- obj$recruit_limits[
        obj$recruit_limits$`Judge Type` == t &
          obj$recruit_limits$Year == y,
        selected_recruitment_scenario    # user-selected, default = column 3 
      ]
      RHS <- as.numeric(RHS) 
      
      constraint_name <- paste("EQ008-MaxRecruit", as.character(y), as.character(t), sep = "-")
      
      # add constraint to jwmodel object in list format
      constraint <- create_constraint(n_cols, coeffs, indices, "<=", RHS, constraint_name)
      obj$constraints$recruitcap <- append(obj$constraints$recruitcap, list(constraint))
      
    }
  }
  
  ##### EQ-009 Set limits on the proportion of demand allocated to different types of judge #####
  
  df <- allocation_vars %>%
    dplyr::left_join(obj$alloc_limits, 
                     by = c("Judge", "Jurisdiction")) %>%
    tidyr::replace_na(list(MinPct = 0, MaxPct = 0))

  obj$constraints$demand_ratio <- list()
  
  for (y in levels(obj$years$Years)) {
    for (j in levels(obj$jurisdictions$Jurisdiction)) {
      for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
  
        indices <- which(df$Year == y & df$Jurisdiction == j & df$Judge == t)
        coeffs <- obj$sitting_days[
          obj$sitting_days$Year == y & obj$sitting_days$`Judge Type` == t,
          3 # `Avg Sitting Days`
        ] %>% as.numeric()
        
        MinProportion <- df[
          df$Judge == t & df$Year == y & df$Jurisdiction == j,
          4 # MinPct
          ] %>% as.numeric()
        
        MaxProportion <- df[
          df$Judge == t & df$Year == y & df$Jurisdiction == j,
          5 # MinPct
          ] %>% as.numeric()
        
        Demand <- obj$demand[
          obj$demand$Jurisdiction == j & obj$demand$Year == y,
          selected_demand_scenario # dependant on user-selected (baseline = 3)
        ] %>% as.numeric()
        
        # apply minimums (EQ-009a)
        if (MinProportion > 0 & MinProportion <=1 ) {
          RHS <- c(MinProportion * Demand)
          
          constraint_name <- paste("EQ009-MinProp", as.character(y), as.character(j), as.character(t), sep = "-")
          
          # add constraint to jwmodel object in list format
          constraint <- create_constraint(n_cols, coeffs, indices, ">=", RHS, constraint_name)
          obj$constraints$demand_ratio <- append(obj$constraints$demand_ratio, list(constraint))
          
        }
        
        # apply maximums (EQ-009b)
        if (MaxProportion > 0 & MaxProportion < 1) {
          RHS <- c(MaxProportion * Demand)
          
          constraint_name <- paste("EQ009-MaxProp", as.character(y), as.character(j), as.character(t), sep = "-")
          
          # add constraint to jwmodel object in list format
          constraint <- create_constraint(n_cols, coeffs, indices, "<=", RHS, constraint_name)
          obj$constraints$demand_ratio <- append(obj$constraints$demand_ratio, list(constraint))
          
        }
      }
    }
  }
  
  ##### EQ-010 Override Hiring (optional minimum-hire constraint) #####
  
  # This applies a *minimum* number of judges to hire in each year if the user
  # has specified via the inclusion of the optional  "Override Hiring" input 
  # worksheet. Applied as a >= constraint rather than an = as with the latter
  # it is far too easy to create an infeasible problem by accident. In practice
  # this is likely to deliver a hiring solution very close to that specified
  # (often within rounding error distance)
  
  obj$constraints$min_hire <- list()
  
  if (!is.null(obj$override_hiring)) {
    df <- resource_vars %>%
      dplyr::left_join(obj$override_hiring, by = c("Year", "Judge"))
    
    for (y in levels(obj$years$Years)) {
      for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
        indices <- which(df$Year == y & df$Judge == t) + nrow(allocation_vars)
        coeffs <- c(0, 1, 0)
        # RHS = User-defined number hired for this year/judge combo
        RHS <- df[df$Year == y & df$Judge == t & df$io == "I", 4] 
        if (is.na(RHS)) {RHS <- 0} # set recruitment to zero if missing
        
        constraint_name <- paste("EQ010-MinHire", as.character(y), as.character(t), sep = "-")
        
        # add constraint to jwmodel object in list format
        constraint <- create_constraint(n_cols, coeffs, indices, ">=", RHS, constraint_name)
        obj$constraints$min_hire <- append(obj$constraints$min_hire, list(constraint))
        
      }
    }
  }
  
  # return updated model
  return(obj)
}
