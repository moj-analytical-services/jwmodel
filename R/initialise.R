
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
  
  n_vars <- (n_years * n_jurisdictions * (n_types + 1)) +
    (n_years * n_types * 3)
  
  # generates correctly ordered unique combos of Year/Jurisdiction/Judge Type
  # for "Allocation Variables", for use in dplyr joins
  allocation_vars <- tidyr::expand_grid(
    Year = levels(obj$years$Years), 
    Jurisdiction = levels(obj$jurisdictions$Jurisdiction),
    Judge = levels(obj$judge_types$`Judge Type`)
  ) %>%
    dplyr::arrange(.data$Year, .data$Jurisdiction, .data$Judge)
  
  # generates corrctly ordered unique combos of Year/Judge Type/In-Out Status
  # for "Resource Variables", for use in dplyr joins
  # NB for In-Out Status (io), "E" = existing / in post, "I" = incoming,
  # "O" = outgoing
  resource_vars <- tidyr::expand_grid(
    Year = levels(obj$years$Years),
    Judge = levels(obj$judge_types$`Judge Type`),
    io = c("E", "I", "O")
  ) %>%
    dplyr::arrange(.data$Year, .data$Judge, .data$io) %>%
    dplyr::filter(.data$Judge != "U")
  
  # initialise model
  lp.wmodel <- lpSolveAPI::make.lp(0, n_vars)
  
  # add bounds to prevent any variable taking a value less t han zero
  lpSolveAPI::set.bounds(lp.wmodel, lower = rep(0, n_vars))
  
  # TODO check if these are now redundant
  index_years <- c(0:(n_years-1)) * n_types * 3
  index_types <- c(0:(n_types-1)) * 3
  i_resource_vars <- n_years * n_jurisdictions * (n_types + 1) + 1
  
  ##### set Objective Function #####
  # Minimise total 'cost': ref EQ000
  
  # define variable cost (fees) coefficients: NB includes unallocated penalty costs
  
  # coefficient = avg sitting days (capacity) x avg per-sitting-day cost
  # (result ordered by Year > Jurisdiction > Judge Type)
  df <- allocation_vars %>%
    dplyr::left_join(obj$variable_costs, by = c("Judge", "Jurisdiction")) %>%
    dplyr::left_join(obj$sitting_days, by = c("Judge" = "Judge Type", "Year")) %>%
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
    dplyr::left_join(obj$fixed_costs, by = c("Judge" = "Judge Type")) %>%
    dplyr::mutate(coeff = dplyr::if_else(.data$io == "E", .data$`Avg Annual Cost`, 0))
  
  # penalty cost per-hire must exceed max difference in avg salary between
  # judges of different types
  # per_hire_penalty <- max(obj$fixed_costs$`Avg Annual Cost`) -
  #   min(obj$fixed_costs$`Avg Annual Cost`)
  
  # interleve with penalty cost coefficients for 'income' and zero cost for
  # 'outgoing' resource variables
  # s_costs_yt <- c(rbind(df$coeff, 0, 0))
  s_costs_yt <- df$coeff
  
  # combine coefficients into a single vector in the correct order
  coeffs <- c(f_costs_yjt, s_costs_yt)
  indices <- 1:length(coeffs)
  
  # create objective function using calculated coefficients
  lpSolveAPI::set.objfn(lp.wmodel, obj = coeffs, indices = indices)
  
  ##### Demand must be satisfied constraint #####
  # See Ref EQ-002b
  
  df <- allocation_vars %>%
    dplyr::left_join(obj$sitting_days, 
                     by = c("Judge" = "Judge Type", "Year" = "Year")) %>%
    dplyr::select(.data$Year, .data$Jurisdiction, .data$Judge, coeff = .data$`Avg Sitting Days`) %>%
    dplyr::mutate(coeff = tidyr::replace_na(.data$coeff, 1))
  
  start_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1] + 1
  
  for (y in levels(obj$years$Years)) {
    
    for (j in levels(obj$jurisdictions$Jurisdiction)) {
      
      indices <- which(df$Year == y & df$Jurisdiction == j)
      coeffs <- df$coeff[indices]
      
      RHS <- obj$demand$`Sitting Days`[
        obj$demand$Year == y & obj$demand$Jurisdiction == j
        ]
      
      lpSolveAPI::add.constraint(lp.wmodel, xt = coeffs, indices = indices,
                                 type = '>=', rhs = RHS)
      
    }
    
  }
  
  end_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1]
  
  obj$constraints$demand <- c(start_row:end_row)
  
  ##### Cannot allocate more judges than available constraint #####
  # Ref EQ-004
  
  df <- allocation_vars %>%
    dplyr::mutate(coeff = dplyr::if_else(.data$Judge == "U", 0, 1))
  
  df2 <- resource_vars %>%
    dplyr::mutate(coeff = dplyr::if_else(.data$io == "E", -1, 0))
  
  start_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1] + 1
  
  for (y in levels(obj$years$Years)) {
    
    for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
      
      indices <- which(df$Year == y & df$Judge == t)
      coeffs <- df$coeff[indices]
      
      indices <- c(indices, 
                   which(df2$Year == y & df2$Judge == t) + nrow(allocation_vars))
      coeffs <- c(coeffs, df2$coeff[df2$Year == y & df2$Judge == t])
      
      lpSolveAPI::add.constraint(lp.wmodel, xt = coeffs, indices = indices,
                                 type = '<=', 0)
      
    }
    
  }
  
  end_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1]
  
  obj$constraints$allocate <- c(start_row:end_row)
  
  ##### In Post Judges constraint #####
  # Ref EQ-001
  
  # contraint such that volume of judges in one year equals volume in previous
  # plus number recruited minus number leaving
  
  start_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1] + 1
  
  for (i_year in 1:n_years) {
    
    y <- levels(obj$years$Years)[i_year]
    
    if (i_year > 1) {
      y_prev <- levels(obj$years$Years)[i_year - 1]
    } else {
      y_prev <- NULL
    }
    
    for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
      
      if (i_year == 1) {
        # In first year the preceding year is a known constant (the number of
        # judges currently in post) rather than another variable. This requires
        # us to rearrange the equation, putting this constant on the RHS.
        # See Ref EQ-001b
        indices <- which(resource_vars$Year == y & resource_vars$Judge == t)
        indices <- indices + n_years * n_jurisdictions * (n_types + 1)
        
        coeffs <- c(1, -1, 1)
        
        RHS <- obj$n_judges$`Number of Judges`[obj$n_judges$`Judge Type` == t]
        
      } else {
        # Ref EQ-001a
        indices <- which(resource_vars$Year == y_prev & 
                           resource_vars$Judge == t & resource_vars$io == "E")
        indices <- c(indices,
                     which(resource_vars$Year == y & resource_vars$Judge == t))
        indices <- indices + n_years * n_jurisdictions * (n_types + 1)
        
        coeffs <- c(1, -1, 1, -1)
        
        RHS <- 0
      }
      
      lpSolveAPI::add.constraint(lp.wmodel, xt = coeffs, indices = indices,
                                 type = "=", rhs = RHS)
      
    }
    
  }
  
  end_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1]
  
  obj$constraints$inpost <- c(start_row:end_row)
  
  ##### Outgoing Judges constraint #####
  
  # Number of judges leaving post in a given year is equal to the number leaving 
  # the profession + the number who are moving to a different role.
  # See Ref EQ-003
  
  df_jp <- obj$judge_progression
  
  start_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1] + 1
  
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
      
      lpSolveAPI::add.constraint(lp.wmodel, xt = coeffs, indices = indices,
                                 type = "=", rhs = RHS)
      
    }
    
  }
  
  end_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1]
  
  obj$constraints$outgoing <- c(start_row:end_row)
  
  ##### Judges only work in certain jurisdictions constraint ####
  
  # Ensure allocation variables for invalid judge-jurisdiction combos = zero.
  # Uses set.bounds; could have also set as a single constraint (working in 
  # conjunction with existing lower bounds = 0)
  # Ref EQ-006
  
  df <- allocation_vars %>%
    dplyr::left_join(obj$variable_costs, by = c("Judge", "Jurisdiction")) %>%
    dplyr::mutate(exclude = dplyr::case_when(
      .data$Judge == "U" ~ FALSE,
      !is.na(.data$`Avg Sitting Day Cost`) ~ FALSE,
      TRUE ~ TRUE
    ))
  
  indices <- which(df$exclude)
  
  ubounds <- rep(0, length(indices))
  
  lpSolveAPI::set.bounds(lp.wmodel, upper = ubounds, columns = indices)
  
  ##### Judges must work a minimum number of sitting days each #####
  
  df <- allocation_vars %>%
    dplyr::left_join(obj$sitting_days, 
                     by = c("Judge" = "Judge Type", "Year" = "Year")) %>%
    dplyr::select(.data$Year, .data$Jurisdiction, .data$Judge, coeff = .data$`Avg Sitting Days`) %>%
    dplyr::mutate(coeff = tidyr::replace_na(.data$coeff, 0))
  
  df2 <- resource_vars %>%
    dplyr::left_join(obj$sitting_days, by = c("Judge" = "Judge Type", "Year")) %>%
    dplyr::select(.data$Year, .data$Judge, .data$io, coeff = .data$`Min Sitting Days`) %>%
    dplyr::mutate(coeff = dplyr::if_else(.data$io == "E", -.data$coeff, 0))
  
  start_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1] + 1
  
  for (y in levels(obj$years$Years)) {
    
    for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
      
      indices <- which(df$Year == y & df$Judge == t)
      coeffs <- df$coeff[indices]
      
      indices2 <- which(df2$Year == y & df2$Judge == t)
      coeffs2 <- df2$coeff[indices2]
      indices2 <- indices2 + n_years * n_jurisdictions * (n_types + 1)
      
      lpSolveAPI::add.constraint(lp.wmodel, xt = c(coeffs, coeffs2),
                                 indices = c(indices, indices2), type = ">=", rhs = 0)
      
    }
    
  }
  
  end_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1]
  
  obj$constraints$mindays <- c(start_row:end_row)
  
  ##### Contrain the maximum number of Judges recruited in one year #####
  df <- resource_vars
  
  start_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1] + 1
  
  max_hires <- 50 ### TODO un-hard code
  
  for (y in levels(obj$years$Years)) {
    for (t in head(levels(obj$judge_types$`Judge Type`), -1)) {
      
      indices <- which(df$Year == y & df$Judge == t) + nrow(allocation_vars)
      coeffs <- c(0, 1, 0)
      
      lpSolveAPI::add.constraint(lp.wmodel, xt = coeffs, indices = indices,
                                 type = "<=", rhs = max_hires)
    }
  }
  
  end_row <- lpSolveAPI::dim.lpExtPtr(lp.wmodel)[1]
  
  obj$constraints$recruitcap <- c(start_row:end_row)
  
  ##### update model #####
  obj$lpmodel <- lp.wmodel
  
  return(obj)
}