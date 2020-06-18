# Defining an S3 class for Judicial Workforce Modelling


#source("utility-functions.R")

# Constructor function ----------------------------------------------------
#' Creates a jwmodel object
#' 
#' Recommended process to create and populate a \code{jwmodel} is to use this function
#' with default values to first create it, then use \code{\link{load_from_file}}
#' to populate it with parameter information contained in an appropriately 
#' formatted Excel file.
#' 
#' @param metadata extendable named list of model metadata, includes model 'name'
#'        and 'description' by default
#' @param judge_types data frame containing list of different judge types
#' @param jurisdictions list of jurisdictions; default empty
#' @param years list of years model will be run for, in accending order; default 
#'        empty
#' @param n_judges data frame of initial number of judges by judge type
#' @param judge_departures data frame of number of judges leaving the profession
#'        by judge type by year
#' @param sitting_days data frame of the sitting day capacity (avg, min, max) for
#'        each WTE judge, by judge type, by year
#' @param demand data frame of number of sitting days predicted to be required by 
#'        jurisdiction by year
#' @param judge_progression data frame holding the mapping of the proportion of 
#'        judges of one type who are recruited from the pool of another type
#' @param fixed_costs data frame... default empty
#' @param variable_costs data frame... default empty
#' @param penalty_costs data frame... default empty
#' @param constraints list of constraints (not used)
#' @param outputs list of ouputs (not used)
#' @param lpmodel default NULL
#' 
#' @return Returns an object of type \code{lpmodel}
#' @export
#' @examples 
#' # creates new, empty jwmodel object
#' mymodel <- jwmodel()
jwmodel <- function(metadata = list(name = "", description = ""),
                    judge_types = data.frame(), 
                    jurisdictions = c(),
                    years = c(), 
                    n_judges = data.frame(), 
                    judge_departures = data.frame(),
                    sitting_days = data.frame(), 
                    demand = data.frame(),
                    # TODO add scenarios
                    judge_progression = data.frame(),
                    fixed_costs = data.frame(),
                    variable_costs = data.frame(),
                    penalty_costs = data.frame(),
                    constraints = list(),
                    outputs = list(),
                    lpmodel = NULL
){
  
  value <- list(
    metadata = metadata,
    judge_types = judge_types,
    jurisdictions = jurisdictions,
    years = years,
    n_judges = n_judges,
    judge_departures = judge_departures,
    sitting_days = sitting_days,
    demand = demand,
    judge_progression = judge_progression,
    fixed_costs = fixed_costs,
    variable_costs = variable_costs,
    penalty_costs = penalty_costs,
    constraints = constraints,
    outputs = outputs,
    lpmodel = NULL
  )
  
  attr(value, "class") <- "jwmodel"
  
  return(value)
  
}



# Load method -------------------------------------------------------------

#' Loads data from a file into a jwmodel object
#' 
#' Loads data from the specified Excel file into a jwmodel object, priming it
#' for use. The Excel file must contain the expected worksheets with data in the
#' expected format.
#' 
#' @param obj a jwmodel object
#' @param filepath a string giving the full filepath name of the Excel file
#'        containing the parameter information to be loaded
#' 
#' @return Returns an object of type \code{lpmodel}
#' @export
#' @examples 
#' \dontrun{
#' mymodel <- load_from_file(mymodel, "modelparams.xlsx")
#' }
load_from_file <- function(obj, filepath) {
  UseMethod("load_from_file")
}

#' @export
load_from_file.default <- function(obj, filepath) {
  cat("'load_from_file' function can only be used on jwmodel objects")
}

#' @importFrom rlang .data
#' @export
load_from_file.jwmodel <- function(obj, filepath) {
  
  readxl::excel_sheets(filepath)
  
  wslist <- c("Judge Types", "Jurisdictions", "Years", "Number of Judges", 
              "Expected Departures", "Sitting Day Capacity", "Baseline Demand",
              "Judge Progression", "Fixed Costs", "Variable Costs")
  
  if (length(wslist) > length(wslist %in% readxl::excel_sheets(filepath))) {
    warning("Incomplete file: data not loaded")
    return(obj)
  }
  
  # load judge types, converting into a factor with order as loaded
  df <- readxl::read_excel(filepath, sheet = wslist[1])
  judge_levels <- c(unique(df$`Judge Type`), "U")
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  obj$judge_types <- df
  
  # load jurisdictions, converting into a factor with order as loaded
  df <- readxl::read_excel(filepath, sheet = wslist[2])
  jurisdiction_levels <- unique(df$Jurisdiction)
  df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
  obj$jurisdictions <- df
  
  # load years
  df <- readxl::read_excel(filepath, sheet = wslist[3])
  year_levels <- unique(df$Years)
  df$Years <- factor(df$Years, levels = year_levels)
  obj$years <- df
  
  # load number of judges initialy employed and available
  df <- readxl::read_excel(filepath, sheet = wslist[4])
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  obj$n_judges <- df
  
  # load expected departures data
  df <- readxl::read_excel(filepath, sheet = wslist[5])
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  df$Year <- factor(df$Year, levels = year_levels)
  obj$judge_departures <- df
  
  # load sitting day capacity data
  df <- readxl::read_excel(filepath, sheet = wslist[6])
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  df$Year <- factor(df$Year, levels = year_levels)
  obj$sitting_days <- df
  
  # load baseline demand data
  df <- readxl::read_excel(filepath, sheet = wslist[7])
  df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
  df$Year <- factor(df$Year, levels = year_levels)
  obj$demand <- df
  
  # load judge progression
  df <- readxl::read_excel(filepath, sheet = wslist[8])
  df$`Recruited Into` <- factor(df$`Recruited Into`, levels = judge_levels)
  df$`Recruited From` <- factor(df$`Recruited From`, levels = judge_levels)
  obj$judge_progression <- df
  
  # load fixed cost data
  df <- readxl::read_excel(filepath, sheet = wslist[9])
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  obj$fixed_costs <- df
  
  # load variable cost data
  df <- readxl::read_excel(filepath, sheet = wslist[10])
  df$Judge <- factor(df$Judge, levels = judge_levels)
  df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
  obj$variable_costs <- df
  
  if (c("Penalty Costs") %in% readxl::excel_sheets(filepath)) {
    # load penalty costs from file if they are defined there
    df <- readxl::read_excel(filepath, sheet = "Penalty Costs")
    df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
    names(df)[2] <- "Penalty Cost"
    df$Judge <- factor("U", levels = judge_levels)
    obj$penalty_costs <- df
    
  } else {
    # derive sensible penalty costs = double max fee for given jurisdiction
    df <- obj$variable_costs %>%
      dplyr::group_by(.data$Jurisdiction) %>%
      dplyr::summarise(`Penalty Cost` = 2 * max(.data$`Avg Sitting Day Cost`))
    df$Judge <- factor("U", levels = judge_levels)
    obj$penalty_costs <- df
  }
  
  if (c("Model Info") %in% readxl::excel_sheets(filepath)) {
    df <- readxl::read_excel(filepath, sheet = "Model Info", 
                             col_names = c("field", "value"))
    # TODO refactor code to be more flexible and add other info into metadata
    # should the user specify any
    obj$metadata$name <- df[1,2]
    obj$metadata$description <- df[2,2]
  } else {
    # name it after load datetime if no name is otherwise specified
    obj$metadata$name <- as.character(Sys.time())
  }
  
  return(obj)
}

# Initialise method -------------------------------------------------------

#' Initialises the optimisation for a jwmodel with all parameters defined
#' 
#' Once a jwmodel object has been loaded with all the necessary parameter
#' information, it must be initialised with this function to ready it for 
#' optimisation.
#' 
#' @param obj jwmodel object to be initialised
initialise <- function(obj) {
  UseMethod("initialise")
}

initialise.default <- function(obj) {
  cat("'initialise' function can only be used on jwmodel objects")
}

# function to initialise optimisation: derives and creates objective function
# and all constraints; loads them into an lpSolveAPI object ready to optimise
#' @importFrom rlang .data
#' @importFrom  utils head
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


# Optimise method ---------------------------------------------------------

#' Runs the optimisation for a jwmodel
#' 
#' Requires the jwmodel to be initialised. Executes the LP optimisation and
#' rights results into the jwmodel object which is returned.
#' 
#' @param obj jwmodel object to be optimised
# generic function definition
optimise <- function(obj) {
  UseMethod("optimise")
}

optimise.default <- function(obj) {
  cat("'optimise' function can only be used on jwmodel objects")
}

#' @importFrom rlang .data
optimise.jwmodel <- function(obj) {
  if (is.null(obj$lpmodel)) {
    initialise(obj)
  }
  
  # add datetime stamp to model metadata
  obj$metadata$lastrun$date <- Sys.time()
  
  # solve
  timed <- system.time({
    solve_outcome <- solve(obj$lpmodel)
  })
  
  # record metadata on the optimisation
  obj$metadata$lastrun$duration <- timed
  
  obj$metadata$lastrun$outcome <- solve_outcome
  
  status_codes <- c(
    "0" = "optimal solution found",
    "1" = "the model is suboptimal",
    "2" = "the model is infeasible",
    "3" = "the model is unbounded",
    "4" = "the model is degenerate",
    "5" = "numerical failure encountered",
    "6" = "process aborted",
    "7" = "timeout",
    "9" = "the model was solved by presolve",
    "10" = "the branch and bound routine failed",
    "11" = "the branch and bound was stopped because of a break-at-first or break-at-value",
    "12" = "a feasible branch and bound solution was found",
    "13" = "no feasible branch and bound solution was found"
  )
  
  obj$metadata$lastrun$outcomedesc <-status_codes[[as.character(solve_outcome)]]
  
  # parse results into dataframes for easy onward analysis
  
  allocation_vars <- allocation_vars_template(obj)
  resource_vars <- resource_vars_template(obj)
  n_alloc <- nrow(allocation_vars)
  n_res <- nrow(resource_vars)
  
  allocation_output <- allocation_vars %>%
    dplyr::mutate(Allocated = lpSolveAPI::get.variables(obj$lpmodel)[1:n_alloc]) %>%
    dplyr::inner_join(obj$variable_costs, by = c("Jurisdiction", "Judge")) %>%
    dplyr::inner_join(obj$sitting_days, by = c("Judge" = "Judge Type", "Year")) %>%
    #select(-`Min Sitting Days`, -`Max Sitting Days`) %>%
    dplyr::mutate(`Total Sitting Days` = .data$Allocated * .data$`Avg Sitting Days`,
           `Total Fee Cost` = .data$`Total Sitting Days` * .data$`Avg Sitting Day Cost`)
  
  obj$outputs$allocation_output <- allocation_output
  
  resource_output <- resource_vars %>%
    dplyr::mutate(
      status = dplyr::case_when(
        io == "E" ~ "Available",
        io == "I" ~ "Hired (in)",
        io == "O" ~ "Departed (out)"
      ),
      FTE = lpSolveAPI::get.variables(obj$lpmodel)[(n_alloc + 1):(n_alloc + n_res)],
    ) %>%
    dplyr::select(-.data$io)
  
  obj$outputs$resource_output <- resource_output
  
  return(obj)
}


# Simulation --------------------------------------------------------------

# generic function definition
simulate <- function(obj, iterations, seed_num) {
  UseMethod("simulate")
}

simulate.default <- function(obj, iterations, seed_num) {
  cat("'simulate' function can only be used on jwmodel objects")
}

simulate.jwmodel <- function(obj, iterations = 100, seed_num = NULL) {
  
  # set & record random generator's seed number (for reproducability)
  if (is.null(seed_num) | !is.numeric(seed_num)) {
    # if no seed given, create one and record it
    seed <- as.numeric(Sys.time())
    obj$metadata$seed <- seed
  } else {
    seed <- as.integer(seed_num)
  }
  
  allocations <- list()
  solver_return_values <- c()
  
  # generate demand scenarios to be simulated
  base_demand_est <- obj$demand$`Sitting Days`
  scenarios <- list()
  
  for (i in 1:iterations) {
    # TODO modify code to simulate here to include four summed components:
    # (1) original baseline demand prediction +
    # (2) variation related to differences in average growth rate +
    # (3) variation related to random step change chances +
    # (4) variation related to random noise
    
    noise <- stats::runif(length(base_demand_est), 10000, 25000)
    
    s <- base_demand_est + noise
    
    # prevent demand value from ever being negative
    s[s<0] <- 0
    
    scenarios <- append(scenarios, list(s))
  }
  
  # fix available resources variables at the levels they were originally optimised to
  n_alloc_vars <- nrow(obj$years) * nrow(obj$jurisdictions) * 
    (nrow(obj$judge_types) + 1)
  n_resource_vars <- nrow(obj$years) * nrow(obj$judge_types) * 3
  
  fixed_resources <- obj$outputs$resource_output
  fixed_resources <- fixed_resources$FTE[seq(1, n_resource_vars, 3)]
  
  indices <- (n_alloc_vars + 1):(n_alloc_vars + n_resource_vars)
  indices <- indices[seq(1, n_resource_vars, 3)]
  
  lpSolveAPI::set.bounds(obj$lpmodel, lower = fixed_resources, upper = fixed_resources,
             columns = indices)
  
  
  # re-run optmisation, optimising only for allocation based on fixed resources,
  # once for each scenario generated
  for (i in 1:iterations) {
    
    # overwrite demand constraints with scenario's demand values
    lpSolveAPI::set.rhs(obj$lpmodel, scenarios[[i]], obj$constraints$demand)
    
    # (re)optimise model
    obj <- optimise(obj)
    solver_return_values <- c(solver_return_values, obj$metadata$lastrun$outcome)
    
    # save simulation output (allocation variable values)
    allocations <- append(allocations, list(obj$outputs$allocation_output$Allocated))
    
  }
  
  # TODO save outputs for posterity
  
  return(obj)
}

