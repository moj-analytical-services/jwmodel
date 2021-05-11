# Use of ".data" explained here: 
# https://www.r-bloggers.com/no-visible-binding-for-global-variable/

# Where it is not possible or desireable to use explicit :: for functions,
# include an @importFrom to avoid R package build check complaining
# Tip sourced from: 
# https://stackoverflow.com/questions/58026637/no-visible-global-function-definition-for

#' @importFrom dplyr %>%
#' @importFrom rlang .data
allocation_vars_template <- function(obj) {
  # generates correctly ordered unique combos of Year/Jurisdiction/Judge Type
  # for "Allocation Variables", for use in dplyr joins
  if (class(obj) == "jwmodel") {
    
    allocation_vars <- tidyr::expand_grid(
      Year = factor(
        levels(obj$years$Years), 
        levels = levels(obj$years$Years)
      ),
      Jurisdiction = factor(
        levels(obj$jurisdictions$Jurisdiction),
        levels = levels(obj$jurisdictions$Jurisdiction)
      ),
      Judge = factor(
        levels(obj$judge_types$`Judge Type`), 
        levels = levels(obj$judge_types$`Judge Type`)
      )
    ) %>%
      dplyr::arrange(.data$Year, .data$Jurisdiction, .data$Judge)
    
    # if a regional split is specified, expand list to all combinations of:
    # Region / Year / Jurisdiction / Judge
    if (!is.null(obj$regions)) {
      allocation_vars <- tidyr::expand_grid(
        Region = factor(
          levels(obj$region$Region), 
          levels = levels(obj$region$Region)
        ),
        allocation_vars
      )
    }
    
    return(allocation_vars)
    
  } else {
    warning("'allocation_vars_template' function received an invalid parameter")
    return(NULL)
  }
  
}

#' @importFrom dplyr %>%
#' @importFrom rlang .data
resource_vars_template <- function(obj) {
  # generates correctly ordered unique combos of Year/Judge Type/In-Out Status
  # for "Resource Variables", for use in dplyr joins
  # NB for In-Out Status (io), "E" = existing / in post, "I" = incoming,
  # "O" = outgoing
  if (class(obj) == "jwmodel") {
    
    resource_vars <- tidyr::expand_grid(
      Year = factor(
        levels(obj$years$Years), 
        levels = levels(obj$years$Years)
      ),
      Judge = factor(
        levels(obj$judge_types$`Judge Type`), 
        levels = levels(obj$judge_types$`Judge Type`)
      ),
      io = c("E", "I", "O")
    ) %>%
      dplyr::arrange(.data$Year, .data$Judge, .data$io) %>%
      dplyr::filter(.data$Judge != "U")
    
    # if a regional split is specified, expand list to all combinations of:
    # Region / Year / Jurisdiction / Judge
    if (!is.null(obj$regions)) {
      resource_vars <- tidyr::expand_grid(
        Region = factor(
          levels(obj$region$Region), 
          levels = levels(obj$region$Region)
        ),
        resource_vars
      )
    }
    
    return(resource_vars)
    
  } else {
    warning("'resource_vars_template' function received an invalid parameter")
    return(NULL)
  }
}

#' @importFrom dplyr %>%
#' @importFrom rlang .data
sim_growth_delta <- function(baseline, growthci = 0.01) {
  
  # baseline = dataframe of demand which constitutes the 'baseline'
  # (data frame has fields: Jurisdiction; Year; `Sitting Days`)
  
  # growthci = growth credible interval, expressed as a plus/minus figure,
  #  e.g. default 0.01 means growth could be +/- 1 percentage point from the
  # implied actual in the baseline
  
  df <- obj$demand %>%
    #pivot_wider(names_from = Jurisdiction, values_from = `Sitting Days`)
    dplyr::group_by(.data$Jurisdiction) %>%
    dplyr::mutate(year_num = as.numeric(.data$Year)) %>%
    dplyr::do(trend = stats::lm(log(.data$`Sitting Days`) ~ .data$year_num, data = .)) #%>%
    # rowwise() %>%
    # tidy(trend)
    
    lm_test <- stats::lm(`Sitting Days` ~ year_num, df)
    lm_test$coefficients
    
    return(NULL)
}

#' @importFrom dplyr %>%
#' @importFrom rlang .data
list_of_input_sheets <- function() {
  
  sheetnames <- c("Judge Types", "Jurisdictions", "Years", "Number of Judges", 
                  "Expected Departures", "Sitting Day Capacity", "Baseline Demand",
                  "Judge Progression", "Fixed Costs", "Variable Costs",
                  "Recruitment Limits", "Allocation Limits", 
                  "Penalty Costs", "Model Info", "Override Hiring")
  
  listnames <- c("judge_types", "jurisdictions", "years", "n_judges", 
                 "judge_departures", "sitting_days", "demand",
                 "judge_progression", "fixed_costs", "variable_costs",
                 "recruit_limits", "alloc_limits", 
                 "penalty_costs", "metadata", "override_hiring")
  
  mandatory <- c(TRUE, TRUE, TRUE, TRUE, 
                 TRUE, TRUE, TRUE,
                 TRUE, TRUE, TRUE,
                 TRUE, TRUE, 
                 FALSE, TRUE, FALSE)
  
  lookup <- dplyr::as_tibble(sheetnames) %>%
    dplyr::bind_cols(dplyr::as_tibble(listnames)) %>%
    dplyr::bind_cols(dplyr::as_tibble(mandatory))
    
    names(lookup) <- c("sheetnames", "listnames", "required")
  
  return(lookup)
}

create_constraint <- function(n_cols, coeffs, indices, constraint_type, rhs, name = NULL) {
  
  # check validity of parameters
  if (
    # check data types are as they should be
    !is.numeric(n_cols) | !is.numeric(coeffs) | !is.numeric(indices) | !is.numeric(rhs) | 
    !(is.numeric(constraint_type) | is.character(constraint_type)) |
    n_cols < 1 | # number of variables (columns) must be > zero
    (min(indices) < 1 | max(indices) > n_cols) # indices must be within valid range
  ) {
    
    warning("Invalid parameters passed to 'create_constriant' function")
    return(NULL)
    
  } else { # parameters passed validity check
    
    constraint <- list()
  
    # coefficients must be a numeric vector with length equal to number of variables
    constraint$coefficients <- rep(0, n_cols)
    constraint$coefficients[as.integer(indices)] <- coeffs
    
    # constraint type must be a numeric or character value from the set
    # {1 = "<="; 2 = ">="; 3 = "="}
    constraint$type <- constraint_type
    
    # numeric value specifying the right-hand side of the constraint
    constraint$rhs <- rhs
    
    # add name of constraint if provided
    if (!is.null(name) & is.character(name)) {
      constraint$name <- name
    }
    
    return(constraint)
  
  }
  
}

create_objective_function <- function(n_cols, coeffs, indices) {
  # check validity of parameters
  if (
    # check data types are as they should be
    !is.numeric(n_cols) | !is.numeric(coeffs) | !is.numeric(indices) |
    n_cols < 1 | # number of variables (columns) must be > zero
    (min(indices) < 1 | max(indices) > n_cols) # indices must be within valid range
  ) {
    
    warning("Invalid parameters passed to 'create_objective_function' function")
    return(NULL)
    
  } else { # parameters passed validity check
    
    obj_fct <- list()
    
    # coefficients must be a numeric vector with length equal to number of variables
    obj_fct$coefficients <- rep(0, n_cols)
    obj_fct$coefficients[as.integer(indices)] <- coeffs
    
    return(obj_fct)
    
  }
}

#' @importFrom dplyr %>%
#' @importFrom rlang .data
create_column_names <- function(obj) {
  
  alloc_cols <- allocation_vars_template(obj) %>%
    tidyr::unite(col_name, dplyr::everything(), sep = "|") %>%
    dplyr::mutate(col_name = paste0("Alloc|", col_name))
  
  res_cols <- resource_vars_template(obj) %>%
    tidyr::unite(col_name, dplyr::everything(), sep = "|") %>%
    dplyr::mutate(col_name = paste0("Resource|", col_name))
  
  new_col_names <- dplyr::bind_rows(
    alloc_cols,
    res_cols,
  )
  
  # return a vector of column names for model variables in the correct order 
  # (excluding any slack variables)
  return(new_col_names$col_name)
}


prepend <- function(input_df, input_value, col_name) {
  df <- input_df
  df[col_name] <- input_value
  df <- df[,c(ncol(df), 1:ncol(df)-1)]
}