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
                  "Penalty Costs", "Model Info")
  
  listnames <- c("judge_types", "jurisdictions", "years", "n_judges", 
                 "judge_departures", "sitting_days", "demand",
                 "judge_progression", "fixed_costs", "variable_costs",
                 "recruit_limits", "alloc_limits", 
                 "penalty_costs", "metadata")
  
  mandatory <- c(TRUE, TRUE, TRUE, TRUE, 
                 TRUE, TRUE, TRUE,
                 TRUE, TRUE, TRUE,
                 TRUE, TRUE, 
                 FALSE, TRUE)
  
  lookup <- dplyr::as_tibble(sheetnames) %>%
    dplyr::bind_cols(dplyr::as_tibble(listnames)) %>%
    dplyr::bind_cols(dplyr::as_tibble(mandatory))
    
    names(lookup) <- c("sheetnames", "listnames", "required")
  
  return(lookup)
}