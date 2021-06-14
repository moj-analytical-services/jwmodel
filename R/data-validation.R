#' @importFrom dplyr %>%
#' @importFrom rlang .data
check_loaded_data <- function(jw) {
  
  # create data frame to hold error info
  errors_found <- data.frame(worksheet = character(), severity = character(),
                             errorMessage = character())
  
  # apply checks upon data loaded from each worksheet
  
  ### judge_types ----
  validJudges <- jw$judge_types$`Judge Type` %>% levels() %>% head(-1)
  nJudges <- length(validJudges)
  
  ### jurisdictions ----
  validJurisdictions <- jw$jurisdictions$Jurisdiction %>% levels()
  nJurisdictions <- length(validJurisdictions)
  
  ### years ----
  validYears <- jw$years$Years %>% levels()
  nYears <- length(validYears)
  
  ### regions ----
  validRegions <- jw$regions$Region %>% levels()
  nRegions <- length(validRegions)
  
  ### Common parameters ----
  validation_params <- list(
    validJudges = validJudges, 
    validJurisdictions = validJurisdictions,
    validRegions = validRegions,
    validYears = validYears
  )
  
  ### n_judges ----
  validation_params[["expectedNRow"]] <- nJudges * nRegions
  
  errors_found <- apply_validation_checks(
    rule_file = "Number_of_Judges.yaml",
    df_to_check = jw$n_judges,
    params = validation_params
  ) %>%
    dplyr::bind_rows(errors_found)
  
  
  ### judge_departures ----
  validation_params[["expectedNRow"]] <- nJudges * nRegions * nYears
  
  errors_found <- apply_validation_checks(
    rule_file = "Expected_Departures.yaml",
    df_to_check = jw$judge_departures,
    params = validation_params
  ) %>%
    dplyr::bind_rows(errors_found)
  
  
  ### sitting_days ----
  validation_params[["expectedNRow"]] <- nJudges * nRegions * nYears
  
  errors_found <- apply_validation_checks(
    rule_file = "Sitting_Day_Capacity.yaml",
    df_to_check = jw$sitting_days,
    params = validation_params
  ) %>%
    dplyr::bind_rows(errors_found)
  
  ### demand ----
  validation_params[["expectedNRow"]] <- nJurisdictions * nRegions * nYears
  
  errors_found <- apply_validation_checks(
    rule_file = "Baseline_Demand.yaml",
    df_to_check = jw$demand,
    params = validation_params
  ) %>%
    dplyr::bind_rows(errors_found)
  
  ### judge_progression ----
  # TODO
  
  ### recruit_limits ----
  validation_params[["expectedNRow"]] <- nJudges * nRegions * nYears
  
  errors_found <- apply_validation_checks(
    rule_file = "Recruitment_Limits.yaml",
    df_to_check = jw$recruit_limits,
    params = validation_params
  ) %>%
    dplyr::bind_rows(errors_found)
  
  ### alloc_limits ----
  errors_found <- apply_validation_checks(
    rule_file = "Allocation_Limits.yaml",
    df_to_check = jw$alloc_limits,
    params = validation_params
  ) %>%
    dplyr::bind_rows(errors_found)
  
  
  # fixed_costs
  validation_params[["expectedNRow"]] <- nJudges * nRegions
  
  errors_found <- apply_validation_checks(
    rule_file = "Fixed_Costs.yaml",
    df_to_check = jw$fixed_costs,
    params = validation_params
  ) %>%
    dplyr::bind_rows(errors_found)
  
  # variable_costs
  
  # penalty_costs
  
  # per_sitting_day
  
  # override_hiring
  
  
  # return data frame of errors & warnings from loaded data
  return(errors_found)
}