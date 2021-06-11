#' @importFrom dplyr %>%
#' @importFrom rlang .data
check_loaded_data <- function(jw) {
  
  # create data frame to hold error info
  errors_found <- data.frame(worksheet = character(), severity = character(),
                             errorMessage = character())
  
  # apply checks upon data loaded from each worksheet
  
  ### judge_types ----
  validJudges <- jw$judge_types$`Judge Type` %>% levels() %>% head(-1)
  
  ### jurisdictions ----
  validJurisdictions <- jw$jurisdictions$Jurisdiction %>% levels()
  
  ### years ----
  validYears <- jw$years$Years %>% levels()
  
  ### regions ----
  validRegions <- jw$regions$Region %>% levels()
  
  ### n_judges ----
  file_path <- system.file("validation_rules", "Number_of_Judges.yaml", package = "jwmodel")
  rules <- validate::validator(.file = file_path)
  checked <- validate::confront(
    jw$n_judges, rules, 
    ref = list(
      validJudges = validJudges, 
      validRegions = validRegions,
      expectedNRow = length(validJudges) * length(validRegions)
    )
  )
  number_of_tests_failed <- length(which(validate::summary(checked)$fails>0))
  
  # append list of (custom) error messages from failed tests, if any
  if (number_of_tests_failed > 0) {
    err_df <- validate::meta(rules[which(validate::summary(checked)$fails>0)]) %>% 
      dplyr::select(worksheet, severity, errorMessage)
    
    errors_found <- errors_found %>%
      dplyr::bind_rows(err_df)
  }
  
  ### judge_departures ----
  file_path <- system.file("validation_rules", "Expected_Departures.yaml", package = "jwmodel")
  rules <- validate::validator(.file = file_path)
  checked <- validate::confront(
    jw$judge_departures, rules, 
    ref = list(
      validJudges = validJudges, 
      validRegions = validRegions,
      validYears = validYears,
      expectedNRow = length(validJudges) * length(validRegions) * length(validYears)
    )
  )
  number_of_tests_failed <- length(which(validate::summary(checked)$fails>0))
  
  # append list of (custom) error messages from failed tests, if any
  if (number_of_tests_failed > 0) {
    err_df <- validate::meta(rules[which(validate::summary(checked)$fails>0)]) %>% 
      dplyr::select(worksheet, severity, errorMessage)
    
    errors_found <- errors_found %>%
      dplyr::bind_rows(err_df)
  }
  
  ### sitting_days ----
  file_path <- system.file("validation_rules", "Sitting_Day_Capacity.yaml", package = "jwmodel")
  rules <- validate::validator(.file = file_path)
  checked <- validate::confront(
    jw$sitting_days, rules, 
    ref = list(
      validJudges = validJudges, 
      validRegions = validRegions,
      validYears = validYears,
      expectedNRow = length(validJudges) * length(validRegions) * length(validYears)
    )
  )
  number_of_tests_failed <- length(which(validate::summary(checked)$fails>0))
  
  # append list of (custom) error messages from failed tests, if any
  if (number_of_tests_failed > 0) {
    err_df <- validate::meta(rules[which(validate::summary(checked)$fails>0)]) %>% 
      dplyr::select(worksheet, severity, errorMessage)
    
    errors_found <- errors_found %>%
      dplyr::bind_rows(err_df)
  }
  
  ### demand ----
  
  ### judge_progression ----
  
  ### recruit_limits ----
  
  ### alloc_limits ----
  file_path <- system.file("validation_rules", "Allocation_Limits.yaml", package = "jwmodel")
  rules <- validate::validator(.file = file_path)
  checked <- validate::confront(jw$alloc_limits, rules, ref = list(validJudges = validJudges))
  number_of_tests_failed <- length(which(validate::summary(checked)$fails>0))
  
  # append list of (custom) error messages from failed tests, if any
  if (number_of_tests_failed > 0) {
    err_df <- validate::meta(rules[which(validate::summary(checked)$fails>0)]) %>% 
      dplyr::select(worksheet, severity, errorMessage)
    
    errors_found <- errors_found %>%
      dplyr::bind_rows(err_df)
  }
  
  # fixed_costs
  
  # variable_costs
  
  # penalty_costs
  
  # per_sitting_day
  
  # override_hiring
  
  
  # return data frame of errors & warnings from loaded data
  return(errors_found)
}