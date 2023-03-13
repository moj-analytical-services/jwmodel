### Compute total sitting days per year for jwmodel object.

source("R/utils.R")

days_sat_yj <- function (obj) {
  UseMethod("days_sat_yj")
}

days_sat_yj.default <- function(obj, filepath) {
  cat("'days_sat_yj' function can only be used on jwmodel objects")
}

days_sat_yj.jwmodel <- function(obj) {
  
  ### Model outputs ###
  allocation_output <- as.data.frame(obj$outputs$allocation_output)
  resource_output <- as.data.frame(obj$outputs$resource_output)

  ### Sitting days per judge type/year ###
  pivot <- allocation_output %>%
    dplyr::group_by(Judge, Year) %>%
    
    # Convert judge types to character.
    dplyr::mutate(Judge = as.character(Judge)) %>%
    
    # Organise by judge type (ascending order).
    dplyr::arrange(Judge) %>%
    
    # Convert years to character.
    dplyr::mutate(Year = as.character(Year)) %>%
    
    # Sum days sat per judge/year.
    dplyr::summarize(`Sum of Total Sitting Days` = sum(`Total Sitting Days`), 
                     na.rm = TRUE) %>%
    dplyr::select(-`na.rm`) %>%
    
    # Transform years to columns.
    tidyr::pivot_wider(names_from = Year, 
                       values_from = `Sum of Total Sitting Days`) %>%
    
    # Include total days per year as new row.
    janitor::adorn_totals(name = "Total") %>%
    
    # Round days sat per judge/year.
    dplyr::mutate_if(is.numeric, 
                     format_numbers)
  
  # Add pivot table to jwmodel object.
  obj$outputs$total_sitting_days <- pivot
  
  return(obj)
  
}
  