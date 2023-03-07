# Compute recruited FTE per year for jwmodel object.

source("R/outputs/utils.R")

recruited_yj <- function (obj) {
  UseMethod("available_yj")
}

recruited_yj.default <- function(obj, filepath) {
  cat("'recruited_yj' function can only be used on jwmodel objects")
}

recruited_yj.jwmodel <- function(obj) {
  
  ### Model output ###
  resource_output <- as.data.frame(obj$outputs$resource_output)
  
  ### Recruited FTE per judge type/status/year ###
  pivot <- resource_output %>%
    dplyr::group_by(Judge, Year) %>%
    
    # Convert judge types to character.
    dplyr::mutate(Judge = as.character(Judge)) %>%
    
    # Organise by judge type (ascending order).
    dplyr::arrange(Judge) %>%
    
    # Convert years to character.
    dplyr::mutate(Year = as.character(Year)) %>%
    
    # Filter for status = 'recruited'.
    dplyr::filter(status == "Hired (in)") %>%
    
    # Transform years to columns.
    tidyr::pivot_wider(names_from = Year, 
                       values_from = `FTE`) %>%
    
    # Round FTE values.
    dplyr::mutate_if(is.numeric, 
                     format_numbers)
  
  # Add pivot table to jwmodel object.
  obj$outputs$pivots$recruited_judges <- pivot
  
  return(obj)
  
}