# Compute departed FTE per year for jwmodel object.

source("R/outputs/utils.R")

departed_yj <- function (obj) {
  UseMethod("available_yj")
}

departed_yj.default <- function(obj, filepath) {
  cat("'departed_yj' function can only be used on jwmodel objects")
}

departed_yj.jwmodel <- function(obj) {
  
  ### Model output ###
  resource_output <- as.data.frame(obj$outputs$resource_output)
  
  ### Departed FTE per judge type/status/year ###
  pivot <- resource_output %>%
    dplyr::group_by(Judge, Year) %>%
    
    # Convert judge types to character.
    dplyr::mutate(Judge = as.character(Judge)) %>%
    
    # Organise by judge type (ascending order).
    dplyr::arrange(Judge) %>%
    
    # Convert years to character.
    dplyr::mutate(Year = as.character(Year)) %>%
    
    # Filter for status = 'departed'.
    dplyr::filter(status == "Departed (out)") %>%
    
    # Transform years to columns.
    tidyr::pivot_wider(names_from = Year, 
                       values_from = `FTE`) %>%
    
    # Round FTE values.
    dplyr::mutate_if(is.numeric, 
                     format_numbers)
  
  # Add pivot table to jwmodel object.
  obj$outputs$pivots$departed_judges <- pivot
  
  return(obj)
  
}