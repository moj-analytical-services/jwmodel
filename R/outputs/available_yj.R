# Compute available FTE per year for jwmodel object.

source("R/outputs/utils.R")

available_yj <- function (obj) {
  UseMethod("available_yj")
}

available_yj.default <- function(obj, filepath) {
  cat("'available_yj' function can only be used on jwmodel objects")
}

available_yj.jwmodel <- function(obj) {
  
  ### Model output ###
  resource_output <- as.data.frame(obj$outputs$resource_output)
  
  ### Available FTE per judge type/status/year ###
  pivot <- resource_output %>%
    dplyr::group_by(Judge, Year) %>%
    
    # Convert judge types to character.
    dplyr::mutate(Judge = as.character(Judge)) %>%
    
    # Organise by judge type (ascending order).
    dplyr::arrange(Judge) %>%
    
    # Convert years to character.
    dplyr::mutate(Year = as.character(Year)) %>%
    
    # Filter for status = 'available'.
    dplyr::filter(status == "Available") %>%
    
    # Transform years to columns.
    tidyr::pivot_wider(names_from = Year, 
                       values_from = `FTE`) %>%
    
    # Include current year FTE.
    merge(obj$n_judges, 
          by.x = "Judge", 
          by.y = "Judge Type") %>%
    
    # Rename column. 
    dplyr::rename("Current" = "Number of Judges") %>%
    
    # Round FTE values.
    dplyr::mutate_if(is.numeric, 
                     format_numbers)
  
  # Re-order columns.
  pivot <- pivot[c(1, 2, 6, 3, 4, 5)]
  
  # Add pivot table to jwmodel object.
  obj$outputs$pivots$available_judges <- pivot
  
  return(obj)
  
}