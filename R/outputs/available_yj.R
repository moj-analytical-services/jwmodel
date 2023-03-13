# Compute available FTE per year for jwmodel object.

source("R/outputs/utils.R")

available_yj <- function (obj) {
  UseMethod("available_yj")
}

available_yj.default <- function(obj) {
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
                     format_numbers) %>%
    
    # Drop status column.
    dplyr::select(-status)
  
  # Re-order columns.
  pivot <- pivot[c(1, 5, 2, 3, 4)]
  
  # Add pivot table to jwmodel object.
  obj$outputs$pivots$available_judges <- pivot
  
  
  ### Plot pivot table ###
  
  # Collaspe year columns into row values.
  pivot <- tidyr::pivot_longer(obj$outputs$pivots$available_judges, 
                               cols = tidyr::starts_with("202"),
                               names_to = "Year", 
                               values_to = "FTE") %>%
  
          # Convert values to numeric.
          dplyr::mutate(FTE = remove_comma(FTE))
  
  
  # Plot graph.
  plot <- pivot_plot(piv = pivot,
                     x = Judge,
                     y = FTE,
                     fill = Year,
                     title = "Available FTE",
                     x_lab = "Judge type",
                     y_lab = "FTE")
  
  # Add graph to jwmodel object.
  obj$outputs$graphs$available_judges <- plot
  
  return(obj)
  
}