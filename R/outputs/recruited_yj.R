# Compute recruited FTE per year for jwmodel object.

source("R/outputs/utils.R")

recruited_yj <- function (obj) {
  UseMethod("available_yj")
}

recruited_yj.default <- function(obj) {
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
                     format_numbers) %>%
    
    # Drop status column.
    dplyr::select(-status)
  
  # Re-order columns.
  pivot <- pivot[c(1, 2, 3, 4)]
  
  # Add pivot table to jwmodel object.
  obj$outputs$pivots$recruited_judges <- pivot
  
  
  ### Plot pivot table ###
  
  # Collaspe year columns into row values.
  pivot <- tidyr::pivot_longer(obj$outputs$pivots$recruited_judges, 
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
                     title = "Recruited FTE",
                     x_lab = "Judge type",
                     y_lab = "FTE")
  
  # Add graph to jwmodel object.
  obj$outputs$graphs$recruited_judges <- plot
  
  return(obj)
  
}