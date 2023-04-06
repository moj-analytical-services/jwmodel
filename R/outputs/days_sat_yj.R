# Compute total sitting days per year for jwmodel object.

source("R/outputs/utils.R")

days_sat_yj <- function (obj) {
  UseMethod("days_sat_yj")
}

days_sat_yj.default <- function(obj) {
  cat("'days_sat_yj' function can only be used on jwmodel objects")
}

days_sat_yj.jwmodel <- function(obj) {
  
  ### Model outputs ###
  allocation_output <- as.data.frame(obj$outputs$allocation_output)

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
  obj$outputs$pivots$total_sitting_days <- pivot
  
  
  ### Plot pivot table ###
  
  pivot <- obj$outputs$pivots$total_sitting_days %>%
  
          # Remove totals at the bottom of the pivot.
          dplyr::slice(1:(n() - 1)) %>%
  
          # Collaspe year columns into row values.
          tidyr::pivot_longer(cols = tidyr::starts_with("202"),
                              names_to = "Year", 
                              values_to = "Sum of Total Sitting Days") %>%
  
          # Convert values to numeric.
          dplyr::mutate(`Sum of Total Sitting Days` = remove_comma(`Sum of Total Sitting Days`))

  # Plot graph.
  plot <- pivot_plot(piv = pivot,
                     x = Judge,
                     y = `Sum of Total Sitting Days`,
                     fill = Year,
                     title = "Total Sitting Days",
                     x_lab = "Judge type",
                     y_lab = "Sitting Days")
  
   # Add graph to jwmodel object.
  obj$outputs$graphs$total_sitting_days <- plot
  
  return(obj)
  
}
