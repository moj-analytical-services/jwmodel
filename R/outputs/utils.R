# Convert numbers to 0 dp and include , separator for large numbers.
format_numbers <- function(x) { formatC(x,
                                        digits = 0,
                                        format = 'f',
                                        big.mark = ',')
}

# Remove comma from numbers and convert to numeric format.
remove_comma <- function(x) { as.numeric(gsub(pattern = ",", 
                                              replacement = "",
                                               x))
}

# Plot graph based on pivot table.
pivot_plot <- function (piv, x, y, fill, title, x_lab, y_lab) {
  
  ggplot2::ggplot({{piv}}, 
                  aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
    geom_bar(stat = "identity",
             position = "dodge") +
    
    # Colourblind friendly palette.
    scale_fill_viridis(discrete = TRUE, 
                       option = "cividis") +
    
    # Apply y limits and remove padding.
    #scale_y_continuous(limits = c(0, max(piv$y) + 50), expand = c(0, 0)) +
    ggtitle(title) +
    labs(x = x_lab, 
         y = y_lab) + 
    
    # Formatting.
    theme(plot.title = element_text(color = "black", size = 13, hjust = 0.5, margin = margin(t = 10, r = 0, b = 20, l = 0)),
          axis.title.x = element_text(color = "black", size = 11, margin = margin(t = 20, r = 0, b = 10, l = 0)),
          axis.title.y = element_text(color = "black", size = 11, margin = margin(t = 0, r = 20, b = 0, l = 10)),
          axis.text.x = element_text(color = "black", angle = 90, size = 10, hjust = 0.5, vjust = 0.5, margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(color = "black", size = 10, hjust = 0.5, vjust = 0.5),
          legend.position = "right",
          legend.margin = margin(t = 0, r = 10, b = 0, l = 20),
          legend.title = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, margin = margin(t = 5, r = 0, b = 5, l = 0)),
          legend.text = element_text(colour = "black", size = 10, hjust = 0.5, vjust = 0.5, margin = margin(t = 10, r = 0, b = 10, l = 0)),
          legend.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white", color = NA),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.7, "cm") 
    )
}
