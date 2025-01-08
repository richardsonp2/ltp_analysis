#' Custom theme with transparent background for plots
#'
#' This function provides a minimal ggplot2 theme with a transparent background and customized axis lines, tick marks, and legend formatting.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @return A ggplot2 theme object with customized settings.
#' @export
theme_blank_background <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),  # Black axis lines
      axis.ticks = element_line(color = "black"),  # Black tick marks
      axis.ticks.length = unit(-0.2, "cm"),  # Position ticks before axis line
      axis.text.x = element_text(margin = margin(0, 0, 5, 0)),  # Adjust x-axis text margin
      axis.text.y = element_text(margin = margin(0, 5, 0, 0)),  # Adjust y-axis text margin
      legend.key = element_rect(fill = "white", color = "white"),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.position = "right",  # Adjust legend position as needed
      strip.background = element_blank(),
      strip.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 1)
    )
}
