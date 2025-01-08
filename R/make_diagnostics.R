#' Run the main diagnostic plot generation
#'
#' This function combines a title and a grid of diagnostic plots into a single plot, and prepares it for visualization.
#'
#' @param dataset A dataframe containing the necessary data for generating diagnostic plots.
#' @return A combined plot with a title and a grid of diagnostic plots.
#' @export
run_main_diagnostic <-function (dataset){
  title_string <- generate_title(dataset)
  diag_plots <- make_diagnostic_grid(dataset)

  # Create a ggplot object for the title
  title_plot <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = title_string, size = 6, fontface = 'bold', hjust = 0.5, vjust = 0.5) +
    theme_void()  # Remove all background and axes

  # Combine the title and the plot grid
  diagnositics_plots <- plot_grid(title_plot, diag_plots, ncol = 1, rel_heights = c(0.1, 1))

}

#' Create a diagnostic grid of Rs, Rin, and DC figures
#'
#' This function generates diagnostic plots for three parameters: "Rs", "Rin", and "DC", and arranges them into a grid layout with one row and three columns.
#'
#' @param dataset A dataframe that contains the numeric columns "Rs", "Rin", and "DC".
#' @return A grid of diagnostic plots arranged horizontally.
#' @export
make_diagnostic_grid <- function(dataset) {
  rs_figure <- make_diagnostic_plot(dataset, "Rs")
  Rin_figure <- make_diagnostic_plot(dataset, "Rin")
  DC_figure <- make_diagnostic_plot(dataset, "DC")

  diagnostic_grid <- gridExtra::grid.arrange(rs_figure, Rin_figure, DC_figure, nrow = 1, ncol = 3)
  return(diagnostic_grid)
}

#' Generate Diagnostic Plots for Specified Parameters
#'
#' This function creates diagnostic plots for selected parameters in a given dataset.
#' The function expects the dataset to be preprocessed and filtered based on manual selections
#' of cells. The plots are generated for parameters such as Rs, Rin, and DC.
#'
#' @param dataset A preprocessed and filtered dataset. Should include time as one of the columns.
#' @param parameter A character string specifying the parameter to be plotted.
#' Accepted values are "Rs" (series resistance), "Rin" (input resistance), and "DC" (drift current).
#' @return A ggplot object representing the diagnostic plot for the specified parameter.
#' @export
#' @examples
#' # Example usage:
#' # make_diagnostic_plot(my_dataset, "Rs")
make_diagnostic_plot <- function(dataset, parameter) {

  # Define the plot title based on the parameter
  title <- switch(parameter,
                  "Rs" = "Rs (\u03A9)",   # Series resistance
                  "Rin" = "Rin (\u03A9)",  # Input resistance
                  "DC" = "DC (A)"     # Drift current
  )

  # Generate the diagnostic plot
  diag_plot <- ggplot2::ggplot(dataset, aes(x = Time, y = !!sym(parameter))) +
    geom_point() +
    scale_x_continuous(name = "Time (min)", limits = c(-10, 40)) +
    scale_y_continuous(name = title)

  return(diag_plot)
}
