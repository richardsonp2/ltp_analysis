#' Create a single diagnostic plot for test and control pathways
#'
#' This function generates a plot for diagnostic purposes using normalized amplitude values for "Test" and "Control" pathways over time.
#'
#' @param dataset A long-format dataframe containing "Test" and "Control" values for different time points.
#' @return A ggplot object for visualizing test and control pathways.
#' @export
make_test_control_plot_single <- function(dataset, top_amplitude_limit = 3,right_timepoint_max_limit = 35){

  # Use this function for the single plots used for diagnosis.
  test_control_plot <- ggplot(dataset, aes(x = Time, y = normalised_amplitude, color = Pathway))+
    geom_point()+
    geom_vline(xintercept = 0, linetype = "dotdash", color = "black", size = 0.5) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 0.5) +
    scale_x_continuous(name = "Time (min)", limits = c(-10, right_timepoint_max_limit)) +
    scale_y_continuous(name = "First EPSC Amplitude (norm)", limits = c(-1, top_figure_limit), breaks = seq(-1, top_figure_limit, by = 1)) +
    scale_color_manual(name = "Pathway", values = test_control_colors) +
    theme_blank_background() +
    geom_text(x = 0, y = top_figure_limit, label = "\u2193", size = 8, color = "black")

  return(test_control_plot)
}


#' Create an error bar plot with mean and SEM for test and control pathways
#'
#' This function generates a plot with error bars to visualize the mean and standard error of the mean (SEM) for the "Test" and "Control" pathways over time.
#'
#' @import ggplot2
#' @param dataset A dataframe containing mean and SEM values for "Test" and "Control" pathways.
#' @return A ggplot object for visualizing the mean and SEM for test and control pathways.
#' @export
#'
make_test_control_plot <- function(dataset, right_timepoint_max_limit = 35, top_figure_height = 3){


  # This function generates the error bar plot with the mean values and sd values
  test_control_plot <- ggplot(dataset, aes(x = Time, y = normalised_amplitude, color = Pathway))+
    geom_point() +
    geom_errorbar(aes(ymin = normalised_amplitude - sem_amplitude,
                      ymax = normalised_amplitude + sem_amplitude,
                      color = Pathway),
                  width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dotdash", color = "black", size = 0.5) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 0.5) +
    scale_x_continuous(name = "Time (min)", limits = c(-10, right_timepoint_max_limit)) +
    scale_y_continuous(name = "First EPSC Amplitude (norm)",
                       limits = c(0, top_figure_height),
                       breaks = seq(0, top_figure_height, by = 1)) +
    scale_color_manual(name = "Pathway", values = test_control_colors) +
    theme_blank_background() +
    geom_text(x = 0, y = top_figure_height, label = "\u2193", size = 8, color = "black")

  return(test_control_plot)
}
