#' Generate Bin Title
#'
#' This function generates a title based on the start and end of a binning range.
#'
#' @param binstart The starting value of the bin.
#' @param binend The ending value of the bin.
#' @return A string representing the title for the bin range.
#' @export
generate_bin_title <- function(binstart, binend) {
  title <- paste0(binstart, "-", binend)
  return(title)
}

#' Filter Dataset by Time Range
#'
#' This function filters a dataset to include only rows within a 5-minute bin starting from the provided bin start time.
#'
#' @param dataset A dataframe containing time-series data.
#' @param bin_start The start of the 5-minute time range.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @return A filtered dataframe containing data within the specified time range.
#' @export
filter_five_bin <- function(dataset, bin_start) {
  bin_end <- bin_start + 5
  filtered <- dataset %>%
    filter(Time >= bin_start & Time <= bin_end)
  return(filtered)
}

#' Generate Mean Values from Bins
#'
#' This function calculates the mean values for test and control pathways, grouping by animal or cell ID, and transforms the dataset into long format.
#'
#' @param dataset A dataframe containing test and control pathway data.
#' @return A long-form dataframe containing the mean values for test and control pathways.
#' @export
generate_means_from_bins <- function(dataset){
  dataset <- dataset %>%
    group_by(animal_cell_id) %>%
    summarise(mean_test = mean(Test), mean_control = mean(Control)) %>%
    pivot_longer(cols = c("mean_test", "mean_control"), names_to = "Pathway", values_to = "normalised_amplitude")
  return(dataset)
}

#' Plot 5-Minute Bin Data
#'
#' This function generates a box plot and jitter plot to visualize normalized amplitudes for test and control pathways within a 5-minute bin.
#'
#' @param dataset A dataframe containing binned data.
#' @param title The title for the plot.
#' @param legend Logical, whether to include the legend in the plot. Defaults to TRUE.
#' @return A ggplot object visualizing the binned data.
#' @export
plot_fivemin_function <- function(dataset, title, legend = TRUE) {

  plot_5min <- ggplot(dataset, aes(x = Pathway, y = normalised_amplitude, color = Pathway)) +
    geom_boxplot() +
    geom_point(position = position_jitter(width = 0.1)) +
    scale_y_continuous(name = "First EPSC Amplitude (norm)", limits = c(-1, 1.5), breaks = seq(-1, 1.5, by = 1)) +
    scale_color_manual(name = "Pathway", values = test_control_colors) +
    scale_x_discrete(labels = c('mean_control' = 'Control', 'mean_test' = 'Test')) +
    theme_bw() +
    labs(title = title)

  if (legend == FALSE){
    plot_5min <- plot_5min + theme(legend.position = "none")
  }

  return(plot_5min)
}

#' Generate Full Bin Plot
#'
#' This function generates a full plot for a specified time bin by filtering the dataset and generating a plot using the 5-minute bin plotting function.
#'
#' @param dataset A dataframe containing time-series data.
#' @param bin_start The start of the time bin for plotting. Defaults to 20.
#' @param legend Logical, whether to include the legend in the plot. Defaults to TRUE.
#' @return A ggplot object visualizing the data within the specified bin.
#' @export
generate_full_bin_figure_function <- function(dataset, bin_start = 20, legend = TRUE) {
  bin_end <- bin_start + 5

  # Title should show between which timepoints.
  generated_title <- generate_bin_title(binstart = bin_start, binend = bin_end)

  filterfive <- filter_five_bin(dataset, bin_start = bin_start)
  means_from_bins <- generate_means_from_bins(filterfive)

  fiveplot <- plot_fivemin_function(means_from_bins, generated_title, legend = legend)


  return(fiveplot)
}

