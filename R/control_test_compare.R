source("./R/BaseLTPFunctions.R")


#' Main Combined Run
#'
#' This function processes a dataset by cleaning outliers, calculating mean and SEM values, transforming the dataset into long-form format, and generating a plot for test and control pathways.
#'
#' @param dataset A dataframe containing data for test and control pathways.
#' @return A ggplot object visualizing the mean and SEM for test and control pathways.
#' @export
main_combined_run <- function(dataset){
  cleaned_outliers <- replace_outliers(dataset)
  mean_sem_dataset_wide <- make_mean_sem_dataset(cleaned_outliers)
  ds_mean_sem_longform <- longform_test_control(mean_sem_dataset_wide, is_mean = TRUE)
  test_control_plot_means <- make_test_control_plot(ds_mean_sem_longform)

  return(test_control_plot_means)
}
print("main combined function opened")


# Bar chart figures
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


#' Combine Longitudinal and Bin Plots
#'
#' This function arranges a longitudinal plot and a bin plot side by side for comparison.
#'
#' @param longplot A ggplot object representing the longitudinal plot.
#' @param binplot A ggplot object representing the bin plot.
#' @return A combined plot object with the longitudinal and bin plots side by side.
#' @export
longitudinal_and_binplot <- function(longplot, binplot){
  plot <- ggarrange(longplot, binplot, ncol = 2, nrow = 1)
  return(plot)
}

#' Apply a linear model and t-test to a dataset
#'
#' This function takes a dataset, standardizes the "Pathway" column as a factor, fits a linear model using the formula `normalised_amplitude ~ Pathway`, and performs a t-test on the same formula. It returns the results of the t-test.
#'
#' @param dataset A dataframe containing the data for analysis. The dataset must include at least the columns "normalised_amplitude" and "Pathway".
#' @return The result of a t-test applied to the model formula <normalised_amplitude ~ Pathway>.
#' @export
apply_test_control_model <- function(dataset){

  test_formula <- normalised_amplitude ~ Pathway

  dataset$Pathway <- as.factor(dataset$Pathway)

  het_model <- lm(test_formula, data = dataset)
  summary(het_model)

  ttest <- t.test(test_formula, data = dataset)
  return (ttest)
}

# wt_longitudinal_and_binplot <- longitudinal_and_binplot(wt_combined_plot,plot_25_30_wt)
# wt_longitudinal_and_binplot
