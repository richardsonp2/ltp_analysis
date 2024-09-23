source("./R/BaseLTPFunctions.R")

main_combined_run <- function(dataset){
  cleaned_outliers <- replace_outliers(dataset)
  mean_sem_dataset_wide <- make_mean_sem_dataset(cleaned_outliers)
  ds_mean_sem_longform <- longform_test_control(mean_sem_dataset_wide, is_mean = TRUE)
  test_control_plot_means <- make_test_control_plot(ds_mean_sem_longform)
  
  return(test_control_plot_means)
}

# Bar chart figures

generate_bin_title <- function(binstart, binend) {
  title <- paste0(binstart, "-", binend)
  return(title)
}

filter_five_bin <- function(dataset, bin_start) {
  bin_end <- bin_start + 5
  filtered <- dataset %>%
    filter(Time >= bin_start & Time <= bin_end)
  return(filtered)
}

generate_means_from_bins <- function(dataset){
  dataset <- dataset %>% 
    group_by(animal_cell_id) %>%
    summarise(mean_test = mean(Test), mean_control = mean(Control)) %>%
    pivot_longer(cols = c("mean_test", "mean_control"), names_to = "Pathway", values_to = "normalised_amplitude")
  return(dataset)
}

plot_fivemin_function <- function(dataset, title, is_combined = FALSE) {
  
  #dataset <- longform_test_control(dataset = dataset)
  # Change names of factors for the figure
  #levels(dataset[[Pathway]])[levels(dataset$Pathway)=='mean_test'] <- 'Test'
  #levels(dataset[[Pathway]])[levels(dataset$Pathway)=='mean_control'] <- 'Control'
  
  plot_5min <- ggplot(dataset, aes(x = Pathway, y = normalised_amplitude, color = Pathway)) +
    geom_boxplot() +
    geom_point(position = position_jitter(width = 0.1))+
    scale_y_continuous(name = "First EPSC Amplitude (norm)", limits = c(-1, 1.5), breaks = seq(-1, 1.5, by = 1))+
    scale_color_manual(name = "Pathway", values = test_control_colors)
  
  if (is_combined == TRUE) {}
  plot_5min <- plot_5min + labs(title = title)
  return(plot_5min)
}


generate_full_bin_figure_function <- function(dataset, bin_start = 20) {
  bin_end <- bin_start + 5
  
  # Title should show between which timepoints.
  generated_title <- generate_bin_title(binstart = bin_start, binend = bin_end)
  
  filterfive <- filter_five_bin(dataset, 20)
  means_from_bins <- generate_means_from_bins(filterfive)
  
  fiveplot <- plot_fivemin_function(means_from_bins, generated_title)
  
  
  return(fiveplot)
}


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