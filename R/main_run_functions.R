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


#' Generate and save diagnostic plots into a PowerPoint slide deck
#'
#' This function processes a dataset, generates diagnostic and test/control plots, and saves them as a slide in a PowerPoint deck. The function filters the dataset based on a specific individual value, preprocesses the data, handles outliers, and arranges the plots into a single figure. The final figure is added to a PowerPoint presentation as a new slide.
#'
#' @param dataset A dataframe containing the data, which should include the column "animal_cell_id".
#' @param individual_value A value used to filter the dataset to include only the data for a specific individual (e.g., animal or cell).
#' @param ppt A PowerPoint object created using the `officer` package, where the slides will be added.
#' @return The updated PowerPoint object with the new slide containing the title and combined diagnostic plots.
#' @export
main_run <- function(dataset, individual_value, ppt){

  filtered_dataset <- dataset %>%
    filter(animal_cell_id == individual_value)

  print(names(filtered_dataset))

  pre_prcessed_dataset <- pre_process_datasets(filtered_dataset)

  title_string <- generate_title(pre_prcessed_dataset)
  diagnostic_figure_grid <- run_main_diagnostic(pre_prcessed_dataset)

  outlier_cleaned_dataset <- replace_outliers(pre_prcessed_dataset)
  longform_ds <- longform_test_control(outlier_cleaned_dataset)
  test_control_figure <- make_test_control_plot_single(longform_ds)
  combined_plots <- ggarrange(diagnostic_figure_grid, test_control_figure, ncol = 1, nrow = 2)

  #Save the plot temporarily
  temp_png <- tempfile(fileext = ".png")
  ggsave(temp_png, plot = combined_plots, width = 8, height = 6)

  # Format the title string
  title_fpar <- fpar(
    ftext(title_string, fp_text(font.size = 10, bold = TRUE))
  )


  # Add a new slide and insert the image
  ppt <- ppt %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = title_fpar, location = ph_location_type(type = "title")) %>%
    ph_with(value = external_img(temp_png, height = 5.5, width = 7.5), location = ph_location_type(type = "body"))

  return(ppt)
}
