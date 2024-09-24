library(tidyverse)
library(cowplot)
library(rlang)
library(ggpubr)
library(officer)
library(devtools)

source("./R/BaseLTPFunctions.R")
# Some color variables
test_control_colors <- c("#000000","#FF0000")

#### Data import and pre-processing ###########################################

ppt <- read_pptx()




####Diagnostic plots############################################################
#' Preprocess the dataset by standardizing column names and converting data types
#'
#' This function takes a dataset, standardizes its column names, and converts specific columns to the appropriate data types (factors or numeric). It ensures that certain columns are treated as factors (e.g., categorical variables) and others as numeric. This step prepares the dataset for further analysis.
#'
#' @param dataset A dataframe containing the data to be preprocessed. The columns should correspond to the expected variables (e.g., "Key", "Date", "Sex", "GT", "animal_id", "cell_num", etc.).
#' @return A dataframe with standardized column names and appropriately converted data types (factors and numeric).
#' @export
pre_process_datasets <- function(dataset){
  # Converts columns that need to be factor to factor, numeric to numeric.
  # Tidies up the names of the columns.

  current_names <- names(dataset)
  standardized_names <- c("Key", "Date", "Sex", "GT", "animal_id", "cell_num", "Time", "Test", "PPR_test", "Control", "PPR_control", "Rs", "Rin", "DC", "animal_cell_id")
  names(dataset) <- standardized_names

  factor_cols <- c("Key", "Date", "Sex", "GT", "animal_id", "cell_num")
  num_cols <- c("Time", "Test", "PPR_test", "Control", "PPR_control", "Rs", "Rin", "DC")
  dataset[factor_cols] <- lapply(dataset[factor_cols], factor)
  dataset[num_cols] <- lapply(dataset[num_cols], as.numeric)
  return (dataset)
}

#' The whole dataset is preprocessed here. Then the filtered dataset is filtered according to the cells I pick
#' after manually choosing the right ones.
# Generate individual diagnostic plots

#' Title of the function
#'
#' Description of what the function does.
#'
#' @param x Description of the input parameter.
#' @return What the function returns.
#' @export
make_diagnostic_plot <- function(dataset,parameter){

  title <- switch(parameter,
                  "Rs" = "Rs (\U2126)",
                  "Rin" = "Rin (\U2126)",
                  "DC" = "DC (A)"
  )
  diag_plot <- ggplot(dataset, aes(x = Time, y = !!sym(parameter)))+
    geom_point()+
    scale_x_continuous(name = "Time (min)", limits = c(-10, 40)) +
    scale_y_continuous(name = title)

  return(diag_plot)
}

#' Generate a title string based on dataset columns
#'
#' This function generates a descriptive title string using specific columns
#' from a dataset. The title is constructed using the "Key", "Sex", "GT",
#' "animal_id", and "cell_num" columns. It checks if the required columns are
#' present in the dataset and returns a formatted string for further use.
#'
#' @param dataset A dataframe containing the data with columns "Key", "Sex",
#' "GT", "animal_id", and "cell_num". The function expects these columns to
#' exist and will throw an error if any are missing.
#' @return A string combining the animal ID, cell number, and key details
#' formatted as "Animal_ID_<animal_id>_Cell_Number_<cell_num>_<key>_<sex>_<GT>".
#' @export
generate_title <- function(dataset) {
  # Print the dataset structure
  # print("Dataset structure in generate_title:")
  # print(str(dataset))

  # Check for required columns
  required_columns <- c("Key", "Sex", "GT", "animal_id", "cell_num")
  missing_columns <- setdiff(required_columns, names(dataset))
  if (length(missing_columns) > 0) {
    stop(paste("Dataset is missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Get the first row, all rows for the title will be the same thing
  animal_title <- dataset %>% filter(row_number() == 1)
  #print("First row of dataset:")
  #print(animal_title)

  # Key sex genotype string for title
  title_string <- paste(
    trimws(animal_title$Key),
    trimws(animal_title$Sex),
    trimws(animal_title$GT),
    sep = "_"
  )
  # String for the animal id and cell number
  animal_id <- animal_title$animal_id
  cell_num <- animal_title$cell_num

  full_title <- paste(
    "Animal_ID_", animal_id, "_",
    "Cell_Number_", cell_num,
    "_", title_string
  )

  return(full_title)
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

#' Reshape dataset into long format for test and control pathways
#'
#' This function reshapes the dataset into long format, with "Test" and "Control" values as separate pathways, and optionally includes standard error of the mean (SEM) columns.
#'
#' @param dataset A dataframe containing "Test", "Control", and optionally SEM columns.
#' @param is_mean Logical; if `TRUE`, the function includes SEM columns in the output.
#' @return A long-format dataframe with pathways and amplitudes.
#' @export
longform_test_control <- function(dataset, is_mean = FALSE){
  if (is_mean == FALSE){
    longer_ds <- dataset %>%
      pivot_longer(cols = c("Test","Control"),names_to = "Pathway", values_to = "normalised_amplitude") %>%
      mutate(Pathway = factor(Pathway))
  }
  else if (is_mean == TRUE){
    longer_ds_sem <- dataset %>%
      pivot_longer(cols = c("sem_test", "sem_control"),
                   names_to = "Pathway",
                   values_to = "sem_amplitude") %>%
      select(sem_amplitude )

    longer_ds_m <- dataset %>%
      pivot_longer(cols = c("Test","Control"),names_to = "Pathway", values_to = "normalised_amplitude")%>%
      select(Time, Pathway, normalised_amplitude)

    longer_ds <- bind_cols(longer_ds_m, longer_ds_sem)
  }
  return (longer_ds)
}

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

#' Create a dataset with mean and SEM for Test and Control pathways
#'
#' This function filters and groups the dataset to calculate mean and standard error of the mean (SEM) for the "Test" and "Control" pathways.
#'
#' @param dataset A dataframe containing columns for "Test" and "Control" pathways, and time points.
#' @return A summarized dataframe with means and SEM for Test and Control pathways.
#' @export
make_mean_sem_dataset <- function(dataset){
  dataset$animal_id <- as.factor(dataset$animal_id)

  ds_mean_sem <- dataset %>%
    filter(Time <= 35 & Time >= -10) %>%
    group_by(Time, animal_cell_id) %>%
    summarise(mean_test = mean(Test), mean_control = mean(Control))%>%
    ungroup() %>%
    group_by(Time)%>%
    summarise(
      Test = mean(mean_test),
      Control = mean(mean_control),
      sem_test = sd(mean_test) / sqrt(n()),
      sem_control = sd(mean_control) / sqrt(n())
    )
  return (ds_mean_sem)

}

#' Create a single diagnostic plot for test and control pathways
#'
#' This function generates a plot for diagnostic purposes using normalized amplitude values for "Test" and "Control" pathways over time.
#'
#' @param dataset A long-format dataframe containing "Test" and "Control" values for different time points.
#' @return A ggplot object for visualizing test and control pathways.
#' @export
make_test_control_plot_single <- function(dataset){
  # Use this function for the single plots used for diagnosis.
  test_control_plot <- ggplot(dataset, aes(x = Time, y = normalised_amplitude, color = Pathway))+
    geom_point()+
    geom_vline(xintercept = 0, linetype = "dotdash", color = "black", size = 0.5) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 0.5) +
    scale_x_continuous(name = "Time (min)", limits = c(-10, 40)) +
    scale_y_continuous(name = "First EPSC Amplitude (norm)", limits = c(-1, 3), breaks = seq(-1, 3, by = 1)) +
    scale_color_manual(name = "Pathway", values = test_control_colors) +
    theme_blank_background() +
    geom_text(x = 0, y = 3, label = "\u2193", size = 8, color = "black")

  return(test_control_plot)
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

#' Run PowerPoint generation for a given dataset and handle errors
#'
#' This function attempts to generate and insert slides into a PowerPoint presentation based on a specific `cell_id` and a given dataset. It checks if the dataset is "df_het" or "df_wt", prints a message about the dataset being processed, and handles errors during slide generation without crashing the entire process. If an error occurs, it logs the error and returns the PowerPoint object unchanged.
#'
#' @param ppt A PowerPoint object created using the `officer` package, to which new slides will be added.
#' @param cell_id The identifier for the specific cell or individual being processed.
#' @param dataframe The dataset being processed, which could be "df_het" or "df_wt".
#' @return The updated PowerPoint object if successful, or the original PowerPoint object if an error occurs.
#' @export
print_try_function <-function (ppt,cell_id,dataframe){
  dataframe_name <- deparse(substitute(dataframe))

  if (grepl("^df_het", dataframe_name)) {
    print("HET DATASET")
  } else if (grepl("^df_wt", dataframe_name)) {
    print("WT DATASET")
  } else {
    print("Unknown dataset")
  }

  print(paste("Processing cell_id:", cell_id))
  tryCatch(
    main_run(dataframe, cell_id, ppt),
    error = function(e) {
      message(paste("Error with cell_id:", cell_id, "-", e$message))
      return(ppt)  # Return the ppt unchanged if there's an error
    })
}

#HET
# HET dataset
# ppt <- Reduce(function(ppt, cell_id) {
#   print_try_function(ppt, cell_id, df_het)
# }, unique_cells_vector_het, ppt)
# print(ppt, target = "het_apamin_individual_cells_presentation.pptx")
