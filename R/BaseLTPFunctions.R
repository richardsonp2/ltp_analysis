#' Create an error bar plot with mean and SEM for test and control pathways
#'
#' This function generates a plot with error bars to visualize the mean and standard error of the mean (SEM) for the "Test" and "Control" pathways over time.
#'
#' @param dataset A dataframe containing mean and SEM values for "Test" and "Control" pathways.
#' @return A ggplot object for visualizing the mean and SEM for test and control pathways.
#' @export
make_test_control_plot <- function(dataset){
  # This function generates the error bar plot with the mean values and sd values
  test_control_plot <- ggplot(dataset, aes(x = Time, y = normalised_amplitude, color = Pathway))+
    geom_point() +
    geom_errorbar(aes(ymin = normalised_amplitude - sem_amplitude, 
                      ymax = normalised_amplitude + sem_amplitude,
                      color = Pathway),
                  width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dotdash", color = "black", size = 0.5) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 0.5) +
    scale_x_continuous(name = "Time (min)", limits = c(-10, 40)) +
    scale_y_continuous(name = "First EPSC Amplitude (norm)",
                       limits = c(0, 3),
                       breaks = seq(0, 3, by = 1)) +
    scale_color_manual(name = "Pathway", values = test_control_colors) +
    theme_blank_background() +
    geom_text(x = 0, y = 3, label = "\u2193", size = 8, color = "black")
  
  return(test_control_plot)
}

#' Read LTP CSV files
#'
#' Description of what the function does.
#'
#' @param address The string address for the csv file. 
#' @return A dataframe containing the 15 columns.
#' @export
read_csv_function <- function(address){
  # Sometimes the script seems to add a column of NA's at the end of the CSV, I will drop the column with those in. No other
  # columns have NA's so we should not loose any information.
  # However, there should be 14 columns at this stage, I will check for this in the function
  
  df <- read.csv2(address, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- df[, colSums(is.na(df)) < nrow(df)]
  
  df <- df %>% 
    mutate(animal_cell_id = paste(animal_id, cell_num, sep = '_'))
  
  # Check if the resulting data frame has exactly 14 columns
  if (ncol(df) == 15) {
    return(df)
  } else {
    warning("The dataset does not have exactly 15 columns. Please check the dataset to make sure no data has been added or lost.")
    return(df)
  }
}


#' Get a list of unique combined animal and cell IDs
#'
#' This function extracts the unique values from the `animal_cell_id` column of the given dataset and returns them as a vector. The `animal_cell_id` typically represents a combined identifier for both animal and cell.
#'
#' @param dataset A dataframe containing the column `animal_cell_id`, which holds the combined animal and cell IDs.
#' @return A vector of unique `animal_cell_id` values from the dataset.
#' @export
find_unique_cells <- function(dataset){
  
  unique_cells <- unique(dataset$animal_cell_id)
  unique_cells_vector <- as.vector(unique_cells)
}

#' Replace outliers in the dataset with the mean value
#'
#' This function checks each numeric column (except "Time") for outliers (values that deviate more than 2 standard deviations from the mean) and replaces them with the mean value of the non-zero "Time" data.
#'
#' @param dataset A dataframe containing numeric columns.
#' @return A dataframe with outliers replaced by the mean of the corresponding column.
#' @export
replace_outliers <- function(dataset) {
  dataset %>%
    mutate(across(where(is.numeric) & !c("Time"), function(x) {
      if (any(dataset$Time != 0)) {
        mean_val <- mean(x[dataset$Time != 0])
        sd_val <- sd(x[dataset$Time != 0])
        x[abs(x - mean_val) > 2 * sd_val & dataset$Time != 0] <- mean_val
      }
      return(x)
    }))
}