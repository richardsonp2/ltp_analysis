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
  standardized_names <- c("Key", "Date", "Sex", "GT", "animal_id", "cell_num", "Time", "Test", "PPR_test", "Control", "PPR_Control", "Rs", "Rin", "DC", "animal_cell_id")
  names(dataset) <- standardized_names

  factor_cols <- c("Key", "Date", "Sex", "GT", "animal_id", "cell_num", "animal_cell_id")
  num_cols <- c("Time", "Test", "PPR_test", "Control", "PPR_Control", "Rs", "Rin", "DC")
  dataset[factor_cols] <- lapply(dataset[factor_cols], factor)
  dataset[num_cols] <- lapply(dataset[num_cols], as.numeric)
  return (dataset)
}
