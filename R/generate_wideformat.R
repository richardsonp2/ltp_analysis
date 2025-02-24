#' Convert dataset to wide format while aligning Time = 0
#'
#' This function takes a dataset with multiple time points for each `animal_cell_id`
#' and expands the dataset to include a full range of time points, ensuring that
#' missing values are filled with `NA`.
#'
#' @param dataset A data frame containing electrophysiology data.
#' @return A data frame reshaped into wide format with missing values padded.
#' @import dplyr tidyr
#' @export
make_wide_function <- function(dataset) {
  # Convert relevant columns to numeric
  dataset <- dataset %>% mutate(across(c(Test, PPR_test, Control, PPR_Control, Rs, Rin, DC), as.numeric))

  # Get the full range of time values
  min_time <- min(dataset$Time, na.rm = TRUE)
  max_time <- max(dataset$Time, na.rm = TRUE)
  full_time_range <- seq(min_time, max_time)

  dataset_expanded <- dataset %>%
    group_by(animal_cell_id) %>%
    complete(Time = full_time_range, fill = list(Test = NA, PPR_test = NA, Control = NA, PPR_Control = NA, Rs = NA, Rin = NA, DC = NA)) %>%
    fill(Key, Date, Sex, GT, animal_id, cell_num, .direction = "downup")

  wide_df <- dataset_expanded %>%
    select(-c(animal_id, cell_num)) %>%
    group_by(Key, Date, Sex, GT, animal_cell_id) %>%
    mutate(row_id = row_number()) %>%
    pivot_wider(
      names_from = c(Key, Date, Sex, GT, animal_cell_id),
      values_from = c(Test, PPR_test, Control, PPR_Control, Rs, Rin, DC),
      names_glue = "{animal_cell_id}_{Key}_{Date}_{Sex}_{GT}_{.value}",
      values_fn = list(Test = first, PPR_test = first, Control = first, PPR_Control= first, Rs = first, Rin = first, DC = first)
    ) %>%
    select(-row_id)  # Remove the helper row identifier

    # Extract column names from the dataframe
    col_names <- colnames(wide_df)

    # Extract the first 11 characters from each column name
    prefixes <- substr(col_names, 1, 11)

    # Get the sorting order based on the extracted prefixes
    sorted_indices <- order(prefixes)

    # Reorder dataframe columns based on the sorted order
    wide_df_sorted <- wide_df[, sorted_indices]


  return(wide_df_sorted)
}
