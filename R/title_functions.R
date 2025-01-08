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
