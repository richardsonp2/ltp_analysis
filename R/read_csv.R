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
