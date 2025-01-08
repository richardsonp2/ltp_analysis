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
