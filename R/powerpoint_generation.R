#' Run PowerPoint generation for a given dataset and handle errors
#'
#' This function attempts to generate and insert slides into a PowerPoint presentation based on a specific `cell_id` and a given dataset. It checks if the dataset contains het or wt and prints a message about the dataset being processed. The function handles errors during slide generation without crashing the entire process. If an error occurs, it logs the error and returns the PowerPoint object unchanged.
#'
#' @param ppt A PowerPoint object created using the `officer` package, to which new slides will be added.
#' @param cell_id The identifier for the specific cell or individual being processed.
#' @param dataframe The dataset being processed, which will be either a HET or WT dataset.
#' @return The updated PowerPoint object if successful, or the original PowerPoint object if an error occurs.
#' @export
print_try_function <-function (ppt,cell_id,dataframe){
  dataframe_name <- deparse(substitute(dataframe))

  if (grepl("het", dataframe_name, ignore.case = TRUE)) {
    print("HET DATASET")
  } else if (grepl("wt", dataframe_name, ignore.case = TRUE)) {
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
