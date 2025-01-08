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
