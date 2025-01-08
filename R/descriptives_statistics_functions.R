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
    filter(Time <= 35 & Time >= -5) %>%
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
