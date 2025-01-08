#' Apply a linear model and t-test to a dataset
#'
#' This function takes a dataset, standardizes the "Pathway" column as a factor, fits a linear model using the formula `normalised_amplitude ~ Pathway`, and performs a t-test on the same formula. It returns the results of the t-test.
#'
#' @param dataset A dataframe containing the data for analysis. The dataset must include at least the columns "normalised_amplitude" and "Pathway".
#' @return The result of a t-test applied to the model formula <normalised_amplitude ~ Pathway>.
#' @export
apply_test_control_model <- function(dataset){

  test_formula <- normalised_amplitude ~ Pathway

  dataset$Pathway <- as.factor(dataset$Pathway)

  het_model <- lm(test_formula, data = dataset)
  summary(het_model)

  ttest <- t.test(test_formula, data = dataset)
  return (ttest)
}
