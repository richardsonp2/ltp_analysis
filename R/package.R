#' ltp_analysis
#'
#' Produces diagnostic and results plots for ltp data
#'
#' @importFrom magrittr %>%
#' @importFrom stats lm t.test sd
#' @importFrom utils read.csv2
#' @importFrom dplyr group_by summarise mutate select ungroup across
#' @importFrom dplyr group_by summarise mutate select ungroup across
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggarrange
#' @importFrom officer add_slide ph_with ph_location_type external_img fpar ftext fp_text

#' @docType package
#' @name mypackage
NULL

# Suppress global variable warnings
utils::globalVariables(c(
  "Time", "animal_cell_id", "Test", "Control", "Pathway",
  "normalised_amplitude", "sem_amplitude", "test_control_colors",
  "mean_test", "mean_control", "top_figure_limit", "animal_id", "cell_num"
))
