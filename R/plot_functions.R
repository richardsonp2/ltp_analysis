#' Combine Longitudinal and Bin Plots
#'
#' This function arranges a longitudinal plot and a bin plot side by side for comparison.
#'
#' @param longplot A ggplot object representing the longitudinal plot.
#' @param binplot A ggplot object representing the bin plot.
#' @return A combined plot object with the longitudinal and bin plots side by side.
#' @export
longitudinal_and_binplot <- function(longplot, binplot){
  plot <- ggarrange(longplot, binplot, ncol = 2, nrow = 1)
  return(plot)
}
