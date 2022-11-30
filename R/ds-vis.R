# Classification ----------------------------------------------------------

#' Plot a ROC curve
#'
#' @param pred predicted value
#' @param truth actual value
#' @param ... additional arguments passed on to ROCR::plot-methods
#'
#' @export
rocplot <- function(pred, truth, ...) {
  predob <- ROCR::prediction(pred, truth)
  perf <- ROCR::performance(predob, "tpr", "fpr")
  ROCR::plot(perf, ...)
}
