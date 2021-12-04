#' rocplot
#'
#' @param pred p
#' @param truth t
#' @param ... etcet
#'
#' @export
rocplot <- function(pred, truth, ...) {
  predob = ROCR::prediction(pred, truth)
  perf = ROCR::performance(predob, "tpr", "fpr")
  ROCR::plot(perf, ...)
}
