#' rocplot
#' @export
rocplot <- function(pred, truth, ...) {
  predob = ROCR::prediction(pred, truth)
  perf = ROCR::performance(predob, "tpr", "fpr")
  ROCR::plot(perf, ...)
}
