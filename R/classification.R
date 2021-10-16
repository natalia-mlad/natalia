#' rocplot
#' @export
rocplot <- function(pred, truth, ...) {
  require(ROCR)
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}
