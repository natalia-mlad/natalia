# LASSO -------------------------------------------------------------------

#' Create and test correlation-based prediction models
#' Training and validation predictors can be different (x1, x2)
#'
#' @param x1 training predictors (dataframe or tibble)
#' @param y response variable
#' @param x2 validation predictors, will be the same as training ones if not provided
#' @param prop the proportion of the sample to use for training (vs holdout)
#'
#' @export
#'
cor_cor <- function(x1, y, x2 = NULL, prop = .67) {
  if(is.null(x2)) x2 <- x1
  s <- sample(nrow(x1), prop * nrow(x1))
  x1 <- sapply(x1, scale)
  x2 <- sapply(x2, scale)
  (x2[-s, ] %*% stats::cor(x1[s, ], y[s])) %>%
    stats::cor(y[-s])
}

#' Function to create and validate elastic net prediction models
#'
#' @param x1 training predictors (dataframe or tibble)
#' @param y response variable
#' @param x2 validation predictors, will be the same as training ones if not provided
#' @param prop the proportion of the sample to use for training (vs holdout)
#' @param alpha passed to cv.glmnet; default is .05
#' @param lambda.type passed to cv.glmnet; default is "lambda.min"
#'
#' @export
#'
glmnet_cor <- function(x1, y, x2 = NULL, prop = .67, alpha = .05, lambda.type = "lambda.min") {
  if (is.null(x2)) x2 <- x1
  s <- sample(nrow(x1), prop * nrow(x1))
  x1 <- sapply(x1, scale)
  x2 <- sapply(x2, scale)
  glmnet::cv.glmnet(x1[s, ], y[s], alpha = alpha) %>%
    stats::predict(x2[-s, ], s = lambda.type) %>%
    stats::cor(y[-s])
}
