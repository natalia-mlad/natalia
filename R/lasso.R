#' @title Create and test correlation-based prediction models
#'
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
  (x2[-s, ] %*% cor(x1[s, ], y[s])) %>%
    cor(y[-s])
}

#' @title Function to create and validate elastic net prediction models
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
  cv.glmnet(x1[s, ], y[s], alpha = alpha) %>%
    predict(x2[-s, ], s = lambda.type) %>%
    cor(y[-s])
}

#' foo1
#'
#' @param x1 training predictors (dataframe or tibble)
#' @param y response variable
#' @param x2 validation predictors, will be the same as training ones if not provided
#' @param p the proportion of the sample to use for training (vs holdout)
#'
foo1 = function(x1, y, x2 = NULL, p = 0.67) {
  s = sample(nrow(x1), p * nrow(x1))
  if (is.null(x2)) x2 = x1
  x1 = sapply(x1, scale)
  x2 = sapply(x2, scale)
  cv.glmnet(x1[s,], y[s], alpha = 0.05) %>%
    predict(x2[-s,], s = "lambda.min") %>%
    cor(y[-s])
}

# Work in Progress --------------------------------------------------------

#' My LASSO Function
#'
#' @param data the dataset
#' @param x x (character)
#' @param y y (character)
#' @param nrep number of repetitions
#' @param family type; default is 'gaussian'
#' @param alpha alpha value for LASSO; default is 1
#' @param lambda provide own matrix of lambdas to test or use the def
#' @param lambda.min TRUE/FALSE; if FALSE lambda.1se is used instead
#'
my.lasso.function <- function(data, x, y, nrep = 100, family = "gaussian",
                              alpha = 1, lambda = NULL, lambda.min = TRUE) {
  stopifnot(is.character(y))

  data <- data
  y <- data[[y]] #y <- data$`CanDo-Beliefs1`
  #starts_with("CanDo.1.Scale")
  x <- model.matrix(data$`CanDo-Beliefs1` ~
                      data$`CanDo.1.Scale_Very confident` +
                      data$CanDo.1.Scale_Confident +
                      data$`CanDo.1.Scale_Somewhat confident` +
                      data$`CanDo.1.Scale_Not very confident` +
                      data$`CanDo.1.Scale_Not confident at all`)[,-1]

  MSEs <- NULL
  if(is.null(lambda)) lambda <- 10^seq(10, -2, length = nrep)

  for (i in 1:nrep) {
    #CV <- cv.glmnet(x, y, family = family, alpha = alpha)
    CV <- cv.glmnet(x, y, family = family, alpha = alpha, lambda = lambda)
    MSEs <- cbind(MSEs, CV$cvm)
  }
  rownames(MSEs) <- CV$lambda

  if(isTRUE(lambda.min)) {
    lambda.min <- as.numeric(names(which.min(rowMeans(MSEs))))
    fit <- glmnet(x, y, family = family, alpha = alpha, lambda = lambda.min)
  } else {
    #next
    # Note: next used in wrong context: no loop is visible at lasso.R:94
    #lambda.min <- as.numeric(names(which.min(rowMeans(MSEs))))
    #fit <- glmnet(x, y, family = family, alpha = alpha, lambda = CV[["lambda.1se"]])
  }

  # output ####
  out2 <- round(predict(fit, exact = T, type = 'coefficients'), 4)
  pred <- predict(fit, s = lambda.min, x)
  out3 <- cor.test(pred, y, method = "pearson")

  # some stats:
  SSE = sum((pred - y)^2)
  SST = sum((y - mean(y))^2)
  R_square = 1 - SSE/SST #0.2656084 <- substantial
  RMSE = sqrt(SSE/nrow(y)) #0.8560325
  #cor.test(pred, y, method = "pearson") #0.5157 <- very large
  #interpret_r2(0.13) #moderate
  #round(0.5157^2 - 0.2656084,3)
  #0.3^2
  #0.09 R_square

  output <- list(family, alpha, lambda, nrep,
                 MSEs, fit, pred, out2, out3,
                 SSE, SST, R_square, RMSE)
  return(output)
}

