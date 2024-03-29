% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds-model.R
\name{cor_cor}
\alias{cor_cor}
\title{Create and test correlation-based prediction models
Training and validation predictors can be different (x1, x2)}
\usage{
cor_cor(x1, y, x2 = NULL, prop = 0.67)
}
\arguments{
\item{x1}{training predictors (dataframe or tibble)}

\item{y}{response variable}

\item{x2}{validation predictors, will be the same as training ones if not provided}

\item{prop}{the proportion of the sample to use for training (vs holdout)}
}
\description{
Create and test correlation-based prediction models
Training and validation predictors can be different (x1, x2)
}
