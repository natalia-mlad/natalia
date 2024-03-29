% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds-tidy.R
\name{test_nest}
\alias{test_nest}
\title{Test Nest}
\usage{
test_nest(x, y)
}
\arguments{
\item{x}{a vector}

\item{y}{another vector}
}
\value{
a print statement whether the variables are nested or not
}
\description{
Check whether variable \code{y} is nested in variable \code{x}.
For instance, variable \code{x} could be phone brands and variable \code{y} is phone models of users who completed a questionnaire.
Or variable \code{x} could be country and variable \code{y} is region or city.
Most relevant for factor/character vectors, rather than numeric ones.
Useful for figuring out redundant variables or when trying to avoid multicollinearity.
}
