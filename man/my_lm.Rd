% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_lm.r
\name{my_lm}
\alias{my_lm}
\title{Linear Model Fit}
\usage{
my_lm(formula, df)
}
\arguments{
\item{formula}{An object of class \code{formula} which is a symbolic
description of the model to be fitted.}

\item{df}{A data frame containing the variables used in the model.}
}
\value{
A table with rows for each coefficient (including the Intercept)
and columns for the Estimate, Std. Error, t value, and Pr(>|t|).
}
\description{
This function fits linear model to data passed as a data frame.
}
\examples{
data(mtcars)
my_lm(mpg ~ hp + wt, mtcars)

## no intercept
my_lm(mpg ~ hp + wt - 1, mtcars)

## interaction
my_lm(mpg ~ hp * wt, mtcars)
}
\keyword{inference}
