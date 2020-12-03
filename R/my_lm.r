#' Linear Model Fit
#'
#' This function fits linear model to data passed as a data frame.
#
#' @param formula An object of class \code{formula} which is a symbolic description
#'    of the model to be fitted.
#' @param df A data frame containing the variables used in the model.
#' @keywords inference
#'
#' @return A table with rows for each coefficient (including the Intercept)
#'   and columns for the Estimate, Std. Error, t value, and Pr(>|t|).
#'
#' @examples
#' data(mtcars)
#' my_lm(mpg ~ hp + wt, mtcars)
#'
#' ## no intercept
#' my_lm(mpg ~ hp + wt - 1, mtcars)
#'
#' ## interaction
#' my_lm(mpg ~ hp * wt, mtcars)
#'
#' @export
my_lm <- function(formula, df) {
  # validate input parameters
  if (!is.object(formula)) {
    stop("Argument formula must be an object of type formula.")
  }
  if (!is.data.frame(df)) {
    stop("Argument df must be data frame.")
  }

  X <- model.matrix(formula, df)
  Y <- model.response(model.frame(formula, df))

  X_t <- t(X)
  kern_X <- solve(X_t %*% X)
  beta_hat <- kern_X %*% X_t %*% Y
  n_cov <- nrow(beta_hat)

  dgf <- length(Y) - n_cov

  sigma_2 <- sum((Y - X %*% beta_hat)^2 / dgf)
  std_err <- sqrt(diag(sigma_2 * kern_X))
  t_val <- beta_hat / std_err

  pr_t <- rep(0, n_cov)
  for (j in 1:n_cov) {
    pr_t[j] <- 2 * pt(abs(t_val[j]), dgf, lower.tail = F)
  }

  # setup return values
  mret <- cbind(beta_hat)
  mret <- cbind(mret, std_err)
  mret <- cbind(mret, t_val)
  mret <- cbind(mret, pr_t)

  # set column names
  colnames(mret) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  return(mret)
}
