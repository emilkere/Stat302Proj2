#' Student's t-test
#'
#' This function performs a one sample Student's t-test on vector data.
#'
#' @param x Numeric vector of data to be tested.
#' @param alternative Character string specifying the alternative hypothesis.
#'      Accepts \code{"two.sided"}, \code{"less"}, or \code{"greater"},
#'      defaults to \code{"two.sided"}.
#' @param mu Numeric input indicating the null hypothesis value of the mean,
#'      defaults to 0.
#' @keywords inference
#'
#' @return A list with the following elements: \code{test_stat} representing
#'   the numeric test statistic, \code{df} representing the degrees
#'   of freedom, \code{alternative} with the value of the parameter \code{alternative},
#'   and \code{p_val} the numeric p-value.
#'
#' @examples
#' tdata <- rnorm(100, 5, 1)
#' my_t.test(tdata, mu = 5)
#'
#' tdata <- rnorm(1000, 1, 1)
#' my_t.test(tdata, mu = 1, alternative = "greater")
#'
#' @export
my_t.test <- function(x, alternative = "two.sided", mu = 0) {
  # validate input
  alternative <- str_to_lower(alternative)
  alt_opptions <- c("two.sided", "less", "greater")
  if (!alternative %in% alt_opptions) {
    stop(paste(
      "Argument: alternative, Value:", alternative,
      ' is not supported. Supported values are "two.sided", "less", or "greater".'
    ))
  }

  sample_size <- length(x)
  theta_0 <- mu
  theta_hat <- mean(x)
  se_theta_hat <- sd(x) / sqrt(sample_size)
  # calculate the t statistic
  t_stat <- (theta_hat - theta_0) / se_theta_hat
  df <- sample_size - 1

  p_val <- switch(alternative,
                  two.sided = 2 * pt(abs(t_stat), df, lower.tail = F),
                  less = pt(t_stat, df, lower.tail = T),
                  greater = pt(t_stat, df, lower.tail = F)
  )

  # set up the return list
  lret <- list("test_stat" = t_stat, "df" = df, "alternative" = alternative, "p-val" = p_val)
  return(lret)
}
