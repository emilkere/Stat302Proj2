#' k-Nearest Neighbour Cross-Validatory Classification
#'
#' This function performs k-nearest neighbour cross-validatory classification from training set.
#'
#' @param train data frame of the training set cases.
#' @param cl factor of true classification values for the training data.
#' @param k_nn numeric input, representing the number of neighbors considered.
#' @param k_cv numeric input, representing the number of folds used
#'   for cross-validation.
#' @keywords prediction
#'
#' @return A list object with the following elements: \code{class} - a vector
#'   of predicted classification for all observations, \code{cv_err} - numeric
#'   with with the cross-validation mis-classification error.
#'
#' @examples
#' library(tidyverse)
#' peng_no_na <- my_penguins %>%
#'   select(species, bill_length_mm, bill_depth_mm, flipper_length_mm,
#'   body_mass_g) %>% drop_na()
#'
#' ## Predict output class species
#' peng_cl <- peng_no_na$species
#' len <- length(peng_cl)
#'
#' ## Setup train data
#' peng_no_na_train <- peng_no_na %>% select(-species)
#'
#' ## setup knn with k = 1 and calculate training error
#'
#' k_cv <- 5
#' knn_1 <- my_knn_cv(peng_no_na_train, peng_cl, k_nn = 1, k_cv = k_cv)
#' train_err_1 <- sum(as.integer(knn_1$class) != as.integer(peng_cl)) / len
#'
#' ##setup knn with k = 5 and calculate training error
#' knn_5 <- my_knn_cv(peng_no_na_train, peng_cl, k_nn = 5, k_cv = k_cv)
#' train_err_5 <- sum(as.integer(knn_5$class) != as.integer(peng_cl)) / len
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # input validation
  if (!is.data.frame(train)) {
    stop("Parameter train needs to be data.frame")
  }

  if (!is.factor(cl)) {
    stop("Parameter cl needs to be factor vector")
  }

  len <- nrow(train)
  if (len != length(cl)) {
    stop("Number of rows in train doesn't match the size of the classifications.")
  }

  if (k_nn <= 0) {
    stop("Number of neighbors k_nn needs to be positive.")
  }

  if (k_cv <= 1) {
    stop("Number of folds for cross validation needs to be bigger than 1.")
  }

  # generate random folds
  # within your function, define a variable fold
  fold <- sample(rep(1:k_cv, length = len))

  # vector for misclassification rate at each step
  mc_rate <- rep(0, k_cv)

  for (j in 1:k_cv) {
    tr_id <- which(fold != j)
    tst_id <- which(fold == j)

    # use knn() from the class package to predict the class of the jth fold
    ppred <- class::knn(train[tr_id, ], train[tst_id, ], cl[tr_id], k_nn)
    npreds <- length(tst_id)

    # record the prediction and the misclassification rate
    mc_rate[j] <- (sum(ppred != cl[tst_id])) / npreds
  }

  # store the vector class as the output of knn()
  # with the full data as both the training and the test data
  class <- class::knn(train, train, cl, k_nn)

  # store the value cv_error as the average misclassification rate
  # from your cross validation
  cv_err <- mean(mc_rate)

  # setup the return list
  lret <- list("class" = class, "cv_err" = cv_err)
  return(lret)
}
