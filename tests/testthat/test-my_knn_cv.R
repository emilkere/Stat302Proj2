test_that("it fails when train is not data frame", {
  expect_error(my_knn_cv(1,2,3,4))
})

test_that("it fails when cl is not factor", {
  n <- 10
  x <- rnorm(n)
  y <- rnorm(n)
  df <- data.frame(x=x, y=y)
  expect_error(my_knn_cv(df,x,3,4))
})

test_that("it fails when length of train and cl do not match", {
  n <- 10
  x <- rnorm(n)
  y <- rnorm(n)
  df <- data.frame(x=x, y=y)
  cl <- sample(c("a", "b", "b"), n+5, replace=T)
  expect_error(my_knn_cv(df,cl,3,4))
})

test_that("it fails when length of train and cl do not match", {
  n <- 10
  x <- rnorm(n)
  y <- rnorm(n)
  df <- data.frame(x=x, y=y)
  cl <- sample(c("a", "b", "b"), n+5, replace=T)
  expect_error(my_knn_cv(df,cl,3,4))
})

test_that("it fails when number of neighbors is 0", {
  n <- 10
  x <- rnorm(n)
  y <- rnorm(n)
  df <- data.frame(x=x, y=y)
  cl <- as.factor(sample(c("a", "b", "b"), n, replace=T))
  expect_error(my_knn_cv(df,cl,0,4))
})

test_that("it fails when number of folds is 1", {
  n <- 10
  x <- rnorm(n)
  y <- rnorm(n)
  df <- data.frame(x=x, y=y)
  cl <- as.factor(sample(c("a", "b", "b"), n, replace=T))
  expect_error(my_knn_cv(df,cl,1,1))
})

test_that("it fails when number of folds is 1", {
  n <- 10
  x <- c(1:10)
  y <- 1/x^2
  df <- data.frame(x=x, y=y)
  cl <- as.factor(c(rep("a", 5), rep("b", 5)))

  rmknn_cv <- my_knn_cv(df,cl,1,2)
  expect_equal(rmknn_cv$class, cl)
})
