test_that("my_lm fails when called with wrong inputs", {
  expect_error(my_lm(1))
})

test_that("my_lm fails when df is not data frame", {
  expect_error(my_lm(x ~ y, df = 1))
})

test_that("my_lm works", {
  x <- c(1, 2, 1, 3, 1, 4)
  y <- c(1, 1, 2, 2, 3, 3)
  df <- data.frame(x = x, y = y)
  mlm <- my_lm(y ~ x, df)
  expect_equal(mlm[1, 1], 1.5)
})
