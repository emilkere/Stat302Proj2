test_that("my_rf_cv fails when number of folds is 1", {
  expect_error(my_rf_cv(1))
})

test_that("my_rf_cv returns numeric and positive", {
  rmse <- my_rf_cv(2)
  expect_true(is.numeric(rmse))
  expect_true(rmse > 0)
})
