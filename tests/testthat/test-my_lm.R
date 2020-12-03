test_that("numeric output throws error", {
  expect_error(my_lm(1))
})

test_that("numeric output throws error", {
  expect_error(my_lm(x~y, df=1))
})
