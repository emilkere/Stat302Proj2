test_that("incorrect alternative input gives error", {
  expect_error(my_t.test(5, "two-sided", 34))
})

test_that("two sided test works", {
  x <- c(0, 0, 1, 0, 0, 0)
  r_myt <- my_t.test(x)
  expect_equal(r_myt$test_stat, 1)
  expect_equal(r_myt$df, 5)
  expect_equal(r_myt$alternative, "two.sided")
  expect_lt(abs(r_myt$"p-val" - 0.3632175), 0.001)
})


test_that("less test works", {
  x <- c(0, 0, 1, 0, 0, 0)
  r_myt <- my_t.test(x, alternative = "less")
  expect_equal(r_myt$test_stat, 1)
  expect_equal(r_myt$df, 5)
  expect_equal(r_myt$alternative, "less")
  expect_lt(abs(r_myt$"p-val" - 0.8183913), 0.001)
})

test_that("greater test works", {
  x <- c(0, 0, 1, 0, 0, 0)
  r_myt <- my_t.test(x, alternative = "greater")
  expect_equal(r_myt$test_stat, 1)
  expect_equal(r_myt$df, 5)
  expect_equal(r_myt$alternative, "greater")
  expect_lt(abs(r_myt$"p-val" - 0.1816087), 0.001)
})

test_that("mu test works", {
  x <- c(1, 0, 1, 1, 1, 1)
  r_myt <- my_t.test(x, mu = 1)
  expect_equal(r_myt$test_stat, -1)
  expect_equal(r_myt$df, 5)
  expect_equal(r_myt$alternative, "two.sided")
  expect_lt(abs(r_myt$"p-val" - 0.3632175), 0.001)
})
