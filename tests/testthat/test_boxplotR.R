# tests/testthat/test_boxplotR.R

test_that("boxplotR works with iris dataset", {
  data(iris)
  plots <- boxplotR(iris)
  expect_true(length(plots) > 0)
  expect_s3_class(plots[[1]], "ggplot")
})

test_that("boxplotR errors on non-data frame input", {
  expect_error(boxplotR(1:10), "Input must be a data frame.")
})

test_that("boxplotR errors on data frame without factor variables", {
  df <- data.frame(a = 1:10, b = 11:20)
  expect_error(boxplotR(df), "Data frame must contain at least one numeric and one factor variable.")
})
