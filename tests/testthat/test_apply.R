test_that("apply works like regular apply", {
  ft <- FeatureTable$new(testdata$count_table)

  fn <- function(x, y) x * y / sum(x)

  expected_result <- apply(testdata$count_table, 2, fn, y = 10)

  result <- ft$apply(2, fn, y = 10)

  expect_equal(result, expected_result)
})

test_that("apply_samples applies fn to samples", {
  ft <- FeatureTable$new(testdata$count_table)

  fn <- function(x, y) x * y / sum(x)

  expected_result <- apply(testdata$count_table, 1, fn, y = 10)

  result <- ft$apply_samples(fn, y = 10)

  expect_equal(result, expected_result)
})

test_that("apply_features applies fn to samples", {
  ft <- FeatureTable$new(testdata$count_table)

  fn <- function(x, y) x * y / sum(x)

  expected_result <- apply(testdata$count_table, 2, fn, y = 10)

  result <- ft$apply_features(fn, y = 10)

  expect_equal(result, expected_result)
})