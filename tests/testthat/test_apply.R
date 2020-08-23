# Apply is like the basic apply.

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

test_that("apply_with_index margin 1 works", {
  zeros <- as.data.frame(matrix(0, nrow = 2, ncol = 3,
                                dimnames = list(paste0("row_", 1:2),
                                                paste0("col", 1:3))))

  result <- apply_with_index(zeros, 1, function(x, i) {
    x + i
  })
  # Return type is matrix regardless of whether input is data frame or matrix, since that's how apply works.
  expected <- matrix(c(1,1,1, 2,2,2), nrow = 3, ncol = 2,
                     dimnames = list(paste0("col", 1:3), NULL))

  ft <- FeatureTable$new(zeros)
  result <- ft$apply_with_index(1, function(x, i) {
    x + i
  })
  expect_equal(result, expected)

  result <- apply_with_index(zeros, 1, function(x, i) {
    i
  })
  expect_equal(result, 1:2)

  result <- ft$apply_with_index(1, function(x, i) {
    i
  })
  expect_equal(result, 1:2)
})

test_that("apply_with_index margin 2 works", {
  zeros <- as.data.frame(matrix(0, nrow = 2, ncol = 3))
  ft <- FeatureTable$new(zeros)

  result <- apply_with_index(zeros, 2, function(x, i) {
    x + i
  })
  # Return type is matrix regardless of whether input is data frame or matrix, since that's how apply works.
  expected <- matrix(c(1,1, 2,2, 3,3), nrow = 2, ncol = 3)
  expect_equal(result, expected)

  # Works as R6 function as well.
  result <- ft$apply_with_index(2, function(x, i) {
    x + i
  })
  expect_equal(result, expected)


  result <- apply_with_index(zeros, 2, function(x, i) {
    i
  })
  expect_equal(result, 1:3)

  # And works with R6.
  result <- ft$apply_with_index(2, function(x, i) {
    i
  })
  expect_equal(result, 1:3)
})

# See above tests for more detailed testing.
test_that("apply_with_index (feature table) works", {
  ft <- FeatureTable$new(testdata$count_table)

  result <- apply_with_index(ft$data, 2, function(x, i) {
    x + i
  })

  expected <- matrix(
    c(
      1, 6, 11, 16, 21,
      2, 7, 12, 17, 22,
      3, 8, 13, 18, 23,
      4, 9, 14, 19, 24
    ),
    nrow = 4,
    ncol = 5,
    dimnames = list(paste0("Sample_", 1:4), NULL),
    byrow = TRUE
  )

  expect_equal(result, expected)
})


