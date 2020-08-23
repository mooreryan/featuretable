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

test_that("apply_with_index with 2d margin raises error", {
  ft <- FeatureTable$new(testdata$count_table)

  expect_error(ft$apply_with_index(1:2, function(x, i) x + i))
})

test_that("apply_with_index with non-2d array input raises error", {
  ary <- array(1:27, dim = c(3, 3, 3))

  expect_error(apply_with_index(ary, 1:2, function(x, i) x + i))
})

test_that("apply_with_index raises error with bad function", {
  ft <- FeatureTable$new(testdata$count_table)

  expect_error(ft$apply_with_index(2, function(x) x))
})

test_that("apply_with_index margin 1 works", {
  dimnames_zeros <- list(paste0("row_", 1:2),
                         paste0("col", 1:3))
  dimnames_zeros_margin_1 <- list(paste0("col", 1:3), paste0("row_", 1:2))

  zeros <- as.data.frame(matrix(0, nrow = 2, ncol = 3,
                                dimnames = dimnames_zeros))

  result <- apply_with_index(zeros, 1, function(x, i, y) {
    (x + i) * y
  }, y = 10)

  # Return type is matrix regardless of whether input is data frame or matrix, since that's how apply works.
  expected <- matrix(c(10,10,10, 20,20,20), nrow = 3, ncol = 2,
                     dimnames = dimnames_zeros_margin_1)

  ft <- FeatureTable$new(zeros)
  result <- ft$apply_with_index(1, function(x, i, y) {
    (x + i) * y
  }, y = 10)
  expect_equal(result, expected)

  # TODO I'm not sure if this is actually the best behavior...but it matches what apply would do with the names.
  result <- apply_with_index(zeros, 1, function(x, i, y) {
    i * y
  }, y = 10)
  expected <- c(10, 20)
  names(expected) <- rownames(zeros)
  expect_equal(result, expected)

  result <- ft$apply_with_index(1, function(x, i, y) {
    i * y
  }, y = 10)
  expected <- c(10, 20)
  names(expected) <- rownames(zeros)
  expect_equal(result, expected)

  ####
  #### Without the extra argument.
  ####
  result <- apply_with_index(zeros, 1, function(x, i) {
    (x + i)
  })
  # Return type is matrix regardless of whether input is data frame or matrix, since that's how apply works.
  expected <- matrix(c(1,1,1, 2,2,2), nrow = 3, ncol = 2,
                     dimnames = dimnames_zeros_margin_1)

  ft <- FeatureTable$new(zeros)
  result <- ft$apply_with_index(1, function(x, i) {
    (x + i)
  })
  expect_equal(result, expected)

  result <- apply_with_index(zeros, 1, function(x, i) {
    i
  })
  expected <- c(1, 2)
  names(expected) <- rownames(zeros)
  expect_equal(result, expected)

  result <- ft$apply_with_index(1, function(x, i, y) {
    i
  })
  expected <- c(1, 2)
  names(expected) <- rownames(zeros)
  expect_equal(result, expected)

})

test_that("apply_with_index margin 2 works", {
  dimnames_zeros <- list(paste0("row_", 1:2),
                         paste0("col", 1:3))
  zeros <- as.data.frame(matrix(0, nrow = 2, ncol = 3,
                                dimnames = dimnames_zeros))
  ft <- FeatureTable$new(zeros)

  result <- apply_with_index(zeros, 2, function(x, i) {
    x + i
  })
  # Return type is matrix regardless of whether input is data frame or matrix, since that's how apply works.
  expected <- matrix(c(1,1, 2,2, 3,3), nrow = 2, ncol = 3,
                     dimnames = dimnames_zeros)
  expect_equal(result, expected)

  # Works as R6 function as well.
  result <- ft$apply_with_index(2, function(x, i) {
    x + i
  })
  expect_equal(result, expected)

  result <- apply_with_index(zeros, 2, function(x, i) {
    i
  })
  expected <- 1:3
  names(expected) <- colnames(zeros)
  expect_equal(result, expected)

  # And works with R6.
  result <- ft$apply_with_index(2, function(x, i) {
    i
  })
  expected <- 1:3
  names(expected) <- colnames(zeros)
  expect_equal(result, expected)
})

# See above tests for more detailed testing.
test_that("apply_with_index (feature table) works", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

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
    # TODO if these dimnames were NAMED dimnames, would this work.
    dimnames = dimnames(testdata$count_table),
    byrow = TRUE
  )
  expect_equal(result, expected)

  # With extra aruments.
  result <- apply_with_index(ft$data, 2, function(x, i, y, z) {
    (x + i) * y * z
  }, y = 10, z = 0.1)
  expected <- matrix(
    c(
      1, 6, 11, 16, 21,
      2, 7, 12, 17, 22,
      3, 8, 13, 18, 23,
      4, 9, 14, 19, 24
    ),
    nrow = 4,
    ncol = 5,
    # TODO if these dimnames were NAMED dimnames, would this work.
    dimnames = dimnames(testdata$count_table),
    byrow = TRUE
  )
  expect_equal(result, expected)
})

test_that("apply_samples_with_index is an helper for MARGIN = 1", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_equal(
    ft$apply_with_index(1, function(x, i, y) x + i, y = 10),
    ft$apply_samples_with_index(function(x, i, y) x + i, y = 10)
  )
})

test_that("apply_features_with_index is an helper for MARGIN = 2", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_equal(
    ft$apply_with_index(2, function(x, i, y) x + i, 10),
    ft$apply_features_with_index(function(x, i, y) x + i, 10)
  )
})

# This will also check that the names are the same.
test_that("apply_with_index gives same answer as apply", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  # With function that returns sample length as argument.
  expect_equal(
    ft$apply(1, function(x) x),
    ft$apply_with_index(1, function(x, i) x)
  )

  expect_equal(
    ft$apply(2, function(x) x),
    ft$apply_with_index(2, function(x, i) x)
  )

  expect_equal(
    ft$apply(1, function(x, y) x + y, y = 10),
    ft$apply_with_index(1, function(x, i, y) x + y, y = 10)
  )

  expect_equal(
    ft$apply(2, function(x, y) x + y, y = 10),
    ft$apply_with_index(2, function(x, i, y) x + y, y = 10)
  )

  expect_equal(
    ft$apply(1, function(x, y, z) x + y + z, y = 10, z = 0.1),
    ft$apply_with_index(1, function(x, i, y, z) x + y + z, y = 10, z = 0.1)
  )

  expect_equal(
    ft$apply(2, function(x, y, z) x + y + z, y = 10, z = 0.1),
    ft$apply_with_index(2, function(x, i, y, z) x + y + z, y = 10, z = 0.1)
  )


  #### With reducer type function.
  ####
  expect_equal(
    ft$apply(1, function(x) sum(x)),
    ft$apply_with_index(1, function(x, i) sum(x))
  )

  expect_equal(
    ft$apply(2, function(x) sum(x)),
    ft$apply_with_index(2, function(x, i) sum(x))
  )

  expect_equal(
    ft$apply(1, function(x, y) sum(x + y), y = 10),
    ft$apply_with_index(1, function(x, i, y) sum(x + y), y = 10)
  )

  expect_equal(
    ft$apply(2, function(x, y) sum(x + y), y = 10),
    ft$apply_with_index(2, function(x, i, y) sum(x + y), y = 10)
  )

  expect_equal(
    ft$apply(1, function(x, y, z) sum(x + y + z), y = 10, z = 0.1),
    ft$apply_with_index(1, function(x, i, y, z) sum(x + y + z), y = 10, z = 0.1)
  )

  expect_equal(
    ft$apply(2, function(x, y, z) sum(x + y + z), y = 10, z = 0.1),
    ft$apply_with_index(2, function(x, i, y, z) sum(x + y + z), y = 10, z = 0.1)
  )
})

test_that("apply with index does fine with unnamed dimnames", {
  dat <- matrix(1:6, 2, 3, dimnames = list(letters[1:2], paste0(letters[1:3], 1:3)))

  expect_equal(
    apply(dat, 1, function(x) x * 10),
    apply_with_index(dat, 1, function(x, i) x * 10)
  )
})

test_that("apply with index does fine with no dimnames", {
  dat <- matrix(1:6, 2, 3)

  expect_equal(
    apply(dat, 1, function(x) x * 10),
    apply_with_index(dat, 1, function(x, i) x * 10)
  )
})