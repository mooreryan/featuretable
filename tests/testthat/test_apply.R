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

test_that("apply_with_* raises with bad margins", {
  ft <- basic_feature_table()

  # With index
  expect_error(ft$apply_with_index(0, function(x, i) x + i),
               class = Error$ArgumentError)
  expect_error(ft$apply_with_index(1.5, function(x, i) x + i),
               class = Error$ArgumentError)
  expect_error(ft$apply_with_index(3, function(x, i) x + i),
               class = Error$ArgumentError)

  # With name
  expect_error(ft$apply_with_name(0, function(x, i) x + i),
               class = Error$ArgumentError)
  expect_error(ft$apply_with_name(1.5, function(x, i) x + i),
               class = Error$ArgumentError)
  expect_error(ft$apply_with_name(3, function(x, i) x + i),
               class = Error$ArgumentError)

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
test_that("apply_with_* gives same answer as apply", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  # With function that returns sample length as argument.
  expect_equal(
    ft$apply(1, function(x) x),
    ft$apply_with_index(1, function(x, i) x)
  )
  expect_equal(
    ft$apply(1, function(x) x),
    ft$apply_with_name(1, function(x, i) x)
  )

  expect_equal(
    ft$apply(2, function(x) x),
    ft$apply_with_index(2, function(x, i) x)
  )
  expect_equal(
    ft$apply(2, function(x) x),
    ft$apply_with_name(2, function(x, i) x)
  )

  expect_equal(
    ft$apply(1, function(x, y) x + y, y = 10),
    ft$apply_with_index(1, function(x, i, y) x + y, y = 10)
  )
  expect_equal(
    ft$apply(1, function(x, y) x + y, y = 10),
    ft$apply_with_name(1, function(x, i, y) x + y, y = 10)
  )

  expect_equal(
    ft$apply(2, function(x, y) x + y, y = 10),
    ft$apply_with_index(2, function(x, i, y) x + y, y = 10)
  )
  expect_equal(
    ft$apply(2, function(x, y) x + y, y = 10),
    ft$apply_with_name(2, function(x, i, y) x + y, y = 10)
  )

  expect_equal(
    ft$apply(1, function(x, y, z) x + y + z, y = 10, z = 0.1),
    ft$apply_with_index(1, function(x, i, y, z) x + y + z, y = 10, z = 0.1)
  )
  expect_equal(
    ft$apply(1, function(x, y, z) x + y + z, y = 10, z = 0.1),
    ft$apply_with_name(1, function(x, i, y, z) x + y + z, y = 10, z = 0.1)
  )

  expect_equal(
    ft$apply(2, function(x, y, z) x + y + z, y = 10, z = 0.1),
    ft$apply_with_index(2, function(x, i, y, z) x + y + z, y = 10, z = 0.1)
  )
  expect_equal(
    ft$apply(2, function(x, y, z) x + y + z, y = 10, z = 0.1),
    ft$apply_with_name(2, function(x, i, y, z) x + y + z, y = 10, z = 0.1)
  )



  #### With reducer type function.
  ####
  expect_equal(
    ft$apply(1, function(x) sum(x)),
    ft$apply_with_index(1, function(x, i) sum(x))
  )
  expect_equal(
    ft$apply(1, function(x) sum(x)),
    ft$apply_with_name(1, function(x, i) sum(x))
  )

  expect_equal(
    ft$apply(2, function(x) sum(x)),
    ft$apply_with_index(2, function(x, i) sum(x))
  )
  expect_equal(
    ft$apply(2, function(x) sum(x)),
    ft$apply_with_name(2, function(x, i) sum(x))
  )

  expect_equal(
    ft$apply(1, function(x, y) sum(x + y), y = 10),
    ft$apply_with_index(1, function(x, i, y) sum(x + y), y = 10)
  )
  expect_equal(
    ft$apply(1, function(x, y) sum(x + y), y = 10),
    ft$apply_with_name(1, function(x, i, y) sum(x + y), y = 10)
  )

  expect_equal(
    ft$apply(2, function(x, y) sum(x + y), y = 10),
    ft$apply_with_index(2, function(x, i, y) sum(x + y), y = 10)
  )
  expect_equal(
    ft$apply(2, function(x, y) sum(x + y), y = 10),
    ft$apply_with_name(2, function(x, i, y) sum(x + y), y = 10)
  )

  expect_equal(
    ft$apply(1, function(x, y, z) sum(x + y + z), y = 10, z = 0.1),
    ft$apply_with_index(1, function(x, i, y, z) sum(x + y + z), y = 10, z = 0.1)
  )
  expect_equal(
    ft$apply(1, function(x, y, z) sum(x + y + z), y = 10, z = 0.1),
    ft$apply_with_name(1, function(x, i, y, z) sum(x + y + z), y = 10, z = 0.1)
  )

  expect_equal(
    ft$apply(2, function(x, y, z) sum(x + y + z), y = 10, z = 0.1),
    ft$apply_with_index(2, function(x, i, y, z) sum(x + y + z), y = 10, z = 0.1)
  )
  expect_equal(
    ft$apply(2, function(x, y, z) sum(x + y + z), y = 10, z = 0.1),
    ft$apply_with_name(2, function(x, i, y, z) sum(x + y + z), y = 10, z = 0.1)
  )

})

test_that("apply with * does fine with unnamed dimnames", {
  dat <- matrix(1:6, 2, 3, dimnames = list(letters[1:2], paste0(letters[1:3], 1:3)))

  expect_equal(
    apply(dat, 1, function(x) x * 10),
    apply_with_index(dat, 1, function(x, i) x * 10)
  )
  expect_equal(
    apply(dat, 1, function(x) x * 10),
    apply_with_name(dat, 1, function(x, i) x * 10)
  )

  expect_equal(
    apply(dat, 2, function(x) x * 10),
    apply_with_index(dat, 2, function(x, i) x * 10)
  )
  expect_equal(
    apply(dat, 2, function(x) x * 10),
    apply_with_name(dat, 2, function(x, i) x * 10)
  )
})

test_that("apply with index does fine with no dimnames", {
  dat <- matrix(1:6, 2, 3)

  expect_equal(
    apply(dat, 1, function(x) x * 10),
    apply_with_index(dat, 1, function(x, i) x * 10)
  )

  expect_equal(
    apply(dat, 2, function(x) x * 10),
    apply_with_index(dat, 2, function(x, i) x * 10)
  )
})

test_that("apply_with_name raises error without proper dimname", {
  dat <- matrix(1:6, 2, 3)
  expect_error(apply_with_name(dat, 1, function(x, i) x * 10),
               class = Error$Error)
  expect_error(apply_with_name(dat, 2, function(x, i) x * 10),
               class = Error$Error)
})

test_that("*_with_name gives correct names", {
  sample_data <- data.frame(
    Count = c(1, 2),
    row.names = c("Sample_1", "Sample_2")
  )
  feature_data <- data.frame(
    Length = c(1, 10, 100),
    row.names = paste0("Feature_", 1:3)
  )
  count_table <- matrix(1, nrow = 2, ncol = 3,
                        dimnames = list(Samples = c("Sample_1", "Sample_2"),
                                        Features = paste0("Feature_", 1:3)))

  ft <- FeatureTable$new(count_table, feature_data, sample_data)

  result <- ft$apply_features_with_name(function(feature, name) {
    feature / feature_data[name, ]
  })
  expected <- matrix(
    c(
      1, 0.1, 0.01,
      1, 0.1, 0.01
    ),
    byrow = TRUE,
    nrow = 2,
    ncol = 3,
    dimnames = dimnames(count_table)
  )
  expect_equal(result, expected)

  # The map version
  result <- ft$map_features_with_name(function(feature, name) {
    feature / feature_data[name, ]
  })
  expected <- FeatureTable$new(expected, feature_data, sample_data)
  expect_equal(result, expected)

  #### Samples
  ####
  result <- ft$apply_samples_with_name(function(sample, name) sample / sample_data[name, ])
  expected <- matrix(
    c(
      1,   1,   1,
      0.5, 0.5, 0.5
    ),
    byrow = FALSE,
    nrow = 3,
    ncol = 2,
    dimnames = dimnames(t(count_table))
  )

  # The map version
  result <- ft$map_samples_with_name(function(sample, name) sample / sample_data[name, ])
  expected <- matrix(
    c(
      1,   1,   1,
      0.5, 0.5, 0.5
    ),
    byrow = TRUE,
    nrow = 2,
    ncol = 3,
    dimnames = dimnames(count_table)
  )
  expected <- FeatureTable$new(expected, feature_data, sample_data)
  expect_equal(result, expected)
})

#### Map uses apply under the hood.
####
####

# maps are a lot like applies, but they return a FeatureTable, so the functions they work with are a bit different.

test_that("map returns a new FT with the same data, but feature table mapped", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expected <- FeatureTable$new(
    apply(testdata$count_table, 2, function(x) x / max(x)),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expected_times_10 <- FeatureTable$new(
    apply(testdata$count_table, 2, function(x, y) x / max(x) * y, y = 10),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$map("features", function(x) x / max(x)), expected)
  expect_equal(ft$map(2, function(x) x / max(x)), expected)
  expect_equal(ft$map_features(function(x) x / max(x)), expected)
  expect_equal(ft$map("features", function(x, y) x / max(x) * y, y = 10), expected_times_10)
  expect_equal(ft$map(2, function(x, y) x / max(x) * y, y = 10), expected_times_10)
  expect_equal(ft$map_features(function(x, y) x / max(x) * y, y = 10), expected_times_10)

  expected <- FeatureTable$new(
    t(apply(testdata$count_table, 1, function(x) x / max(x))),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expected_times_10 <- FeatureTable$new(
    t(apply(testdata$count_table, 1, function(x) x / max(x) * 10)),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$map("samples", function(x) x / max(x)), expected)
  expect_equal(ft$map(1, function(x) x / max(x)), expected)
  expect_equal(ft$map_samples(function(x) x / max(x)), expected)
  expect_equal(ft$map("samples", function(x, y) x / max(x) * y, y = 10), expected_times_10)
  expect_equal(ft$map(1, function(x, y) x / max(x) * y, y = 10), expected_times_10)
  expect_equal(ft$map_samples(function(x, y) x / max(x) * y, y = 10), expected_times_10)
})

test_that("map raises error if the output dimensions aren't correct", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_error(ft$map("samples", sum), class = Error$BadFunctionError)
  expect_error(ft$map(1, sum), class = Error$BadFunctionError)
  expect_error(ft$map_samples(sum), class = Error$BadFunctionError)

  expect_error(ft$map("features", sum), class = Error$BadFunctionError)
  expect_error(ft$map(2, sum), class = Error$BadFunctionError)
  expect_error(ft$map_features(sum), class = Error$BadFunctionError)
})

test_that("map raises error if the margin isn't correct", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_error(ft$map("apples", sum), class = Error$ArgumentError)
  expect_error(ft$map(0, sum), class = Error$ArgumentError)
  expect_error(ft$map(3, sum), class = Error$ArgumentError)
})

test_that("basic map_with_index margin 2 works", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  result <- ft$map_with_index("features", function(x, i, y) (x + i) * y, y = 10)
  result <- ft$map_with_index(2, function(x, i, y) (x + i) * y, y = 10)

  mm <- matrix(
    c(
      10, 60, 110, 160, 210,
      20, 70, 120, 170, 220,
      30, 80, 130, 180, 230,
      40, 90, 140, 190, 240
    ),
    nrow = 4,
    ncol = 5,
    byrow = TRUE,
    dimnames = list(
      Samples = paste0("Sample_", 1:4),
      Features = paste0("Feature_", 1:5)
    )
  )

  expected_result <- FeatureTable$new(mm,
                                      feature_data = testdata$feature_data,
                                      sample_data = testdata$sample_data)

  expect_equal(result, expected_result)
})

test_that("basic map_with_index margin 1 works", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  result <- ft$map_with_index("samples", function(x, i) x)
  result <- ft$map_with_index(1, function(x, i) x)
  expect_equal(result, ft)

  result <- ft$map_samples_with_index(function(x, i) x)
  expect_equal(result, ft)
})

test_that("apply_with raises if dim(X) != 2", {
  expect_error(apply_with_wrapper("index", array(1:27, dim = c(3, 3, 3)), 2, identity),
               class = Error$ArgumentError)
})

# TODO looks like there aren't map_with_name tests
# TODO add 'features' and 'samples' to the apply tests