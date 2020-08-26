test_that("reduce margin 1 works", {
  ft <- FeatureTable$new(testdata$count_table)

  expected_sums <- c(40, 45, 50, 55)
  names(expected_sums) <- ft$sample_names()

  expected_diffs <- c(-40, -43, -46, -49)
  names(expected_diffs) <- ft$sample_names()

  expect_equal(ft$reduce(1, sum), expected_sums)
  expect_equal(ft$reduce(1, `+`), expected_sums)
  expect_equal(ft$reduce(1, function(a, b) a + b, init = 100), expected_sums + 100)

  # And check the helper version.
  expect_equal(ft$reduce_samples(sum), ft$reduce(1, sum))
  expect_equal(ft$reduce_samples(`+`), ft$reduce(1, `+`))
  expect_equal(ft$reduce_samples(function(a, b) a + b, init = 100),
               ft$reduce(1, function(a, b) a + b, init = 100))


  expect_equal(ft$reduce(1, `-`), expected_diffs)
  expect_equal(ft$reduce(1, function(a, b) a - b), expected_diffs)

  # And the helper version
  expect_equal(ft$reduce_samples(`-`), ft$reduce(1, `-`))
  expect_equal(ft$reduce_samples(function(a, b) a - b), ft$reduce(1, function(a, b) a - b))

  expected_diffs <- c(8, 9, 10, 11)
  names(expected_diffs) <- ft$sample_names()
  expect_equal(ft$reduce(1, function(a, b) b - a), expected_diffs)
  expect_equal(ft$reduce_samples(function(a, b) b - a), ft$reduce(1, function(a, b) b - a))
})

test_that("reduce margin 2 works", {
  ft <- FeatureTable$new(testdata$count_table)

  expected_sums <- c(6, 22, 38, 54, 70)
  names(expected_sums) <- ft$feature_names()

  expected_diffs <- c(-6, -14, -22, -30, -38)
  names(expected_diffs) <- ft$feature_names()

  expect_equal(ft$reduce(2, sum), expected_sums)
  expect_equal(ft$reduce(2, `+`), expected_sums)
  expect_equal(ft$reduce(2, function(a, b) a + b, init = 100), expected_sums + 100)

  # Test helpers
  expect_equal(ft$reduce_features(sum), ft$reduce(2, sum))
  expect_equal(ft$reduce_features(`+`), ft$reduce(2, `+`))
  expect_equal(ft$reduce_features(function(a, b) a + b, init = 100),
               ft$reduce(2, function(a, b) a + b, init = 100))


  expect_equal(ft$reduce(2, `-`), expected_diffs)
  expect_equal(ft$reduce(2, function(a, b) a - b), expected_diffs)

  # Test helpers
  expect_equal(ft$reduce_features(`-`), ft$reduce(2, `-`))
  expect_equal(ft$reduce_features(function(a, b) a - b),
               ft$reduce(2, function(a, b) a - b))

  expected_diffs <- c(2, 2, 2, 2, 2)
  names(expected_diffs) <- ft$feature_names()
  expect_equal(ft$reduce(2, function(a, b) b - a), expected_diffs)
  expect_equal(ft$reduce_features(function(a, b) b - a),
               ft$reduce(2, function(a, b) b - a))
})

test_that("reduce gives an error when you're not actually sending a reduce type function", {
  ft <- FeatureTable$new(testdata$count_table)

  expect_error(ft$reduce(2, sqrt))
})

test_that("reduce with margin not 1 or 2 raises an error", {
  ft <- FeatureTable$new(testdata$count_table)

  expect_error(ft$reduce(1:2, sum), class = Error$ArgumentError)
  expect_error(ft$reduce(0, sum), class = Error$ArgumentError)
  expect_error(ft$reduce(3, sum), class = Error$ArgumentError)
})

test_that("reduce_all reduces on all values in the feature_table", {
  ft <- FeatureTable$new(testdata$count_table)

  expect_equal(ft$reduce_all(sum), sum(ft$data))
})

test_that("reduce_all raises error on non reduce style function", {
  ft <- FeatureTable$new(testdata$count_table)

  expect_error(ft$reduce_all(sqrt))
})