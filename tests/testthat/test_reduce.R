test_that("reduce margin 1 works", {
  ft <- FeatureTable$new(testdata$count_table)

  expected_sums <- c(40, 45, 50, 55)
  names(expected_sums) <- ft$sample_names

  expected_diffs <- c(-40, -43, -46, -49)
  names(expected_diffs) <- ft$sample_names

  expect_equal(ft$reduce(1, sum), expected_sums)
  expect_equal(ft$reduce(1, `+`), expected_sums)
  expect_equal(ft$reduce(1, function(a, b) a + b, init = 100), expected_sums + 100)

  expect_equal(ft$reduce(1, `-`), expected_diffs)
  expect_equal(ft$reduce(1, function(a, b) a - b), expected_diffs)

  expected_diffs <- c(8, 9, 10, 11)
  names(expected_diffs) <- ft$sample_names
  expect_equal(ft$reduce(1, function(a, b) b - a), expected_diffs)
})

test_that("reduce margin 2 works", {
  ft <- FeatureTable$new(testdata$count_table)

  expected_sums <- c(6, 22, 38, 54, 70)
  names(expected_sums) <- ft$feature_names

  expected_diffs <- c(-6, -14, -22, -30, -38)
  names(expected_diffs) <- ft$feature_names

  expect_equal(ft$reduce(2, sum), expected_sums)
  expect_equal(ft$reduce(2, `+`), expected_sums)
  expect_equal(ft$reduce(2, function(a, b) a + b, init = 100), expected_sums + 100)

  expect_equal(ft$reduce(2, `-`), expected_diffs)
  expect_equal(ft$reduce(2, function(a, b) a - b), expected_diffs)

  expected_diffs <- c(2, 2, 2, 2, 2)
  names(expected_diffs) <- ft$feature_names
  expect_equal(ft$reduce(2, function(a, b) b - a), expected_diffs)
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