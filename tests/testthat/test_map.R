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
  expect_equal(ft$map(2, function(x) x / max(x)), expected)
  expect_equal(ft$map_features(function(x) x / max(x)), expected)
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
  expect_equal(ft$map(1, function(x) x / max(x)), expected)
  expect_equal(ft$map_samples(function(x) x / max(x)), expected)
  expect_equal(ft$map(1, function(x, y) x / max(x) * y, y = 10), expected_times_10)
  expect_equal(ft$map_samples(function(x, y) x / max(x) * y, y = 10), expected_times_10)
})

test_that("map raises error if the output dimensions aren't correct", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_error(ft$map(1, sum), class = Error$BadFunctionError)
  expect_error(ft$map_samples(sum), class = Error$BadFunctionError)

  expect_error(ft$map(2, sum), class = Error$BadFunctionError)
  expect_error(ft$map_features(sum), class = Error$BadFunctionError)
})

test_that("map raises error if the margin isn't correct", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_error(ft$map(0, sum), class = Error$ArgumentError)
  expect_error(ft$map(3, sum), class = Error$ArgumentError)
})

test_that("basic map_with_index margin 2 works", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

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

  result <- ft$map_with_index(1, function(x, i) x)

  expect_equal(result, ft)
})
