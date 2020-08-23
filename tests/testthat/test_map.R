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
  expect_equal(ft$map(2, function(x) x / max(x)), expected)
  expect_equal(ft$map_features(function(x) x / max(x)), expected)

  expected <- FeatureTable$new(
    t(apply(testdata$count_table, 1, function(x) x / max(x))),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$map(1, function(x) x / max(x)), expected)
  expect_equal(ft$map_samples(function(x) x / max(x)), expected)
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

