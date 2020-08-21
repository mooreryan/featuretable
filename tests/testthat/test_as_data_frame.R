test_that("as.data.frame() gives you back a data frame of the feature table", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_equal(as.data.frame(ft), testdata$count_table)
})

test_that("as.data.frame() returns samples X features regardless of input orientation", {
  ft <- FeatureTable$new(t(testdata$count_table),
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data,
                         feature_table_rows_are_samples = FALSE)

  expect_equal(as.data.frame(ft), testdata$count_table)
})
