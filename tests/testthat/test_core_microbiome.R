test_that("core_microbiome works with min_sample_proportion argument", {
  ft <- otu_feature_table()

  # Counts look like this:
  # (
  #   0, 0, 0, 1, 10, | 11
  #   0, 0, 1, 2, 20, | 23
  #   0, 1, 2, 3, 30, | 36
  #   1, 2, 3, 4, 40  | 50
  #   ----------------|
  #   1  3  6  10 100
  # )

  actual <- ft$core_microbiome(detection_limit = 1, min_sample_proportion = 0/4)
  expect_equal(actual, ft)
  actual <- core_microbiome(ft, detection_limit = 1, min_sample_proportion = 0/4)
  expect_equal(actual, ft)

  actual <- ft$core_microbiome(detection_limit = 1, min_sample_proportion = 1/4)
  expect_equal(actual, ft)
  actual <- core_microbiome(ft, detection_limit = 1, min_sample_proportion = 1/4)
  expect_equal(actual, ft)

  expected <- FeatureTable$new(ft$data[, 2:5], ft$feature_data, ft$sample_data)
  actual <- ft$core_microbiome(detection_limit = 1, min_sample_proportion = 2/4)
  expect_equal(actual, expected)
  actual <- core_microbiome(ft, detection_limit = 1, min_sample_proportion = 2/4)
  expect_equal(actual, expected)

  expected <- FeatureTable$new(ft$data[, 3:5], ft$feature_data, ft$sample_data)
  actual <- ft$core_microbiome(detection_limit = 1, min_sample_proportion = 3/4)
  expect_equal(actual, expected)
  actual <- core_microbiome(ft, detection_limit = 1, min_sample_proportion = 3/4)
  expect_equal(actual, expected)

  expected <- FeatureTable$new(ft$data[, 4:5], ft$feature_data, ft$sample_data)
  actual <- ft$core_microbiome(detection_limit = 1, min_sample_proportion = 4/4)
  expect_equal(actual, expected)
  actual <- core_microbiome(ft, detection_limit = 1, min_sample_proportion = 4/4)
  expect_equal(actual, expected)
})

test_that("core_microbiome treats min_samples numeric count argument", {
  ft <- otu_feature_table()

  # Counts look like this:
  # (
  #   0, 0, 0, 1, 10, | 11
  #   0, 0, 1, 2, 20, | 23
  #   0, 1, 2, 3, 30, | 36
  #   1, 2, 3, 4, 40  | 50
  #   ----------------|
  #   1  3  6  10 100
  # )

  actual <- ft$core_microbiome(detection_limit = 1, min_samples = 0)
  expect_equal(actual, ft)
  actual <- core_microbiome(ft, detection_limit = 1, min_samples = 0)
  expect_equal(actual, ft)

  actual <- ft$core_microbiome(detection_limit = 1, min_samples = 1)
  expect_equal(actual, ft)
  actual <- core_microbiome(ft, detection_limit = 1, min_samples = 1)
  expect_equal(actual, ft)

  expected <- FeatureTable$new(ft$data[, 2:5], ft$feature_data, ft$sample_data)
  actual <- ft$core_microbiome(detection_limit = 1, min_samples = 2)
  expect_equal(actual, expected)
  actual <- core_microbiome(ft, detection_limit = 1, min_samples = 2)
  expect_equal(actual, expected)

  expected <- FeatureTable$new(ft$data[, 3:5], ft$feature_data, ft$sample_data)
  actual <- ft$core_microbiome(detection_limit = 1, min_samples = 3)
  expect_equal(actual, expected)
  actual <- core_microbiome(ft, detection_limit = 1, min_samples = 3)
  expect_equal(actual, expected)

  expected <- FeatureTable$new(ft$data[, 4:5], ft$feature_data, ft$sample_data)
  actual <- ft$core_microbiome(detection_limit = 1, min_samples = 4)
  expect_equal(actual, expected)
  actual <- core_microbiome(ft, detection_limit = 1, min_samples = 4)
  expect_equal(actual, expected)
})

test_that("core_microbiome raises if both min_samples and min_sample_proportion are given", {
  ft <- otu_feature_table()

  expect_error(ft$core_microbiome(min_samples = 2, min_sample_proportion = 0.5),
               class = Error$ArgumentError)
  expect_error(core_microbiome(ft, min_samples = 2, min_sample_proportion = 0.5),
               class = Error$ArgumentError)
})

test_that("core_microbiome raises if neither min_samples nor min_sample_proportion are given", {
  ft <- otu_feature_table()

  expect_error(ft$core_microbiome(),
               class = Error$ArgumentError)
  expect_error(core_microbiome(ft),
               class = Error$ArgumentError)
})

test_that("core_microbiome raises if min_sample_proportion is not a proportion", {
  ft <- otu_feature_table()

  expect_error(ft$core_microbiome(min_sample_proportion = 1.3),
               class = Error$ArgumentError)
  expect_error(core_microbiome(ft, min_sample_proportion = 1.3),
               class = Error$ArgumentError)

  expect_error(ft$core_microbiome(min_sample_proportion = -1),
               class = Error$ArgumentError)
  expect_error(core_microbiome(ft, min_sample_proportion = -1),
               class = Error$ArgumentError)

  expect_error(ft$core_microbiome(min_sample_proportion = 2),
               class = Error$ArgumentError)
  expect_error(core_microbiome(ft, min_sample_proportion = 2),
               class = Error$ArgumentError)
})

test_that("core_microbiome raises if min_samples < 0 or > num_samples", {
  ft <- otu_feature_table()

  expect_error(ft$core_microbiome(min_samples = -1),
               class = Error$ArgumentError)
  expect_error(core_microbiome(ft, min_samples = -1),
               class = Error$ArgumentError)

  expect_error(ft$core_microbiome(min_samples = -1.5),
               class = Error$ArgumentError)
  expect_error(core_microbiome(ft, min_samples = -1.5),
               class = Error$ArgumentError)

  expect_error(ft$core_microbiome(min_samples = 4.1),
               class = Error$ArgumentError)
  expect_error(core_microbiome(ft, min_samples = 4.1),
               class = Error$ArgumentError)

  expect_error(ft$core_microbiome(min_samples = 5),
               class = Error$ArgumentError)
  expect_error(core_microbiome(ft, min_samples = 5),
               class = Error$ArgumentError)
})

test_that("core_microbiome warns if min_samples is a proportion", {
  ft <- otu_feature_table()

  expect_warning(ft$core_microbiome(min_samples = 0.0001),
                 regexp = "looks like a proportion")
  expect_warning(core_microbiome(ft, min_samples = 0.0001),
                 regexp = "looks like a proportion")

  expect_warning(ft$core_microbiome(min_samples = 0.5),
                 regexp = "looks like a proportion")
  expect_warning(core_microbiome(ft, min_samples = 0.5),
                 regexp = "looks like a proportion")

  expect_warning(ft$core_microbiome(min_samples = 0.99999),
                 regexp = "looks like a proportion")
  expect_warning(core_microbiome(ft, min_samples = 0.99999),
                 regexp = "looks like a proportion")
})

test_that("core_microbiome works with fractional detection limit", {
  ft <- otu_feature_table()
  ft <- ft$map_features(function(feature) feature / sum(feature))

  #         Features
  # Samples    Feature_1 Feature_2 Feature_3 Feature_4 Feature_5
  #   Sample_1         0 0.0000000 0.0000000       0.1       0.1
  #   Sample_2         0 0.0000000 0.1666667       0.2       0.2
  #   Sample_3         0 0.3333333 0.3333333       0.3       0.3
  #   Sample_4         1 0.6666667 0.5000000       0.4       0.4

  expected <- FeatureTable$new(ft$data[, 4:5], ft$feature_data, ft$sample_data)
  actual <- ft$core_microbiome(detection_limit = 0.18, min_samples = 3)
  expect_equal(actual, expected)
  actual <- core_microbiome(ft, detection_limit = 0.18, min_samples = 3)
  expect_equal(actual, expected)
})