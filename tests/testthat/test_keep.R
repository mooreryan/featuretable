test_that("keep can filter wrt features with a predicate function or logical vec", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  result <- ft$keep("features", function(feature) sum(feature) < 35)
  expected <- FeatureTable$new(
    testdata$count_table[, 1:2],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)
  expect_equal(ft$keep("features", c(TRUE, TRUE, FALSE, FALSE, FALSE)), expected)

  result <- ft$keep(2, function(feature) sum(feature) > 35)
  expected <- FeatureTable$new(
    testdata$count_table[, 3:5],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)
  expect_equal(ft$keep("features", c(FALSE, FALSE, TRUE, TRUE, TRUE)), expected)
})

test_that("keep can filter wrt samples with a predicate function", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  result <- ft$keep("samples", function(feature) sum(feature) < 48)
  expected <- FeatureTable$new(
    testdata$count_table[1:2, ],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)
  expect_equal(ft$keep("samples", c(TRUE, TRUE, FALSE, FALSE)), expected)

  result <- ft$keep(1, function(feature) sum(feature) > 48)
  expected <- FeatureTable$new(
    testdata$count_table[3:4, ],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)
  expect_equal(ft$keep("samples", c(FALSE, FALSE, TRUE, TRUE)), expected)
})

test_that("keep handles more interesting predicates", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  # Keep features that have more than one count divisible by 3.
  result <- ft$keep("features", function(feature) sum(feature %% 3 == 0) > 1)
  expected <- FeatureTable$new(
    testdata$count_table[, c("Feature_1", "Feature_4")],
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)

  # Keep samples that have more than one count divisible by 3.
  result <- ft$keep("samples", function(samples) sum(samples %% 3 == 0) > 1)
  expected <- FeatureTable$new(
    testdata$count_table[c("Sample_1", "Sample_3", "Sample_4"), ],
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)
})

test_that("keep raises error if 0 samples or features would be returned", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  # Features
  expect_error(ft$keep("features", function(feature) sum(feature) > 10000),
               class = Error$NoFeaturesRemainingError)

  # Samples
  expect_error(ft$keep("samples", function(sample) sum(sample) > 10000),
               class = Error$NoSamplesRemainingError)
})

test_that("keep works fine if 1 sample or feature would be returned", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  #### Features
  result <- ft$keep("features", function(feature) sum(feature) > 65)
  expected <- FeatureTable$new(
    matrix(testdata$count_table[, 5], nrow = 4, ncol = 1,
           dimnames = list(Samples = paste0("Sample_", 1:4),
                           Features = "Feature_5")),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)

  #### Samples
  result <- ft$keep("samples", function(sample) sum(sample) > 53)
  expected <- FeatureTable$new(
    matrix(testdata$count_table[4, ],
           nrow = 1, ncol = 5,
           dimnames = list(Samples = "Sample_4",
                           Features = paste0("Feature_", 1:5))),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)
})

test_that("keep works fine when all samples are returned", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_equal(ft$keep(1, function(x) sum(x) >= 0), ft)
  expect_equal(ft$keep(2, function(x) sum(x) >= 0), ft)
})

test_that("keep raises error if function (or vec) isn't a predicate", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

    expect_error(ft$keep(1, function(x) x), class = Error$NonPredicateFunctionError)
    expect_error(ft$keep(2, function(x) x), class = Error$NonPredicateFunctionError)
    expect_error(ft$keep(1, 1:4), class = Error$NonPredicateFunctionError)
    expect_error(ft$keep(2, 1:5), class = Error$NonPredicateFunctionError)
})

test_that("keep raises error if a vec of incorrect length is predicate", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_error(ft$keep(1, rep(TRUE, 10)), class = Error$IncorrectLengthError)
  expect_error(ft$keep(2, rep(TRUE, 10)), class = Error$IncorrectLengthError)
})

test_that("NAs in the predicate result get treated like FALSE", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_equal(
    ft$keep(1, c(TRUE, TRUE, FALSE, FALSE)),
    ft$keep(1, c(TRUE, TRUE, NA, NA))
  )
  expect_equal(
    ft$keep(2, c(TRUE, TRUE, FALSE, FALSE, FALSE)),
    ft$keep(2, c(TRUE, TRUE, NA, NA, NA))
  )
})

test_that("the aliases work like base keep", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_equal(
    ft$keep("samples", c(TRUE, TRUE, FALSE, FALSE)),
    ft$keep_samples(c(TRUE, TRUE, FALSE, FALSE))
  )
  expect_equal(
    ft$keep("features", c(TRUE, TRUE, FALSE, FALSE, FALSE)),
    ft$keep_features(c(TRUE, TRUE, FALSE, FALSE, FALSE))
  )
})

test_that("the sample data is available when the margin is samples (1)", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  result <- ft$keep("samples", Location == "Spain")
  expected <- FeatureTable$new(
    matrix(testdata$count_table[c(1, 4), ],
           nrow = 2, ncol = 5,
           dimnames = list(Samples = c("Sample_1", "Sample_4"),
                           Features = paste0("Feature_", 1:5))),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)

  expect_equal(ft$keep_samples(Location == "Spain"), expected)

  # Boop
  expect_equal(ft$keep_samples(ft$sample_data$Location == "Spain"),
               ft$keep_samples(Location == "Spain"))
})

test_that("the feature data is available when the margin is features (2)", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  result <- ft$keep("features", Color == "red")
  expected <- FeatureTable$new(
    matrix(testdata$count_table[, c(1, 3)],
           nrow = 4, ncol = 2,
           dimnames = list(Samples = paste0("Sample_", 1:4),
                           Features = c("Feature_1", "Feature_3"))),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)

  expect_equal(ft$keep_features(Color == "red"), expected)

  # Boop
  expect_equal(ft$keep_features(ft$feature_data$Color == "red"),
               ft$keep_features(Color == "red"))
})

test_that("keep features can remove features with NA in the specified column", {
  ft <- basic_feature_table()

  expected <- FeatureTable$new(
    matrix(ft$data[, c(1, 3, 5)],
           nrow = 4, ncol = 3,
           dimnames = list(Samples = ft$sample_names(),
                           Features = paste("Feature", c(1, 3, 5), sep = "_"))),
    feature_data = ft$feature_data[c(1, 3, 5), ],
    sample_data = ft$sample_data
  )

  predicate <- !is.na(ft$feature_data$Color)

  expect_equal(ft$keep("features", !is.na(Color)), expected)
  expect_equal(ft$keep("features", predicate), expected)
  expect_equal(ft$keep_features(!is.na(Color)), expected)
  expect_equal(ft$keep_features(predicate), expected)

  expect_equal(keep(ft, "features", !is.na(Color)), expected)
  expect_equal(keep(ft, "features", predicate), expected)
  expect_equal(keep_features(ft, !is.na(Color)), expected)
  expect_equal(keep_features(ft, predicate), expected)

  # These don't work since the predicate is bad!
  expect_error(ft$keep_features(!is.na("Color")), class = Error$IncorrectLengthError)
  expect_error(keep_features(ft, !is.na("Color")), class = Error$IncorrectLengthError)
})