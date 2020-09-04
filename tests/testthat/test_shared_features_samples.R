test_that("shared_features returns new FT with only features shared between A and B", {
  ft1 <- FeatureTable$new(
    testdata$count_table[, 1:4],
    testdata$feature_data,
    testdata$sample_data
  )

  ft2 <- FeatureTable$new(
    testdata$count_table[, 2:5],
    testdata$feature_data,
    testdata$sample_data
  )

  shared <- FeatureTable$new(
    testdata$count_table[, 2:4],
    testdata$feature_data,
    testdata$sample_data
  )

  #### Keep

  expect_equal(ft1$shared_features("keep", ft2), shared)
  expect_equal(ft2$shared_features("keep", ft1), shared)
  expect_equal(shared_features(ft1, "keep", ft2), shared)
  expect_equal(shared_features(ft2, "keep", ft1), shared)

  expect_equal(ft1$keep_shared_features(ft2), shared)
  expect_equal(ft2$keep_shared_features(ft1), shared)
  expect_equal(keep_shared_features(ft1, ft2), shared)
  expect_equal(keep_shared_features(ft2, ft1), shared)

  #### Names

  expect_equal(ft1$shared_features("names", ft2), shared$feature_names())
  expect_equal(ft2$shared_features("names", ft1), shared$feature_names())
  expect_equal(shared_features(ft1, "names", ft2), shared$feature_names())
  expect_equal(shared_features(ft2, "names", ft1), shared$feature_names())

  expect_equal(ft1$shared_feature_names(ft2), shared$feature_names())
  expect_equal(ft2$shared_feature_names(ft1), shared$feature_names())
  expect_equal(shared_feature_names(ft1, ft2), shared$feature_names())
  expect_equal(shared_feature_names(ft2, ft1), shared$feature_names())
})

test_that("shared_features with itself gives back the original", {
  ft <- basic_feature_table()

  expect_equal(ft$shared_features("names", ft), ft$feature_names())
  expect_equal(shared_features(ft, "names", ft), ft$feature_names())
  expect_equal(ft$shared_feature_names(ft), ft$feature_names())
  expect_equal(shared_feature_names(ft, ft), ft$feature_names())

  expect_equal(ft$shared_features("keep", ft), ft)
  expect_equal(shared_features(ft, "keep", ft), ft)
  expect_equal(ft$keep_shared_features(ft), ft)
  expect_equal(keep_shared_features(ft, ft), ft)
})

test_that("shared_features raises if the method is invalid", {
  ft <- basic_feature_table()

  expect_error(ft$shared_features("", ft),
               class = Error$ArgumentError)

  expect_error(ft$shared_features("silly", ft),
               class = Error$ArgumentError)
})

test_that("shared_features keep raises if there are no shared features", {
  ft1 <- FeatureTable$new(
    testdata$count_table[, 1:3]
  )
  ft2 <- FeatureTable$new(
    testdata$count_table[, 4:5]
  )

  expect_error(ft1$shared_features("keep", ft2), class = Error$ArgumentError)
  expect_error(ft2$shared_features("keep", ft1), class = Error$ArgumentError)
  expect_error(ft1$keep_shared_features(ft2), class = Error$ArgumentError)
  expect_error(ft2$keep_shared_features(ft1), class = Error$ArgumentError)
  expect_error(shared_features(ft1, "keep", ft2), class = Error$ArgumentError)
  expect_error(shared_features(ft2, "keep", ft1), class = Error$ArgumentError)
  expect_error(keep_shared_features(ft1, ft2), class = Error$ArgumentError)
  expect_error(keep_shared_features(ft2, ft1), class = Error$ArgumentError)
})

test_that("shared_features names gives empty vec if there are no shared features", {
  ft1 <- FeatureTable$new(
    testdata$count_table[, 1:3]
  )
  ft2 <- FeatureTable$new(
    testdata$count_table[, 4:5]
  )

  expected <- vector("character", 0)

  expect_equal(ft1$shared_features("names", ft2), expected)
  expect_equal(ft2$shared_features("names", ft1), expected)
  expect_equal(ft1$shared_feature_names(ft2), expected)
  expect_equal(ft2$shared_feature_names(ft1), expected)
  expect_equal(shared_features(ft1, "names", ft2), expected)
  expect_equal(shared_features(ft2, "names", ft1), expected)
  expect_equal(shared_feature_names(ft1, ft2), expected)
  expect_equal(shared_feature_names(ft2, ft1), expected)
})

test_that("shared features raises if there are no feature names", {
  d <- testdata$count_table
  colnames(d) <- NULL

  ft1 <- FeatureTable$new(
    d
  )
  ft2 <- FeatureTable$new(
    testdata$count_table
  )

  expect_error(ft1$shared_features("keep", ft2), class = Error$MissingFeatureNamesError)
  expect_error(ft2$shared_features("keep", ft1), class = Error$MissingFeatureNamesError)
  expect_error(ft1$keep_shared_features(ft2), class = Error$MissingFeatureNamesError)
  expect_error(ft2$keep_shared_features(ft1), class = Error$MissingFeatureNamesError)
  expect_error(shared_features(ft1, "keep", ft2), class = Error$MissingFeatureNamesError)
  expect_error(shared_features(ft2, "keep", ft1), class = Error$MissingFeatureNamesError)
  expect_error(keep_shared_features(ft1, ft2), class = Error$MissingFeatureNamesError)
  expect_error(keep_shared_features(ft2, ft1), class = Error$MissingFeatureNamesError)

  expect_error(ft1$shared_features("names", ft2), class = Error$MissingFeatureNamesError)
  expect_error(ft2$shared_features("names", ft1), class = Error$MissingFeatureNamesError)
  expect_error(ft1$shared_feature_names(ft2), class = Error$MissingFeatureNamesError)
  expect_error(ft2$shared_feature_names(ft1), class = Error$MissingFeatureNamesError)
  expect_error(shared_features(ft1, "names", ft2), class = Error$MissingFeatureNamesError)
  expect_error(shared_features(ft2, "names", ft1), class = Error$MissingFeatureNamesError)
  expect_error(shared_feature_names(ft1, ft2), class = Error$MissingFeatureNamesError)
  expect_error(shared_feature_names(ft2, ft1), class = Error$MissingFeatureNamesError)
})

test_that("shared features raises if 'other' is not a FeatureTable", {
  d <- testdata$count_table
  colnames(d) <- NULL

  ft1 <- FeatureTable$new(
    d
  )
  ft2 <- "hi ryan"

  expected_error <- Error$ArgumentError

  expect_error(ft1$shared_features("keep", ft2), class = expected_error)
  expect_error(ft1$keep_shared_features(ft2), class = expected_error)
  expect_error(shared_features(ft1, "keep", ft2), class = expected_error)
  expect_error(keep_shared_features(ft1, ft2), class = expected_error)

  expect_error(ft1$shared_features("names", ft2), class = expected_error)
  expect_error(ft1$shared_feature_names(ft2), class = expected_error)
  expect_error(shared_features(ft1, "names", ft2), class = expected_error)
  expect_error(shared_feature_names(ft1, ft2), class = expected_error)
})