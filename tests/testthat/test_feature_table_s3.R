test_that("is works", {
  ft <- basic_feature_table()

  expect_true(is.FeatureTable(ft))
})

test_that("as.data.frame returns data.frame of the feature_table", {
  ft <- basic_feature_table()

  expect_equal(as.data.frame(ft), testdata$count_table)
})


################################################################################
#### keep ######################################################################
################################################################################

keep_test_fn <- function(x) sum(x) > 35

test_that("the s3 keep function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    keep(ft, 2, keep_test_fn),
    ft$keep(2, keep_test_fn)
  )
})

#### keep_features

test_that("the s3 keep_features function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    keep_features(ft, keep_test_fn),
    ft$keep_features(keep_test_fn)
  )
})

#### keep_samples

test_that("the s3 keep_samples function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    keep_samples(ft, keep_test_fn),
    ft$keep_samples(keep_test_fn)
  )
})
