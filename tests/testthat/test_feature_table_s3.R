test_that("is works", {
  ft <- basic_feature_table()

  expect_true(is.FeatureTable(ft))
})

test_that("as.data.frame returns data.frame of the feature_table", {
  ft <- basic_feature_table()

  expect_equal(as.data.frame(ft), testdata$count_table)
})

test_that("the s3 reduce function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    reduce(ft, 2, sum),
    ft$reduce(2, sum)
  )
})

test_that("the s3 reduce_features function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    reduce_features(ft, sum),
    ft$reduce_features(sum)
  )
})

test_that("the s3 reduce_samples function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    reduce_samples(ft, sum),
    ft$reduce_samples(sum)
  )
})

test_that("the s3 reduce_all function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    reduce_all(ft, sum),
    ft$reduce_all(sum)
  )
})

################################################################################
#### map #######################################################################
################################################################################

map_test_fn <- function(x) x / max(x)
map_with_fn <- function(x, y) x / max(x)

test_that("the s3 map function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    map(ft, 2, map_test_fn),
    ft$map(2, map_test_fn)
  )
})

test_that("the s3 map_with_index function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    map_with_index(ft, 2, map_with_fn),
    ft$map_with_index(2, map_with_fn)
  )
})

test_that("the s3 map_with_name function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    map_with_name(ft, 2, map_with_fn),
    ft$map_with_name(2, map_with_fn)
  )
})

#### map_features

test_that("the s3 map_features function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    map_features(ft, map_test_fn),
    ft$map_features(map_test_fn)
  )
})

test_that("the s3 map_features_with_index function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    map_features_with_index(ft, map_with_fn),
    ft$map_features_with_index(map_with_fn)
  )
})

test_that("the s3 map_features_with_name function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    map_features_with_name(ft, map_with_fn),
    ft$map_features_with_name(map_with_fn)
  )
})

#### map_samples

test_that("the s3 map_samples function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    map_samples(ft, map_test_fn),
    ft$map_samples(map_test_fn)
  )
})

test_that("the s3 map_samples_with_index function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    map_samples_with_index(ft, map_with_fn),
    ft$map_samples_with_index(map_with_fn)
  )
})

test_that("the s3 map_samples_with_name function matches the R6 version", {
  ft <- basic_feature_table()

  expect_equal(
    map_samples_with_name(ft, map_with_fn),
    ft$map_samples_with_name(map_with_fn)
  )
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