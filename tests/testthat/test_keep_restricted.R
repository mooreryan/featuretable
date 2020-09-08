test_that("keep features works with 'query'", {
  ft <- otu_feature_table()

  expected <- FeatureTable$new(ft$data[, c(2, 5)], ft$feature_data, ft$sample_data)

  expect_equal(ft$keep("features", query(Shape == "circle")), expected)
  expect_equal(ft$keep_features(query(Shape == "circle")), expected)

  expect_equal(ft$keep("features", query(ft$feature_data$Shape == "circle")), expected)
  expect_equal(ft$keep_features(query(ft$feature_data$Shape == "circle")), expected)

  expect_equal(keep(ft, "features", query(Shape == "circle")), expected)
  expect_equal(keep_features(ft, query(Shape == "circle")), expected)

  expect_equal(keep(ft, "features", query(ft$feature_data$Shape == "circle")), expected)
  expect_equal(keep_features(ft, query(ft$feature_data$Shape == "circle")), expected)
})

test_that("keep features queries can wrap other queries", {
  ft <- otu_feature_table()

  expected <- FeatureTable$new(ft$data[, c(2, 5)], ft$feature_data, ft$sample_data)

  expect_equal(ft$keep("features", query(query(Shape == "circle"))), expected)
  expect_equal(ft$keep_features(query(query(Shape == "circle"))), expected)

  expect_equal(ft$keep("features", query(query(ft$feature_data$Shape == "circle"))), expected)
  expect_equal(ft$keep_features(query(query(ft$feature_data$Shape == "circle"))), expected)

  expect_equal(keep(ft, "features", query(query(Shape == "circle"))), expected)
  expect_equal(keep_features(ft, query(query(Shape == "circle"))), expected)

  expect_equal(keep(ft, "features", query(query(ft$feature_data$Shape == "circle"))), expected)
  expect_equal(keep_features(ft, query(query(ft$feature_data$Shape == "circle"))), expected)
})

test_that("you can join queries with logical operators", {
  ft <- otu_feature_table()

  # TODO test me too (this way doesn't actually work!)
  #expected <- ft$keep_features(Shape == "circle" & Color == "square")

  ### AND

  expected <- ft$keep_features(Color == "red" & Shape == "circle")

  expect_equal(ft$keep("features", query(Shape == "circle" & Color == "red")), expected)
  expect_equal(ft$keep("features", query(Shape == "circle") & query(Color == "red")), expected)
  expect_equal(ft$keep("features", TRUE & query(Shape == "circle") & query(Color == "red")), expected)

  expect_equal(keep(ft, "features", query(Shape == "circle" & Color == "red")), expected)
  expect_equal(keep(ft, "features", query(Shape == "circle") & query(Color == "red")), expected)
  expect_equal(keep(ft, "features", TRUE & query(Shape == "circle") & query(Color == "red")), expected)

  expect_equal(ft$keep_features(query(Shape == "circle" & Color == "red")), expected)
  expect_equal(ft$keep_features(query(Shape == "circle") & query(Color == "red")), expected)
  expect_equal(ft$keep_features(TRUE & query(Shape == "circle") & query(Color == "red")), expected)

  expect_equal(keep_features(ft, query(Shape == "circle" & Color == "red")), expected)
  expect_equal(keep_features(ft, query(Shape == "circle") & query(Color == "red")), expected)
  expect_equal(keep_features(ft, TRUE & query(Shape == "circle") & query(Color == "red")), expected)

  ### OR

  expected <- ft$keep_features(Color == "red" | Shape == "circle")

  expect_equal(ft$keep("features", query(Shape == "circle" | Color == "red")), expected)
  expect_equal(ft$keep("features", query(Shape == "circle") | query(Color == "red")), expected)
  expect_equal(ft$keep("features", FALSE | query(Shape == "circle") | query(Color == "red")), expected)

  expect_equal(keep(ft, "features", query(Shape == "circle" | Color == "red")), expected)
  expect_equal(keep(ft, "features", query(Shape == "circle") | query(Color == "red")), expected)
  expect_equal(keep(ft, "features", FALSE | query(Shape == "circle") | query(Color == "red")), expected)

  expect_equal(ft$keep_features(query(Shape == "circle" | Color == "red")), expected)
  expect_equal(ft$keep_features(query(Shape == "circle") | query(Color == "red")), expected)
  expect_equal(ft$keep_features(FALSE | query(Shape == "circle") | query(Color == "red")), expected)

  expect_equal(keep_features(ft, query(Shape == "circle" | Color == "red")), expected)
  expect_equal(keep_features(ft, query(Shape == "circle") | query(Color == "red")), expected)
  expect_equal(keep_features(ft, FALSE | query(Shape == "circle") | query(Color == "red")), expected)
})

test_that("raises error if query is also defined in the calling environment", {
  ft <- otu_feature_table()
  expected <- ft$keep_features(Shape == "circle")

  query <- function(x) {
    print("~~~~~~~~~~~~~~~query() in the caller~~~~~~~~~~~~~~~")
  }

  expect_equal(ft$keep("features", query(Shape == "circle")), expected)
  expect_warning(ft$keep("features", query(Shape == "circle")), regexp = "'query'")

  expect_equal(ft$keep_features(query(Shape == "circle")), expected)
  expect_warning(ft$keep_features(query(Shape == "circle")), regexp = "'query'")

  expect_equal(keep(ft, "features", query(Shape == "circle")), expected)
  expect_warning(keep(ft, "features", query(Shape == "circle")), regexp = "'query'")

  expect_equal(keep_features(ft, query(Shape == "circle")), expected)
  expect_warning(keep_features(ft, query(Shape == "circle")), regexp = "'query'")
})

test_that("you can use functions over data in queries", {
  ft <- otu_feature_table()

  expected <- ft$keep_features(function(feature) sum(feature) > 5)

  expect_equal(ft$keep("features", query(function(feature) sum(feature) > 5)), expected)
  expect_equal(ft$keep_features(query(function(feature) sum(feature) > 5)), expected)

  expect_equal(keep(ft, "features", query(function(feature) sum(feature) > 5)), expected)
  expect_equal(keep_features(ft, query(function(feature) sum(feature) > 5)), expected)
})

# Finally we get to the point....restricting queries to subsets of the data based on sample_data!

test_that("you can restrict samples to which a query function over features is applied", {
  ft <- otu_feature_table()

  ### KEEP_FEATURES

  expected <- ft$keep_features(c(FALSE, FALSE, FALSE, TRUE, TRUE))

  expect_equal(ft$keep_features(query(function(feature) sum(feature) > 5,
                                      restrict = Season == "Winter")),
               expected)

  expected <- ft$keep_features(c(FALSE, TRUE, FALSE, TRUE, TRUE))

  expect_equal(ft$keep_features(query(Shape == "circle") |
                                  query(function(feature) sum(feature) > 5,
                                        restrict = Season == "Winter")),
               expected)
  # The order doesn't matter!
  expect_equal(ft$keep_features(query(function(feature) sum(feature) > 5,
                                      restrict = Season == "Winter") |
                                  query(Shape == "circle")),
               expected)

  expect_equal(ft$keep_features(Shape == "circle" |
                                  query(function(feature) sum(feature) > 5,
                                        restrict = Season == "Winter")),
               expected)
  # The order doesn't matter!
  expect_equal(ft$keep_features(query(function(feature) sum(feature) > 5,
                                      restrict = Season == "Winter") |
                                  Shape == "circle"),
               expected)

  # s3 versions

  expected <- ft$keep_features(c(FALSE, FALSE, FALSE, TRUE, TRUE))

  expect_equal(keep_features(ft, query(function(feature) sum(feature) > 5,
                                   restrict = Season == "Winter")),
               expected)

  expected <- ft$keep_features(c(FALSE, TRUE, FALSE, TRUE, TRUE))

  expect_equal(keep_features(ft, query(Shape == "circle") |
                                  query(function(feature) sum(feature) > 5,
                                        restrict = Season == "Winter")),
               expected)
  # The order doesn't matter!
  expect_equal(keep_features(ft, query(function(feature) sum(feature) > 5,
                                      restrict = Season == "Winter") |
                                  query(Shape == "circle")),
               expected)

  expect_equal(keep_features(ft, Shape == "circle" |
                                  query(function(feature) sum(feature) > 5,
                                        restrict = Season == "Winter")),
               expected)
  # The order doesn't matter!
  expect_equal(keep_features(ft, query(function(feature) sum(feature) > 5,
                                      restrict = Season == "Winter") |
                                  Shape == "circle"),
               expected)


  ### KEEP("FEATURES")

  expected <- ft$keep_features(c(FALSE, FALSE, FALSE, TRUE, TRUE))

  expect_equal(ft$keep("features", query(function(feature) sum(feature) > 5,
                                         restrict = Season == "Winter")),
               expected)

  expected <- ft$keep("features", c(FALSE, TRUE, FALSE, TRUE, TRUE))

  expect_equal(ft$keep("features", query(Shape == "circle") |
                         query(function(feature) sum(feature) > 5,
                               restrict = Season == "Winter")),
               expected)
  # The order doesn't matter!
  expect_equal(ft$keep("features", query(function(feature) sum(feature) > 5,
                                         restrict = Season == "Winter") |
                         query(Shape == "circle")),
               expected)

  expect_equal(ft$keep("features", Shape == "circle" |
                         query(function(feature) sum(feature) > 5,
                               restrict = Season == "Winter")),
               expected)
  # The order doesn't matter!
  expect_equal(ft$keep("features", query(function(feature) sum(feature) > 5,
                                         restrict = Season == "Winter") |
                         Shape == "circle"),
               expected)

  # s3

  expected <- ft$keep_features(c(FALSE, FALSE, FALSE, TRUE, TRUE))

  expect_equal(keep(ft, "features", query(function(feature) sum(feature) > 5,
                                         restrict = Season == "Winter")),
               expected)

  expected <- ft$keep("features", c(FALSE, TRUE, FALSE, TRUE, TRUE))

  expect_equal(keep(ft, "features", query(Shape == "circle") |
                         query(function(feature) sum(feature) > 5,
                               restrict = Season == "Winter")),
               expected)
  # The order doesn't matter!
  expect_equal(keep(ft, "features", query(function(feature) sum(feature) > 5,
                                         restrict = Season == "Winter") |
                         query(Shape == "circle")),
               expected)

  expect_equal(keep(ft, "features", Shape == "circle" |
                         query(function(feature) sum(feature) > 5,
                               restrict = Season == "Winter")),
               expected)
  # The order doesn't matter!
  expect_equal(keep(ft, "features", query(function(feature) sum(feature) > 5,
                                         restrict = Season == "Winter") |
                         Shape == "circle"),
               expected)

})

test_that("using restrict with non-function queries raises", {
  # function type queries are applied to the data, so using them with non-anonymous functions
  # shows warning.

  ft <- otu_feature_table()

  expect_error(ft$keep("features", query(Shape == "circle", restrict = Season == "Winter")),
               class = Error$ArgumentError)
  expect_error(ft$keep_features(query(Shape == "circle", restrict = Season == "Winter")),
               class = Error$ArgumentError)
  expect_error(keep(ft, "features", query(Shape == "circle", restrict = Season == "Winter")),
               class = Error$ArgumentError)
  expect_error(keep_features(ft, query(Shape == "circle", restrict = Season == "Winter")),
               class = Error$ArgumentError)
})
