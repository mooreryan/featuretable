# Testing how functions work together in a common workflow.

test_that("thing", {
  ft <- ft_for_collapse_testing()

  actual <- ft$keep_features(Color == "blue")$collapse_features(Color)
  expected <- FeatureTable$new(
    matrix(c(11,
             22,
             33,
             44),
           nrow = 4, ncol = 1, byrow = TRUE,
           dimnames = list(Samples = ft$sample_names(),
                           Features = "blue")),
    feature_data = data.frame(
      Color = "blue",
      row.names = "blue"
    ),
    sample_data = ft$sample_data
  )

  expect_equal(actual, expected)

  ####


})

test_that("the sorting of factors is stable even when dropping levels", {
  ft <- ft_for_collapse_testing()

  #           Color  Shape Length
  # Feature_1   red square    5.0
  # Feature_2   red square    6.0
  # Feature_3 green square    2.3
  # Feature_4  blue square    7.0
  # Feature_5  blue square   10.0

  # Notice that the keep function will give green, blue, blue for color. "Auto" factor level sorting would make the new levels green, blue. But, we want them to match the original.


  actual <- ft$keep_features(Color == "blue" | Color == "green")$collapse_features(Color)
  expected <- FeatureTable$new(
    matrix(c(11, 0,
             22, 1,
             33, 2,
             44, 3),
           nrow = 4, ncol = 2, byrow = TRUE,
           dimnames = list(Samples = ft$sample_names(),
                           Features = c("blue", "green"))),
    feature_data = data.frame(
      Color = c("blue", "green"),
      row.names = c("blue", "green")
    ),
    sample_data = ft$sample_data
  )

  expect_equal(actual, expected)

})