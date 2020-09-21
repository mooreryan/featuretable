test_that("collapse_features works if the user changes an entire column of metadata", {

  ft <- ft_for_collapse_testing()
  ft$feature_data[, "Color"] <- rep("green", 5)

  expected <- FeatureTable$new(
    matrix(c(11,
             23,
             36,
             50),
           nrow = 4, ncol = 1, byrow = TRUE,
           dimnames = list(Samples = ft$sample_names(),
                           Features = "green")),
    feature_data = data.frame(
      Color = "green",
      row.names = "green"
    ),
    sample_data = ft$sample_data
  )

  expect_equal(ft$collapse("features", "Color"), expected)
  expect_equal(collapse(ft, "features", "Color"), expected)
  expect_equal(ft$collapse_features("Color"), expected)
  expect_equal(collapse_features(ft, "Color"), expected)

  # And this one works because the user 'fixes' the factor.

  expected <- FeatureTable$new(
    matrix(c(11,
             23,
             36,
             50),
           nrow = 4, ncol = 1, byrow = TRUE,
           dimnames = list(Samples = ft$sample_names(),
                           Features = "Green")),
    feature_data = data.frame(
      Color = "Green",
      row.names = "Green"
    ),
    sample_data = ft$sample_data
  )

  ft <- ft_for_collapse_testing()
  ft$feature_data[, "Color"] <- factor(rep("Green", 5), levels = "Green")

  expect_equal(ft$collapse("features", "Color"), expected)
  expect_equal(collapse(ft, "features", "Color"), expected)
  expect_equal(ft$collapse_features("Color"), expected)
  expect_equal(collapse_features(ft, "Color"), expected)

  # This one works too since there are no factors levels originally.
  ft <- ft_for_collapse_testing()
  ft$feature_data[, "Color"] <- rep("Green", 5)

  expect_equal(ft$collapse("features", "Color"), expected)
  expect_equal(collapse(ft, "features", "Color"), expected)
  expect_equal(ft$collapse_features("Color"), expected)
  expect_equal(collapse_features(ft, "Color"), expected)
})

test_that("collapse features it works fine if user drops a factor level out by accident", {
  ft <- ft_for_collapse_testing()

  # This will cause the 'Portugal' level to not be present in the column.
  ft$feature_data[3, "Color"] <- "blue"

  expected <- FeatureTable$new(
    matrix(c(11, 0,
             23, 0,
             35, 1,
             47, 3),
           nrow = 4, ncol = 2, byrow = TRUE,
           dimnames = list(Samples = ft$sample_names(),
                           Features = c("blue", "red"))),
    feature_data = data.frame(
      Color = c("blue", "red"),
      row.names = c("blue", "red")
    ),
    sample_data = ft$sample_data
  )

  expect_equal(ft$collapse("features", "Color"), expected)
  expect_equal(collapse(ft, "features", "Color"), expected)
  expect_equal(ft$collapse_features("Color"), expected)
  expect_equal(collapse_features(ft, "Color"), expected)

})


test_that("collapse_samples works if the user changes an entire column of metadata or drops a level by hand", {
  ft <- ft_for_collapse_testing()
  ft$sample_data[, "Location"] <- rep("Spain", 4)

  expected <- FeatureTable$new(
    matrix(c(1, 3, 6, 10, 100),
           nrow = 1, ncol = 5, byrow = TRUE,
           dimnames = list(Samples = "Spain",
                           Features = ft$feature_names())),
    sample_data = data.frame(
      Location = "Spain",
      row.names = "Spain"
    ),
    feature_data = ft$feature_data
  )


  expect_equal(ft$collapse("samples", "Location"), expected)
  expect_equal(collapse(ft, "samples", "Location"), expected)
  expect_equal(ft$collapse_samples("Location"), expected)
  expect_equal(collapse_samples(ft, "Location"), expected)

  #### Or if a user drops a level by hand

  ft <- ft_for_collapse_testing()

  # This will cause the 'Portugal' level to not be present in the column.
  ft$sample_data[3, "Location"] <- "Spain"

  expect_equal(ft$collapse("samples", "Location"), expected)
  expect_equal(collapse(ft, "samples", "Location"), expected)
  expect_equal(ft$collapse_samples("Location"), expected)
  expect_equal(collapse_samples(ft, "Location"), expected)
})

