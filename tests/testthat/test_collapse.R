ft_for_collapse_testing <- function() {
  count_table <- matrix(
    c(
      0, 0, 0, 1, 10,
      0, 0, 1, 2, 20,
      0, 1, 2, 3, 30,
      1, 2, 3, 4, 40
    ),
    byrow = TRUE,
    nrow = 4,
    ncol = 5,
    dimnames = list(Samples = paste0("Sample_", 1:4),
                    Features = paste0("Feature_", 1:5))
  )

  feature_data <- data.frame(
    # This has one category with a single thing!
    Color = c("red", "red", "green", "blue", "blue"),
    # This has all the same thing!
    Shape = rep("square", times = 5),
    Length = c(5, 6, 2.3, 7, 10),
    row.names = paste0("Feature_", 1:5)
  )

  sample_data <- data.frame(
    Location = c("Spain", "Spain", "Portugal", "Spain"),
    Season = c("Summer", "Summer", "Winter", "Winter"),
    Silliness = c("Silly", "Silly", "Silly", "Silly"),
    SnazzyFactor = c(10, 12, 25, 3),
    row.names = paste0("Sample_", 1:4)
  )

  FeatureTable$new(count_table,
                   feature_data = feature_data,
                   sample_data = sample_data)
}

# TODO method={sum,mean}
# TODO is_hierarchical={TRUE, FALSE}
# TODO multiple "by" categories

#### Checking if 'by' is valid

test_that("collapse features raises if 'by' isn't valid", {
  ft <- ft_for_collapse_testing()

  expect_error(ft$collapse("features", by = "COLORS"),
               class = Error$ArgumentError)
  expect_error(collapse(ft, "features", by = "COLORS"),
               class = Error$ArgumentError)

  expect_error(ft$collapse_features(by = "COLORS"),
               class = Error$ArgumentError)
  expect_error(collapse_features(ft, by = "COLORS"),
               class = Error$ArgumentError)

  expect_error(ft$collapse("features", by = NA),
               class = Error$ArgumentError)
  expect_error(collapse(ft, "features", by = NA),
               class = Error$ArgumentError)

  expect_error(ft$collapse_features(by = NA),
               class = Error$ArgumentError)
  expect_error(collapse_features(ft, by = NA),
               class = Error$ArgumentError)

  expect_error(ft$collapse("features", by = NULL),
               class = Error$ArgumentError)
  expect_error(collapse(ft, "features", by = NULL),
               class = Error$ArgumentError)

  expect_error(ft$collapse_features(by = NULL),
               class = Error$ArgumentError)
  expect_error(collapse_features(ft, by = NULL),
               class = Error$ArgumentError)
})

test_that("collapse samples raises if 'by' isn't valid", {
  ft <- ft_for_collapse_testing()

  expect_error(ft$collapse("samples", by = "COLORS"),
               class = Error$ArgumentError)
  expect_error(collapse(ft, "samples", by = "COLORS"),
               class = Error$ArgumentError)

  expect_error(ft$collapse_samples(by = "COLORS"),
               class = Error$ArgumentError)
  expect_error(collapse_samples(ft, by = "COLORS"),
               class = Error$ArgumentError)

  expect_error(ft$collapse("samples", by = NA),
               class = Error$ArgumentError)
  expect_error(collapse(ft, "samples", by = NA),
               class = Error$ArgumentError)

  expect_error(ft$collapse_samples(by = NA),
               class = Error$ArgumentError)
  expect_error(collapse_samples(ft, by = NA),
               class = Error$ArgumentError)

  expect_error(ft$collapse("samples", by = NULL),
               class = Error$ArgumentError)
  expect_error(collapse(ft, "samples", by = NULL),
               class = Error$ArgumentError)

  expect_error(ft$collapse_samples(by = NULL),
               class = Error$ArgumentError)
  expect_error(collapse_samples(ft, by = NULL),
               class = Error$ArgumentError)
})


test_that("collapse features works even if one category has only a single feature", {
  ft <- ft_for_collapse_testing()

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # Before collapse: red, red, green, blue, blue
  # After collapse: blue, green, red

  expected <- FeatureTable$new(
    matrix(
      c(
        11, 0, 0,
        22, 1, 0,
        33, 2, 1,
        44, 3, 3
      ),
      byrow = TRUE,
      nrow = 4,
      ncol = 3,
      dimnames = list(Samples = paste0("Sample_", 1:4),
                      Features = c("Color_blue", "Color_green", "Color_red"))
    ),
    feature_data = data.frame(
      Color = c("blue", "green", "red"),
      row.names = c("Color_blue", "Color_green", "Color_red")
    ),
    sample_data = ft$sample_data
  )

  expect_equal(ft$collapse("features", "Color"), expected)
  expect_equal(collapse(ft, "features", "Color"), expected)
  expect_equal(ft$collapse_features("Color"), expected)
  expect_equal(collapse_features(ft, "Color"), expected)
})

test_that("collapse samples works even if one category has only a single feature", {
  ft <- ft_for_collapse_testing()

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),


  # sample_data <- data.frame(
  #   Location = c("Spain", "Spain", "Portugal", "Spain"),
  #   Season = c("Summer", "Summer", "Winter", "Winter"),
  #   SnazzyFactor = c(10, 12, 25, 3),
  #   row.names = paste0("Sample_", 1:4)
  # )


  expected <- FeatureTable$new(
    matrix(
      c(
        0, 1, 2, 3, 30,
        1, 2, 4, 7, 70
      ),
      byrow = TRUE,
      nrow = 2,
      ncol = 5,
      dimnames = list(Samples = c("Location_Portugal", "Location_Spain"),
                      Features = ft$feature_names())
    ),
    feature_data = ft$feature_data,
    sample_data = data.frame(
      Location = c("Portugal", "Spain"),
      row.names = c("Location_Portugal", "Location_Spain")
    )
  )

  expect_equal(ft$collapse("samples", "Location"), expected)
  expect_equal(collapse(ft, "samples", "Location"), expected)
  expect_equal(ft$collapse_samples("Location"), expected)
  expect_equal(collapse_samples(ft, "Location"), expected)
})


test_that("collapse features works even if a feature data col is the same for all features", {
  ft <- ft_for_collapse_testing()

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # Before collapse: All square
  # After collapse: Shape_square (one column.)

  expected <- FeatureTable$new(
    matrix(
      c(
        11,
        23,
        36,
        50
      ),
      byrow = TRUE,
      nrow = 4,
      ncol = 1,
      dimnames = list(Samples = paste0("Sample_", 1:4),
                      Features = c("Shape_square"))
    ),
    feature_data = data.frame(
      Shape = c("square"),
      row.names = c("Shape_square")
    ),
    sample_data = ft$sample_data
  )

  expect_equal(ft$collapse("features", "Shape"), expected)
  expect_equal(collapse(ft, "features", "Shape"), expected)
  expect_equal(ft$collapse_features("Shape"), expected)
  expect_equal(collapse_features(ft, "Shape"), expected)
})

test_that("raiseseif the user changes things so that a column in metadata isn't a factor", {
  ft <- ft_for_collapse_testing()
  ft$feature_data[, "Color"] <- rep("Green", 5)

  expect_error(ft$collapse("features", "Color"), class = Error$NonFactorDataError)
  expect_error(collapse(ft, "features", "Color"), class = Error$NonFactorDataError)
  expect_error(ft$collapse_features("Color"), class = Error$NonFactorDataError)
  expect_error(collapse_features(ft, "Color"), class = Error$NonFactorDataError)

  ft <- ft_for_collapse_testing()
  ft$sample_data[, "Location"] <- rep("Spain", 4)

  expect_error(ft$collapse("samples", "Location"), class = Error$NonFactorDataError)
  expect_error(collapse(ft, "samples", "Location"), class = Error$NonFactorDataError)
  expect_error(ft$collapse_samples("Location"), class = Error$NonFactorDataError)
  expect_error(collapse_samples(ft, "Location"), class = Error$NonFactorDataError)
})

test_that("it raises if user drops a factor level out by accident", {
  ft <- ft_for_collapse_testing()

  # This will cause the 'Portugal' level to not be present in the column.
  ft$feature_data[3, "Color"] <- "blue"

  expect_error(ft$collapse("features", "Color"), class = Error$MissingFactorLevelError)
  expect_error(collapse(ft, "features", "Color"), class = Error$MissingFactorLevelError)
  expect_error(ft$collapse_features("Color"), class = Error$MissingFactorLevelError)
  expect_error(collapse_features(ft, "Color"), class = Error$MissingFactorLevelError)

  ####

  ft <- ft_for_collapse_testing()

  # This will cause the 'Portugal' level to not be present in the column.
  ft$sample_data[3, "Location"] <- "Spain"

  expect_error(ft$collapse("samples", "Location"), class = Error$MissingFactorLevelError)
  expect_error(collapse(ft, "samples", "Location"), class = Error$MissingFactorLevelError)
  expect_error(ft$collapse_samples("Location"), class = Error$MissingFactorLevelError)
  expect_error(collapse_samples(ft, "Location"), class = Error$MissingFactorLevelError)
})

test_that("collapse samples works even if a feature data col is the same for all features", {
  ft <- ft_for_collapse_testing()
  # Set them to all be the same

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # Before collapse: All Spain
  # After collapse: Location_Spain (one row)

  expected <- FeatureTable$new(
    matrix(
      c(
        1, 3, 6, 10, 100
      ),
      byrow = TRUE,
      nrow = 1,
      ncol = 5,
      dimnames = list(Samples = "Silliness_Silly",
                      Features = ft$feature_names())
    ),
    feature_data = ft$feature_data,
    sample_data = data.frame(
      Silliness = "Silly",
      row.names = "Silliness_Silly"
    )
  )

  expect_equal(ft$collapse("samples", "Silliness"), expected)
  expect_equal(collapse(ft, "samples", "Silliness"), expected)
  expect_equal(ft$collapse_samples("Silliness"), expected)
  expect_equal(collapse_samples(ft, "Silliness"), expected)
})

test_that("collapse with margin=2 is the same as 'features'", {
  ft <- ft_for_collapse_testing()

  expect_equal(ft$collapse(2, "Shape"), ft$collapse("features", "Shape"))
  expect_equal(collapse(ft, 2, "Shape"), collapse(ft, "features", "Shape"))
  expect_equal(ft$collapse(2, "Shape"), ft$collapse_features("Shape"))
  expect_equal(collapse(ft, 2, "Shape"), collapse_features(ft, "Shape"))

  expect_equal(ft$collapse(2, "Color"), ft$collapse("features", "Color"))
  expect_equal(collapse(ft, 2, "Color"), collapse(ft, "features", "Color"))
  expect_equal(ft$collapse(2, "Color"), ft$collapse_features("Color"))
  expect_equal(collapse(ft, 2, "Color"), collapse_features(ft, "Color"))
})

test_that("collapse with margin=1 is the same as 'samples'", {
  ft <- ft_for_collapse_testing()

  expect_equal(ft$collapse(1, "Location"), ft$collapse("samples", "Location"))
  expect_equal(collapse(ft, 1, "Location"), collapse(ft, "samples", "Location"))
  expect_equal(ft$collapse(1, "Location"), ft$collapse_samples("Location"))
  expect_equal(collapse(ft, 1, "Location"), collapse_samples(ft, "Location"))

  expect_equal(ft$collapse(1, "Season"), ft$collapse("samples", "Season"))
  expect_equal(collapse(ft, 1, "Season"), collapse(ft, "samples", "Season"))
  expect_equal(ft$collapse(1, "Season"), ft$collapse_samples("Season"))
  expect_equal(collapse(ft, 1, "Season"), collapse_samples(ft, "Season"))
})

test_that("collapse features can use tidy eval", {
  ft <- ft_for_collapse_testing()

  expect_equal(ft$collapse("features", Shape),
               ft$collapse("features", "Shape"))
  expect_equal(collapse(ft, "features", Shape),
               collapse(ft, "features", "Shape"))

  expect_equal(ft$collapse("features", Color),
               ft$collapse("features", "Color"))
  expect_equal(collapse(ft, "features", Color),
               collapse(ft, "features", "Color"))


  expect_equal(ft$collapse_features(Shape),
               ft$collapse_features("Shape"))
  expect_equal(collapse_features(ft, Shape),
               collapse_features(ft, "Shape"))

  expect_equal(ft$collapse_features(Color),
               ft$collapse_features("Color"))
  expect_equal(collapse_features(ft, Color),
               collapse_features(ft, "Color"))
})

test_that("collapse samples can use tidy eval", {
  ft <- ft_for_collapse_testing()

  expect_equal(ft$collapse("samples", Location),
               ft$collapse("samples", "Location"))
  expect_equal(collapse(ft, "samples", Location),
               collapse(ft, "samples", "Location"))

  expect_equal(ft$collapse("samples", Season),
               ft$collapse("samples", "Season"))
  expect_equal(collapse(ft, "samples", Season),
               collapse(ft, "samples", "Season"))


  expect_equal(ft$collapse_samples(Location),
               ft$collapse_samples("Location"))
  expect_equal(collapse_samples(ft, Location),
               collapse_samples(ft, "Location"))

  expect_equal(ft$collapse_samples(Season),
               ft$collapse_samples("Season"))
  expect_equal(collapse_samples(ft, Season),
               collapse_samples(ft, "Season"))
})


test_that("s3 collapse raises when passed weird args", {
  ft <- ft_for_collapse_testing()

  expect_error(collapse(ft, apple = 3, pie = "yum"))
  expect_error(collapse_features(ft, apple = 3, pie = "yum"))
  expect_error(collapse_samples(ft, apple = 3, pie = "yum"))
})

test_that("multiple 'by' categories can be used for sample data", {
  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # sample_data <- data.frame(
  #   Location = c("Spain", "Spain", "Portugal", "Spain"),
  #   Season = c("Summer", "Summer", "Winter", "Winter"),
  #   Silliness = c("Silly", "Silly", "Silly", "Silly"),
  #   SnazzyFactor = c(10, 12, 25, 3),
  #   row.names = paste0("Sample_", 1:4)
  # )

  # Location then Season
  # TODO test that Season then Location gives the same thing.

  # expected <- FeatureTable$new(
  #   matrix(
  #     c(
  #       0, 1, 2, 3, 30,
  #       1, 2, 4, 7, 70
  #     ),
  #     byrow = TRUE,
  #     nrow = 2,
  #     ncol = 5,
  #     dimnames = list(Samples = c("Location_Portugal", "Location_Spain"),
  #                     Features = ft$feature_names())
  #   ),
  #   feature_data = ft$feature_data,
  #   sample_data = data.frame(
  #     Location = c("Portugal", "Spain"),
  #     row.names = c("Location_Portugal", "Location_Spain")
  #   )
  # )
  #
  # expect_equal(ft$collapse("samples", "Location"), expected)
  # expect_equal(collapse(ft, "samples", "Location"), expected)
  # expect_equal(ft$collapse_samples("Location"), expected)
  # expect_equal(collapse_samples(ft, "Location"), expected)
})

################################################################################
#### handling NAs in feature data ##############################################
################################################################################

test_that("collapse features drops features with NA in the category by default", {
  ft <- ft_for_collapse_testing()

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # Before collapse: red, NA, green, NA, blue
  # After collapse: blue, green, red

  ft$feature_data[c(2, 4), "Color"] <- NA

  expected <- FeatureTable$new(
    matrix(
      c(
        10, 0, 0,
        20, 1, 0,
        30, 2, 0,
        40, 3, 1
      ),
      byrow = TRUE,
      nrow = 4,
      ncol = 3,
      dimnames = list(Samples = ft$sample_names(),
                      Features = paste("Color", c("blue", "green", "red"), sep = "_"))
    ),
    feature_data = data.frame(
      Color = c("blue", "green", "red"),
      row.names = paste("Color", c("blue", "green", "red"), sep = "_")
    ),
    sample_data = ft$sample_data
  )

  expect_equal(ft$collapse("features", Color), expected)
  expect_equal(collapse(ft, "features", Color), expected)
  expect_equal(ft$collapse("features", Color, keep_na = FALSE), expected)
  expect_equal(collapse(ft, "features", Color, keep_na = FALSE), expected)

  expect_equal(ft$collapse_features(Color), expected)
  expect_equal(collapse_features(ft, Color), expected)
  expect_equal(ft$collapse_features(Color, keep_na = FALSE), expected)
  expect_equal(collapse_features(ft, Color, keep_na = FALSE), expected)
})

test_that("collapse features combines all NAs into single category if keep_na=TRUE", {
  ft <- ft_for_collapse_testing()

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # Before collapse: red, NA, green, NA, blue
  # After collapse: blue, green, red, NA

  ft$feature_data[c(2, 4), "Color"] <- NA

  # All NAs go at the end.
  expected <- FeatureTable$new(
    matrix(
      c(
        10, 0, 0, 1,
        20, 1, 0, 2,
        30, 2, 0, 4,
        40, 3, 1, 6
      ),
      byrow = TRUE,
      nrow = 4,
      ncol = 4,
      dimnames = list(Samples = ft$sample_names(),
                      Features = paste("Color", c("blue", "green", "red", "NA"), sep = "_"))
    ),
    feature_data = data.frame(
      Color = c("blue", "green", "red", "NA"),
      row.names = paste("Color", c("blue", "green", "red", "NA"), sep = "_")
    ),
    sample_data = ft$sample_data
  )

  expect_equal(ft$collapse("features", Color, keep_na = TRUE), expected)
  expect_equal(collapse(ft, "features", Color, keep_na = TRUE), expected)

  expect_equal(ft$collapse_features(Color, keep_na = TRUE), expected)
  expect_equal(collapse_features(ft, Color, keep_na = TRUE), expected)

  #### Also works fine with a single feature with NA
  ft <- ft_for_collapse_testing()
  ft$feature_data[2, "Color"] <- NA

  # All NAs go at the end.
  expected <- FeatureTable$new(
    matrix(
      c(
        11, 0, 0, 0,
        22, 1, 0, 0,
        33, 2, 0, 1,
        44, 3, 1, 2
      ),
      byrow = TRUE,
      nrow = 4,
      ncol = 4,
      dimnames = list(Samples = ft$sample_names(),
                      Features = paste("Color", c("blue", "green", "red", "NA"), sep = "_"))
    ),
    feature_data = data.frame(
      Color = c("blue", "green", "red", "NA"),
      row.names = paste("Color", c("blue", "green", "red", "NA"), sep = "_")
    ),
    sample_data = ft$sample_data
  )

  expect_equal(ft$collapse("features", Color, keep_na = TRUE), expected)
  expect_equal(collapse(ft, "features", Color, keep_na = TRUE), expected)

  expect_equal(ft$collapse_features(Color, keep_na = TRUE), expected)
  expect_equal(collapse_features(ft, Color, keep_na = TRUE), expected)
})

################################################################################
#### handling NAs in sample data ###############################################
################################################################################

test_that("collapse samples drops samples with NA in the category by default", {
  ft <- ft_for_collapse_testing()

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # sample_data <- data.frame(
  #   Location = c("Spain", "Spain", "Portugal", "Spain"),
  #   Season = c("Summer", "Summer", "Winter", "Winter"),
  #   Silliness = c("Silly", "Silly", "Silly", "Silly"),
  #   SnazzyFactor = c(10, 12, 25, 3),
  #   row.names = paste0("Sample_", 1:4)
  # )


  ft$sample_data[c(2, 4), "Location"] <- NA

  expected <- FeatureTable$new(
    matrix(
      c(
        0, 1, 2, 3, 30,
        0, 0, 0, 1, 10
      ),
      byrow = TRUE,
      nrow = 2,
      ncol = 5,
      dimnames = list(Samples = c("Location_Portugal", "Location_Spain"),
                      Features = ft$feature_names())
    ),
    feature_data = ft$feature_data, data.frame(
      Color = c("blue", "green", "red"),
      row.names = paste("Color", c("blue", "green", "red"), sep = "_")
    ),
    sample_data = data.frame(
      Location = c("Portugal", "Spain"),
      row.names = c("Location_Portugal", "Location_Spain")
    )
  )

  expect_equal(ft$collapse("samples", Location), expected)
  expect_equal(collapse(ft, "samples", Location), expected)
  expect_equal(ft$collapse("samples", Location, keep_na = FALSE), expected)
  expect_equal(collapse(ft, "samples", Location, keep_na = FALSE), expected)

  expect_equal(ft$collapse_samples(Location), expected)
  expect_equal(collapse_samples(ft, Location), expected)
  expect_equal(ft$collapse_samples(Location, keep_na = FALSE), expected)
  expect_equal(collapse_samples(ft, Location, keep_na = FALSE), expected)
})

test_that("collapse samples combines all NAs into single category if keep_na=TRUE", {
  ft <- ft_for_collapse_testing()

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # sample_data <- data.frame(
  #   Location = c("Spain", "Spain", "Portugal", "Spain"),
  #   Season = c("Summer", "Summer", "Winter", "Winter"),
  #   Silliness = c("Silly", "Silly", "Silly", "Silly"),
  #   SnazzyFactor = c(10, 12, 25, 3),
  #   row.names = paste0("Sample_", 1:4)
  # )

  ft$sample_data[c(2, 4), "Location"] <- NA

  # All NAs go at the end.
  expected <- FeatureTable$new(
    matrix(
      c(
        0, 1, 2, 3, 30,
        0, 0, 0, 1, 10,
        1, 2, 4, 6, 60
      ),
      byrow = TRUE,
      nrow = 3,
      ncol = 5,
      dimnames = list(Samples = paste("Location", c("Portugal", "Spain", "NA"), sep = "_"),
                      Features = ft$feature_names())
    ),
    feature_data = ft$feature_data,
    sample_data = data.frame(
      Location = c("Portugal", "Spain", "NA"),
      row.names = c("Location_Portugal", "Location_Spain", "Location_NA")
    )
  )

  expect_equal(ft$collapse("samples", Location, keep_na = TRUE), expected)
  expect_equal(collapse(ft, "samples", Location, keep_na = TRUE), expected)

  expect_equal(ft$collapse_samples(Location, keep_na = TRUE), expected)
  expect_equal(collapse_samples(ft, Location, keep_na = TRUE), expected)

  #### Also works fine with a single feature with NA
  ft <- ft_for_collapse_testing()
  ft$sample_data[2, "Location"] <- NA

  # All NAs go at the end.
  expected <- FeatureTable$new(
    matrix(
      c(
        0, 1, 2, 3, 30,
        1, 2, 3, 5, 50,
        0, 0, 1, 2, 20
      ),
      byrow = TRUE,
      nrow = 3,
      ncol = 5,
      dimnames = list(Samples = paste("Location", c("Portugal", "Spain", "NA"), sep = "_"),
                      Features = ft$feature_names())
    ),
    feature_data = ft$feature_data,
    sample_data = data.frame(
      Location = c("Portugal", "Spain", "NA"),
      row.names = c("Location_Portugal", "Location_Spain", "Location_NA")
    )
  )


  expect_equal(ft$collapse("samples", Location, keep_na = TRUE), expected)
  expect_equal(collapse(ft, "samples", Location, keep_na = TRUE), expected)

  expect_equal(ft$collapse_samples(Location, keep_na = TRUE), expected)
  expect_equal(collapse_samples(ft, Location, keep_na = TRUE), expected)
})

test_that("collapse raises if the thing doesn't exist", {
  ft <- otu_feature_table()

  # It just gives a simple error.  Would be nicer to have a FeatureTable error class though.
  expect_error(ft$collapse("features", Beep), class = Error$ArgumentError)
  expect_error(ft$collapse_features(Beep), class = Error$ArgumentError)
  expect_error(collapse(ft, "features", Beep), class = Error$ArgumentError)
  expect_error(collapse_features(ft, Beep), class = Error$ArgumentError)

  expect_error(ft$collapse("samples", Beep), class = Error$ArgumentError)
  expect_error(ft$collapse_samples(Beep), class = Error$ArgumentError)
  expect_error(collapse(ft, "samples", Beep), class = Error$ArgumentError)
  expect_error(collapse_samples(ft, Beep), class = Error$ArgumentError)
})

test_that("collapse raises with bad margin", {
  ft <- otu_feature_table()

  expect_error(ft$collapse("beep", Location), class = Error$ArgumentError)
  expect_error(collapse(ft, "beep", Location), class = Error$ArgumentError)
  expect_error(ft$collapse(0, Location), class = Error$ArgumentError)
  expect_error(collapse(ft, 0, Location), class = Error$ArgumentError)
  expect_error(ft$collapse(3, Location), class = Error$ArgumentError)
  expect_error(collapse(ft, 3, Location), class = Error$ArgumentError)
})