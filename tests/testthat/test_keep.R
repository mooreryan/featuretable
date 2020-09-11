test_that("keep can filter wrt features with a predicate function or logical vec", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expected <- FeatureTable$new(
    testdata$count_table[, 1:2],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep("features", function(feature) sum(feature) < 35), expected)
  expect_equal(keep(ft, "features", function(feature) sum(feature) < 35), expected)
  expect_equal(ft$keep("features", c(TRUE, TRUE, FALSE, FALSE, FALSE)), expected)
  expect_equal(keep(ft, "features", c(TRUE, TRUE, FALSE, FALSE, FALSE)), expected)

  expected <- FeatureTable$new(
    testdata$count_table[, 3:5],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep(2, function(feature) sum(feature) > 35), expected)
  expect_equal(keep(ft, 2, function(feature) sum(feature) > 35), expected)
  expect_equal(ft$keep("features", c(FALSE, FALSE, TRUE, TRUE, TRUE)), expected)
  expect_equal(keep(ft, "features", c(FALSE, FALSE, TRUE, TRUE, TRUE)), expected)
})

test_that("keep can filter wrt samples with a predicate function", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expected <- FeatureTable$new(
    testdata$count_table[1:2, ],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep("samples", function(feature) sum(feature) < 48), expected)
  expect_equal(keep(ft, "samples", function(feature) sum(feature) < 48), expected)
  expect_equal(ft$keep("samples", c(TRUE, TRUE, FALSE, FALSE)), expected)
  expect_equal(keep(ft, "samples", c(TRUE, TRUE, FALSE, FALSE)), expected)

  expected <- FeatureTable$new(
    testdata$count_table[3:4, ],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep(1, function(feature) sum(feature) > 48), expected)
  expect_equal(keep(ft, 1, function(feature) sum(feature) > 48), expected)
  expect_equal(ft$keep("samples", c(FALSE, FALSE, TRUE, TRUE)), expected)
  expect_equal(keep(ft, "samples", c(FALSE, FALSE, TRUE, TRUE)), expected)
})

test_that("keep handles more interesting predicates", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  # Keep features that have more than one count divisible by 3.
  expected <- FeatureTable$new(
    testdata$count_table[, c("Feature_1", "Feature_4")],
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep("features", function(feature) sum(feature %% 3 == 0) > 1), expected)
  expect_equal(keep(ft, "features", function(feature) sum(feature %% 3 == 0) > 1), expected)

  # Keep samples that have more than one count divisible by 3.
  expected <- FeatureTable$new(
    testdata$count_table[c("Sample_1", "Sample_3", "Sample_4"), ],
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep("samples", function(samples) sum(samples %% 3 == 0) > 1), expected)
  expect_equal(keep(ft, "samples", function(samples) sum(samples %% 3 == 0) > 1), expected)
})

test_that("keep raises error if 0 samples or features would be returned", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  # Features
  expect_error(ft$keep("features", function(feature) sum(feature) > 10000),
               class = Error$NoFeaturesRemainingError)
  expect_error(keep(ft, "features", function(feature) sum(feature) > 10000),
               class = Error$NoFeaturesRemainingError)

  # Samples
  expect_error(ft$keep("samples", function(sample) sum(sample) > 10000),
               class = Error$NoSamplesRemainingError)
  expect_error(keep(ft, "samples", function(sample) sum(sample) > 10000),
               class = Error$NoSamplesRemainingError)
})

test_that("keep works fine if 1 sample or feature would be returned", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  #### Features
  expected <- FeatureTable$new(
    matrix(testdata$count_table[, 5], nrow = 4, ncol = 1,
           dimnames = list(Samples = paste0("Sample_", 1:4),
                           Features = "Feature_5")),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep("features", function(feature) sum(feature) > 65), expected)
  expect_equal(keep(ft, "features", function(feature) sum(feature) > 65), expected)

  #### Samples
  expected <- FeatureTable$new(
    matrix(testdata$count_table[4, ],
           nrow = 1, ncol = 5,
           dimnames = list(Samples = "Sample_4",
                           Features = paste0("Feature_", 1:5))),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep("samples", function(sample) sum(sample) > 53), expected)
  expect_equal(keep(ft, "samples", function(sample) sum(sample) > 53), expected)
})

test_that("keep works fine when all samples are returned", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_equal(ft$keep(1, function(x) sum(x) >= 0), ft)
  expect_equal(keep(ft, 1, function(x) sum(x) >= 0), ft)

  expect_equal(ft$keep(2, function(x) sum(x) >= 0), ft)
  expect_equal(keep(ft, 2, function(x) sum(x) >= 0), ft)
})

test_that("keep raises error if function (or vec) isn't a predicate", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_error(ft$keep(1, function(x) x), class = Error$NonPredicateFunctionError)
  expect_error(ft$keep(2, function(x) x), class = Error$NonPredicateFunctionError)
  expect_error(ft$keep(1, 1:4), class = Error$NonPredicateFunctionError)
  expect_error(ft$keep(2, 1:5), class = Error$NonPredicateFunctionError)

  expect_error(keep(ft, 1, function(x) x), class = Error$NonPredicateFunctionError)
  expect_error(keep(ft, 2, function(x) x), class = Error$NonPredicateFunctionError)
  expect_error(keep(ft, 1, 1:4), class = Error$NonPredicateFunctionError)
  expect_error(keep(ft, 2, 1:5), class = Error$NonPredicateFunctionError)
})

test_that("keep raises error if a vec of incorrect length is predicate", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expect_error(ft$keep(1, rep(TRUE, 10)), class = Error$IncorrectLengthError)
  expect_error(ft$keep(2, rep(TRUE, 10)), class = Error$IncorrectLengthError)

  expect_error(keep(ft, 1, rep(TRUE, 10)), class = Error$IncorrectLengthError)
  expect_error(keep(ft, 2, rep(TRUE, 10)), class = Error$IncorrectLengthError)
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

  expect_equal(
    keep(ft, 1, c(TRUE, TRUE, FALSE, FALSE)),
    keep(ft, 1, c(TRUE, TRUE, NA, NA))
  )
  expect_equal(
    keep(ft, 2, c(TRUE, TRUE, FALSE, FALSE, FALSE)),
    keep(ft, 2, c(TRUE, TRUE, NA, NA, NA))
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

  expect_equal(
    keep(ft, "samples", c(TRUE, TRUE, FALSE, FALSE)),
    keep_samples(ft, c(TRUE, TRUE, FALSE, FALSE))
  )
  expect_equal(
    keep(ft, "features", c(TRUE, TRUE, FALSE, FALSE, FALSE)),
    keep_features(ft, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  )
})

test_that("the sample data (tidy) is available when the margin is samples (1)", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expected <- FeatureTable$new(
    matrix(testdata$count_table[c(1, 4), ],
           nrow = 2, ncol = 5,
           dimnames = list(Samples = c("Sample_1", "Sample_4"),
                           Features = paste0("Feature_", 1:5))),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep("samples", Location == "Spain"), expected)
  expect_equal(ft$keep_samples(Location == "Spain"), expected)
  expect_equal(ft$keep_samples(ft$sample_data$Location == "Spain"),
               ft$keep_samples(Location == "Spain"))

  expect_equal(keep(ft, "samples", Location == "Spain"), expected)
  expect_equal(keep_samples(ft, Location == "Spain"), expected)
  expect_equal(keep_samples(ft, ft$sample_data$Location == "Spain"),
               keep_samples(ft, Location == "Spain"))
})

test_that("the feature data (tidy) is available when the margin is features (2)", {
  ft <- FeatureTable$new(testdata$count_table,
                         feature_data = testdata$feature_data,
                         sample_data = testdata$sample_data)

  expected <- FeatureTable$new(
    matrix(testdata$count_table[, c(1, 3)],
           nrow = 4, ncol = 2,
           dimnames = list(Samples = paste0("Sample_", 1:4),
                           Features = c("Feature_1", "Feature_3"))),
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(ft$keep("features", Color == "red"), expected)
  expect_equal(ft$keep_features(Color == "red"), expected)
  expect_equal(ft$keep_features(ft$feature_data$Color == "red"),
               ft$keep_features(Color == "red"))

  expect_equal(keep(ft, "features", Color == "red"), expected)
  expect_equal(keep_features(ft, Color == "red"), expected)
  expect_equal(keep_features(ft, ft$feature_data$Color == "red"),
               keep_features(ft, Color == "red"))
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

test_that("keep can use the presence/absence wordy helpers", {
  ft <- FeatureTable$new(
    matrix(
      c(
        1, 0, 0,
        1, 0, 1,
        2, 0, 0.5
      ),
      nrow = 3, ncol = 3,
      byrow = TRUE
    )
  )

  expected <- FeatureTable$new(
    matrix(
      c(
        1, 0,
        1, 1,
        2, 0.5
      ),
      nrow = 3, ncol = 2,
      byrow = TRUE
    )
  )

  expect_equal(ft$keep("features", present), expected)
  expect_equal(ft$keep("features", if_present), expected)
  expect_equal(ft$keep("features", that_are_present), expected)
  expect_equal(ft$keep("features", that_were_present), expected)

  expect_equal(ft$keep_features(present), expected)
  expect_equal(ft$keep_features(if_present), expected)
  expect_equal(ft$keep_features(that_are_present), expected)
  expect_equal(ft$keep_features(that_were_present), expected)

  expect_equal(keep(ft, "features", present), expected)
  expect_equal(keep(ft, "features", if_present), expected)
  expect_equal(keep(ft, "features", that_are_present), expected)
  expect_equal(keep(ft, "features", that_were_present), expected)

  expect_equal(keep_features(ft, present), expected)
  expect_equal(keep_features(ft, if_present), expected)
  expect_equal(keep_features(ft, that_are_present), expected)
  expect_equal(keep_features(ft, that_were_present), expected)

  expected <- FeatureTable$new(
    matrix(
      c(
        0,
        0,
        0
      ),
      nrow = 3, ncol = 1,
      byrow = TRUE
    )
  )

  expect_equal(ft$keep("features", absent), expected)
  expect_equal(ft$keep("features", if_absent), expected)
  expect_equal(ft$keep("features", that_are_absent), expected)
  expect_equal(ft$keep("features", that_were_absent), expected)

  expect_equal(ft$keep_features(absent), expected)
  expect_equal(ft$keep_features(if_absent), expected)
  expect_equal(ft$keep_features(that_are_absent), expected)
  expect_equal(ft$keep_features(that_were_absent), expected)

  expect_equal(keep(ft, "features", absent), expected)
  expect_equal(keep(ft, "features", if_absent), expected)
  expect_equal(keep(ft, "features", that_are_absent), expected)
  expect_equal(keep(ft, "features", that_were_absent), expected)

  expect_equal(keep_features(ft, absent), expected)
  expect_equal(keep_features(ft, if_absent), expected)
  expect_equal(keep_features(ft, that_are_absent), expected)
  expect_equal(keep_features(ft, that_were_absent), expected)
})


test_that("you can join expressions with & and |", {
  ft <- otu_feature_table()

  # You have to wrap this in `query`.  See test_keep_restricted.R
  expect_equal(ft$keep_features(Shape == "circle" & Color == "red"),
               ft$keep_features(c(FALSE, TRUE, FALSE, FALSE, FALSE)))

  expect_equal(ft$keep_features(Shape == "circle" | Color == "red"),
               ft$keep_features(c(TRUE, TRUE, TRUE, FALSE, TRUE)))
})

test_that("keep raises if the thing doesn't exist", {
  ft <- otu_feature_table()

  # It just gives a simple error.  Would be nicer to have a FeatureTable error class though.

  expect_error(ft$keep("features", Beep == "boop"))
  expect_error(ft$keep_features(Beep == "boop"))
  expect_error(keep(ft, "features", Beep == "boop"))
  expect_error(keep_features(ft, Beep == "boop"))

  expect_error(ft$keep("samples", Beep == "boop"))
  expect_error(ft$keep_samples(Beep == "boop"))
  expect_error(keep(ft, "samples", Beep == "boop"))
  expect_error(keep_samples(ft, Beep == "boop"))

  # With query

  expect_error(ft$keep("features", query(Beep == "boop")))
  expect_error(ft$keep_features(query(Beep == "boop")))
  expect_error(keep(ft, "features", query(Beep == "boop")))
  expect_error(keep_features(ft, query(Beep == "boop")))

  expect_error(ft$keep("samples", query(Beep == "boop")))
  expect_error(ft$keep_samples(query(Beep == "boop")))
  expect_error(keep(ft, "samples", query(Beep == "boop")))
  expect_error(keep_samples(ft, query(Beep == "boop")))
})

test_that("keep raises with bad margin", {
  ft <- otu_feature_table()

  expect_error(ft$keep("beep", Location == "Spain"), class = Error$ArgumentError)
  expect_error(keep(ft, "beep", Location == "Spain"), class = Error$ArgumentError)
  expect_error(ft$keep(0, Location == "Spain"), class = Error$ArgumentError)
  expect_error(keep(ft, 0, Location == "Spain"), class = Error$ArgumentError)
  expect_error(ft$keep(3, Location == "Spain"), class = Error$ArgumentError)
  expect_error(keep(ft, 3, Location == "Spain"), class = Error$ArgumentError)
})

test_that("extra args are ignored with non-function predicate", {
  ft <- otu_feature_table()

  expect_equal(ft$keep("features", Color == "blue", "apple", "pie"),
               ft$keep("features", Color == "blue"))
  expect_equal(ft$keep_features(Color == "blue", "apple", "pie"),
               ft$keep_features(Color == "blue"))
  expect_equal(keep(ft, "features", Color == "blue", "apple", "pie"),
               keep(ft, "features", Color == "blue"))
  expect_equal(keep_features(ft, Color == "blue", "apple", "pie"),
               keep_features(ft, Color == "blue"))

  expect_equal(ft$keep("features", query(Color == "blue"), "apple", "pie"),
               ft$keep("features", query(Color == "blue")))
  expect_equal(ft$keep_features(query(Color == "blue"), "apple", "pie"),
               ft$keep_features(query(Color == "blue")))
  expect_equal(keep(ft, "features", query(Color == "blue"), "apple", "pie"),
               keep(ft, "features", query(Color == "blue")))
  expect_equal(keep_features(ft, query(Color == "blue"), "apple", "pie"),
               keep_features(ft, query(Color == "blue")))

  expect_equal(ft$keep("samples", Location == "Spain", "apple", "pie"),
               ft$keep("samples", Location == "Spain"))
  expect_equal(ft$keep_samples(Location == "Spain", "apple", "pie"),
               ft$keep_samples(Location == "Spain"))
  expect_equal(keep(ft, "samples", Location == "Spain", "apple", "pie"),
               keep(ft, "samples", Location == "Spain"))
  expect_equal(keep_samples(ft, Location == "Spain", "apple", "pie"),
               keep_samples(ft, Location == "Spain"))

  # `query` not yet implemented for samples

  # expect_equal(ft$keep("samples", query(Location == "Spain"), "apple", "pie"),
  #              ft$keep("samples", query(Location == "Spain")))
  # expect_equal(ft$keep_samples(query(Location == "Spain"), "apple", "pie"),
  #              ft$keep_samples(query(Location == "Spain")))
  # expect_equal(keep(ft, "samples", query(Location == "Spain"), "apple", "pie"),
  #              keep(ft, "samples", query(Location == "Spain")))
  # expect_equal(keep_samples(ft, query(Location == "Spain"), "apple", "pie"),
  #              keep_samples(ft, query(Location == "Spain")))
})