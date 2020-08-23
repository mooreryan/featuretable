# testdata <- list(
#   nsamples = 4,
#   nfeatures = 5,
#   count_table = matrix(0:19, 4, 5,
#                        dimnames = list(Samples = paste0("Sample_", 1:4),
#                                        Features = paste0("Feature_", 1:5))),
#
#   feature_data = data.frame(
#     Color = c("red", "red", "blue"),
#     Shape = c("square", "circle", "square"),
#     row.names = paste0("Feature_", c(1, 3, 5))
#   ),
#
#   sample_data = data.frame(
#     Location = c("Spain", "Portugal", "Spain"),
#     Season = c("Summer", "Winter", "Winter"),
#     row.names = paste0("Sample_", c(1, 2, 4))
#   ),
#
#   expected_sample_data = data.frame(
#     Location = c("Spain", "Portugal", NA, "Spain"),
#     Season = c("Summer", "Winter", NA, "Winter"),
#     row.names = paste0("Sample_", 1:4)
#   ),
#
#   expected_feature_data = data.frame(
#     Color = c("red", NA, "red", NA, "blue"),
#     Shape = c("square", NA, "circle", NA, "square"),
#     row.names = paste0("Feature_", 1:5)
#   )
# )

#### With predicate functions
####
####

test_that("keep can filter wrt features with a predicate function", {
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

  result <- ft$keep(2, function(feature) sum(feature) > 35)
  expected <- FeatureTable$new(
    testdata$count_table[, 3:5],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)
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

  result <- ft$keep(1, function(feature) sum(feature) > 48)
  expected <- FeatureTable$new(
    testdata$count_table[3:4, ],
    # The feature data also gets handled properly, because of the FeatureTable constructor!
    feature_data = testdata$feature_data,
    sample_data = testdata$sample_data
  )
  expect_equal(result, expected)
})

