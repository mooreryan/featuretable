# TODO pull out this setup code into helper.

nsamples <- 4
nfeatures <- 5

count_table <- matrix(0:19, nsamples, nfeatures)

rownames(count_table) <- paste0("Sample_", 1:nsamples)
colnames(count_table) <- paste0("Feature_", 1:nfeatures)

feature_data <- data.frame(
  Color = c("red", "red", "blue"),
  Shape = c("square", "circle", "square"),
  row.names = paste0("Feature_", c(1, 3, 5))
)

sample_data <- data.frame(
  Location = c("Spain", "Portugal", "Spain"),
  Season = c("Summer", "Winter", "Winter"),
  row.names = paste0("Sample_", c(1, 2, 4))
)

feature_table <- FeatureTable$new(count_table, feature_data, sample_data)

expected_sample_data <- data.frame(
  Location = c("Spain", "Portugal", NA, "Spain"),
  Season = c("Summer", "Winter", NA, "Winter"),
  row.names = paste0("Sample_", 1:4)
)

expected_feature_data <- data.frame(
  Color = c("red", NA, "red", NA, "blue"),
  Shape = c("square", NA, "circle", NA, "square"),
  row.names = paste0("Feature_", 1:5)
)

test_that("as.data.frame() gives you back a data frame of the feature table", {
  ft <- FeatureTable$new(count_table, feature_data = feature_data, sample_data = sample_data)

  expect_equal(as.data.frame(ft), count_table)
})

test_that("as.data.frame() returns samples X features regardless of input orientation", {
  ft <- FeatureTable$new(t(count_table),
                         feature_data = feature_data,
                         sample_data = sample_data,
                         feature_table_rows_are_samples = FALSE)

  expect_equal(as.data.frame(ft), count_table)
})

