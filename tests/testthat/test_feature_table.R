feature_table <- FeatureTable$new(
  testdata$count_table,
  testdata$feature_data,
  testdata$sample_data
)

expect_attribute <- function(thing, attr) {
  expect_true(exists(attr, thing))
}

################################################################################
#### Testing attributes ########################################################
################################################################################

test_that("$new makes a new Featuretable", {
  expect_is(feature_table, "FeatureTable")
})

test_that("FeatureTable has the right attributes", {
  expect_attribute(feature_table, "data")
  expect_attribute(feature_table, "feature_data")
  expect_attribute(feature_table, "sample_data")
})

test_that("NAs in the data are no problem", {
  d <- testdata$count_table
  d[1, 1] <- NA

  expect_error(FeatureTable$new(d, testdata$feature_data, testdata$sample_data), NA)
})

test_that("$new sets all the attributes correctly", {
  ft <- FeatureTable$new(testdata$count_table, testdata$feature_data, testdata$sample_data, feature_table_rows_are_samples = TRUE)

  expect_equal(ft$num_samples(), testdata$nsamples)
  expect_equal(ft$num_features(), testdata$nfeatures)

  expect_equal(ft$sample_names(), rownames(testdata$count_table))
  expect_equal(ft$feature_names(), colnames(testdata$count_table))

  expect_equal(ft$dim(), c(testdata$nsamples, testdata$nfeatures))
  expect_equal(ft$nrow(), testdata$nsamples)
  expect_equal(ft$ncol(), testdata$nfeatures)

  expect_equal(ft$data, testdata$count_table)
  expect_equal(ft$feature_data, testdata$expected_feature_data)
  expect_equal(ft$sample_data, testdata$expected_sample_data)
})

test_that("$new transposes the feature_table if feature_table_rows_are_samples = FALSE", {
  ft <- FeatureTable$new(t(testdata$count_table),
                         testdata$feature_data,
                         testdata$sample_data,
                         feature_table_rows_are_samples = FALSE)

  expect_equal(dim(ft$data), dim(testdata$count_table))

  expect_equal(ft$dim(), dim(testdata$count_table))
  expect_equal(c(ft$nrow(), ft$ncol()), dim(testdata$count_table))
  expect_equal(c(ft$num_samples(), ft$num_features()), dim(testdata$count_table))

  expect_equal(ft$sample_names(), rownames(testdata$count_table))
  expect_equal(ft$feature_names(), colnames(testdata$count_table))

  expect_equal(ft$data, testdata$count_table)
  expect_equal(ft$feature_data, testdata$expected_feature_data)
  expect_equal(ft$sample_data, testdata$expected_sample_data)
})

test_that("$new raises error when feature_table argument isn'd a 2d thing", {
  expect_error(FeatureTable$new(1:10), class = Error$ArgumentError)
  expect_error(FeatureTable$new(array(1:27, dim = c(3, 3, 3))), class = Error$ArgumentError)
})

test_that("$new raises error when feature table is empty", {
  expect_error(FeatureTable$new(data.frame()), class = Error$ArgumentError)
})

################################################################################
#### *_data with > 2 dimensions ###############################################
################################################################################

test_that("$new raises error when feature_table has > 2 dimensions", {
  silly_data <- array(1:27, dim = c(3, 3, 3))

  expect_error(FeatureTable$new(silly_data), class = Error$ArgumentError)
  expect_error(FeatureTable$new(1:10), class = Error$ArgumentError)

})

################################################################################
################################################################################
#### Weird sample data #########################################################
################################################################################
################################################################################

################################################################################
#### Empty sample data #########################################################
################################################################################

test_that("$new raises error if sample_data is empty", {
  expect_error(FeatureTable$new(testdata$count_table, sample_data = data.frame()),
               class = Error$ArgumentError)
})

################################################################################
#### Sample data with > 2 dimensions ###########################################
################################################################################

test_that("$new raises error when sample_data has > 2 dimensions", {
  silly_data <- array(1:27, dim = c(3, 3, 3))

  expect_error(FeatureTable$new(testdata$count_table, sample_data = silly_data), class = Error$ArgumentError)
})

################################################################################
#### sample_data with a single sample ##########################################
################################################################################

# Just make sure that no weird dimension things happen.
test_that("$new gives good sample data even if there is only one sample", {
  ## Single covariate
  sample_data <- data.frame(
    Location = c("Portugal"),
    row.names = c("Sample_2"),
    stringsAsFactors = TRUE
  )
  expected_sample_data <- data.frame(
    Location = c(NA, "Portugal", NA, NA),
    row.names = paste0("Sample_", 1:4),
    stringsAsFactors = TRUE
  )
  ft <- FeatureTable$new(testdata$count_table, sample_data = sample_data)
  expect_equal(ft$sample_data, expected_sample_data)

  ## Multiple covariates
  sample_data <- data.frame(
    Location = c("Portugal"),
    Season = c("Winter"),
    row.names = c("Sample_2"),
    stringsAsFactors = TRUE
  )
  expected_sample_data <- data.frame(
    Location = c(NA, "Portugal", NA, NA),
    Season = c(NA, "Winter", NA, NA),
    row.names = paste0("Sample_", 1:4),
    stringsAsFactors = TRUE
  )
  ft <- FeatureTable$new(testdata$count_table, sample_data = sample_data)
  expect_equal(ft$sample_data, expected_sample_data)
})


################################################################################
#### sample_data with a single covariate #######################################
################################################################################

#### Good stuff

test_that("$new gives good sample data even with single covariate as data frame", {
  sample_data <- data.frame(
    Location = c("Spain", "Portugal", "Spain"),
    row.names = paste0("Sample_", c(1, 2, 4)),
    stringsAsFactors = TRUE
  )

  expected_sample_data <- data.frame(
    Location = c("Spain", "Portugal", NA, "Spain"),
    row.names = paste0("Sample_", 1:4),
    stringsAsFactors = TRUE
  )

  ft <- FeatureTable$new(testdata$count_table, sample_data = sample_data)

  expect_equal(ft$sample_data, expected_sample_data)
})

test_that("$new gives good sample data even with single covariate as a vector with names", {
  sample_data <- c("Spain", "Portugal", "Spain")
  names(sample_data) <- paste0("Sample_", c(1, 2, 4))

  # The colnames is just X since it has to pick something.
  expected_sample_data <- data.frame(
    X = c("Spain", "Portugal", NA, "Spain"),
    row.names = paste0("Sample_", 1:4),
    stringsAsFactors = TRUE
  )

  ft <- FeatureTable$new(testdata$count_table, sample_data = sample_data)

  expect_equal(ft$sample_data, expected_sample_data)
})

#### Expected errors

test_that("$new raises error with 1d sample data with no names", {
  sample_data <- c("Spain", "Portugal", "Spain")

  expect_error(FeatureTable$new(testdata$count_table, sample_data = sample_data),
               class = Error$ArgumentError)
})

test_that("$new raises error with 1d sample data that have no matches in feature_table", {
  sample_data <- c("Spain", "Portugal", "Spain")
  names(sample_data) <- letters[1:3]

  expect_error(FeatureTable$new(testdata$count_table, sample_data = sample_data),
               class = Error$ArgumentError)
})

test_that("$new gives a error if none of the samples have data in sample_data", {
  sample_data <- data.frame(Silly = c(TRUE, FALSE, TRUE), 
                            row.names = c("apple", "gie", "good"),
                            stringsAsFactors = TRUE)

  expect_error(FeatureTable$new(testdata$count_table, sample_data = sample_data),
               class = Error$ArgumentError)
})


################################################################################
################################################################################
#### Weird feature data ########################################################
################################################################################
################################################################################

################################################################################
#### Empty feature data ########################################################
################################################################################

test_that("$new raises error if feature_data is empty", {
  expect_error(FeatureTable$new(testdata$count_table, feature_data = data.frame()),
               class = Error$ArgumentError)
})

################################################################################
#### Feature data with > 2 dimensions ##########################################
################################################################################

test_that("$new raises error when feature_data has > 2 dimensions", {
  silly_data <- array(1:27, dim = c(3, 3, 3))

  expect_error(FeatureTable$new(testdata$count_table, feature_data = silly_data), class = Error$ArgumentError)
})

################################################################################
#### feature_data with a single feature ########################################
################################################################################

# Just make sure that no weird dimension things happen.
test_that("$new gives good feature data even if there is only one feature", {
  ## Single covariate
  feature_data <- data.frame(
    Color = c("red"),
    row.names = c("Feature_3"),
    stringsAsFactors = TRUE
  )
  expected_feature_data <- data.frame(
    Color = c(NA, NA, "red", NA, NA),
    row.names = paste0("Feature_", 1:5),
    stringsAsFactors = TRUE
  )
  ft <- FeatureTable$new(testdata$count_table, feature_data = feature_data)
  expect_equal(ft$feature_data, expected_feature_data)

  ## Multiple covariates
  feature_data <- data.frame(
    Color = c("red"),
    Shape = c("circle"),
    row.names = c("Feature_3"),
    stringsAsFactors = TRUE
  )
  expected_feature_data <- data.frame(
    Color = c(NA, NA, "red", NA, NA),
    Shape = c(NA, NA, "circle", NA, NA),
    row.names = paste0("Feature_", 1:5),
    stringsAsFactors = TRUE
  )
  ft <- FeatureTable$new(testdata$count_table, feature_data = feature_data)
  expect_equal(ft$feature_data, expected_feature_data)
})

################################################################################
#### feature_data with a single covariate ######################################
################################################################################

#### Good stuff

test_that("$new gives good feature data even with single covariate as data frame", {
  feature_data <- data.frame(
    Color = c("red", "red", "blue"),
    row.names = paste0("Feature_", c(1, 3, 5)),
    stringsAsFactors = TRUE
  )

  expected_feature_data <- data.frame(
    Color = c("red", NA, "red", NA, "blue"),
    row.names = paste0("Feature_", 1:5),
    stringsAsFactors = TRUE
  )

  ft <- FeatureTable$new(testdata$count_table, feature_data = feature_data)

  expect_equal(ft$feature_data, expected_feature_data)
})

test_that("$new gives good feature data even with single covariate as a vector with names", {
  feature_data <- c("red", "red", "blue")
  names(feature_data) <-paste0("Feature_", c(1, 3, 5))

  # The colnames is just X since it has to pick something.
  expected_feature_data <- data.frame(
    X = c("red", NA, "red", NA, "blue"),
    row.names = paste0("Feature_", 1:5),
    stringsAsFactors = TRUE
  )

  ft <- FeatureTable$new(testdata$count_table, feature_data = feature_data)

  expect_equal(ft$feature_data, expected_feature_data)
})

#### Expected errors

test_that("$new raises error with 1d feature data with no names", {
  feature_data <- c("red", "red", "blue")

  expect_error(FeatureTable$new(testdata$count_table, feature_data = feature_data),
               class = Error$ArgumentError)
})

test_that("$new raises error with 1d sample data that have no matches in feature_table", {
  feature_data <- c("red", "red", "blue")
  names(feature_data) <- letters[1:3]

  expect_error(FeatureTable$new(testdata$count_table, feature_data = feature_data),
               class = Error$ArgumentError)
})

test_that("$new gives a error if none of the features have data in feature_data", {
  feature_data <- data.frame(Silly = c(TRUE, FALSE, TRUE), 
                             row.names = c("apple", "gie", "good"),
                             stringsAsFactors = TRUE)

  expect_error(FeatureTable$new(testdata$count_table, feature_data = feature_data),
               class = Error$ArgumentError)
})

# TODO input is a matrix...does it work?

# TODO sample_sums
# TODO feature_sums, both these as "active" calculate then cache

# TODO map really is apply but forced to return the same dimension...need a real map function that can take multi param input function

################################################################################
#### querying aspects of FeatureTable ##########################################
################################################################################

test_that("$is_count_table returns TRUE if all numbers in feature_table are natural numbers", {
  ft <- basic_feature_table()

  expect_true(ft$is_count_table())
  expect_true(is_count_table(ft))
})

test_that("$is_count_table returns FALSE if all numbers in feature_table are NOT natural numbers", {
  ft <- basic_feature_table()

  ft$data[1, 2] <- 0.5

  expect_false(ft$is_count_table())
  expect_false(is_count_table(ft))

  # Negatives

  ft <- basic_feature_table()

  ft$data[1, 2] <- -1

  expect_false(ft$is_count_table())
  expect_false(is_count_table(ft))
})

################################################################################
#### little data utils #########################################################
################################################################################

test_that("non_zero_min gives the minimum value excluding zeros", {
  ft <- basic_feature_table()

  expect_equal(ft$non_zero_min(), 1)
  expect_equal(non_zero_min(ft), 1)

  # No message for normal usage.
  expect_message(ft$non_zero_min(), NA)
  expect_message(non_zero_min(ft), NA)

  ft$data[1, 1] <- NA

  expect_true(is.na(ft$non_zero_min()))
  expect_true(is.na(non_zero_min(ft)))

  expect_equal(ft$non_zero_min(na.rm = TRUE), 1)
  expect_equal(non_zero_min(ft, na.rm = TRUE), 1)

  ft$data[1, 1] <- -100

  expect_equal(ft$non_zero_min(), -100)
  expect_equal(non_zero_min(ft), -100)

  # User probably didn't mean to use this if there are negative values.  Warn them.
  expect_warning(ft$non_zero_min(), regexp = "negative")
  expect_warning(non_zero_min(ft), regexp = "negative")
})

test_that("min and max give correct values", {
  ft <- basic_feature_table()

  expect_equal(ft$min(), min(ft$data))
  expect_equal(ft$max(), max(ft$data))

  expect_equal(min(ft), min(ft$data))
  expect_equal(max(ft), max(ft$data))

  ft$data[1, 1] <- NA

  expect_true(is.na(ft$min()))
  expect_true(is.na(ft$max()))

  expect_equal(ft$min(na.rm = TRUE), min(ft$data, na.rm = TRUE))
  expect_equal(ft$max(na.rm = TRUE), max(ft$data, na.rm = TRUE))

  expect_equal(min(ft, na.rm = TRUE), min(ft$data, na.rm = TRUE))
  expect_equal(max(ft, na.rm = TRUE), max(ft$data, na.rm = TRUE))
})

test_that("size returns data nrows * ncols", {
  ft <- basic_feature_table()

  expect_equal(ft$size(), nrow(ft$data) * ncol(ft$data))
  expect_equal(size(ft), nrow(ft$data) * ncol(ft$data))
})
