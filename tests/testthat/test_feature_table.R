################################################################################
#### Test data #################################################################
################################################################################

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
  expect_attribute(feature_table, "num_samples")
  expect_attribute(feature_table, "num_features")
  expect_attribute(feature_table, "sample_names")
  expect_attribute(feature_table, "feature_names")
  expect_attribute(feature_table, "dim")
  expect_attribute(feature_table, "nrow")
  expect_attribute(feature_table, "ncol")
  expect_attribute(feature_table, "data")
  expect_attribute(feature_table, "feature_data")
  expect_attribute(feature_table, "sample_data")
})

test_that("$new sets all the attributes correctly", {
  ft <- FeatureTable$new(count_table, feature_data, sample_data, feature_table_rows_are_samples = TRUE)

  expect_equal(ft$num_samples, nsamples)
  expect_equal(ft$num_features, nfeatures)

  expect_equal(ft$sample_names, rownames(count_table))
  expect_equal(ft$feature_names, colnames(count_table))

  expect_equal(ft$dim, c(nsamples, nfeatures))
  expect_equal(ft$nrow, nsamples)
  expect_equal(ft$ncol, nfeatures)

  expect_equal(ft$data, count_table)
  expect_equal(ft$feature_data, expected_feature_data)
  expect_equal(ft$sample_data, expected_sample_data)
})

test_that("$new transposes the feature_table if feature_table_rows_are_samples = FALSE", {
  ft <- FeatureTable$new(t(count_table),
                         feature_data,
                         sample_data,
                         feature_table_rows_are_samples = FALSE)

  expect_equal(dim(ft$data), dim(count_table))

  expect_equal(ft$dim, dim(count_table))
  expect_equal(c(ft$nrow, ft$ncol), dim(count_table))
  expect_equal(c(ft$num_samples, ft$num_features), dim(count_table))

  expect_equal(ft$sample_names, rownames(count_table))
  expect_equal(ft$feature_names, colnames(count_table))

  expect_equal(ft$data, count_table)
  expect_equal(ft$feature_data, expected_feature_data)
  expect_equal(ft$sample_data, expected_sample_data)
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
  expect_error(FeatureTable$new(count_table, sample_data = data.frame()),
               class = Error$ArgumentError)
})

################################################################################
#### Sample data with > 2 dimensions ###########################################
################################################################################

test_that("$new raises error when sample_data has > 2 dimensions", {
  silly_data <- array(1:27, dim = c(3, 3, 3))

  expect_error(FeatureTable$new(count_table, sample_data = silly_data), class = Error$ArgumentError)
})

################################################################################
#### sample_data with a single sample ##########################################
################################################################################

# Just make sure that no weird dimension things happen.
test_that("$new gives good sample data even if there is only one sample", {
  ## Single covariate
  sample_data <- data.frame(
    Location = c("Portugal"),
    row.names = c("Sample_2")
  )
  expected_sample_data <- data.frame(
    Location = c(NA, "Portugal", NA, NA),
    row.names = paste0("Sample_", 1:4)
  )
  ft <- FeatureTable$new(count_table, sample_data = sample_data)
  expect_equal(ft$sample_data, expected_sample_data)

  ## Multiple covariates
  sample_data <- data.frame(
    Location = c("Portugal"),
    Season = c("Winter"),
    row.names = c("Sample_2")
  )
  expected_sample_data <- data.frame(
    Location = c(NA, "Portugal", NA, NA),
    Season = c(NA, "Winter", NA, NA),
    row.names = paste0("Sample_", 1:4)
  )
  ft <- FeatureTable$new(count_table, sample_data = sample_data)
  expect_equal(ft$sample_data, expected_sample_data)
})


################################################################################
#### sample_data with a single covariate #######################################
################################################################################

#### Good stuff

test_that("$new gives good sample data even with single covariate as data frame", {
  sample_data <- data.frame(
    Location = c("Spain", "Portugal", "Spain"),
    row.names = paste0("Sample_", c(1, 2, 4))
  )

  expected_sample_data <- data.frame(
    Location = c("Spain", "Portugal", NA, "Spain"),
    row.names = paste0("Sample_", 1:4)
  )

  ft <- FeatureTable$new(count_table, sample_data = sample_data)

  expect_equal(ft$sample_data, expected_sample_data)
})

test_that("$new gives good sample data even with single covariate as a vector with names", {
  sample_data <- c("Spain", "Portugal", "Spain")
  names(sample_data) <- paste0("Sample_", c(1, 2, 4))

  # The colnames is just X since it has to pick something.
  expected_sample_data <- data.frame(
    X = c("Spain", "Portugal", NA, "Spain"),
    row.names = paste0("Sample_", 1:4)
  )

  ft <- FeatureTable$new(count_table, sample_data = sample_data)

  expect_equal(ft$sample_data, expected_sample_data)
})

#### Expected errors

test_that("$new raises error with 1d sample data with no names", {
  sample_data <- c("Spain", "Portugal", "Spain")

  expect_error(FeatureTable$new(count_table, sample_data = sample_data),
               class = Error$ArgumentError)
})

test_that("$new raises error with 1d sample data that have no matches in feature_table", {
  sample_data <- c("Spain", "Portugal", "Spain")
  names(sample_data) <- letters[1:3]

  expect_error(FeatureTable$new(count_table, sample_data = sample_data),
               class = Error$ArgumentError)
})

test_that("$new gives a error if none of the samples have data in sample_data", {
  sample_data <- data.frame(Silly = c(TRUE, FALSE, TRUE), row.names = c("apple", "gie", "good"))

  expect_error(FeatureTable$new(count_table, sample_data = sample_data),
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
  expect_error(FeatureTable$new(count_table, feature_data = data.frame()),
               class = Error$ArgumentError)
})

################################################################################
#### Feature data with > 2 dimensions ##########################################
################################################################################

test_that("$new raises error when feature_data has > 2 dimensions", {
  silly_data <- array(1:27, dim = c(3, 3, 3))

  expect_error(FeatureTable$new(count_table, feature_data = silly_data), class = Error$ArgumentError)
})

################################################################################
#### feature_data with a single feature ########################################
################################################################################

# Just make sure that no weird dimension things happen.
test_that("$new gives good feature data even if there is only one feature", {
  ## Single covariate
  feature_data <- data.frame(
    Color = c("red"),
    row.names = c("Feature_3")
  )
  expected_feature_data <- data.frame(
    Color = c(NA, NA, "red", NA, NA),
    row.names = paste0("Feature_", 1:5)
  )
  ft <- FeatureTable$new(count_table, feature_data = feature_data)
  expect_equal(ft$feature_data, expected_feature_data)

  ## Multiple covariates
  feature_data <- data.frame(
    Color = c("red"),
    Shape = c("circle"),
    row.names = c("Feature_3")
  )
  expected_feature_data <- data.frame(
    Color = c(NA, NA, "red", NA, NA),
    Shape = c(NA, NA, "circle", NA, NA),
    row.names = paste0("Feature_", 1:5)
  )
  ft <- FeatureTable$new(count_table, feature_data = feature_data)
  expect_equal(ft$feature_data, expected_feature_data)
})

################################################################################
#### feature_data with a single covariate ######################################
################################################################################

#### Good stuff

test_that("$new gives good feature data even with single covariate as data frame", {
  feature_data <- data.frame(
    Color = c("red", "red", "blue"),
    row.names = paste0("Feature_", c(1, 3, 5))
  )

  expected_feature_data <- data.frame(
    Color = c("red", NA, "red", NA, "blue"),
    row.names = paste0("Feature_", 1:5)
  )

  ft <- FeatureTable$new(count_table, feature_data = feature_data)

  expect_equal(ft$feature_data, expected_feature_data)
})

test_that("$new gives good feature data even with single covariate as a vector with names", {
  feature_data <- c("red", "red", "blue")
  names(feature_data) <-paste0("Feature_", c(1, 3, 5))

  # The colnames is just X since it has to pick something.
  expected_feature_data <- data.frame(
    X = c("red", NA, "red", NA, "blue"),
    row.names = paste0("Feature_", 1:5)
  )

  ft <- FeatureTable$new(count_table, feature_data = feature_data)

  expect_equal(ft$feature_data, expected_feature_data)
})

#### Expected errors

test_that("$new raises error with 1d feature data with no names", {
  feature_data <- c("red", "red", "blue")

  expect_error(FeatureTable$new(count_table, feature_data = feature_data),
               class = Error$ArgumentError)
})

test_that("$new raises error with 1d sample data that have no matches in feature_table", {
  feature_data <- c("red", "red", "blue")
  names(feature_data) <- letters[1:3]

  expect_error(FeatureTable$new(count_table, feature_data = feature_data),
               class = Error$ArgumentError)
})

test_that("$new gives a error if none of the features have data in feature_data", {
  feature_data <- data.frame(Silly = c(TRUE, FALSE, TRUE), row.names = c("apple", "gie", "good"))

  expect_error(FeatureTable$new(count_table, feature_data = feature_data),
               class = Error$ArgumentError)
})

# TODO input is a matrix...does it work?

# TODO sample_sums
# TODO feature_sums, both these as "active" calculate then cache

# TODO map really is apply but forced to return the same dimension...need a real map function that can take multi param input function

