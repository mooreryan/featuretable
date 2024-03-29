# This title may be a bit misleading since some of these things aren't really attributes,
# rather accessor functions!

test_that("num_samples (and aliases) always gives correct result", {
  ft <- basic_feature_table()

  expect_equal(ft$num_samples(), testdata$nsamples)
  expect_equal(num_samples(ft), testdata$nsamples)

  expect_equal(ft$nsamples(), testdata$nsamples)
  expect_equal(nsamples(ft), testdata$nsamples)

  expect_equal(ft$num_observations(), testdata$nsamples)
  expect_equal(num_observations(ft), testdata$nsamples)

  expect_equal(ft$nobservations(), testdata$nsamples)
  expect_equal(nobservations(ft), testdata$nsamples)

  # Manually removing a sample.
  # TODO would be nice to prevent the user from doing this in the code somewhere.
  ft$data <- ft$data[-1, ]

  expect_equal(ft$num_samples(), testdata$nsamples - 1)
  expect_equal(num_samples(ft), testdata$nsamples - 1)

  expect_equal(ft$nsamples(), testdata$nsamples - 1)
  expect_equal(nsamples(ft), testdata$nsamples - 1)

  expect_equal(ft$num_observations(), testdata$nsamples - 1)
  expect_equal(num_observations(ft), testdata$nsamples - 1)

  expect_equal(ft$nobservations(), testdata$nsamples - 1)
  expect_equal(nobservations(ft), testdata$nsamples - 1)
})

test_that("num_features (and aliases) always gives correct result", {
  ft <- basic_feature_table()

  expect_equal(ft$num_features(), testdata$nfeatures)
  expect_equal(num_features(ft), testdata$nfeatures)

  expect_equal(ft$nfeatures(), testdata$nfeatures)
  expect_equal(nfeatures(ft), testdata$nfeatures)

  # Manually removing a feature
  # TODO would be nice to prevent the user from doing this in the code somewhere.
  ft$data <- ft$data[, -1]

  expect_equal(ft$num_features(), testdata$nfeatures - 1)
  expect_equal(num_features(ft), testdata$nfeatures - 1)

  expect_equal(ft$nfeatures(), testdata$nfeatures - 1)
  expect_equal(nfeatures(ft), testdata$nfeatures - 1)
})

test_that("sample_names (and aliases) always gives correct result", {
  ft <- basic_feature_table()
  expected_sample_names <- rownames(testdata$count_table)

  expect_equal(ft$sample_names(), expected_sample_names)
  expect_equal(sample_names(ft), expected_sample_names)

  expect_equal(ft$observation_names(), expected_sample_names)
  expect_equal(observation_names(ft), expected_sample_names)

  # Manually removing a sample
  # TODO would be nice to prevent the user from doing this in the code somewhere.
  ft$data <- ft$data[-1, ]

  expect_equal(ft$sample_names(), expected_sample_names[-1])
  expect_equal(sample_names(ft), expected_sample_names[-1])

  expect_equal(ft$observation_names(), expected_sample_names[-1])
  expect_equal(observation_names(ft), expected_sample_names[-1])
})

test_that("feature_names (and aliases) always gives correct result", {
  ft <- basic_feature_table()
  expected_feature_names <- colnames(testdata$count_table)

  expect_equal(ft$feature_names(), expected_feature_names)
  expect_equal(feature_names(ft), expected_feature_names)

  # Manually removing a feature
  # TODO would be nice to prevent the user from doing this in the code somewhere.
  ft$data <- ft$data[, -1]

  expect_equal(ft$feature_names(), expected_feature_names[-1])
  expect_equal(feature_names(ft), expected_feature_names[-1])
})

test_that("dim always gives correct result", {
  ft <- basic_feature_table()
  expected_dim <- c(testdata$nsamples, testdata$nfeatures)

  expect_equal(ft$dim(), expected_dim)
  expect_equal(dim(ft), expected_dim)

  expect_equal(ft$nrow(), expected_dim[[1]])
  expect_equal(nrow(ft), expected_dim[[1]])

  expect_equal(ft$ncol(), expected_dim[[2]])
  expect_equal(ncol(ft), expected_dim[[2]])

  # Manually removing a feature and a sample
  # TODO would be nice to prevent the user from doing this in the code somewhere.
  ft$data <- ft$data[-1, -1]

  expect_equal(ft$dim(), expected_dim - 1)
  expect_equal(dim(ft), expected_dim - 1)

  expect_equal(ft$nrow(), expected_dim[[1]] - 1)
  expect_equal(nrow(ft), expected_dim[[1]] - 1)

  expect_equal(ft$ncol(), expected_dim[[2]] - 1)
  expect_equal(ncol(ft), expected_dim[[2]] - 1)
})
