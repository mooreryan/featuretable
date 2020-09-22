# If this test is failing for no good reason, you probably need to rebuild the `data`, since it depends on `featuretable::lee`.
#
# TODO make a small custom set to test this stuff instead.

test_that("all collapse features helpers match", {
  lee <- featuretable::lee

  expected <- lee$collapse_features(Class, keep_hierarchy = TRUE)

  expect_equal(collapse_features(lee, Class, keep_hierarchy = TRUE), expected)

  expect_equal(lee$collapse("features", Class, keep_hierarchy = TRUE), expected)
  expect_equal(collapse(lee, "features", Class, keep_hierarchy = TRUE), expected)
})


# TODO add a test like this for the collapse samples
test_that("collapse features treats NA the same way with either keep_hierarchy option", {
  lee <- featuretable::lee

  expect_equal(lee$collapse_features(Phylum, keep_na = TRUE, keep_hierarchy = TRUE)$feature_data$Phylum,
               lee$collapse_features(Phylum, keep_na = TRUE, keep_hierarchy = FALSE)$feature_data$Phylum)
})

test_that("collapse features won't add NA levels if they're aren't any even if keep_na is TRUE", {
  lee <- featuretable::lee

  # There are no NA in domain, so keep_hierarchy doesn't add NA to the level names.
  expect_equal(lee$collapse_features(Domain, keep_hierarchy = TRUE, keep_na = TRUE),
               lee$collapse_features(Domain, keep_hierarchy = FALSE, keep_na = TRUE))
})

test_that("collapse features with keep_hierarchy doesn't drop cols above the `by` column in the hierarchy", {
  lee <- featuretable::lee

  expect_equal(lee$collapse_features(Domain, keep_hierarchy = TRUE),
               lee$collapse_features(Domain, keep_hierarchy = FALSE))

  #### Phylum
  collapsed <- lee$collapse_features(Phylum, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$feature_data), 2)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_false("NA" %in% rownames(collapsed$feature_data))
  expect_false("NA" %in% colnames(collapsed$data))

  ## keep_na = TRUE
  collapsed <- lee$collapse_features(Phylum, keep_hierarchy = TRUE, keep_na = TRUE)
  expect_equal(ncol(collapsed$feature_data), 2)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("NA" %in% rownames(collapsed$feature_data))
  expect_true("NA" %in% colnames(collapsed$data))

  #### Class
  collapsed <- lee$collapse_features(Class, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$feature_data), 3)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_false("NA" %in% rownames(collapsed$feature_data))
  expect_false("NA" %in% colnames(collapsed$data))

  ## keep_na = TRUE
  collapsed <- lee$collapse_features(Class, keep_hierarchy = TRUE, keep_na = TRUE)
  expect_equal(ncol(collapsed$feature_data), 3)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_true("NA" %in% rownames(collapsed$feature_data))
  expect_true("NA" %in% colnames(collapsed$data))

  #### Order
  collapsed <- lee$collapse_features(Order, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$feature_data), 4)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_true("Order" %in% colnames(collapsed$feature_data))
  expect_false("NA" %in% rownames(collapsed$feature_data))
  expect_false("NA" %in% colnames(collapsed$data))

  ## keep_na = TRUE
  collapsed <- lee$collapse_features(Order, keep_hierarchy = TRUE, keep_na = TRUE)
  expect_equal(ncol(collapsed$feature_data), 4)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_true("Order" %in% colnames(collapsed$feature_data))
  expect_true("NA" %in% rownames(collapsed$feature_data))
  expect_true("NA" %in% colnames(collapsed$data))

  #### Family
  collapsed <- lee$collapse_features(Family, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$feature_data), 5)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_true("Order" %in% colnames(collapsed$feature_data))
  expect_true("Family" %in% colnames(collapsed$feature_data))
  expect_false("NA" %in% rownames(collapsed$feature_data))
  expect_false("NA" %in% colnames(collapsed$data))

  ## keep_na = TRUE
  collapsed <- lee$collapse_features(Family, keep_hierarchy = TRUE, keep_na = TRUE)
  expect_equal(ncol(collapsed$feature_data), 5)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_true("Order" %in% colnames(collapsed$feature_data))
  expect_true("Family" %in% colnames(collapsed$feature_data))
  expect_true("NA" %in% rownames(collapsed$feature_data))
  expect_true("NA" %in% colnames(collapsed$data))

  #### Genus
  collapsed <- lee$collapse_features(Genus, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$feature_data), 6)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_true("Order" %in% colnames(collapsed$feature_data))
  expect_true("Family" %in% colnames(collapsed$feature_data))
  expect_true("Genus" %in% colnames(collapsed$feature_data))
  expect_false("NA" %in% rownames(collapsed$feature_data))
  expect_false("NA" %in% colnames(collapsed$data))

  ## keep_na = TRUE
  collapsed <- lee$collapse_features(Genus, keep_hierarchy = TRUE, keep_na = TRUE)
  expect_equal(ncol(collapsed$feature_data), 6)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_true("Order" %in% colnames(collapsed$feature_data))
  expect_true("Family" %in% colnames(collapsed$feature_data))
  expect_true("Genus" %in% colnames(collapsed$feature_data))
  expect_true("NA" %in% rownames(collapsed$feature_data))
  expect_true("NA" %in% colnames(collapsed$data))

  #### Species
  collapsed <- lee$collapse_features(Species, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$feature_data), 7)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_true("Order" %in% colnames(collapsed$feature_data))
  expect_true("Family" %in% colnames(collapsed$feature_data))
  expect_true("Genus" %in% colnames(collapsed$feature_data))
  expect_true("Species" %in% colnames(collapsed$feature_data))
  expect_false("NA" %in% rownames(collapsed$feature_data))
  expect_false("NA" %in% colnames(collapsed$data))

  ## keep_na = TRUE
  collapsed <- lee$collapse_features(Species, keep_hierarchy = TRUE, keep_na = TRUE)
  expect_equal(ncol(collapsed$feature_data), 7)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))
  expect_true("Class" %in% colnames(collapsed$feature_data))
  expect_true("Order" %in% colnames(collapsed$feature_data))
  expect_true("Family" %in% colnames(collapsed$feature_data))
  expect_true("Genus" %in% colnames(collapsed$feature_data))
  expect_true("Species" %in% colnames(collapsed$feature_data))
  expect_true("NA" %in% rownames(collapsed$feature_data))
  expect_true("NA" %in% colnames(collapsed$data))
})

#### keep_samples

# TODO might want to test one with NA in the hiearchical columns.

test_that("all collapse samples helpers match", {
  lee <- featuretable::lee

  expected <- lee$collapse_samples(Char, keep_hierarchy = TRUE)

  expect_equal(collapse_samples(lee, Char, keep_hierarchy = TRUE), expected)

  expect_equal(lee$collapse("samples", Char, keep_hierarchy = TRUE), expected)
  expect_equal(collapse(lee, "samples", Char, keep_hierarchy = TRUE), expected)
})

test_that("collapse features with keep_hierarchy doesn't drop cols above the `by` column in the hierarchy", {
  lee <- featuretable::lee

  # Temp has no hiearchical relation to any of the other variables.
  expect_equal(lee$collapse_samples(Temp, keep_hierarchy = TRUE),
               lee$collapse_samples(Temp, keep_hierarchy = FALSE))

  expect_equal(lee$collapse_samples(Temp, keep_hierarchy = TRUE, keep_na = TRUE),
               lee$collapse_samples(Temp, keep_hierarchy = FALSE, keep_na = TRUE))

  #### Char
  collapsed <- lee$collapse_samples(Char, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$sample_data), 3)
  expect_true("Char" %in% colnames(collapsed$sample_data))
  expect_true("Type" %in% colnames(collapsed$sample_data))
  expect_true("Color" %in% colnames(collapsed$sample_data))

  # It actually has no NA so keep_na matches.
  expect_equal(lee$collapse_samples(Char, keep_hierarchy = TRUE),
               lee$collapse_samples(Char, keep_hierarchy = TRUE, keep_na = TRUE))

  #### Color
  collapsed <- lee$collapse_samples(Color, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$sample_data), 3)
  expect_true("Char" %in% colnames(collapsed$sample_data))
  expect_true("Type" %in% colnames(collapsed$sample_data))
  expect_true("Color" %in% colnames(collapsed$sample_data))

  # It actually has no NA so keep_na matches.
  expect_equal(lee$collapse_samples(Color, keep_hierarchy = TRUE),
               lee$collapse_samples(Color, keep_hierarchy = TRUE, keep_na = TRUE))

  #### Type
  collapsed <- lee$collapse_samples(Type, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$sample_data), 1)
  expect_true("Type" %in% colnames(collapsed$sample_data))

  # It actually has no NA so keep_na matches.
  expect_equal(lee$collapse_samples(Type, keep_hierarchy = TRUE),
               lee$collapse_samples(Type, keep_hierarchy = TRUE, keep_na = TRUE))
})
