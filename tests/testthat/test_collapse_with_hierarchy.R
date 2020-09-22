test_that("all collapse features helpers match", {
  #lee <- featuretable::lee
  lee <- featuretable:::ft_from.phyloseq(DivNet::Lee)
  expected <- lee$collapse_features(Class, keep_hierarchy = TRUE)

  expect_equal(collapse_features(lee, Class, keep_hierarchy = TRUE), expected)

  expect_equal(lee$collapse("features", Class, keep_hierarchy = TRUE), expected)
  expect_equal(collapse(lee, "features", Class, keep_hierarchy = TRUE), expected)
})

test_that("collapse features with keep_hierarchy doesn't drop cols above the `by` column in the hierarchy", {
  #lee <- featuretable::lee
  lee <- featuretable:::ft_from.phyloseq(DivNet::Lee)

  expect_equal(lee$collapse_features(Domain, keep_hierarchy = TRUE),
               lee$collapse_features(Domain, keep_hierarchy = FALSE))

  # Actually, there are no NA in this one.
  expect_equal(lee$collapse_features(Domain, keep_hierarchy = TRUE, keep_na = TRUE),
               lee$collapse_features(Domain, keep_hierarchy = FALSE, keep_na = TRUE))

  #### Phylum
  collapsed <- lee$collapse_features(Phylum, keep_hierarchy = TRUE)
  expect_equal(ncol(collapsed$feature_data), 2)
  expect_true("Domain" %in% colnames(collapsed$feature_data))
  expect_true("Phylum" %in% colnames(collapsed$feature_data))

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
