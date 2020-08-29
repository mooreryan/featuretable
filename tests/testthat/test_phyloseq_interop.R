test_that("as.phyloseq.FeatureTable converts FeatureTable to phyloseq", {
  skip_if_not_installed("phyloseq")

  ft <- basic_feature_table()

  expect_true(inherits(ft$as_phyloseq(), "phyloseq"))
  expect_true(inherits(as.phyloseq(ft), "phyloseq"))
})

test_that("as.phyloseq.FeatureTable raises error if called when phyloseq isn't available", {
  ft <- basic_feature_table()

  with_mock(
    "featuretable:::package_available" = function(...) FALSE,
    expect_error(ft$as_phyloseq(), class = Error$PhyloseqUnavailableError)
  )

  with_mock(
    "featuretable:::package_available" = function(...) FALSE,
    expect_error(as.phyloseq(ft), class = Error$PhyloseqUnavailableError)
  )
})

test_that("as.phyloseq.FeatureTable raises error if called when phyloseq isn't available", {
  ft <- basic_feature_table()

  with_mock(
    "featuretable:::package_available" = function(...) FALSE,
    expect_error(ft$as_phyloseq(), class = Error$PhyloseqUnavailableError)
  )

  with_mock(
    "featuretable:::package_available" = function(...) FALSE,
    expect_error(as.phyloseq(ft), class = Error$PhyloseqUnavailableError)
  )
})

test_that("ft_from.phyloseq makes a new FeatureTable from a phyloseq object", {
  skip_if_not_installed("phyloseq")

  ft <- feature_table_with_numeric_extra_data()

  phy <- ft$as_phyloseq()

  ft_from_phy <- ft_from(phy, numeric_feature_data_columns = c("Apple", "Orange"))

  expect_is(ft_from_phy, "FeatureTable")

  expect_equal(ft_from_phy, ft)

  ##### With different rows.

  ft <- feature_table_with_numeric_extra_data()

  phy2 <- phyloseq::phyloseq(
    phyloseq::otu_table(t(ft$data), taxa_are_rows = TRUE),
    phyloseq::sample_data(ft$sample_data),
    phyloseq::tax_table(as.matrix(ft$feature_data))
  )

  ft_from_phy2 <- ft_from(phy2, numeric_feature_data_columns = c("Apple", "Orange"))

  expect_equal(ft_from_phy2, ft)
})

test_that("ft_from.phyloseq raises if numeric_feature_data_columns aren't valid", {
  skip_if_not_installed("phyloseq")

  ft <- feature_table_with_numeric_extra_data()

  phy <- ft$as_phyloseq()

  expect_error(ft_from(phy, numeric_feature_data_columns = "Beep"),
               class = Error$ArgumentError)
  expect_error(ft_from(phy, numeric_feature_data_columns = 23424),
               class = Error$ArgumentError)
  expect_error(ft_from(phy, numeric_feature_data_columns = c("Beep", 2342)),
               class = Error$ArgumentError)
})
