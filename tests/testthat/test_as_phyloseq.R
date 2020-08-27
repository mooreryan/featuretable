test_that("as.phyloseq.FeatureTable converts FeatureTable to phyloseq", {
  skip_if_not_installed("phyloseq")

  ft <- basic_feature_table()

  expect_true(inherits(ft$as_phyloseq(), "phyloseq"))
  expect_true(inherits(as.phyloseq(ft), "phyloseq"))
})

test_that("as.phyloseq.FeatureTable raises error if called when phyloseq isn't available", {
  ft <- basic_feature_table()

  with_mock(
    package_available = function(...) FALSE,
    expect_error(ft$as_phyloseq(), class = Error$PhyloseqUnavailableError)
  )

  with_mock(
    package_available = function(...) FALSE,
    expect_error(as.phyloseq(ft), class = Error$PhyloseqUnavailableError)
  )
})