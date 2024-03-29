test_that("package_available returns TRUE if the package is available", {
  expect_true(package_available("base"))
})

test_that("package_available returns FALSE if the package is NOT available", {
  expect_false(package_available("baseeeeeeeeeeeeeeeeeeee23231231"))
})

test_that("package_unavailable returns FALSE if the package is available", {
  expect_false(package_unavailable("base"))
})

test_that("package_unavailable returns TRUE if the package is NOT available", {
  expect_true(package_unavailable("baseeeeeeeeeeeeeeeeeeee23231231"))
})

test_that("is_whole_number returns TRUE for integers, FALSE otherwise", {
  expect_true(is_whole_number(-1))
  expect_true(is_whole_number(0))
  expect_true(is_whole_number(1))

  expect_false(is_whole_number(-0.5))
  expect_false(is_whole_number(0.5))

  expect_true(all.equal(is_whole_number(c(-1, -0.5, 0, 0.5, 1)),
                        c(TRUE, FALSE, TRUE, FALSE, TRUE)))
})

test_that("is_whole_number gives an error with non number input", {
  expect_error(is_whole_number("apple"))
})

test_that("is_natural_number returns TRUE for integers, FALSE otherwise", {
  expect_true(is_natural_number(0))
  expect_true(is_natural_number(1))

  expect_false(is_natural_number(-1))
  expect_false(is_natural_number(-0.5))
  expect_false(is_natural_number(0.5))

  expect_true(all.equal(is_natural_number(c(-1, -0.5, 0, 0.5, 1)),
                        c(FALSE, FALSE, TRUE, FALSE, TRUE)))
})

test_that("is_natural_number gives an error with non number input", {
  expect_error(is_natural_number("apple"))
})

test_that("wide_to_long pivots a df or mat to long format from wide", {
  ft <- otu_feature_table()

  expected <- data.frame(
    Sample = paste("Sample", rep(1:4, 5), sep = "_"),
    Feature = paste("Feature", c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4)), sep = "_"),
    Value = c(ft$data[, "Feature_1"],
              ft$data[, "Feature_2"],
              ft$data[, "Feature_3"],
              ft$data[, "Feature_4"],
              ft$data[, "Feature_5"]),
    stringsAsFactors = TRUE
  )

  expect_equal(wide_to_long(ft$data), expected)
  expect_equal(wide_to_long(as.data.frame(ft$data, stringsAsFactors = TRUE)), expected)
})

#### Testing which columns to keep when treating some as hiearachical

test_that("hierarchical_columns keeps all columns that would be 'above' the current one in a hiearachy as well as the bottom level", {
  # ie if level = Phylum, then Domain and Phylumn would be TRUE, but Class and Order would be FALSE
  lee <- featuretable::lee$clone()

  #### Feature data

  # First permute the columns to make sure it doesn't depend on column order.
  lee$feature_data <- lee$feature_data[, c(2, 7, 4, 3, 6, 5, 1)]

  actual <- hierarchical_columns(lee$feature_data, "Domain")
  expect_equal(sum(actual), 1)
  expect_true(actual["Domain"])

  actual <- hierarchical_columns(lee$feature_data, "Phylum")
  expect_equal(sum(actual), 2)
  expect_true(actual["Domain"])
  expect_true(actual["Phylum"])

  actual <- hierarchical_columns(lee$feature_data, "Class")
  expect_equal(sum(actual), 3)
  expect_true(actual["Domain"])
  expect_true(actual["Phylum"])
  expect_true(actual["Class"])

  actual <- hierarchical_columns(lee$feature_data, "Order")
  expect_equal(sum(actual), 4)
  expect_true(actual["Domain"])
  expect_true(actual["Phylum"])
  expect_true(actual["Class"])
  expect_true(actual["Order"])

  actual <- hierarchical_columns(lee$feature_data, "Family")
  expect_equal(sum(actual), 5)
  expect_true(actual["Domain"])
  expect_true(actual["Phylum"])
  expect_true(actual["Class"])
  expect_true(actual["Order"])
  expect_true(actual["Family"])

  actual <- hierarchical_columns(lee$feature_data, "Genus")
  expect_equal(sum(actual), 6)
  expect_true(actual["Domain"])
  expect_true(actual["Phylum"])
  expect_true(actual["Class"])
  expect_true(actual["Order"])
  expect_true(actual["Family"])
  expect_true(actual["Genus"])

  actual <- hierarchical_columns(lee$feature_data, "Species")
  expect_equal(sum(actual), 7)
  expect_true(actual["Domain"])
  expect_true(actual["Phylum"])
  expect_true(actual["Class"])
  expect_true(actual["Order"])
  expect_true(actual["Family"])
  expect_true(actual["Genus"])
  expect_true(actual["Species"])


  #### Samples

  lee <- featuretable::lee

  # Sample data already has the hierarchy in two directions so no need to permute it.

  actual <- hierarchical_columns(lee$sample_data, "Temp")
  expect_equal(sum(actual), 1)
  expect_true(actual["Temp"])

  actual <- hierarchical_columns(lee$sample_data, "Type")
  expect_equal(sum(actual), 1)
  expect_true(actual["Type"])

  actual <- hierarchical_columns(lee$sample_data, "Char")
  expect_equal(sum(actual), 3)
  expect_true(actual["Type"])
  expect_true(actual["Char"])
  expect_true(actual["Color"])

  actual <- hierarchical_columns(lee$sample_data, "Color")
  expect_equal(sum(actual), 3)
  expect_true(actual["Type"])
  expect_true(actual["Char"])
  expect_true(actual["Color"])
})


# if blah

if (isTRUE(requireNamespace("tibble", quietly = TRUE)) &&
    isTRUE(requireNamespace("tidyr", quietly = TRUE)) &&
    isTRUE(requireNamespace("dplyr", quietly = TRUE))) {

  test_that("wide_to_long matches the pivot_longer function", {
    ft <- otu_feature_table()

    # Not using the pipe here to avoid the magrittr dep.
    expected <- as.data.frame(
      dplyr::arrange(
        tidyr::pivot_longer(
          tibble::rownames_to_column(
            as.data.frame(ft$data),
            "Sample"
          ),
          cols = -Sample,
          names_to = "Feature",
          values_to = "Value"
        ),
        Feature
      ),
      stringsAsFactors = TRUE
    )
    expected$Sample <- as.factor(expected$Sample)
    expected$Feature <- as.factor(expected$Feature)

    # TODO how will R v4 affect this?
    expect_equal(wide_to_long(ft$data), expected)
    expect_equal(wide_to_long(as.data.frame(ft$data, stringsAsFactors = TRUE)), expected)
  })
}

#### Some "wordy" helpers

test_that("present is true if something is there (ie val > 0)", {
  # present()
  expect_true(present(0.0001))
  expect_true(present(0.5))
  expect_true(present(1))

  expect_false(present(0))

  # is_present()
  expect_true(is_present(0.0001))
  expect_true(is_present(0.5))
  expect_true(is_present(1))

  expect_false(is_present(0))
})

test_that("absent is the opposite of present (val <= 0)", {
  # absent()
  expect_equal(absent(0.0001), !present(0.0001))
  expect_equal(absent(0.5), !present(0.5))
  expect_equal(absent(1), !present(1))

  expect_equal(absent(0), !present(0))

  # is_absent()
  expect_equal(is_absent(0.0001), !present(0.0001))
  expect_equal(is_absent(0.5), !present(0.5))
  expect_equal(is_absent(1), !present(1))

  expect_equal(is_absent(0), !present(0))
})

test_that("present/absent work with vectors too", {
  expect_true(present(c(0, 1)))
  expect_true(is_present(c(0, 1)))
  expect_false(present(c(0, 0)))
  expect_false(is_present(c(0, 0)))

  expect_false(absent(c(0, 1)))
  expect_false(is_absent(c(0, 1)))
  expect_true(absent(c(0, 0)))
  expect_true(is_absent(c(0, 0)))
})

test_that("present/absent always ignore NA", {
  expect_true(present(c(0, 1, NA)))
  expect_true(is_present(c(0, 1, NA)))
  expect_false(absent(c(0, 1, NA)))
  expect_false(is_absent(c(0, 1, NA)))

  expect_false(present(c(0, 0, NA)))
  expect_false(is_present(c(0, 0, NA)))
  expect_true(absent(c(0, 0, NA)))
  expect_true(is_absent(c(0, 0, NA)))
})

test_that("present/absent raise when negative numbers are present", {
  expect_error(present(-1:1), class = Error$ArgumentError)
  expect_error(is_present(-1:1), class = Error$ArgumentError)
  expect_error(absent(-1:1), class = Error$ArgumentError)
  expect_error(is_absent(-1:1), class = Error$ArgumentError)
})
