################################################################################
#### replace_zeros #############################################################
################################################################################

test_that("replace_zeros raises if use_cmultRepl = TRUE, but it isn't available", {
  ft <- basic_feature_table()

  with_mock(
    "featuretable:::package_available" = function(...) FALSE,
    expect_error(ft$replace_zeros(use_cmultRepl = TRUE),
                 class = Error$zCompositionsUnavailableError)
  )
  with_mock(
    "featuretable:::package_available" = function(...) FALSE,
    expect_error(replace_zeros(ft, use_cmultRepl = TRUE),
                 class = Error$zCompositionsUnavailableError)
  )

})

test_that("replace_zeros uses zCompositions::cmultRepl if available", {
  skip_if_not_installed("zCompositions")

  ft <- basic_feature_table()
  stopifnot(!all(ft$data > 0))
  ft_zr <- ft$replace_zeros(use_cmultRepl = TRUE)
  expect_true(all(ft_zr$data > 0))
  expect_equal(ft_zr$sample_data, ft$sample_data)
  expect_equal(ft_zr$feature_data, ft$feature_data)

  ft <- basic_feature_table()
  ft_zr <- replace_zeros(ft, use_cmultRepl = TRUE)
  expect_true(all(ft_zr$data > 0))
  expect_equal(ft_zr$sample_data, ft$sample_data)
  expect_equal(ft_zr$feature_data, ft$feature_data)
})

test_that("replace_zeros with cmutlRepl can specify output", {
  # By default, it will be p-counts, but make sure you can overwrite it.

  skip_if_not_installed("zCompositions")

  ft <- basic_feature_table()

  ft_zr <- ft$replace_zeros(use_cmultRepl = TRUE, output = "prop")
  # ie it output as proportions
  expect_true(all(ft_zr$data > 0 & ft_zr$data <= 1))

  ft_zr <- replace_zeros(ft, use_cmultRepl = TRUE, output = "prop")
  # ie it output as proportions
  expect_true(all(ft_zr$data > 0 & ft_zr$data <= 1))

  # Check output NOT all proportions.

  ft_zr <- ft$replace_zeros(use_cmultRepl = TRUE, output = "p-counts")
  expect_true(all(ft_zr$data > 0))
  expect_false(all(ft_zr$data <= 1))

  ft_zr <- replace_zeros(ft, use_cmultRepl = TRUE, output = "p-counts")
  expect_true(all(ft_zr$data > 0))
  expect_false(all(ft_zr$data <= 1))

  # And the default is not all proportions.
  ft_zr <- ft$replace_zeros(use_cmultRepl = TRUE)
  expect_true(all(ft_zr$data > 0))
  expect_false(all(ft_zr$data <= 1))

  ft_zr <- replace_zeros(ft, use_cmultRepl = TRUE)
  expect_true(all(ft_zr$data > 0))
  expect_false(all(ft_zr$data <= 1))

})

test_that("replace zeros works without zCompositions", {
  ft <- basic_feature_table()

  replacement <- 0.05

  stopifnot(!all(ft$data > 0))
  ft_zr <- ft$replace_zeros(use_cmultRepl = FALSE)
  expect_true(all(ft_zr$data > 0))
  expect_true(ft_zr$data[1, 1] == replacement)
  expect_equal(ft_zr$sample_data, ft$sample_data)
  expect_equal(ft_zr$feature_data, ft$feature_data)

  ft <- basic_feature_table()
  ft_zr <- replace_zeros(ft, use_cmultRepl = FALSE)

  expect_true(all(ft_zr$data > 0))
  expect_true(ft_zr$data[1, 1] == replacement)
  expect_equal(ft_zr$sample_data, ft$sample_data)
  expect_equal(ft_zr$feature_data, ft$feature_data)
})

test_that("replace_zeros raises if tol >= replacement", {
  ft <- basic_feature_table()

  expect_error(ft$replace_zeros(replacement = 1, tol = 1),
               class = Error$ArgumentError)
  expect_error(replace_zeros(ft, replacement = 1, tol = 1),
               class = Error$ArgumentError)

  expect_error(ft$replace_zeros(replacement = 1, tol = 2),
               class = Error$ArgumentError)
  expect_error(replace_zeros(ft, replacement = 1, tol = 2),
               class = Error$ArgumentError)
})

# TODO check that output for pcounts isnt overwritten in a weird way

################################################################################
#### clr #######################################################################
################################################################################

test_that("clr transforms feature_table to centered log ratio", {
  ft <- basic_feature_table()
  ft$data[1, 1] <- 1

  expected <- FeatureTable$new(
    t(apply(ft$data, 1, function(x) log2(x) - mean(log2(x)))),
    feature_data = ft$feature_data,
    sample_data = ft$sample_data
  )

  expect_equal(ft$clr(), expected)
  expect_equal(clr(ft), expected)
})

test_that("clr takes arbitrary base", {
  ft <- basic_feature_table()
  ft$data[1, 1] <- 1

  expected <- FeatureTable$new(
    t(apply(ft$data, 1, function(x) log(x, base = 2.5) - mean(log(x, base = 2.5)))),
    feature_data = ft$feature_data,
    sample_data = ft$sample_data
  )

  expect_equal(ft$clr(base = 2.5), expected)
  expect_equal(clr(ft, base = 2.5), expected)
})

test_that("clr raises error if any values are <= 0", {
  ft <- basic_feature_table()

  expect_error(ft$clr(), class = Error$DomainError)
  expect_error(clr(ft), class = Error$DomainError)
})
