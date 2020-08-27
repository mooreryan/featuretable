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