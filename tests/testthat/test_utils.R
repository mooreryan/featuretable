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