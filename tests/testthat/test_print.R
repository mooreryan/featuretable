test_that("print works", {
  ft <- basic_feature_table()

  expect_invisible(ft$print())

  expect_output(ft$print(), "4 samples")
  expect_output(ft$print(), "5 features")
  expect_output(ft$print(), "2 covariates")

  expect_output(print(ft), "4 samples")
  expect_output(print(ft), "5 features")
  expect_output(print(ft), "2 covariates")
})
