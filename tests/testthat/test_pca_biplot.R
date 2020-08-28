# TODO if possible check that error is raised with too low biplotr version.

test_that("pca_biplot passes extra arguments to biplot function", {
  ft <- basic_feature_table()

  expect_error(ft$pca_biplot(pc.biplot = TRUE), NA)
  expect_error(pca_biplot(ft, pc.biplot = TRUE), NA)
})


test_that("pca_biplot raises if use_biplotr = TRUE, but it isn't available", {
  ft <- basic_feature_table()

  with_mock(
    package_available = function(...) FALSE,
    expect_error(ft$pca_biplot(use_biplotr = TRUE),
                 class = Error$PackageUnavailableError)
  )
  with_mock(
    package_available = function(...) FALSE,
    expect_error(pca_biplot(ft, use_biplotr = TRUE),
                 class = Error$PackageUnavailableError)
  )
})

test_that("pca_biplot can use biplotr if available", {
  skip_if_not_installed("biplotr")

  ft <- iris_feature_table()

  expect_error(bp <- ft$pca_biplot(use_biplotr = TRUE), NA)

  # cairo_pdf("~/Desktop/hi.pdf", width = 5, height = 5)
  expect_error(plot(bp$biplot), NA)
  # dev.off()
  # system("open ~/Desktop/hi.pdf")

  expect_error(bp <- pca_biplot(ft, use_biplotr = TRUE), NA)
  expect_error(plot(bp$biplot), NA)
})

test_that("pca_biplot raises if biplotr specific options set with use_biplotr = FALSE", {
    skip_if_not_installed("biplotr")

    ft <- iris_feature_table()

    expect_error(
      ft$pca_biplot(use_biplotr = FALSE, include_sample_data = TRUE),
      class = Error$ArgumentError
    )
    expect_error(
      pca_biplot(ft, use_biplotr = FALSE, include_sample_data = TRUE),
      class = Error$ArgumentError
    )
})

test_that("pca_biplot can use sample data for point_color with biplotr", {
  skip_if_not_installed("biplotr")

  ft <- iris_feature_table()

  # TODO can i test the condition where point color is given and use_sample_data is not?

  expect_error(bp <- ft$pca_biplot(use_biplotr = TRUE,
                                   include_sample_data = TRUE,
                                   point_color = "Species"), NA)

  # cairo_pdf("~/Desktop/hi2.pdf", width = 5, height = 5)
  expect_error(plot(bp$biplot), NA)
  # dev.off()
  # system("open ~/Desktop/hi2.pdf")

  expect_error(bp <- pca_biplot(ft,
                                use_biplotr = TRUE,
                                include_sample_data = TRUE,
                                point_color = "Species"), NA)
  expect_error(plot(bp$biplot), NA)
})
