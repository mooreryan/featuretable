# ft_for_merge_testing <- function() {
#   count_table <- matrix(
#     c(
#       0, 0, 0, 1, 10,
#       0, 0, 1, 2, 20,
#       0, 1, 2, 3, 30,
#       1, 2, 3, 4, 40
#     ),
#     byrow = TRUE,
#     nrow = 4,
#     ncol = 5,
#     dimnames = list(Samples = paste0("Sample_", 1:4),
#                     Features = paste0("Feature_", 1:5))
#   )
#
#   feature_data <- data.frame(
#     # This has one category with a single thing!
#     Color = c("red", "red", "green", "blue", "blue"),
#     # This has all the same thing!
#     Shape = rep("square", times = 5),
#     Length = c(5, 6, 2.3, 7, 10),
#     row.names = paste0("Feature_", 1:5)
#   )
#
#   sample_data <- data.frame(
#     Location = c("Spain", "Spain", "Portugal", "Spain"),
#     Season = c("Summer", "Summer", "Winter", "Winter"),
#     SnazzyFactor = c(10, 12, 25, 3),
#     row.names = paste0("Sample_", 1:4)
#   )
#
#   FeatureTable$new(count_table,
#                    feature_data = feature_data,
#                    sample_data = sample_data)
# }

# TODO change name of merge to agglomerate or something
# TODO method={sum,mean}
# TODO is_hierarchical={TRUE, FALSE}
# TODO test when feature has NA for by
# TODO multiple "by" categories

test_that("merge features raises if 'by' isn't valid", {
  ft <- ft_for_merge_testing()

  expect_error(ft$merge("features", by = "COLORS"),
               class = Error$ArgumentError)
  expect_error(merge(ft, "features", by = "COLORS"),
               class = Error$ArgumentError)

})

test_that("merge features works even if one category has only a single feature", {
  ft <- ft_for_merge_testing()

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # Before merge: red, red, green, blue, blue
  # After merge: blue, green, red

  expected <- FeatureTable$new(
    matrix(
      c(
        11, 0, 0,
        22, 1, 0,
        33, 2, 1,
        44, 3, 3
      ),
      byrow = TRUE,
      nrow = 4,
      ncol = 3,
      dimnames = list(Samples = paste0("Sample_", 1:4),
                      Features = c("Color_blue", "Color_green", "Color_red"))
    ),
    feature_data = data.frame(
      Color = c("blue", "green", "red"),
      row.names = c("Color_blue", "Color_green", "Color_red")
    ),
    sample_data = ft$sample_data
  )

  actual <- ft$merge("features", "Color")

  expect_equal(actual, expected)
  expect_equal(merge(ft, "features", "Color"), expected)
})

test_that("merge features works even if a feature data col is the same for all features", {
  ft <- ft_for_merge_testing()

  #     c(
  #       0, 0, 0, 1, 10,
  #       0, 0, 1, 2, 20,
  #       0, 1, 2, 3, 30,
  #       1, 2, 3, 4, 40
  #     ),

  # Before merge: All square
  # After merge: Shape_square (one column.)

  expected <- FeatureTable$new(
    matrix(
      c(
        11,
        23,
        36,
        50
      ),
      byrow = TRUE,
      nrow = 4,
      ncol = 1,
      dimnames = list(Samples = paste0("Sample_", 1:4),
                      Features = c("Shape_square"))
    ),
    feature_data = data.frame(
      Shape = c("square"),
      row.names = c("Shape_square")
    ),
    sample_data = ft$sample_data
  )

  actual <- ft$merge("features", "Shape")

  expect_equal(actual, expected)
  expect_equal(merge(ft, "features", "Shape"), expected)
})

test_that("merge with margin=2 is the same as 'features'", {
  ft <- ft_for_merge_testing()

  expect_equal(ft$merge(2, "Shape"), ft$merge("features", "Shape"))
  expect_equal(merge(ft, 2, "Shape"), merge(ft, "features", "Shape"))

  expect_equal(ft$merge(2, "Color"), ft$merge("features", "Color"))
  expect_equal(merge(ft, 2, "Color"), merge(ft, "features", "Color"))
})

test_that("merge features can use tidy eval", {
  ft <- ft_for_merge_testing()

  expect_equal(ft$merge("features", Shape),
               ft$merge("features", "Shape"))

  expect_equal(merge(ft, "features", Shape),
               merge(ft, "features", "Shape"))

  expect_equal(ft$merge("features", Color),
               ft$merge("features", "Color"))
  expect_equal(merge(ft, "features", Color),
               merge(ft, "features", "Color"))
})

test_that("s3 merge raises when passed weird args", {
  ft <- ft_for_merge_testing()

  expect_error(merge(ft, apple = 3, pie = "yum"))
})
