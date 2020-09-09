## code to prepare `basic_ft` dataset goes here

make_basic_ft <- function() {
  count_table <- matrix(
    c(
      0, 0, 0, 1, 10,
      0, 0, 1, 2, 20,
      0, 1, 2, 3, 30,
      1, 2, 3, 4, 40
    ),
    byrow = TRUE,
    nrow = 4,
    ncol = 5,
    dimnames = list(Samples = paste0("Sample_", 1:4),
                    Features = paste0("Feature_", 1:5))
  )

  feature_data <- data.frame(
    Color = c("red", "red", "red", "blue", "blue"),
    Shape = c("square", "circle", "square", NA, "circle"),
    Length = c(5, 6, 2.3, 7, 10),
    row.names = paste0("Feature_", 1:5)
  )

  sample_data <- data.frame(
    Location = c("Spain", "Spain", "Portugal", "Spain"),
    Season = c("Summer", "Summer", "Winter", "Winter"),
    SnazzyFactor = c(10, 12, 25, 3),
    row.names = paste0("Sample_", 1:4)
  )

  featuretable::FeatureTable$new(count_table,
                                 feature_data = feature_data,
                                 sample_data = sample_data)
}

ft <- make_basic_ft()

usethis::use_data(ft, overwrite = TRUE)
