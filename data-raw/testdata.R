## code to prepare `testdata` dataset goes here

testdata <- list(
  nsamples = 4,
  nfeatures = 5,
  count_table = matrix(0:19, 4, 5,
                       dimnames = list(Samples = paste0("Sample_", 1:4),
                                       Features = paste0("Feature_", 1:5))),

  feature_data = data.frame(
    Color = c("red", "red", "blue"),
    Shape = c("square", "circle", "square"),
    row.names = paste0("Feature_", c(1, 3, 5))
  ),

  sample_data = data.frame(
    Location = c("Spain", "Portugal", "Spain"),
    Season = c("Summer", "Winter", "Winter"),
    row.names = paste0("Sample_", c(1, 2, 4))
  ),

  expected_sample_data = data.frame(
    Location = c("Spain", "Portugal", NA, "Spain"),
    Season = c("Summer", "Winter", NA, "Winter"),
    row.names = paste0("Sample_", 1:4)
  ),

  expected_feature_data = data.frame(
    Color = c("red", NA, "red", NA, "blue"),
    Shape = c("square", NA, "circle", NA, "square"),
    row.names = paste0("Feature_", 1:5)
  )
)

usethis::use_data(testdata, overwrite = TRUE)
