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

basic_feature_table <- function() {
  FeatureTable$new(testdata$count_table,
                   feature_data = testdata$feature_data,
                   sample_data = testdata$sample_data)
}

feature_table_with_numeric_extra_data <- function() {
  fd <- testdata$feature_data
  sd <- testdata$sample_data

  fd$Apple <- seq(from = 1, to = nrow(fd), by = 1)
  fd$Orange <- seq(from = nrow(fd), to = 1, by = -1)
  sd$Pie <- seq(from = 1, to = nrow(sd), by = 1)

  FeatureTable$new(testdata$count_table,
                   feature_data = fd,
                   sample_data = sd)
}


rnorm_feature_table <- function() {
  count_table <- matrix(rnorm(20), 4, 5,
                        dimnames = list(Samples = paste0("Sample_", 1:4),
                                        Features = paste0("Feature_", 1:5)))

  FeatureTable$new(count_table,
                   feature_data = testdata$feature_data,
                   sample_data = testdata$sample_data)
}

iris_feature_table <- function() {
  d <- data.frame(Species = iris[, 5])

  rownames(d) <- rownames(iris)

  FeatureTable$new(iris[, 1:4], sample_data = d)
}

otu_feature_table <- function() {
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

  FeatureTable$new(count_table,
                   feature_data = feature_data,
                   sample_data = sample_data)
}

ft_for_collapse_testing <- function() {
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
    # This has one category with a single thing!
    Color = c("red", "red", "green", "blue", "blue"),
    # This has all the same thing!
    Shape = rep("square", times = 5),
    Length = c(5, 6, 2.3, 7, 10),
    row.names = paste0("Feature_", 1:5)
  )

  sample_data <- data.frame(
    Location = c("Spain", "Spain", "Portugal", "Spain"),
    Season = c("Summer", "Summer", "Winter", "Winter"),
    Silliness = c("Silly", "Silly", "Silly", "Silly"),
    SnazzyFactor = c(10, 12, 25, 3),
    row.names = paste0("Sample_", 1:4)
  )

  FeatureTable$new(count_table,
                   feature_data = feature_data,
                   sample_data = sample_data)
}
