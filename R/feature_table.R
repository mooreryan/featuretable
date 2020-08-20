FeatureTable <- R6::R6Class(
  "FeatureTable",
  list(
    # Attributes
    num_samples = NULL,
    num_features = NULL,

    sample_names = NULL,
    feature_names = NULL,

    dim = NULL,
    nrow = NULL,
    ncol = NULL,

    data = NULL,

    # Rows are features, columns are stuff about the features.  TODO should it be hierarchical?
    feature_data = NULL,

    # Rows are samples, columns are things about the samples.
    sample_data = NULL,

    initialize = function(feature_table,
                          feature_data = NULL,
                          sample_data = NULL,
                          feature_table_rows_are_samples = TRUE) {

      if (length(dim(feature_table)) != 2) {
        rlang::abort("feature_table should be 2 dimensional!", class = Error$ArgumentError)
      }

      if (isFALSE(feature_table_rows_are_samples)) {
        self$data <- t(feature_table)
      } else {
        self$data <- feature_table
      }

      self$nrow <- nrow(self$data)
      self$ncol <- ncol(self$data)

      self$num_samples <- nrow(self$data)
      self$num_features <- ncol(self$data)

      self$sample_names <- rownames(self$data)
      self$feature_names <- colnames(self$data)

      self$dim <- dim(self$data)

      if (!is.null(feature_data)) {
        if (length(dim(feature_data)) > 2) {
          rlang::abort("Dim of feature_data was > 2.", class = Error$ArgumentError)
        }

        if (!any(self$feature_names %in% rownames(feature_data))) {
          rlang::abort("None of the feature names in the feature_table were found in the feature_data. Check your input!",
                       class = Error$ArgumentError)
        }

        # Select only features present in the feature table.  User may provide feature data that includes features not seen in the actual data.

        # feature_data has features X covariates
        self$feature_data <- feature_data[colnames(self$data), ]

        # Ensure correct rownames are there in case feature_data row names didn't match up.
        rownames(self$feature_data) <- colnames(self$data)
      }

      if (!is.null(sample_data)) {
        if (length(dim(sample_data)) > 2) {
          rlang::abort("Dim of sample_data was > 2.", class = Error$ArgumentError)
        }

        # Data is some sort of vector.
        if (is.null(dim(sample_data))) {
          if (is.null(names(sample_data))) {
            rlang::abort("1d sample_data had no names attribute", class = Error$ArgumentError)
          }

          # TODO check for no names in common.
          if (!any(self$sample_names %in% names(sample_data))) {
            rlang::abort("None of the sample names in feature_table were found in sample_data.  Check your input!",
                         class = Error$ArgumentError)
          }

          self$sample_data <- data.frame(X = sample_data[self$sample_names],
                                         # Need to set this manually.  If more samples in feature_table
                                         # than in sample_data, you will get some missing row names here.
                                         row.names = 1:self$num_samples)
        } else {
          # At least we have a 2d structure.

          if (!any(self$sample_names %in% rownames(sample_data))) {
            rlang::abort("None of the sample names in feature_table were found in sample_data.  Check your input!",
                         class = Error$ArgumentError)
          }

          if (ncol(sample_data) == 1) {
            self$sample_data <- data.frame(X = sample_data[self$sample_names, ])
            colnames(self$sample_data) <- colnames(sample_data)[[1]]
          } else if (ncol(sample_data) > 1) {
            self$sample_data <- sample_data[rownames(self$data), ]
          }
        }

        # Ensure correct rownames are there in case there are more samples in feature_table than sample_data.
        rownames(self$sample_data) <- rownames(self$data)
      }
    }
  )
)

