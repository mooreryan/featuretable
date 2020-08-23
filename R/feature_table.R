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

    # Rows are features, columns are stuff about the features.  It currently isn't hierarchical.
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

      if (nrow(feature_table) == 0 | ncol(feature_table) == 0) {
        rlang::abort("feature_table was empty!", class = Error$ArgumentError)
      }

      if (isFALSE(feature_table_rows_are_samples)) {
        # Feature tables are always stored with the samples as the rows.
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

      #### Set the feature data ####
      if (!is.null(feature_data)) {
        private$handle_feature_data(feature_data)
      }

      #### Set the sample data ####
      if (!is.null(sample_data)) {
        private$handle_sample_data(sample_data)
      }
    },

    apply = function(margin, fn, ...) {
      base::apply(X = self$data, MARGIN = margin, FUN = fn, ...)
    },

    # `fn` takes data and index
    apply_with_index = function(margin, fn, ...) {
      apply_with_index(self$data, margin, fn, ...)
    },

    apply_features = function(fn, ...) {
      base::apply(X = self$data, MARGIN = 2, FUN = fn, ...)
    },

    apply_samples = function(fn, ...) {
      base::apply(X = self$data, MARGIN = 1, FUN = fn, ...)
    }
  ),
  private = list(
    handle_feature_data = function(feature_data) {
      if (length(dim(feature_data)) > 2) {
        rlang::abort("Dim of feature_data was > 2.", class = Error$ArgumentError)
      }

      # Check if data is some sort of 1d thing.
      if (is.null(dim(feature_data))) {
        # Yeah, it's 1d.

        # If there are no names, then we abort.
        if (is.null(names(feature_data))) {
          rlang::abort("1d feature_data had no names attribute",
                       class = Error$ArgumentError)
        }

        # Make sure that there are at least some names in common.
        if (!any(self$feature_names %in% names(feature_data))) {
          rlang::abort("None of the feature names in feature_table were found in feature_data. Check your input!", class = Error$ArgumentError)
        }

        # Create a data frame from the 1d data.
        self$feature_data <- data.frame(X = feature_data[self$feature_names],
                                        row.names = seq_len(self$num_features))
      }
      # At least we have a 3d structure....
      else {
        # Check if any dimension is zero.  Lots of checks for better error messages.
        if (nrow(feature_data) == 0 | ncol(feature_data) == 0) {
          rlang::abort("feature_data had zero rows and columns!", class = Error$ArgumentError)
        }

        if (nrow(feature_data) == 0) {
          rlang::abort("feature_data had zero rows!", class = Error$ArgumentError)
        }

        if (ncol(feature_data) == 0) {
          rlang::abort("feature_data had zero columns!", class = Error$ArgumentError)
        }

        if (!any(self$feature_names %in% rownames(feature_data))) {
          rlang::abort("None of the feature names in the feature_table were found in the feature_data. Check your input!",
                       class = Error$ArgumentError)
        }

        # Even if it is a data frame, it still may have 1 covariate, which needs special treatment.
        if (ncol(feature_data) == 1) {
          self$feature_data <- data.frame(X = feature_data[self$feature_names, ])
          colnames(self$feature_data) <- colnames(feature_data)[[1]]
        } else if (ncol(feature_data) > 1) {
          self$feature_data <- feature_data[self$feature_names, ]
        } else {
          rlang::abort("it should be impossible to get here...",
                       class = Error$ImpossibleConditionError)
        }
      }

      # Ensure correct rownames are there in case feature_data row names didn't match up.
      rownames(self$feature_data) <- colnames(self$data)
    },

    handle_sample_data = function(sample_data) {
      if (length(dim(sample_data)) > 2) {
        rlang::abort("Dim of sample_data was > 2.", class = Error$ArgumentError)
      }

      # Data is some sort of vector.
      if (is.null(dim(sample_data))) {
        if (is.null(names(sample_data))) {
          rlang::abort("1d sample_data had no names attribute", class = Error$ArgumentError)
        }

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

        # Check if any dimension is zero.  Lots of checks for better error messages.
        if (nrow(sample_data) == 0 | ncol(sample_data) == 0) {
          rlang::abort("sample_data had zero rows and columns!", class = Error$ArgumentError)
        }

        if (nrow(sample_data) == 0) {
          rlang::abort("sample_data had zero rows!", class = Error$ArgumentError)
        }

        if (ncol(sample_data) == 0) {
          rlang::abort("sample_data had zero columns!", class = Error$ArgumentError)
        }

        if (!any(self$sample_names %in% rownames(sample_data))) {
          rlang::abort("None of the sample names in feature_table were found in sample_data.  Check your input!",
                       class = Error$ArgumentError)
        }

        if (ncol(sample_data) == 1) {
          self$sample_data <- data.frame(X = sample_data[self$sample_names, ])
          colnames(self$sample_data) <- colnames(sample_data)[[1]]
        } else if (ncol(sample_data) > 1) {
          self$sample_data <- sample_data[self$sample_names, ]
        } else {
          rlang::abort("it should be impossible to get here...",
                       class = Error$ImpossibleConditionError)
        }
      }

      # Ensure correct rownames are there in case there are more samples in feature_table than sample_data.
      rownames(self$sample_data) <- rownames(self$data)
    }
  )
)

as.data.frame.FeatureTable <- function(ft) {
  ft$data
}

# Like `apply`, it coerces X to a matrix first.
# FUN should take two arguments, whatever the MARGIN would have, plus the index.
apply_with_index <- function(X, MARGIN, FUN, ...) {
  if (!is.null(dim(MARGIN))) {
    stop("MARGIN must be a scaler (1 or 2).")
  }

  if (length(dim(X)) != 2) {
    stop("dim(X) must be 2.  Other stuff not implemented!")
  } else {
    X <- as.matrix(X)
  }

  if (MARGIN == 1) {
    # By rows.
    indices <- seq_len(nrow(X))

    sapply(indices, function(i) {
      dat <- X[i, ]

      FUN(dat, i)
    })
  } else if (MARGIN == 2) {
    # By cols.
    indices <- seq_len(ncol(X))

    sapply(indices, function(j) {
      dat <- X[, j]

      FUN(dat, j)
    })
  } else {
    stop("MARGIN must be 1 or 2.")
  }
}
