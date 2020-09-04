#' FeatureTable
#'
#' @description
#' R6 Class representing a feature table and its associated data.
#'
#' @details
#' A FeatureTable has a feature_table, sample_data, and feature_data.
#'
#' @export
FeatureTable <- R6::R6Class(
  "FeatureTable",
  list(
    # Attributes

    data = NULL,

    # Rows are features, columns are stuff about the features.  It currently isn't hierarchical.
    feature_data = NULL,

    # Rows are samples, columns are things about the samples.
    sample_data = NULL,

    #' @description
    #' Create a new FeatureTable object.
    #'
    #' @param feature_table Feature table, count table, or something similar.
    #' @param feature_data Feature data (data about the features).
    #' @param sample_data Sample/observation data (data about the samples/observations).
    #' @param feature_table_rows_are_samples Are the rows of the feature_table samples/observations?
    #'
    #' @return A new `FeatureTable` object.
    #'
    #' @examples
    #' "TODO"
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

      #### Set the feature data ####
      if (!is.null(feature_data)) {
        private$handle_feature_data(feature_data)
      }

      #### Set the sample data ####
      if (!is.null(sample_data)) {
        private$handle_sample_data(sample_data)
      }
    },

    #### Dim

    #' @description
    #' Return the dimensions of the FeatureTable.
    #'
    #' @return The dimension of the feature_table.
    dim = function() {
      dim(self$data)
    },

    #### Dealing with number of rows

    #' @description
    #' Return the number of samples/observations/rows in the FeatureTable.
    #'
    #' @return The number of rows in the FeatureTable.
    num_samples = function() {
      nrow(self$data)
    },

    #' @description
    #' Return the number of samples/observations/rows in the FeatureTable.
    #'
    #' @return The number of rows in the FeatureTable.
    nsamples = function() {
      nrow(self$data)
    },

    #' @description
    #' Return the number of samples/observations/rows in the FeatureTable.
    #'
    #' @return The number of rows in the FeatureTable.
    num_observations = function() {
      nrow(self$data)
    },

    #' @description
    #' Return the number of samples/observations/rows in the FeatureTable.
    #'
    #' @return The number of rows in the FeatureTable.
    nobservations = function() {
      nrow(self$data)
    },

    #' @description
    #' Return the number of samples/observations/rows in the FeatureTable.
    #'
    #' @return The number of rows in the FeatureTable.
    nrow = function() {
      nrow(self$data)
    },

    #### Dealing with number of columns

    #' @description
    #' Return the number of features/columns in the FeatureTable.
    #'
    #' @return The number of columns in the FeatureTable.
    num_features = function() {
      ncol(self$data)
    },

    #' @description
    #' Return the number of features/columns in the FeatureTable.
    #'
    #' @return The number of columns in the FeatureTable.
    nfeatures = function() {
      ncol(self$data)
    },

    #' @description
    #' Return the number of features/columns in the FeatureTable.
    #'
    #' @return The number of columns in the FeatureTable.
    ncol = function() {
      ncol(self$data)
    },

    #### Dealing with row names

    #' @description
    #' Return the names of samples/observations/rows in the FeatureTable.
    #'
    #' @return The names of samples/observations/rows in the FeatureTable.
    sample_names = function() {
      rownames(self$data)
    },

    #' @description
    #' Return the names of samples/observations/rows in the FeatureTable.
    #'
    #' @return The names of samples/observations/rows in the FeatureTable.
    observation_names = function() {
      rownames(self$data)
    },

    #### Dealing with col names

    #' @description
    #' Return the names of features/columns in the FeatureTable.
    #'
    #' @return The names of features/columns in the FeatureTable.
    feature_names = function() {
      colnames(self$data)
    },

    print = function(...) {
      cat("FeatureTable: \n")
      cat("  data         -- ",
          self$num_samples(),
          " samples, ",
          self$num_features(),
          " features\n",
          sep = "")

      if (!is.null(self$feature_data)) {
        cat("  feature_data -- ",
            ncol(self$feature_data),
            " covariates\n",
            sep = "")
      }

      if (!is.null(self$sample_data)) {
        cat("  sample_data  -- ",
            ncol(self$sample_data),
            " covariates\n",
            sep = "")
      }

      invisible(self)
    },

    apply = function(margin, fn, ...) {
      base::apply(X = self$data, MARGIN = margin, FUN = fn, ...)
    },

    # `fn` takes data and index
    apply_with_index = function(margin, fn, ...) {
      apply_with_index(self$data, margin, fn, ...)
    },

    # `fn` takes data and name
    apply_with_name = function(margin, fn, ...) {
      apply_with_name(self$data, margin, fn, ...)
    },

    apply_features = function(fn, ...) {
      base::apply(X = self$data, MARGIN = 2, FUN = fn, ...)
    },

    apply_features_with_index = function(fn, ...) {
      self$apply_with_index(margin = 2, fn, ...)
    },

    apply_features_with_name = function(fn, ...) {
      self$apply_with_name(margin = 2, fn, ...)
    },

    apply_samples = function(fn, ...) {
      base::apply(X = self$data, MARGIN = 1, FUN = fn, ...)
    },

    apply_samples_with_index = function(fn, ...) {
      self$apply_with_index(margin = 1, fn, ...)
    },

    apply_samples_with_name = function(fn, ...) {
      self$apply_with_name(margin = 1, fn, ...)
    },

    reduce = function(margin, fn, ...) {
      if (length(margin) != 1 ||
          (margin != 1 && margin != 2)) {
        rlang::abort(sprintf("margin must be 1 or 2.  Got %s.", margin),
                     class = Error$ArgumentError)
      }

      # TODO: Some special reducers will be slow: sum, prod, length, etc.
      result <- apply(X = self$data, MARGIN = margin, FUN = function(x) Reduce(fn, x, ...))

      if (length(result) != self$dim()[margin]) {
        rlang::abort("something bad happened", class = Error$ImpossibleConditionError)
      } else {
        result
      }
    },

    reduce_features = function(fn, ...) {
      self$reduce(2, fn, ...)
    },

    # TODO add an alias called `reduce_observations`.
    reduce_samples = function(fn, ...) {
      self$reduce(1, fn, ...)
    },

    reduce_all = function(fn, ...) {
      result <- Reduce(fn, self$data, ...)

      if (length(result) != 1) {
        rlang::abort(sprintf("Length of result should be 1. Got %d", length(result)),
                     class = Error$Error)
      } else {
        result
      }
    },

    map = function(margin, fn, ...) {
      private$map_wrapper(self$apply, margin, fn, ...)
    },

    map_with_index = function(margin, fn, ...) {
      private$map_wrapper(self$apply_with_index, margin, fn, ...)
    },

    map_with_name = function(margin, fn, ...) {
      private$map_wrapper(self$apply_with_name, margin, fn, ...)
    },

    map_features = function(fn, ...) {
      self$map(2, fn, ...)
    },

    map_features_with_index = function(fn, ...) {
      self$map_with_index(2, fn, ...)
    },

    map_features_with_name = function(fn, ...) {
      self$map_with_name(2, fn, ...)
    },

    map_samples = function(fn, ...) {
      self$map(1, fn, ...)
    },

    map_samples_with_index = function(fn, ...) {
      self$map_with_index(1, fn, ...)
    },

    map_samples_with_name = function(fn, ...) {
      self$map_with_name(1, fn, ...)
    },

    # TODO what if the predicate attempts to get a column not in the feature/sample data?
    keep = function(margin, predicate, ...) {
      if (margin == "features" || margin == 2) {
        # Let the user have access to the feature_data
        predicate <- rlang::eval_tidy(rlang::enquo(predicate), self$feature_data)

        if (isTRUE(inherits(predicate, "function"))) {
          predicate_result <- self$apply(2, predicate, ...)
        } else {
          predicate_result <- predicate
        }

        if (isFALSE(inherits(predicate_result, "logical"))) {
          rlang::abort("Predicate result was not logical.  Check your predicate function.",
                       class = Error$NonPredicateFunctionError)
        }

        if (length(predicate_result) != self$num_features()) {
          rlang::abort(
            sprintf(
              "Predicate result should have length %s. Got %s. Check your predicate!",
              self$num_features(),
              length(predicate_result)
            ),
            class = Error$IncorrectLengthError
          )
        }

        # Change all NA to FALSE.
        predicate_result <- ifelse(is.na(predicate_result), FALSE, predicate_result)

        if (sum(predicate_result) == 0) {
          rlang::abort("No features remaining after filtering",
                       class = Error$NoFeaturesRemainingError)
        } else if (sum(predicate_result) == 1) {
          feature_name <- colnames(self$data)[predicate_result]
          stopifnot(length(feature_name) == 1)

          result <- self$data[, predicate_result]

          # nrow IS num_samples here and not non_samples
          result <- matrix(result, nrow = self$num_samples(), ncol = 1,
                           dimnames = list(Samples = rownames(self$data),
                                           Features = feature_name))
        } else {
          result <- self$data[, predicate_result]
        }

        FeatureTable$new(result, self$feature_data, self$sample_data)
      } else if (margin == "samples" || margin == 1) {
        # Let the user have access to the sample_data
        predicate <- rlang::eval_tidy(rlang::enquo(predicate), self$sample_data)

        if (isTRUE(inherits(predicate, "function"))) {
          predicate_result <- self$apply(1, predicate, ...)
        } else {
          predicate_result <- predicate
        }

        if (isFALSE(inherits(predicate_result, "logical"))) {
          rlang::abort("Predicate result was not logical.  Check your predicate function.",
                       class = Error$NonPredicateFunctionError)
        }

        if (length(predicate_result) != self$num_samples()) {
          rlang::abort(
            sprintf(
              "Predicate result should have length %s. Got %s. Check your predicate!",
              self$num_samples(),
              length(predicate_result)
            ),
            class = Error$IncorrectLengthError
          )
        }

        # Change all NA to FALSE.
        predicate_result <- ifelse(is.na(predicate_result), FALSE, predicate_result)

        if (sum(predicate_result) == 0) {
          rlang::abort("No samples remaining after filtering",
                       class = Error$NoSamplesRemainingError)
        } else if (sum(predicate_result) == 1) {
          sample_name <- rownames(self$data)[predicate_result]
          stopifnot(length(sample_name) == 1)

          result <- self$data[predicate_result, ]

          result <- matrix(result, nrow = 1, ncol = self$num_features(),
                           dimnames = list(Samples = sample_name,
                                           Features = colnames(self$data)))
        } else {
          result <- self$data[predicate_result, ]
        }

        FeatureTable$new(result, self$feature_data, self$sample_data)
      } else {
        stop("todo")
      }
    },

    keep_features = function(predicate, ...) {
      predicate <- rlang::eval_tidy(rlang::enquo(predicate), self$feature_data)

      self$keep(2, predicate, ...)
    },

    keep_samples = function(predicate, ...) {
      predicate <- rlang::eval_tidy(rlang::enquo(predicate), self$sample_data)

      self$keep(1, predicate, ...)
    },

    #### Conversion

    #' Convert FeatureTable to phyloseq object.
    #'
    #' @description
    #' If the 'phyloseq' package is not installed, it raises an Error.
    #'
    #' @param ft A FeatureTable
    #'
    #' @return a phyloseq object
    as_phyloseq = function() {
      if (package_available("phyloseq")) {
        phyloseq::phyloseq(
          phyloseq::otu_table(self$data, taxa_are_rows = FALSE),

          # Need to manually ensure that this is a matrix.
          #
          # TODO FeatureTable feature_data doesn't have to be hierarchical, will this mess up phyloseq?
          phyloseq::tax_table(as.matrix(self$feature_data)),

          phyloseq::sample_data(self$sample_data)
        )
      } else {
        rlang::abort("Package 'phyloseq' is not available.  Try installing it first!",
                     class = Error$PhyloseqUnavailableError)
      }
    },

    #### Querying

    #' Is the FeatureTable a count table?
    #'
    #' @param ft A FeatureTable
    #'
    #' @return TRUE if \code{feature_table} contains only natural numbers (counting numbers >= 0), FALSE otherwise.
    is_count_table = function() {
      all(is_natural_number(self$data))
    },

    #### CoDA basics

    # TODO note that replacement is ignored if use_cmultRepl = TRUE.

    #' Replacing zeros.
    #'
    #' @param ft
    #' @param replacement (Ignored if \code{use_cmultRepl = TRUE})
    #' @param tol (Ignored if \code{use_cmultRepl = TRUE})
    #' @param use_cmultRepl TRUE/FALSE whether to use \code{cmultRepl} function.
    #' @param ... Extra arguments (i.g., passed to \code{cmultRepl})
    #'
    #' @return A new FeatureTable with the zeros in \code{data} replaced.
    replace_zeros = function(replacement = 0.05,
                             tol = .Machine$double.eps ^ 0.5,
                             use_cmultRepl = FALSE,
                             ...) {
      if (isTRUE(use_cmultRepl) && package_available("zCompositions")) {
        extra_args <- eval(substitute(alist(...)))

        if (is.null(extra_args$output)) {
          new_feature_table <- zCompositions::cmultRepl(self$data, output = "p-counts", ...)
        } else {
          new_feature_table <- zCompositions::cmultRepl(self$data, ...)
        }
      } else if (isTRUE(use_cmultRepl)) {
        rlang::abort("Package 'zCompositions' is not available.  Try installing it!",
                     class = Error$zCompositionsUnavailableError)
      } else if (tol >= replacement) {
        rlang::abort("tol must be < replacement", class = Error$ArgumentError)
      } else {
        new_feature_table <- ifelse(self$data < tol, replacement, self$data)
      }

      FeatureTable$new(feature_table = new_feature_table,
                       sample_data = self$sample_data,
                       feature_data = self$feature_data)
    },

    #' Centered log ratio.
    #'
    #' @param ft A Feature table.
    #' @param base Base of logarithm.
    #'
    #' @return A FeatureTable with the \code{data} transformed with centered log ratio.
    clr = function(base = 2) {
      if (any(self$data <= 0)) {
        rlang::abort("At least one value was <= 0.  Did you replace zeros?",
                     class = Error$DomainError)
      }

      transformed_data <- t(apply(self$data, 1, function(x) {
        log(x, base = base) - mean(log(x, base = base))
      }))


      FeatureTable$new(
        transformed_data,
        sample_data = self$sample_data,
        feature_data = self$feature_data
      )
    },

    # Be careful if you use_biplotr = TRUE, not to set options for the normal biplot function.
    pca_biplot = function(use_biplotr = FALSE, include_sample_data = FALSE, ...) {
      if (isTRUE(use_biplotr)) {
        # Need to check some things!
        if (package_unavailable("biplotr")) {
          rlang::abort("Package 'biplotr' is not available. Try installing it.",
                       class = Error$PackageUnavailableError)
        }

        if (isTRUE(include_sample_data)) {
          if (is.null(self$sample_data)) {
            rlang::abort("self$sample_data is NULL, but include_sample_data was TRUE. Check your arguments.",
                         class = Error$ArgumentError)
          }

          plot_data <- merge(self$data, self$sample_data, by = "row.names")

          # plot_data doesn't have rownames correct...fix it!
          rownames(plot_data) <- plot_data$Row.names
          plot_data$Row.names <- NULL

          # TODO handle case where na are present in sample_data

          biplotr::pca_biplot(plot_data, data_cols = 1:self$num_features(), ...)
        } else {
          biplotr::pca_biplot(self$data, ...)
        }
      } else {
        # Need to check some more things!
        if (isTRUE(include_sample_data)) {
          rlang::abort("include_sample_data was TRUE, but use_biplotr was FALSE. Check your arguments.",
                       class = Error$ArgumentError)
        }

        biplot(prcomp(self$data), ...)
      }
    },

    # both are greater than or equal to
    core_microbiome = function(detection_limit = 1,
                               min_sample_proportion = NULL,
                               max_sample_proportion = NULL,
                               min_samples = NULL,
                               max_samples = NULL) {
      # At least one must be given.
      if (is.null(min_sample_proportion) &&
          is.null(max_sample_proportion) &&
          is.null(min_samples) &&
          is.null(max_samples)) {
        rlang::abort("No filtering was specified. Check your args!",
                     class = Error$ArgumentError)
      }

      # At least one of the proportions were given.
      if (!is.null(min_sample_proportion) || !is.null(max_sample_proportion)) {
        if (!is.null(min_samples)) {
          rlang::abort("Don't mix min_samples with proportion arguments!",
                       class = Error$ArgumentError)
        }

        if (!is.null(max_samples)) {
          rlang::abort("Don't mix max_samples with proportion arguments!",
                       class = Error$ArgumentError)
        }

        # At least one proportion arg was given, so make sure the one that wasn't given is the correct value.
        if (is.null(min_sample_proportion)) {
          min_sample_proportion <- 0
        }

        if (is.null(max_sample_proportion)) {
          max_sample_proportion <- 1
        }

        # Check that proportion args were good.
        if (min_sample_proportion < 0 || min_sample_proportion > 1) {
          rlang::abort(paste("min_sample_proportion must be >= 0 and <= 1. Got", min_sample_proportion),
                       class = Error$ArgumentError)
        }
        if (max_sample_proportion <= 0 || max_sample_proportion > 1) {
          rlang::abort(paste("max_sample_proportion must be > 0 and <= 1. Got", max_sample_proportion),
                       class = Error$ArgumentError)
        }
        if (min_sample_proportion > max_sample_proportion) {
          rlang::abort("min_sample_proportion must be <= max_sample_proportion.",
                       class = Error$ArgumentError)
        }

        # Set the min and max samples using the proportion arguments.
        min_samples <- min_sample_proportion * self$num_samples()
        max_samples <- max_sample_proportion * self$num_samples()
      }
      # At least one of the counts args were given.
      else if (!is.null(min_samples) || !is.null(max_samples)) {
        if (!is.null(min_sample_proportion)) {
          # Technically, this should never happen.
          rlang::abort("Don't mix min_sample_proportion with count arguments!",
                       class = Error$ArgumentError)
        }

        if (!is.null(max_sample_proportion)) {
          # Technically, this should never happen.
          rlang::abort("Don't mix max_sample_proportion with count arguments!",
                       class = Error$ArgumentError)
        }

        # At least one count arg was given, so make sure the one that wasn't given is the correct value.
        if (is.null(min_samples)) {
          min_samples <- 0
        }

        if (is.null(max_samples)) {
          max_samples <- self$num_samples()
        }

        # Check the count args.
        if (min_samples < 0 || min_samples > self$num_samples()) {
          rlang::abort(paste("min_samples must be >= 0 and <= num_samples. Got", min_samples),
                       class = Error$ArgumentError)
        }
        if (min_samples > 0 && min_samples < 1) {
          rlang::warn("min_samples looks like a proportion. Did you mean min_sample_proportion?")
        }
        if (max_samples < 1 || max_samples > self$num_samples()) {
          rlang::abort(paste("max_samples must be >= 1 and <= num_samples. Got", max_samples),
                       class = Error$ArgumentError)
        }
        if (min_samples > max_samples) {
          rlang::abort("min_samples must be <= max_samples",
                       class = Error$ArgumentError)
        }
      } else {
        rlang::abort("should be impossible", class = Error$ImpossibleConditionError)
      }

      # Finally filter!
      self$keep_features(function(feature) {
        samples_present <- sum(feature >= detection_limit)

        samples_present >= min_samples && samples_present <= max_samples
      })
    },

    #### Merging ####

    merge = function(margin, by, keep_na = FALSE) {
      if (margin == "features" || margin == 2) {
        by_expr <- rlang::enexpr(by)

        if (inherits(by_expr, "name")) {
          by <- as.character(by_expr)
        }

        if (all(by %in% colnames(self$feature_data))) {
          # TODO This is a bad variable name!
          categories <- self$feature_data[, by]

          if (any(is.na(categories)) && isTRUE(keep_na)) {
            # We have NAs and we want to keep them!
            #
            # Note: If we don't want to keep them, we don't have to do anything
            # as it will be taken care of later!

            # We will make a new category called "NA".  First check if any "NA" string already there.
            #
            # Note na.rm = TRUE because we want to check all the non-NA values to see if there
            # are any NA character/factor type things in the data.  Shouldn't be, but just a
            # sanity check.
            if (any(categories == "NA", na.rm = TRUE)) {
              stop("TODO test me implement me")
            }

            # Replace real NAs with the fake "NA" level.
            categories <- `levels<-`(addNA(categories), c(levels(categories), "NA"))
          }

          # TODO what if it doesn't have levels? ie is character vector?  assert that it has levels
          category_levels <- levels(categories)

          if (is.null(category_levels)) {
            stop("TODO category_levels was NULL")
          }

          new_feature_names <- paste(by, category_levels, sep = "_")

          merged <- sapply(category_levels, function(level) {
            keep_these <- categories == level
            keep_these <- ifelse(is.na(keep_these), FALSE, keep_these)

            if (sum(keep_these) == 0) {
              stop("TODO test me")
            } else if (sum(keep_these) == 1) {
              # Only a single feature in this category.
              self$data[, keep_these]
            } else {
              # Multiple features are present for this category.
              rowSums(self$data[, keep_these])
            }
          })

          dimnames(merged) <- list(Samples = self$sample_names(),
                                   Features = new_feature_names)

          new_feature_data <- data.frame(X = category_levels)
          colnames(new_feature_data) <- c(by)
          rownames(new_feature_data) <- new_feature_names

          FeatureTable$new(
            feature_table = merged,
            feature_data = new_feature_data,
            sample_data = self$sample_data
          )
        } else {
          rlang::abort("Not all data categories passed to the 'by' argument were present in feature_data!",
                       class = Error$ArgumentError)
        }
      } else {
        stop("TODO")
      }
    },

    #### little data utils #######################################################

    max = function(...) {
      max(self$data, ...)
    },

    min = function(...) {
      min(self$data, ...)
    },

    non_zero_min = function(...) {
      result <- min(self$data[self$data != 0], ...)

      if (!is.na(result) && result < 0) {
        print(result)
        rlang::warn("Non-zero min was negative. Do you really want this function?")
      }

      result
    }

  ),

  ################################################################################
  #### private methods ###########################################################
  ################################################################################

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
        if (!any(self$feature_names() %in% names(feature_data))) {
          rlang::abort("None of the feature names in feature_table were found in feature_data. Check your input!", class = Error$ArgumentError)
        }

        # Create a data frame from the 1d data.
        self$feature_data <- data.frame(X = feature_data[self$feature_names()],
                                        row.names = seq_len(self$num_features()))
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

        if (!any(self$feature_names() %in% rownames(feature_data))) {
          rlang::abort("None of the feature names in the feature_table were found in the feature_data. Check your input!",
                       class = Error$ArgumentError)
        }

        # Even if it is a data frame, it still may have 1 covariate, which needs special treatment.
        if (ncol(feature_data) == 1) {
          self$feature_data <- data.frame(X = feature_data[self$feature_names(), ])
          colnames(self$feature_data) <- colnames(feature_data)[[1]]
        } else if (ncol(feature_data) > 1) {
          self$feature_data <- feature_data[self$feature_names(), ]
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

        if (!any(self$sample_names() %in% names(sample_data))) {
          rlang::abort("None of the sample names in feature_table were found in sample_data.  Check your input!",
                       class = Error$ArgumentError)
        }

        self$sample_data <- data.frame(X = sample_data[self$sample_names()],
                                       # Need to set this manually.  If more samples in feature_table
                                       # than in sample_data, you will get some missing row names here.
                                       row.names = 1:self$num_samples())
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

        if (!any(self$sample_names() %in% rownames(sample_data))) {
          rlang::abort("None of the sample names in feature_table were found in sample_data.  Check your input!",
                       class = Error$ArgumentError)
        }

        if (ncol(sample_data) == 1) {
          self$sample_data <- data.frame(X = sample_data[self$sample_names(), ])
          colnames(self$sample_data) <- colnames(sample_data)[[1]]
        } else if (ncol(sample_data) > 1) {
          self$sample_data <- sample_data[self$sample_names(), ]
        } else {
          rlang::abort("it should be impossible to get here...",
                       class = Error$ImpossibleConditionError)
        }
      }

      # Ensure correct rownames are there in case there are more samples in feature_table than sample_data.
      rownames(self$sample_data) <- rownames(self$data)
    },

    map_wrapper = function(apply_fn, margin, fn, ...) {
      if (margin == 1) {
        result <- t(apply_fn(margin, fn, ...))
      } else if (margin == 2) {
        result <- apply_fn(margin, fn, ...)
      } else {
        rlang::abort(sprintf("margin should be 1 or 2.  Got %s.", margin),
                     class = Error$ArgumentError)
      }

      if (is.null(dim(result)) ||
          !isTRUE(all.equal(dim(result), dim(self$data)))) {
        rlang::abort(sprintf("Dimension of result is wrong.  Should be %s. Check your mapping function.",
                             paste(dim(self$data), collapse = ", ")),
                     class = Error$BadFunctionError)
      }

      FeatureTable$new(result, feature_data = self$feature_data, sample_data = self$sample_data)
    }
  )
)

# TODO should all data be converted either to df or matrix?
