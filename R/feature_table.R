#' FeatureTable
#'
#' @description
#' R6 Class for managing feature tables and associated metadata.
#'
#' @details
#' For an overview of the package, run \code{?featuretable} at the R console, or use this link: \link{featuretable}.
#'
#' Each of the methods listed below have extensive documention in their individual help files.  For example, to access detailed documentation for \code{FeatureTable$keep}, run this in the R console: \code{?keep}.  As such, the documentation on this page is intentionally kept light.
#'
#' @export
FeatureTable <- R6::R6Class(
  "FeatureTable",
  list(
    # Attributes

    #' @field data The data table with samples/observations as rows and features as columns.
    data = NULL,

    #' @field feature_data Rows are features, columns are stuff about the features.  Not necessarily hierarchical.
    feature_data = NULL,

    #' @field sample_data Rows are samples, columns are things about the samples.
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
    #' count_table <- matrix(
    #'   c(
    #'     0, 0, 0, 1, 10,
    #'     0, 0, 1, 2, 20,
    #'     0, 1, 2, 3, 30,
    #'     1, 2, 3, 4, 40
    #'   ),
    #'   byrow = TRUE,
    #'   nrow = 4,
    #'   ncol = 5,
    #'   dimnames = list(Samples = paste0("Sample_", 1:4),
    #'                   Features = paste0("Feature_", 1:5))
    #' )
    #'
    #' feature_data <- data.frame(
    #'   Color = c("red", "red", "red", "blue", "blue"),
    #'   Shape = c("square", "circle", "square", NA, "circle"),
    #'   Length = c(5, 6, 2.3, 7, 10),
    #'   row.names = paste0("Feature_", 1:5),
    #'   stringsAsFactors = TRUE
    #' )
    #'
    #' sample_data <- data.frame(
    #'   Location = c("Spain", "Spain", "Portugal", "Spain"),
    #'   Season = c("Summer", "Summer", "Winter", "Winter"),
    #'   SnazzyFactor = c(10, 12, 25, 3),
    #'   row.names = paste0("Sample_", 1:4),
    #'   stringsAsFactors = TRUE
    #' )
    #'
    #' FeatureTable$new(count_table,
    #'                  feature_data = feature_data,
    #'                  sample_data = sample_data)
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

    #' @description
    #' Print FeatureTable
    #'
    #' @details
    #' Prints \code{self} and returns it invisibly (via \code{invisible(self)}).
    #'
    #' @examples
    #' # `lee` is a dataset included in the FeatureTable package.
    #' featuretable::lee$print()
    #' # it would print out something like this:
    #' # FeatureTable:
    #' #   data         -- 16 samples, 1490 features
    #' #   feature_data -- 7 covariates
    #' #   sample_data  -- 4 covariates
    #'
    #' @return \code{invisible(self)}
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

    #' @description
    #' Return the dimensions of the FeatureTable \code{data} field.
    #'
    #' @return The dimension of the feature_table.
    #'
    #' @examples
    #' ft <- FeatureTable$new(matrix(1:12, 3, 4))
    #' stopifnot(ft$dim() == 12)
    #' stopifnot(ft$nrow() == 3)
    #' stopifnot(ft$ncol() == 4)
    dim = function() {
      dim(self$data)
    },

    #' @description See \code{FeatureTable$dim()}.
    nrow = function() {
      nrow(self$data)
    },

    #' @description See \code{FeatureTable$dim()}.
    ncol = function() {
      ncol(self$data)
    },

    #' @description
    #' Get the minimum, maximum, size, and other features of the FeatureTable \code{data} field.
    #'
    #' @details
    #' \code{non_zero_min} returns the smallest non-zero number in the \code{data} field.  If any of the values in the \code{data} field are less than zero, then a warning will be given.  In this case, you probably don't want to use \code{non_zero_min}.
    #'
    #' @return The min (\code{min}), non-zero min (\code{non_zero_min}), and max (\code{max}) values.  \code{size} returns the number of elements in the \code{data} field (i.e., \code{nrow * ncol} for the \code{data} field).
    #'
    #' @examples
    #' ft <- FeatureTable$new(matrix(0:8, 3, 3))
    #' stopifnot(ft$min() == 0)
    #' stopifnot(ft$non_zero_min() == 1)
    #' stopifnot(ft$max() == 8)
    #' stopifnot(ft$size() == 9)
    max = function(...) {
      max(self$data, ...)
    },

    #' @description See \code{FeatureTable$max()}.
    min = function(...) {
      min(self$data, ...)
    },

    #' @description See \code{FeatureTable$max()}.
    non_zero_min = function(...) {
      result <- min(self$data[self$data != 0], ...)

      if (!is.na(result) && result < 0) {
        rlang::warn("Non-zero min was negative. Do you really want this function?")
      }

      result
    },

    #' @description See \code{FeatureTable$max()}.
    size = function(...) {
      nrow(self$data) * ncol(self$data)
    },

    #' @description
    #' Return the number of samples/observations/rows in the FeatureTable.
    #'
    #' @return The number of samples/observations/rows in the FeatureTable.
    num_samples = function() {
      nrow(self$data)
    },

    #' @description Alias of \code{FeatureTable$num_samples()}.
    nsamples = function() {
      nrow(self$data)
    },

    #' @description Alias of \code{FeatureTable$num_samples()}.
    num_observations = function() {
      nrow(self$data)
    },

    #' @description Alias of \code{FeatureTable$num_samples()}.
    nobservations = function() {
      nrow(self$data)
    },

    #' @description
    #' Return the number of features/columns in the FeatureTable.
    #'
    #' @return The number of features/columns in the FeatureTable.
    num_features = function() {
      ncol(self$data)
    },

    #' @description Alias of \code{FeatureTable$num_features()}.
    nfeatures = function() {
      ncol(self$data)
    },

    #' @description
    #' Return the names of samples/observations/rows in the FeatureTable.
    #'
    #' @return The names of samples/observations/rows in the FeatureTable.
    sample_names = function() {
      rownames(self$data)
    },

    #' @description Alias of \code{FeatureTable$sample_names()}.
    observation_names = function() {
      rownames(self$data)
    },

    #' @description
    #' Return the names of samples/observations/rows in the FeatureTable.
    #'
    #' @return The names of samples/observations/rows in the FeatureTable.
    feature_names = function() {
      colnames(self$data)
    },

    ################################################################################
    #### apply #####################################################################
    ################################################################################

    #' @description
    #' Apply functions over FeatureTable data margins.
    #'
    #' @details
    #' This method uses \code{base::apply} to "apply" functions on to the
    #' \code{data} field of the \code{FeatureTable}.
    #'
    #' For details about \code{base::apply}, see \code{\link{apply}}.
    #'
    #' @examples
    #' # `ft` is a dataset included in FeatureTable.
    #' #
    #' # The apply method is a convenience wrapper around `base::apply`.
    #' stopifnot(
    #'   featuretable::ft$apply(2, sum) == apply(featuretable::ft$data, 2, sum)
    #' )
    #'
    #' @param margin A vector giving the subscripts which the function will be
    #'   applied over.  See \code{link{apply}} for details.
    #' @param fn The function to be applied.  See \code{link{apply}} for details.
    #' @param ... Optional arguments to \code{fn}.
    #'
    #' @return
    #' A vector or array or list of values obtained by applying a function to
    #' margins of an array or matrix.
    apply = function(margin, fn, ...) {
      base::apply(X = self$data, MARGIN = margin, FUN = fn, ...)
    },

    # `fn` takes data and index
    #' @description
    #' Apply functions over FeatureTable data margins.
    #'
    #' @details
    #' This method uses \code{base::apply} to "apply" functions on to the
    #' \code{data} field of the \code{FeatureTable}.  This method will also
    #' provide the current \code{index} to the applied function (\code{fn}).
    #'
    #' For details about \code{base::apply}, see \code{\link{apply}}.
    #'
    #' @examples
    #' ft <- featuretable::ft
    #'
    #' # `apply_with_index` takes a function of two or more arguments, the first is the data you're working on, the second is the index of that data (e.g., row or column number), and then any other arguments are additional.
    #' ft$apply_with_index(2, function(x, i) sum(x))
    #'
    #' # This could of course be written as
    #' ft$apply(2, sum)
    #'
    #' stopifnot(
    #'   ft$apply_with_index(2, function(x, i) sum(x)) ==
    #'     ft$apply(2, sum)
    #' )
    #'
    #' # But that's a boring example.  Say you had a vector of lengths, one for
    #' # each feature.
    #' lengths <- c(10, 15, 12, 25, 13)
    #'
    #' # Now you could weight the sum by the length of that feature.
    #' ft$apply_with_index(2, function(feature, index) {
    #'   sum(feature) / lengths[[index]]
    #' })
    #'
    #' # This data (`ft`) happens to have a `Length` column in the `feature_data`
    #' # field.  Let's use that instead.
    #' ft$apply_with_index(2, function(feature, index) {
    #'   sum(feature) / ft$featue_data$Length[[index]]
    #' })
    #'
    #' @param margin A vector giving the subscripts which the function will be
    #'   applied over.  See \code{link{apply}} for details.
    #' @param fn The function to be applied.  See \code{\link{apply}}. Note that
    #'   this one is a bit different from \code{base::apply}'s \code{FUN} in
    #'   that it should take \code{data} and an \code{index}.  See \code{Examples}
    #'   for details.
    #' @param ... Optional arguments to \code{fn}.
    #'
    #' @return
    #' A vector or array or list of values obtained by applying a function to
    #' margins of an array or matrix.
    apply_with_index = function(margin, fn, ...) {
      apply_with_index(self$data, margin, fn, ...)
    },

    #' @description
    #' Similar to \code{apply_with_index}, except that \code{fn} takes a
    #' (row or column) \code{name} rather than an \code{index}.
    #'
    #' See \code{FeatureTable$apply_with_index} for details.
    #'
    #' @examples
    #' ft <- featuretable::ft
    #'
    #' # Unlike `apply_with_index`, `fn` takes a name.  Its usage is similar.
    #' ft$apply_with_name(2, function(feature, name) {
    #'   sum(feature) / ft$feature_data[name, "Length"]
    #' })
    apply_with_name = function(margin, fn, ...) {
      apply_with_name(self$data, margin, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$apply(2, fn, ...)}.
    #'
    #' See \code{FeatureTable$apply}.
    apply_features = function(fn, ...) {
      base::apply(X = self$data, MARGIN = 2, FUN = fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$apply_with_index(2, fn, ...)}.
    #'
    #' See \code{FeatureTable$apply_with_index}.
    apply_features_with_index = function(fn, ...) {
      self$apply_with_index(margin = 2, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$apply_with_name(2, fn, ...)}.
    #'
    #' See \code{FeatureTable$apply_with_name}.
    apply_features_with_name = function(fn, ...) {
      self$apply_with_name(margin = 2, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$apply(1, fn, ...)}.
    #'
    #' See \code{FeatureTable$apply}.
    apply_samples = function(fn, ...) {
      base::apply(X = self$data, MARGIN = 1, FUN = fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$apply_with_index(1, fn, ...)}.
    #'
    #' See \code{FeatureTable$apply_with_index}.
    apply_samples_with_index = function(fn, ...) {
      self$apply_with_index(margin = 1, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$apply_with_name(1, fn, ...)}.
    #'
    #' See \code{FeatureTable$apply_with_name}.
    apply_samples_with_name = function(fn, ...) {
      self$apply_with_name(margin = 1, fn, ...)
    },

    ################################################################################
    #### map #######################################################################
    ################################################################################

    #' @description
    #' Returns a new FeatureTable by applying a function to margins of the
    #' \code{$data} field of a \code{FeatureTable}.
    #'
    #' @examples
    #' data(ft)
    #'
    #' ## Using R6 methods
    #'
    #' # Map over features
    #' ft$map("features", function(feature) feature / sum(feature))
    #'
    #' # Map over samples
    #' ft$map("samples", function(sample) sample / sum(sample))
    #'
    #' ## Using s3 methods
    #'
    #' # Map over features
    #' map(ft, "features", function(feature) feature / sum(feature))
    #'
    #' # Map over samples
    #' map(ft, "samples", function(sample) sample / sum(sample))
    #'
    #' #### map_with_index
    #'
    #' # Make up some fake lengths for the features.
    #' lengths <- 1:ft$num_features()
    #'
    #' # Map over features using the index to divide the feature by its length.
    #' ft$map_with_index("features", function(feature, index) feature / lengths[[index]])
    #'
    #' # And of course, the s3 function is also available!
    #' map_with_index(ft, "features", function(feature, index) feature / lengths[[index]])
    #'
    #' #### map_with_name
    #'
    #' # This FeatureTable includes a \code{Length} variable in the \code{feature_data}.
    #' #
    #' # You can access it like this:
    #' ft$feature_data$Length
    #'
    #' # Let's say you want to divide the count of each feature by its length.
    #' #
    #' # To get easy access to that data, you can use map_with_name like so:
    #' ft$map_with_name("features", function(feature, name) {
    #'   feature_length <- ft$feature_data[name, "Length"]
    #'
    #'   # Some features have an NA for length, so just set those to 1.
    #'   feature_length <- ifelse(is.na(feature_length), 1, feature_length)
    #'
    #'   feature / feature_length
    #' })
    #'
    #' # And of course, the s3 function is also available!
    #' map_with_name(ft, "features", function(feature, name) {
    #'   feature_length <- ft$feature_data[name, "Length"]
    #'
    #'   # Some features have an NA for length, so just set those to 1.
    #'   feature_length <- ifelse(is.na(feature_length), 1, feature_length)
    #'
    #'   feature / feature_length
    #' })
    #'
    #' @param ft A FeatureTable. (only needed in the \code{S3} version)
    #' @param margin Margin to apply the function over.  E.g., \code{1} or
    #'   \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates
    #'   columns.
    #' @param fn The function to be applied over the specified margin.  For the
    #'   \code{map_with_*} variants, the function should have at least 2 parameters,
    #'   the data to be applied over, and the \code{name} or \code{index} of the
    #'   current row or column.  See details.
    #' @param ... Optional arguments to \code{fn}.
    #'
    #' @return A new FeatureTable with the result of applying \code{fn} over
    #'   \code{margin}.
    map = function(margin, fn, ...) {
      private$map_wrapper(self$apply, margin, fn, ...)
    },

    #' @description
    #' Like \code{FeatureTable$map()} but also passes the \code{index} to
    #' \code{fn} similar to \code{FeatureTable$apply_with_index()}.
    #'
    #' See \code{FeatureTable$map()}.
    map_with_index = function(margin, fn, ...) {
      private$map_wrapper(self$apply_with_index, margin, fn, ...)
    },

    #' @description
    #' Like \code{FeatureTable$map()} but also passes the \code{name} to
    #' \code{fn} similar to \code{FeatureTable$apply_with_name()}.
    #'
    #' See \code{FeatureTable$map()}.
    map_with_name = function(margin, fn, ...) {
      private$map_wrapper(self$apply_with_name, margin, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$map(2, fn, ...)}.
    #'
    #' See \code{FeatureTable$map}.
    map_features = function(fn, ...) {
      self$map(2, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$map_with_index(2, fn, ...)}.
    #'
    #' See \code{FeatureTable$map}.
    map_features_with_index = function(fn, ...) {
      self$map_with_index(2, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$map_with_name(2, fn, ...)}.
    #'
    #' See \code{FeatureTable$map}.
    map_features_with_name = function(fn, ...) {
      self$map_with_name(2, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$map(1, fn, ...)}.
    #'
    #' See \code{FeatureTable$map}.
    map_samples = function(fn, ...) {
      self$map(1, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$map_with_index(1, fn, ...)}.
    #'
    #' See \code{FeatureTable$map}.
    map_samples_with_index = function(fn, ...) {
      self$map_with_index(1, fn, ...)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$map_with_name(1, fn, ...)}.
    #'
    #' See \code{FeatureTable$map}.
    map_samples_with_name = function(fn, ...) {
      self$map_with_name(1, fn, ...)
    },

    ################################################################################
    #### collapse ##################################################################
    ################################################################################

    #' @description
    #' Collapse samples/observations/rows or features/columns based on metadata. For
    #' features/samples, any feature/sample with the same metadata for selected
    #' category will be grouped.
    #'
    #' @details
    #' Grouping is done by summing counts for each category.
    #'
    #' If you to keep features/samples with \code{NA} for the \code{by} category,
    #' pass \code{keep_na = TRUE}.  Then the NA will become a new factor in the
    #' collapsed data.
    #'
    #' Currently, you can only collapse on one metadata column at a time.
    #'
    #' @examples
    #' data(ft)
    #'
    #' # You can direcctly access variables in the metadata.
    #' ft$collapse("features", Color)
    #' ft$collapse_features(Color)
    #'
    #' # Or refer to them by strings.
    #' ft$collapse("features", "Color")
    #' ft$collapse_features("Color")
    #'
    #' # You can collapse samples on metadata as well.
    #' ft$collapse("samples", Location)
    #' ft$collapse_samples(Location)
    #'
    #' # And you can use the s3 style functions.
    #' collapse(ft, "samples", Location)
    #' collapse_samples(ft, Location)
    #'
    #' collapse(ft, "features", Shape)
    #' collapse_features(ft, Shape)
    #'
    #' # For now, you can't do more than one variable at a time.  Sorry!
    #' \dontrun{
    #'   ft$collapse_samples(c("Location", "Season"))
    #' }
    #'
    #' @param ft A FeatureTable. (Only used in the \code{S3} version.)
    #' @param margin Margin to collapse.  E.g., \code{1} or \code{"samples"}
    #'   indicates rows, \code{2} or \code{"features"} indicates columns.
    #' @param by The data column to collapse by.
    #' @param keep_na Do you want to group all NAs together (TRUE) or drop them
    #' (FALSE, the defult)?
    #' @param keep_hierarchy Do you want to keep all data above the level specified
    #'   with the \code{by} argument? Pass \code{TRUE} to this parameter if you
    #'   know some of your data is hierarchical and you want to treat it as such.
    #'   See vignettes for details.
    #'
    #' @return A new FeatureTable with the specified margin collapsed on the
    #'   specified metadata column.
    collapse = function(margin, ...) {
      if (margin == "features" || margin == 2) {
        self$collapse_features(...)
      } else if (margin == "samples" || margin == 1) {
        self$collapse_samples(...)
      } else {
        rlang::abort(
          paste("'margin' must be 'samples' (or 1), or 'features' (or 2).  Got", margin),
          class = Error$ArgumentError
        )
      }
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$collapse("features", fn, ...)}.
    #'
    #' See \code{FeatureTable$collapse}.
    collapse_features = function(by, keep_na = FALSE, keep_hierarchy = FALSE) {
      # TODO better error message for when you pass multiple `by` columns.
      # TODO `keep_hiearchical_columns` may be a better param name.

      by_expr <- rlang::enexpr(by)

      if (rlang::is_null(by_expr)) {
        rlang::abort("'by' was NULL!", class = Error$ArgumentError)
      }

      if (rlang::is_na(by_expr)) {
        rlang::abort("'by' was NULL!", class = Error$ArgumentError)
      }

      if (inherits(by_expr, "name")) {
        by <- as.character(by_expr)
      }

      if (all(by %in% colnames(self$feature_data))) {
        # TODO This is a bad variable name!
        categories <- self$feature_data[, by]

        # Calling unique like this will keep any NA in the data.
        #
        # Also, this relies on `unique` NOT changing the order...this may be a problem in the future.
        unique_categories <- unique(categories)

        # If the user has set the levels themselves, then they are going to care about preserving the order, even if some have dropped out due to things like running `keep` first.
        original_levels <- levels(categories)

        if (is.null(original_levels)) {
          new_levels <- sort(unique_categories)
        } else {
          new_levels <- original_levels[original_levels %in% unique_categories]

          # If the new_levels is NULL or 0 length, then the user probably changed something manually and broke the levels.  Probably not what they really wanted to do or else they would have also changed the levels.
          #
          # On the other hand, if the new_levels is NOT but also fewer levels than original, then the user probably ran a `keep` function first.

          if (is.null(new_levels) || length(new_levels) == 0) {
            rlang::abort("New levels will be NULL or empty.  Did you convert a column of the feature_data from a factor into a character?  Did you manually change one of the factor variables (e.g., trying to change add a new level or remove one)?", class = Error$BadFactorDataError)
          }
        }

        categories <- factor(categories, levels = new_levels)

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

          # Replace real NAs with the fake "NA" level that behaves the why we want.
          categories <- `levels<-`(addNA(categories), c(levels(categories), "NA"))
        }

        category_levels <- levels(categories)

        # Does this still get hit now with the new_levels check? Pretty sure this can't happen anymore.
        if (is.null(category_levels)) {
          # TODO better error message would mention the `by` argument.
          rlang::abort(
            "category_levels was NULL.  Did you convert a column of the feature_data from a factor into a character, or manually adjust one of the factor variables?",
            class = Error$NonFactorDataError
          )
        }

        collapsed <- sapply(category_levels, function(level) {
          keep_these <- categories == level
          keep_these <- ifelse(is.na(keep_these), FALSE, keep_these)

          if (sum(keep_these) == 0) { # Pretty sure this can't happen anymore.
            # You can get here if a user manually changes a column in the sample
            # data and that change eliminates all occurences of a factor level from
            # the data frame.
            rlang::abort(
              "One of the factor levels has dropped out of the 'by' data col(s).  Did you manually change the data in sample_data?",
              class = Error$MissingFactorLevelError
            )
          } else if (sum(keep_these) == 1) {
            # Only a single feature in this category.
            self$data[, keep_these]
          } else {
            # Multiple features are present for this category.
            rowSums(self$data[, keep_these])
          }
        })

        dimnames(collapsed) <- list(Samples = self$sample_names(),
                                    Features = category_levels)

        if (isTRUE(keep_hierarchy)) {
          # TODO this will break if you let `by` be more than one column.
          keep_these <- hierarchical_columns(self$feature_data, by)

          # All keep_these columns that are not `by`, will be equal to `by` (even if they are NA)

          # Assuming `by` is a single thing.
          #
          # For now, throw out the NA category. We will add it back in later.
          new_feature_data <- unique(na.exclude(self$feature_data[, keep_these]))
          # ^ this will have one row with proper columns for each unique by category

          if (sum(keep_these) == 0) {
            stop("should be impossible")
          } else if (sum(keep_these) == 1) {

            # Make sure it's actually a data frame.
            new_feature_data <- as.data.frame(new_feature_data, stringsAsFactors = TRUE)
            # Again, assuming `by` is a single thing.
            stopifnot(length(by) == 1) # TODO will this mess up the enexpr?
            colnames(new_feature_data) <- by
          }

          # Now set the proper levels for the by category. And order/sort it by the levels.
          new_feature_data[, by] <- sort(factor(new_feature_data[, by], levels = category_levels))

          # And the rownames should be whatever the unique things in the by category are
          rownames(new_feature_data) <- new_feature_data[, by]

          # If keep_na is true but there are actually no NA values, we don't want to add them.
          if (isTRUE(keep_na) && any(is.na(self$feature_data[, keep_these]))) {
            # Add on a row with all "NA" strings and
            tmp <- rep("NA", times = ncol(new_feature_data))
            # Convert it to a 1 X ncol df
            tmp <- as.data.frame(as.list(tmp), row.names = "NA", stringsAsFactors = TRUE)
            colnames(tmp) <- colnames(new_feature_data)
            new_feature_data <- rbind(new_feature_data, tmp)
          }
        } else {
          # Set X as a factor this way to preserve the original levels.
          new_feature_data <- data.frame(X = factor(category_levels, levels = category_levels),
                                         stringsAsFactors = TRUE)
          colnames(new_feature_data) <- c(by)
          rownames(new_feature_data) <- category_levels
        }

        FeatureTable$new(
          feature_table = collapsed,
          feature_data = new_feature_data,
          sample_data = self$sample_data
        )
      } else {
        rlang::abort("Not all data categories passed to the 'by' argument were present in feature_data!",
                     class = Error$ArgumentError)
      }
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$collapse("samples", fn, ...)}.
    #'
    #' See \code{FeatureTable$collapse}.
    collapse_samples = function(by, keep_na = FALSE, keep_hierarchy = FALSE) {
      by_expr <- rlang::enexpr(by)

      if (rlang::is_null(by_expr)) {
        rlang::abort("'by' was NULL!", class = Error$ArgumentError)
      }

      if (rlang::is_na(by_expr)) {
        rlang::abort("'by' was NULL!", class = Error$ArgumentError)
      }

      if (inherits(by_expr, "name")) {
        by <- as.character(by_expr)
      }

      if (all(by %in% colnames(self$sample_data))) {
        # TODO This is a bad variable name!
        categories <- self$sample_data[, by]

        # TODO pull out this code into a function...it's also used in collapse_features.

        # Calling unique like this will keep any NA in the data.
        #
        # Also, this relies on `unique` NOT changing the order...this may be a problem in the future.
        unique_categories <- unique(categories)

        # If the user has set the levels themselves, then they are going to care about preserving the order, even if some have dropped out due to things like running `keep` first.
        original_levels <- levels(categories)

        if (is.null(original_levels)) {
          new_levels <- sort(unique_categories)
        } else {
          new_levels <- original_levels[original_levels %in% unique_categories]

          # If the new_levels is NULL or 0 length, then the user probably changed something manually and broke the levels.  Probably not what they really wanted to do or else they would have also changed the levels.
          #
          # On the other hand, if the new_levels is NOT but also fewer levels than original, then the user probably ran a `keep` function first.

          if (is.null(new_levels) || length(new_levels) == 0) {
            rlang::abort("New levels will be NULL or empty.  Did you convert a column of the sample_data from a factor into a character?  Did you manually change one of the factor variables (e.g., trying to change add a new level or remove one)?", class = Error$BadFactorDataError)
          }
        }

        categories <- factor(categories, levels = new_levels)

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

        category_levels <- levels(categories)

        # Does this still get hit now with the new_levels check? Pretty sure this can't happen anymore.
        if (is.null(category_levels)) {
          # TODO better error message would mention the `by` argument.
          rlang::abort(
            "category_levels was NULL.  Did you convert a column of the sample_data from a factor into a character, or manually adjust one of the factor variables?",
            class = Error$NonFactorDataError
          )
        }

        new_names <- category_levels

        # Note that this one needs the transpose!
        collapsed <- t(sapply(category_levels, function(level) {
          keep_these <- categories == level
          keep_these <- ifelse(is.na(keep_these), FALSE, keep_these)

          if (sum(keep_these) == 0) { # Pretty sure this can't happen anymore.
            # You can get here if a user manually changes a column in the sample
            # data and that change eliminates all occurences of a factor level from
            # the data frame.
            rlang::abort(
              "One of the factor levels has dropped out of the 'by' data col(s).  Did you manually change the data in sample_data?",
              class = Error$MissingFactorLevelError
            )
          } else if (sum(keep_these) == 1) {
            # Only a single feature in this category.
            self$data[keep_these, ]
          } else {
            # Multiple features are present for this category.
            colSums(self$data[keep_these, ])
          }
        }))

        dimnames(collapsed) <- list(Samples = new_names,
                                    Features = self$feature_names())

        if (isTRUE(keep_hierarchy)) {
          # TODO this will break if you let `by` be more than one column.
          keep_these <- hierarchical_columns(self$sample_data, by)

          # All keep_these columns that are not `by`, will be equal to `by` (even if they are NA)

          # Assuming `by` is a single thing.
          #
          # For now, throw out the NA category. We will add it back in later.
          new_sample_data <- unique(na.exclude(self$sample_data[, keep_these]))
          # ^ this will have one row with proper columns for each unique by category

          if (sum(keep_these) == 0) {
            stop("should be impossible")
          } else if (sum(keep_these) == 1) {

            # Make sure it's actually a data frame.
            new_sample_data <- as.data.frame(new_sample_data, stringsAsFactors = TRUE)
            # Again, assuming `by` is a single thing.
            stopifnot(length(by) == 1) # TODO will this mess up the enexpr?
            colnames(new_sample_data) <- by
          }

          # Now set the proper levels for the by category. And order/sort it by the levels.
          new_sample_data[, by] <- sort(factor(new_sample_data[, by], levels = category_levels))

          # And the rownames should be whatever the unique things in the by category are
          rownames(new_sample_data) <- new_sample_data[, by]

          # If keep_na is true but there are actually no NA values, we don't want to add them.
          if (isTRUE(keep_na) && any(is.na(self$sample_data[, keep_these]))) {
            # Add on a row with all "NA" strings and
            tmp <- rep("NA", times = ncol(new_sample_data))
            # Convert it to a 1 X ncol df
            tmp <- as.data.frame(as.list(tmp), row.names = "NA", stringsAsFactors = TRUE)
            colnames(tmp) <- colnames(new_sample_data)
            new_sample_data <- rbind(new_sample_data, tmp)
          }
        } else {
          # TODO i think there is technically a bug here....levels will have "NA" (as string) if keep_na is true, but the sample_data itself will have an actual NA value for those places.
          new_sample_data <- data.frame(X = factor(category_levels, levels = category_levels),
                                        stringsAsFactors = TRUE)
          colnames(new_sample_data) <- c(by)
          rownames(new_sample_data) <- new_names
        }

        FeatureTable$new(
          feature_table = collapsed,
          feature_data = self$feature_data,
          sample_data = new_sample_data
        )
      } else {
        rlang::abort("Not all data categories passed to the 'by' argument were present in sample_data!",
                     class = Error$ArgumentError)
      }
    },


    ################################################################################
    #### keep ######################################################################
    ################################################################################

    # TODO what if the predicate attempts to get a column not in the feature/sample data?

    #' @description
    #' Keep samples/observations/rows or features/columns based on result of a
    #' predicate function or expression.
    #'
    #' @details
    #' A \code{predicate} can be a function that evaluates to \code{TRUE}, or
    #' \code{FALSE} for each item it is applied to, or it can be a logical vector
    #' the same length as the \code{margin} you're working with, or it can be some
    #' other expresssion/call that will evaluate to a logical vector.  See Examples
    #' for more information.
    #'
    #' Note that `query` is currently only available for filtering features.  See
    #' Examples for details.
    #'
    #' @examples
    #' data(ft)
    #'
    #' #### Filtering based on metadata ####
    #'
    #' # First, let me show you how to filter based on metadata (i.e., the stuff in
    #' # `$feature_data` or `$sample_data`).  Note that this kind of filtering works
    #' # the same for samples.
    #'
    #' # Here is what the `feature_data` looks like.
    #' #
    #' # > ft$feature_data
    #' #           Color  Shape Length
    #' # Feature_1   red square    5.0
    #' # Feature_2   red circle    6.0
    #' # Feature_3   red square    2.3
    #' # Feature_4  blue   <NA>    7.0
    #' # Feature_5  blue circle   10.0
    #'
    #' # To keep features, you can use either `keep` and specify the correct margin,
    #' # or use the `keep_features` helper function.  For the examples that follow,
    #' # I will show you both ways, but of course, you can pick whichever one you
    #' # prefer!
    #'
    #' # Also note that whenever you see something with the R6 calling convention:
    #' # E.g., ft$keep("features", ...), feel free to use the S3 calling convention
    #' # instead: keep(ft, "features", ...).
    #'
    #' # Keep features that have 'circle' in the `feature_data`.  Note that you can
    #' # refer to variables/colnames in the data directly inside the `keep`
    #' # functions!
    #' #
    #' ft$keep("features", Shape == "circle")
    #' ft$keep_features(Shape != "circle")
    #'
    #' # You can use a logical vector directly.
    #' #
    #' ft$keep("features", c(TRUE, TRUE, FALSE, TRUE, TRUE))
    #' ft$keep_features(c(TRUE, TRUE, FALSE, TRUE, TRUE))
    #'
    #' # Or you can use any expressions that will evaluate to a logical vector.
    #' #
    #' ft$keep("features", ft$feature_data$Shape == "circle")
    #' ft$keep_features(ft$feature_data$Shape == "circle")
    #'
    #' # Keep features that do NOT have 'circle' in the `feature_data`.
    #' #
    #' ft$keep("features", Shape != "circle")
    #' ft$keep_features(Shape != "circle")
    #'
    #' # You can also have more complicated expressions for predicates.
    #' #
    #' # Keep the red circles
    #' ft$keep("features", Shape == "circle" & Color == "red")
    #' ft$keep_features(Shape == "circle" & Color == "red")
    #'
    #' # Keep any features that are 'red' OR 'circles'.
    #' ft$keep("features", Shape == "circle" | Color == "red")
    #' ft$keep_features(Shape == "circle" | Color == "red")
    #'
    #' # Of course, you can keep adding on expressions....
    #' ft$keep("features", (Shape == "square" & Color == "red") | Length > 5)
    #' ft$keep_features((Shape == "square" & Color == "red") | Length > 5)
    #'
    #' #### Filtering based on actual data (i.e., in `$data`) ####
    #'
    #' # Sometimes you want to filter features or samples based on the actual data
    #' # (i.e., the stuff in `$data`), rather than on the metadata (i.e., the stuff
    #' # in `$feature_data` or `$sample_data`).  No problem!  For that, we will use
    #' # functions that apply over the margin we want.
    #'
    #' # For reference, here is the data:
    #' #
    #' # > ft$data
    #' #           Features
    #' # Samples    Feature_1 Feature_2 Feature_3 Feature_4 Feature_5
    #' #   Sample_1         0         0         0         1        10
    #' #   Sample_2         0         0         1         2        20
    #' #   Sample_3         0         1         2         3        30
    #' #   Sample_4         1         2         3         4        40
    #'
    #' # Keep features whose sum is more than 5.
    #' ft$keep("features", function(feature) sum(feature) > 5)
    #' ft$keep_features(function(feature) sum(feature) > 5)
    #'
    #' # Keep samples whose sum is more than 25.
    #' ft$keep("samples", function(sample) sum(sample) > 5)
    #' ft$keep_samples(function(sample) sum(sample) > 5)
    #'
    #' # Note that there is nothing special about using `sample` or `feature` as a
    #' # parameter to those anonymous functions.  I could have used `x` or anything
    #' # else.
    #'
    #' #### Predicates that work on data and metadata together! ####
    #'
    #' # What if you want to filter based on both metadata and the actual data?
    #' # For that, you need to use `query`.  It is a special function only
    #' # accessible from inside the keep functions. (Note that currently, you can
    #' # only use `query` from `keep` with margin 'features', or keep_features.)
    #'
    #' # Keep features that are circles and also have an abundance of > 5.
    #' ft$keep("features",
    #'         query(Shape == "circle") & query(function(feature) sum(feature) > 5))
    #' ft$keep_features(
    #'   query(Shape == "circle") & query(function(feature) sum(feature) > 5)
    #' )
    #'
    #' # Notice how I wrapped both expressions in a `query` function.  You can also
    #' # have more than two queries just like before.
    #'
    #' # One slightly weird thing is that the expression that evalautes w.r.t. the
    #' # metadata doesn't actually need to be wrapped in a query.  For example, this
    #' # will also work:
    #' #
    #' ft$keep_features(
    #'   Shape == "circle" & query(function(feature) sum(feature) > 5)
    #' )
    #'
    #' # But I would consider that a quirk of the implementation and not rely on it,
    #' # as it may change in the future.
    #'
    #' #### Filtering on actual data, feature data, and sample data together ####
    #'
    #' # For reference, here is the sample data:
    #' # > ft$sample_data
    #' #           Location Season SnazzyFactor
    #' #  Sample_1    Spain Summer           10
    #' #  Sample_2    Spain Summer           12
    #' #  Sample_3 Portugal Winter           25
    #' #  Sample_4    Spain Winter            3
    #'
    #' # (Note that like the above section, this only works for features currently.)
    #' #
    #' # Here's something cool.  Let's say you wanted to filter features based on
    #' # their abundance, some bit of metadata about them (e.g., taxonomy) but
    #' # restrict your search to a subset of samples (say, all samples from Summer,
    #' # or all samples from Spain).  You can do that with the `query` function as
    #' # well!  Let me show you what I mean....
    #'
    #' # Keep features whose abundance is > 5 in 'Winter' samples (i.e., you don't
    #' # care what there abundance in other seasons, as long as they fit the
    #' # criteria for the 'Winter' samples.)
    #'
    #' ft$keep("features", query(function(feature) sum(feature) > 5,
    #'                           restrict = Season == "Winter"))
    #'
    #' # Notice the use of the `restrict` parameter.  It takes expressions that
    #' # evaluate in the context of the sample data.
    #'
    #' # And you can make more complicated queries if you want.
    #' ft$keep_features(query(Shape == "circle") |
    #'                    query(function(feature) sum(feature) > 5,
    #'                          restrict = Season == "Winter"))
    #'
    #' # Here is something that may trip you up.
    #'
    #' \dontrun{
    #'   ft$keep_features(query(Shape == "circle", restrict = Season == "Winter"))
    #' }
    #'
    #' # The above code will raise an `ArgumentError`.  Here's the thing: feature
    #' # metadata does not change based on the sample metadata.  In other words, a
    #' # feature is a 'circle' or a 'square' regardless of whether it is in a
    #' # 'Summer' sample, a 'Winter' sample, or both.  So restricting a metadata
    #' # query to a subset of samples just doesn't make any sense.
    #' #
    #' # If you find yourself wanting to do that, my guess is what you really want
    #' # are to filter features whose 'Shape' is 'circle' and that are actually
    #' # present in 'Winter' samples.  If that is the case, you want to do something
    #' # like this:
    #' #
    #' ft$keep_features(
    #'   query(Shape == "circle") &
    #'     query(function(feature) sum(feature) > 0,
    #'           restrict = Season == "Winter")
    #' )
    #'
    #' @param ft A FeatureTable. (only used in the \code{S3} version)
    #' @param margin Margin to apply the predicate over.  E.g., \code{1} or
    #'   \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates
    #'   columns.
    #' @param predicate The predicate function or expression to be applied.  Only
    #'   those elements where \code{predicate} is \code{TRUE} or evaluates to
    #'   \code{TRUE} will be kept.
    #' @param ... Optional arguments to \code{predicate} if \code{predicate} is a
    #'   function.  If it is something like \code{Location == "Spain"}, then
    #'   optional arguments will be ignored.
    #'
    #' @return A new FeatureTable with the elements that were kept.
    keep = function(margin, ...) {
      if (margin == "features" || margin == 2) {
        self$keep_features(...)
      } else if (margin == "samples" || margin == 1) {
        self$keep_samples(...)
      } else {
        rlang::abort(
          paste("'margin' must be 'samples' (or 1), or 'features' (or 2).  Got", margin),
          class = Error$ArgumentError
        )
      }
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$keep("features", fn, ...)}.
    #'
    #' See \code{FeatureTable$keep}.
    keep_features = function(predicate, ...) {
      # Helpers.

      keep_helpers <- rlang::env(
        query = function(qry, restrict = NULL) {
          restrict_given <- !rlang::quo_is_null(rlang::enquo(restrict))

          if (restrict_given) {
            # Any errors that this would call will just bubble up.
            #
            # TODO may be good to catch them here to give a better error msg?
            dat <- self$keep_samples(!!rlang::enquo(restrict))$data
          } else {
            dat <- self$data
          }

          qry_eval <- rlang::eval_tidy(rlang::enquo(qry), self$feature_data)

          if (rlang::is_function(qry_eval)) {
            result <- apply(dat, 2, qry_eval)
          } else {
            if (restrict_given) {
              rlang::abort("'restrict' was passed with a query that doesn't apply on the actual data.  I'm guessing you want to restrict features to those matching your query AND that are present in certain samples.  If that is the case, you should do something like ft$keep_features(query(Shape == 'circle') & query(function(feature) sum(feature) > 0, restrict = Season == 'Winter'))",
                           class = Error$ArgumentError)
            }
            result <- qry_eval
          }

          if (!rlang::is_logical(result)) {
            stop("TODO the thingy didn't give a logical result")
          }

          if (length(result) != self$num_features()) {
            stop("TODO not the right length")
          }

          ifelse(is.na(result), FALSE, result)
        },

        quo_is_query = function(quo) {
          if (!rlang::is_quosure(quo)) {
            stop("TODO NEEDED A QUOSURE")
          }

          rlang::quo_is_call(quo) && rlang::quo_get_expr(quo)[[1]] == "query"
        },

        expr_has_query_function = function(ex) {
          if (!rlang::is_expression(ex)) {
            stop("TODO NEEDED AN EXPRESSION")
          }

          # TODO what happens if user has 'query' in their calling env?
          any(
            unlist(
              lapply(ex, function(x) {
                grepl("query", x, fixed = TRUE)
              })
            )
          )
        }
      )

      # First off, did the user pass a query?  If so, we deal with that and recurse.
      # if (keep_helpers$quo_is_query(rlang::enquo(predicate))) {
      if (keep_helpers$expr_has_query_function(rlang::enexpr(predicate))) {
        # First, we need to check if the calling environment also has a query function.
        #
        # Note that this is the user's calling environment, so we can't just remove 'query' from it.
        pred_env <- rlang::quo_get_env(rlang::enquo(predicate))


        if (exists("query", pred_env)) {
          # TODO either we could drop the users from a list and rebuild, or try the pronoun?
          #stop("TODO bad things.  'query' is defined in the calling environment. You need to remove it from calling environment.")

          rlang::warn("A function named 'query' was found in the calling environment. Your 'query' function will be shadowed by an internal 'query' function inside the 'keep' functions! If this is NOT the behavior you want, please open an issue on the GitHub page.")

          pred_env_list <- as.list(pred_env)
          pred_env_list$query <- NULL
        } else {
          pred_env_list <- as.list(pred_env)
        }

        silly_env <- rlang::new_environment(
          c(
            pred_env_list,
            # TODO technically we need to check if anything in the pred helpers is defined in calling env
            as.list(keep_helpers),
            # You also need to include feature data so users can mix queries and raw exprs.
            as.list(self$feature_data)
          ),
          parent = rlang::caller_env()
        )

        evaluated_predicate <- rlang::eval_tidy(
          rlang::enexpr(predicate),
          env = silly_env
        )

        return(self$keep_features(evaluated_predicate))
      }

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
        # Note that this can be NULL if there are no colnames.
        feature_name <- colnames(self$data)[predicate_result]
        stopifnot(length(feature_name) == 1 || is.null(feature_name))

        result <- self$data[, predicate_result]

        # If the input data does NOT have dimnames, and we keep only a single result,
        # we don't want to add new dimnames.
        #
        # TODO need some test for this in the keep samples section, I'm pretty sure.
        if (is.null(dimnames(self$data))) {
          new_dimnames <- NULL
        } else {
          new_dimnames <- list(Samples = rownames(self$data),
                               Features = feature_name)
        }

        result <- matrix(result, nrow = self$num_samples(), ncol = 1,
                         dimnames = new_dimnames)
      } else {
        result <- self$data[, predicate_result]
      }

      FeatureTable$new(result, self$feature_data, self$sample_data)
    },

    #' @description
    #' Convenience wrapper for \code{FeatureTable$keep("samples", fn, ...)}.
    #'
    #' See \code{FeatureTable$keep}.
    keep_samples = function(predicate, ...) {
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

        # TODO see the section on features for how to deal with NOT adding dimnames
        # for data that didn't already have them.
        result <- matrix(result, nrow = 1, ncol = self$num_features(),
                         dimnames = list(Samples = sample_name,
                                         Features = colnames(self$data)))
      } else {
        result <- self$data[predicate_result, ]
      }

      FeatureTable$new(result, self$feature_data, self$sample_data)
    },

    #' @description
    #' Merge FeatureTables by keeping only features that are shared between them
    #' (base on feature names).
    #'
    #' @details
    #' If you just have a list of feature names, you don't need these, you'd use
    #' \code{\link{keep}} directly.
    #'
    #' Consider this API pretty unstable.  I haven't decided the on how I want these
    #' functions to behave in the end.
    #'
    #' @param ft A FeatureTable
    #' @param method One of "keep" or "names".
    #' @param other Another FeatureTable
    #'
    #' @return
    #' If \code{method = "keep"}, return a new FeatureTable containing only features
    #' shared between \code{ft} (or \code{self} for the R6 method) and \code{other}.
    #'
    #' If \code{method = "names"}, return the shared feature names between \code{ft}
    #' (or \code{self} for the R6 method) and \code{other}.
    shared_features = function(method, other) {
      if (method == "keep") {
        self$keep_shared_features(other)
      } else if (method == "names") {
        self$shared_feature_names(other)
      } else {
        rlang::abort(
          "method must be either 'keep' or 'names'",
          class = Error$ArgumentError
        )
      }
    },

    #' @description
    #' Alias for \code{FeatureTable$shared_features("keep", ...)}.
    #'
    #' See \code{FeatureTable$shared_features}.
    keep_shared_features = function(other) {
      shared_names <- self$shared_feature_names(other)

      if (length(shared_names) == 0) {
        rlang::abort("There were no shared features!",
                     class = Error$ArgumentError)
      } else {
        self$keep_features(self$feature_names() %in% shared_names)
      }
    },

    #' @description
    #' Alias for \code{FeatureTable$shared_features("names", ...)}.
    #'
    #' See \code{FeatureTable$shared_features}.
    shared_feature_names = function(other) {
      if (!inherits(other, "FeatureTable")) {
        rlang::abort(
          "'other' is not a FeatureTable!",
          class = Error$ArgumentError
        )
      }

      if (is.null(self$feature_names())) {
        rlang::abort(
          "FeatureTable (self) has no feature names!",
          class = Error$MissingFeatureNamesError
        )
      }
      if (is.null(other$feature_names())) {
        rlang::abort(
          "FeatureTable (other) has no feature names!",
          class = Error$MissingFeatureNamesError
        )
      }

      base::intersect(self$feature_names(), other$feature_names())
    },

    ################################################################################
    #### core microbiome ###########################################################
    ################################################################################

    #' @description Core Microbiome
    #'
    #' @details
    #' If both \code{min_sample_proportion} and \code{min_samples} are given, an
    #'   error will be raised. If \code{min_sample_proportion} is not a proportion,
    #'   an error will be raised.  If \code{min_samples} looks like a proportion, a
    #'    warning will be given.
    #'
    #' @param ft A FeatureTable
    #' @param detection_limit Limit of detection for a feature to be considered
    #'   present in a sample
    #' @param min_sample_proportion Minimum proportion of samples in which a feature
    #'    must be present to be kept.
    #' @param max_sample_proportion Maximum proportion of samples in which a feature
    #'    must be present to be kept.
    #' @param min_samples Minimum number of samples in which a feature must be
    #'   present to be kept.
    #' @param max_samples Maximum number of samples in which a feature must be
    #'   present to be kept.
    #'
    #' @return A new FeatureTable with only the specified "core" features.
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

    ################################################################################
    #### CoDA ######################################################################
    ################################################################################

    #' @description Is the FeatureTable a count table?
    #'
    #' @return TRUE if \code{feature_table} contains only natural numbers (counting numbers >= 0), FALSE otherwise.
    is_count_table = function() {
      all(is_natural_number(self$data))
    },

    #' @description Replacing zeros.
    #'
    #' @details
    #' If \code{use_cmultRepl = TRUE} but \code{zCompositions::cmultRepl} is not
    #' availabel, it will raise an error.
    #'
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

    #' @description Centered log ratio.
    #'
    #' @details
    #' See
    #' \url{https://en.wikipedia.org/wiki/Compositional_data#Center_logratio_transform}.
    #'
    #' @param base Base of logarithm.
    #'
    #' @return
    #' A FeatureTable with the \code{data} transformed with centered log ratio.
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

    ################################################################################
    #### plotting ##################################################################
    ################################################################################

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

          plot_data <- base::merge(self$data, self$sample_data, by = "row.names")

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

    # TODO add ... to end of samples and feature names if they're too long
    # TODO proportions? options for grouping? add docs about how you do that before calling plot
    # TODO doc stuff like this: lee$collapse_features(Family)$collapse_samples(char)$map_samples(function(sample) sample / sum(sample) * 100) %>% plot
    plot = function(num_features = 8,
                    other_feature_name = "Other",
                    fill = TRUE,
                    # if NULL use kelly
                    palette = "kelly",
                    show_legend = TRUE,
                    legend_title = NULL,
                    plot_title = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    axis.text.x = NULL,
                    ...) {
      # args <- list(...)

      prepare_palette <- function(palette_with_gray) {
        # This palette should have one "extra" gray at the end.
        end <- length(palette_with_gray) - 1
        palette_with_gray[1:end]

        list(colors = palette_with_gray[1:end],
             gray = palette_with_gray[[end + 1]])
      }

      if (is.null(num_features) ||
          isFALSE(num_features) ||
          (rlang::is_scalar_integerish(num_features) &&
           num_features > self$num_features())) {
        num_features_to_show <- self$num_features()
      } else if (!rlang::is_scalar_integerish(num_features) ||
                 (rlang::is_scalar_integerish(num_features) &&
                  num_features < 1)) {
        rlang::abort(
          "num_features must be NULL, FALSE, or an integer greater than 1",
          class = Error$ArgumentError
        )
      } else {
        num_features_to_show <- num_features
      }

      if (num_features_to_show + 1 == self$num_features()) {
        rlang::inform(
          "num_features was one less than total number of features. There will be an 'Other' category, but it will only contain 1 feature!"
        )
      }

      # num_features_to_show is now guaranteed to be in [1, self$num_features()]

      # These all have gray at the end.
      if (palette == "high contrast") {
        palette <- prepare_palette(ft_palette$pthc)
      } else if (palette == "muted") {
        palette <- prepare_palette(ft_palette$ptm)
      } else if (palette == "bright") {
        palette <- prepare_palette(ft_palette$ptb)
      } else if (palette == "vibrant") {
        palette <- prepare_palette(ft_palette$ptv)
      } else { # Kelly
        palette <- prepare_palette(ft_palette$kelly)
      }

      # Recycle palette if necessary
      if (num_features_to_show > length(palette$colors)) {
        rlang::inform(
          "You want to plot more features than your palette can handle.  I will recycle the palette to handle this."
        )

        palette$colors <- rep(
          palette$colors,
          times = ceiling(num_features_to_show / length(palette$colors))
        )[1:num_features_to_show]
      }

      # TODO technically don't need this step if fill is false.
      plot_data <- as.data.frame(self$data[, order(colSums(self$data), decreasing = TRUE)],
                                 stringsAsFactors = TRUE)

      # Need to make an other category.
      if (self$num_features() > num_features_to_show) {
        if (!is.character(other_feature_name)) {
          other_feature_name <- "Other"
        }

        plot_data_other <- plot_data[, (num_features_to_show + 1):ncol(plot_data)]

        if (num_features_to_show == 1) {
          # We only want a single feature (plus other), so we need to force
          # the plot_data to be a data.frame.
          new_colname <- colnames(plot_data[[1]])
          plot_data <- data.frame(X = plot_data[, 1], stringsAsFactors = TRUE)
          colnames(plot_data) <- new_colname
        } else {
          plot_data <- plot_data[, 1:num_features_to_show]
        }

        # Check if it is in any of the features names, not just the kept ones.
        #
        # Do this to keep it from being confusing for the user.
        if (other_feature_name %in% self$feature_names()) {
          rlang::abort(
            paste0(
              "Your choice for 'other_feature_name' (",
              other_feature_name,
              ") is already present in the feature_names.  Choose a different name!"
            ),
            class = Error$ArgumentError
          )
        }

        if (is.null(dim(plot_data_other))) {
          # It's already a vector-like object.
          plot_data[[other_feature_name]] <- plot_data_other
        } else {
          # Need to reduce it down to a vector, so get sum of each sample across features.
          plot_data[[other_feature_name]] <- rowSums(plot_data_other)
        }

        # The last thing will be other, so make it gray.
        legend_colors <- rev(c(palette$colors[1:num_features_to_show], palette$gray))
      } else {
        # There is no other category, so just the regular palette colors.
        legend_colors <- rev(palette$colors[1:num_features_to_show])
      }

      the_levels <- rev(colnames(plot_data))
      legend_labels <- the_levels

      if (isFALSE(fill)) {
        fill <- NULL
        color <- NULL

        # TODO data
        p <- ggplot2::ggplot(
          # We just need the row sums and nothing else.
          data = data.frame(Sample = rownames(plot_data),
                            Value = rowSums(plot_data),
                            stringsAsFactors = TRUE),
          mapping = ggplot2::aes_string(x = "Sample", y = "Value")
        )

        p <- p + ggplot2::geom_bar(stat = "identity")
      } else {
        # Use default
        fill <- "Feature"
        color = "#333333"

        pdat <- wide_to_long(plot_data)
        pdat$Feature <- factor(pdat$Feature, levels = the_levels)

        p <- ggplot2::ggplot(
          data = pdat,
          mapping = ggplot2::aes_string(x = "Sample", y = "Value", fill = fill)
        )

        p <- p + ggplot2::geom_bar(stat = "identity", color = color)
      }

      p <- p +
        default_theme()

      if (!is.null(axis.text.x)) {
        # TODO actually check what's passed in.....
        if (inherits(axis.text.x, "element_text")) {
          p <- p + ggplot2::theme(axis.text.x = axis.text.x)
        } else {
          rlang::abort(
            "You passed axis.text.x, but the argument was not an instance of 'element_text'",
            class = Error$ArgumentError
          )
        }
      } else {
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
      }


      if (is.null(show_legend) || isFALSE(show_legend)) {
        p <- p + ggplot2::theme(legend.position = "none")
      }


      if (is.null(legend_title)) {
        p <- p + ggplot2::scale_fill_manual(values = legend_colors, labels = legend_labels)
      } else {
        p <- p + ggplot2::scale_fill_manual(values = legend_colors, labels = legend_labels,
                                            name = legend_title)
      }

      if (!is.null(xlab)) {
        p <- p + ggplot2::xlab(xlab)
      }

      if (!is.null(ylab)) {
        p <- p + ggplot2::ylab(ylab)
      }

      if (!is.null(plot_title)) {
        p <- p + ggplot2::ggtitle(plot_title)
      }

      p
    },

    ################################################################################
    #### Conversion ################################################################
    ################################################################################

    #' @description Convert FeatureTable to phyloseq object.
    #'
    #' @details
    #' If the 'phyloseq' package is not installed, it raises an Error.
    #'
    #' @param ft A FeatureTable
    #'
    #' @return a phyloseq object
    #'
    #' @export
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
                                        row.names = seq_len(self$num_features()),
                                        stringsAsFactors = TRUE)
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
          self$feature_data <- data.frame(X = feature_data[self$feature_names(), ],
                                          stringsAsFactors = TRUE)
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
                                       row.names = 1:self$num_samples(),
                                       stringsAsFactors = TRUE)
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
          self$sample_data <- data.frame(X = sample_data[self$sample_names(), ],
                                         stringsAsFactors = TRUE)
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
      if (margin == 1 || margin == "samples") {
        result <- t(apply_fn(1, fn, ...))
      } else if (margin == 2 || margin == "features") {
        result <- apply_fn(2, fn, ...)
      } else {
        rlang::abort(sprintf("margin should be 1, 'samples', 2, or 'features'.  Got %s.", margin),
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

#### Helpers for plotting

# `%>%` <- magrittr::`%>%`

ft_kelly <- list(
  purple = "#875692",
  orange = "#F38400",
  light_blue = "#A1CAF1",
  red = "#BE0032",
  buff = "#C2B280",
  green = "#008856",
  purplish_pink = "#E68FAC",
  blue = "#0067A5",
  yellowish_pink = "#F99379",
  violet = "#604E97",
  orange_yellow = "#F6A600",
  purplish_red = "#B3446C",
  reddish_brown = "#882D17",
  yellow_green = "#8DB600",
  yellowish_brown = "#654522",
  reddish_orange = "#E25822",
  olive_green = "#2B3D26",
  yellow = "#F3C300",
  gray = "#848482"
)

ft_pthc <- list(
  blue = "#004488",
  yellow = "#DDAA33",
  red = "#BB5566",
  gray = "#848482"
)

ft_ptm <- list(
  ptm_rose = "#CC6677",
  ptm_indigo = "#332288",
  ptm_sand = "#DDCC77",
  ptm_green = "#117733",
  ptm_cyan = "#88CCEE",
  ptm_wine = "#882255",
  ptm_teal = "#44AA99",
  ptm_olive = "#999933",
  ptm_purple = "#AA4499",
  ptm_gray = "#DDDDDD"
)

ft_ptb <- list(
  ptb_blue = "#4477AA",
  ptb_pink = "#EE6677",
  ptb_green = "#228833",
  ptb_olive = "#CCBB44",
  ptb_light_blue = "#66CCEE",
  ptb_purple = "#AA3377",
  ptb_gray = "#BBBBBB"
)

ft_ptv <- list(
  ptv_orange = "#EE7733",
  ptv_blue = "#0077BB",
  ptv_teal = "#009988",
  ptv_pink = "#EE3377",
  ptv_light_blue = "#33BBEE",
  ptv_red = "#CC3311",
  ptv_gray = "#BBBBBB"
)

ft_palette <- list(
  kelly = `names<-`(sapply(ft_kelly, identity), NULL),
  pthc = `names<-`(sapply(ft_pthc, identity), NULL),
  ptm = `names<-`(sapply(ft_ptm, identity), NULL),
  ptb = `names<-`(sapply(ft_ptb, identity), NULL),
  ptv = `names<-`(sapply(ft_ptv, identity), NULL)
)


theme_featuretable <- function(base_size = 14) {
  `%+replace%` <- ggplot2::`%+replace%`

  base_color <- "#333333"
  alpha <- 0.1
  width <- 10
  height <- 6.25

  # TODO would be great to always put the zero line a bit darker.

  ggplot2::theme_bw(base_size = base_size) %+replace%
    ggplot2::theme(line = ggplot2::element_line(color = base_color),
                   rect = ggplot2::element_rect(color = base_color),
                   text = ggplot2::element_text(color = base_color),
                   title = ggplot2::element_text(color = base_color),
                   panel.grid = ggplot2::element_line(color = "#e8e8e8"),
                   #panel.grid.minor = element_line(size = rel(0.5)),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(margin = ggplot2::margin(-1, 0, 0, 0),
                                                       color = base_color),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(5, 0, 0, 0),
                                                        color = base_color),

                   axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, -1, 0, 0),
                                                       color = base_color),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 5, 0, 0),
                                                        color = base_color,
                                                        angle = 90),
                   axis.ticks = ggplot2::element_blank())
}

default_theme <- function() theme_featuretable()

darker_line_color <- "#999999"

