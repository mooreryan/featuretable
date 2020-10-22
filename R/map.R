# TODO add examples for the helpers.

# FeatureTable s3 functions for `map`

#' Apply a function over FeatureTable margins and return a new FeatureTable.
#'
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
#' @param ft A FeatureTable.
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
#'
#' @export
map <- function(ft, margin, fn, ...) {
  UseMethod("map")
}

#' @rdname map
#' @export
map.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map(margin, fn, ...)
}

#' @rdname map
#' @export
map_with_index <- function(ft, margin, fn, ...) {
  UseMethod("map_with_index")
}

#' @rdname map
#' @export
map_with_index.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map_with_index(margin, fn, ...)
}

#' @rdname map
#' @export
map_with_name <- function(ft, margin, fn, ...) {
  UseMethod("map_with_name")
}

#' @rdname map
#' @export
map_with_name.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map_with_name(margin, fn, ...)
}


#' @rdname map
#' @export
map_features <- function(ft, fn, ...) {
  UseMethod("map_features")
}

#' @rdname map
#' @export
map_features.FeatureTable <- function(ft, fn, ...) {
  ft$map_features(fn, ...)
}

#' @rdname map
#' @export
map_features_with_index <- function(ft, fn, ...) {
  UseMethod("map_features_with_index")
}

#' @rdname map
#' @export
map_features_with_index.FeatureTable <- function(ft, fn, ...) {
  ft$map_features_with_index(fn, ...)
}

#' @rdname map
#' @export
map_features_with_name <- function(ft, fn, ...) {
  UseMethod("map_features_with_name")
}

#' @rdname map
#' @export
map_features_with_name.FeatureTable <- function(ft, fn, ...) {
  ft$map_features_with_name(fn, ...)
}

#' @rdname map
#' @export
map_samples <- function(ft, fn, ...) {
  UseMethod("map_samples")
}

#' @rdname map
#' @export
map_samples.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples(fn, ...)
}

#' @rdname map
#' @export
map_samples_with_index <- function(ft, fn, ...) {
  UseMethod("map_samples_with_index")
}

#' @rdname map
#' @export
map_samples_with_index.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples_with_index(fn, ...)
}

#' @rdname map
#' @export
map_samples_with_name <- function(ft, fn, ...) {
  UseMethod("map_samples_with_name")
}

#' @rdname map
#' @export
map_samples_with_name.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples_with_name(fn, ...)
}
