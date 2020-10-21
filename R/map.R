# TODO add examples for the helpers.

# FeatureTable s3 functions for `map`

#' Apply a function over FeatureTable margins and return a new FeatureTable.
#'
#' @description
#' Returns a new FeatureTable by applying a function to margins of the
#' \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable.
#' @param margin Margin to apply the function over.  E.g., \code{1} or
#'   \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates
#'   columns.
#' @param fn The function to be applied over the specified margin.
#' @param ... Optional arguments to \code{fn}.
#'
#' @return A new FeatureTable with the result of applying \code{fn} over
#'   \code{margin}.
#'
#' @details
#' Named helpers can also be used rather than specifying the \code{margin}
#' argument.
#'
#' \itemize{
#'   \item \code{\link{map_features}}
#'   \item \code{\link{map_samples}}
#' }
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
#' @family apply functions
#'
#' @export
map <- function(ft, margin, fn, ...) {
  UseMethod("map")
}

#' @export
map.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map(margin, fn, ...)
}

#' Apply a function over FeatureTable margins and return a new FeatureTable.
#'
#' @description
#' Returns a new FeatureTable by applying a function to margins of the
#' \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable.
#' @param margin Margin to apply the function over.  E.g., \code{1} or
#'   \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates
#'   columns.
#' @param fn The function to be applied over the specified margin.  This
#'   function should have at least 2 parameters, the data to be applied over,
#'   and the index of the current row or column.  See details.
#' @param ... Optional arguments to \code{fn}.
#'
#' @return A new FeatureTable with the result of applying \code{fn} over
#'   \code{margin}.
#'
#' @details
#' This function is similar to \code{\link{map}}, except that the function
#' should accept index of the row or column currently being applied over.  See
#' Examples.
#'
#' Note that if you want to include something from the \code{feature_data} or
#' \code{sample_data} in the mapping function, you probably want to use
#' \code{\link{map_with_name}}.
#'
#' Named helpers can also be used rather than specifying the \code{margin}
#' argument.
#'
#' \itemize{
#'   \item \code{\link{map_features_with_index}}
#'   \item \code{\link{map_samples_with_index}}
#' }
#'
#' @examples
#' data(ft)
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
#' @family apply functions
#'
#' @export
map_with_index <- function(ft, margin, fn, ...) {
  UseMethod("map_with_index")
}

#' @export
map_with_index.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map_with_index(margin, fn, ...)
}

#' Apply a function over FeatureTable margins and return a new FeatureTable.
#'
#' @description
#' Returns a new FeatureTable by applying a function to margins of the
#' \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable.
#' @param margin Margin to apply the function over.  E.g., \code{1} or
#'   \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates
#'   columns.
#' @param fn The function to be applied over the specified margin.  This
#'   function should have at least 2 parameters, the data to be applied over,
#'   and the name of the current row or column.  See details.
#' @param ... Optional arguments to \code{fn}.
#'
#' @return A new FeatureTable with the result of applying \code{fn} over
#'   \code{margin}.
#'
#' @details
#' This function is similar to \code{\link{map}}, except that the function
#' should accept name of the row or column currently being applied over.  See
#' Examples.
#'
#' Note that if you want to include some external data about samples or features
#' inside the mapping function, you may want to use
#' \code{\link{map_with_index}}.
#'
#' Named helpers can also be used rather than specifying the \code{margin}
#' argument.
#'
#' \itemize{
#'   \item \code{\link{map_features_with_name}}
#'   \item \code{\link{map_samples_with_name}}
#' }
#'
#' @examples
#' data(ft)
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
#' @family apply functions
#'
#' @export
map_with_name <- function(ft, margin, fn, ...) {
  UseMethod("map_with_name")
}

#' @export
map_with_name.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map_with_name(margin, fn, ...)
}


#' @rdname map
#' @export
map_features <- function(ft, fn, ...) {
  UseMethod("map_features")
}

#' @export
map_features.FeatureTable <- function(ft, fn, ...) {
  ft$map_features(fn, ...)
}

#' @rdname map_with_index
#' @export
map_features_with_index <- function(ft, fn, ...) {
  UseMethod("map_features_with_index")
}

#' @export
map_features_with_index.FeatureTable <- function(ft, fn, ...) {
  ft$map_features_with_index(fn, ...)
}

#' @rdname map_with_name
#' @export
map_features_with_name <- function(ft, fn, ...) {
  UseMethod("map_features_with_name")
}

#' @export
map_features_with_name.FeatureTable <- function(ft, fn, ...) {
  ft$map_features_with_name(fn, ...)
}

#' @rdname map
#' @export
map_samples <- function(ft, fn, ...) {
  UseMethod("map_samples")
}

#' @export
map_samples.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples(fn, ...)
}

#' @rdname map_with_index
#' @export
map_samples_with_index <- function(ft, fn, ...) {
  UseMethod("map_samples_with_index")
}

#' @export
map_samples_with_index.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples_with_index(fn, ...)
}

#' @rdname map_with_name
#' @export
map_samples_with_name <- function(ft, fn, ...) {
  UseMethod("map_samples_with_name")
}

#' @export
map_samples_with_name.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples_with_name(fn, ...)
}
