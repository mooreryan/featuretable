#' Is an Object a FeatureTable?
#'
#' @return \code{TRUE} if Object is a \code{FeatureTable}, \code{FALSE} otherwise.
#'
#' @param x Any R object.
#'
#' @export
is.FeatureTable <- function(x) {
  inherits(x, "FeatureTable")
}

#' Dimensions of FeatureTable data.
#'
#' @description
#' Return the dimensions of the FeatureTable \code{data} field.
#'
#' @family general s3-like functions
#'
#' @return The dimension of the feature_table.
#'
#' @examples
#' ft <- FeatureTable$new(matrix(1:12, 3, 4))
#' stopifnot(dimft() == 12)
#' stopifnot(nrow(ft) == 3)
#' stopifnot(ncol(ft) == 4)
#'
#' @param x A FeatureTable object.
#'
#' @seealso \code{\link{dim}} for generic method.
#'
#' @export
dim.FeatureTable <- function(x) {
  dim(x$data)
}

#' Minimum, maximum, size, etc., of FeatureTable data field.
#'
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
#' stopifnot(min(ft) == 0)
#' stopifnot(non_zero_min(ft) == 1)
#' stopifnot(max(ft) == 8)
#' stopifnot(size(ft) == 9)
#'
#' @seealso [featuretable::is_count_table()]
#'
#' @export
max.FeatureTable <- function(ft, ...) {
  ft$max(...)
}

#' @rdname max.FeatureTable
#' @export
min.FeatureTable <- function(ft, ...) {
  ft$min(...)
}

#' @rdname max.FeatureTable
#' @export
non_zero_min <- function(ft, ...) {
  UseMethod("non_zero_min")
}

#' @rdname max.FeatureTable
#' @export
non_zero_min.FeatureTable <- function(ft, ...) {
  ft$non_zero_min(...)
}

#' @rdname max.FeatureTable
#' @export
size <- function(ft, ...) {
  UseMethod("size")
}

#' @rdname max.FeatureTable
#' @export
size.FeatureTable <- function(ft) {
  ft$size()
}

#' The number of samples/observations/rows of a FeatureTable
#'
#' @description
#' Return the number of samples/observations/rows in the FeatureTable.
#'
#' @return The number of samples/observations/rows in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @family dimension functions
#'
#' @export
num_samples <- function(ft) {
  UseMethod("num_samples")
}

#' @rdname num_samples
#' @export
num_samples.FeatureTable <- function(ft) {
  ft$num_samples()
}

#' @rdname num_samples
#' @export
nsamples <- function(ft) {
  UseMethod("nsamples")
}

#' @rdname num_samples
#' @export
nsamples.FeatureTable <- function(ft) {
  ft$nsamples()
}

#' @rdname num_samples
#' @export
num_observations <- function(ft) {
  UseMethod("num_observations")
}

#' @rdname num_samples
#' @export
num_observations.FeatureTable <- function(ft) {
  ft$num_observations()
}

#' @rdname num_samples
#' @export
nobservations <- function(ft) {
  UseMethod("nobservations")
}

#' @rdname num_samples
#' @export
nobservations.FeatureTable <- function(ft) {
  ft$nobservations()
}

#' The number of features/columns of a FeatureTable
#'
#' @description
#' Return the number of features/columns in the FeatureTable.
#'
#' @return The number of features/columns in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @family dimension functions
#'
#' @export
num_features <- function(ft) {
  UseMethod("num_features")
}

#' @rdname num_features
#' @export
num_features.FeatureTable <- function(ft) {
  ft$num_features()
}

#' @rdname num_features
#' @export
nfeatures <- function(ft) {
  UseMethod("nfeatures")
}

#' @rdname num_features
#' @export
nfeatures.FeatureTable <- function(ft) {
  ft$nfeatures()
}

#' Sample/observation/row names of a FeatureTable
#'
#' @description
#' Return the names of samples/observations/rows in the FeatureTable.
#'
#' @return The names of samples/observations/rows in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @family row name functions
#'
#' @export
sample_names <- function(ft) {
  UseMethod("sample_names")
}

#' @rdname sample_names
#' @export
sample_names.FeatureTable <- function(ft) {
  ft$sample_names()
}

#' @rdname sample_names
#' @export
observation_names <- function(ft) {
  UseMethod("observation_names")
}

#' @rdname sample_names
#' @export
observation_names.FeatureTable <- function(ft) {
  ft$observation_names()
}

#' Feature/column names of a FeatureTable.
#'
#' @description
#' Return the names of samples/observations/rows in the FeatureTable.
#'
#' @return The names of samples/observations/rows in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @family row name functions
#'
#' @export
feature_names <- function(ft) {
  UseMethod("feature_names")
}

#' @rdname feature_names
#' @export
feature_names.FeatureTable <- function(ft) {
  ft$feature_names()
}
