#' @export
as.data.frame.FeatureTable <- function(ft) {
  ft$data
}

#' @export
is.FeatureTable <- function(ft) {
  inherits(ft, "FeatureTable")
}

################################################################################
#### attribute type things #####################################################
################################################################################

#### num_samples & aliases

#' The number of samples/observations/rows of a FeatureTable
#'
#' @description
#' Return the number of samples/observations/rows in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @return The number of rows in the FeatureTable.
#'
#' @family dimension functions
#'
#' @export
num_samples <- function(ft) {
  UseMethod("num_samples")
}

#' @export
num_samples.FeatureTable <- function(ft) {
  ft$num_samples()
}

#' The number of samples/observations/rows of a FeatureTable
#'
#' @description
#' Return the number of samples/observations/rows in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @return The number of rows in the FeatureTable.
#'
#' @family dimension functions
#'
#' @export
nsamples <- function(ft) {
  UseMethod("nsamples")
}

#' @export
nsamples.FeatureTable <- function(ft) {
  ft$nsamples()
}

#' The number of samples/observations/rows of a FeatureTable
#'
#' @description
#' Return the number of samples/observations/rows in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @return The number of rows in the FeatureTable.
#'
#' @family dimension functions
#'
#' @export
num_observations <- function(ft) {
  UseMethod("num_observations")
}

#' @export
num_observations.FeatureTable <- function(ft) {
  ft$num_observations()
}

#' The number of samples/observations/rows of a FeatureTable
#'
#' @description
#' Return the number of samples/observations/rows in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @return The number of rows in the FeatureTable.
#'
#' @family dimension functions
#'
#' @export
nobservations <- function(ft) {
  UseMethod("nobservations")
}

#' @export
nobservations.FeatureTable <- function(ft) {
  ft$nobservations()
}

#### num_features & aliases

#' The number of features/columns of a FeatureTable
#'
#' @description
#' Return the number of features/columns in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @return The number of columns in the FeatureTable.
#'
#' @family dimension functions
#'
#' @export
num_features <- function(ft) {
  UseMethod("num_features")
}

#' @export
num_features.FeatureTable <- function(ft) {
  ft$num_features()
}

#' The number of features/columns of a FeatureTable
#'
#' @description
#' Return the number of features/columns in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @return The number of columns in the FeatureTable.
#'
#' @family dimension functions
#'
#' @export
nfeatures <- function(ft) {
  UseMethod("nfeatures")
}

#' @export
nfeatures.FeatureTable <- function(ft) {
  ft$nfeatures()
}

#### sample_names & aliases

#' Samples/observations/rows of a FeatureTable
#'
#' @description
#' Return the names of samples/observations/rows in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @return The names of samples/observations/rows in the FeatureTable.
#'
#' @family row name functions
#'
#' @export
sample_names <- function(ft) {
  UseMethod("sample_names")
}

#' @export
sample_names.FeatureTable <- function(ft) {
  ft$sample_names()
}

#' Samples/observations/rows of a FeatureTable
#'
#' @description
#' Return the names of samples/observations/rows in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @return The names of samples/observations/rows in the FeatureTable.
#'
#' @family row name functions
#'
#' @export
observation_names <- function(ft) {
  UseMethod("observation_names")
}

#' @export
observation_names.FeatureTable <- function(ft) {
  ft$observation_names()
}

#### feature_names & aliases

#' Features/columns of a FeatureTable
#'
#' @description
#' Return the names of features/columns in the FeatureTable.
#'
#' @param ft A FeatureTable object.
#'
#' @return The names of features/columns in the FeatureTable.
#'
#' @export
feature_names <- function(ft) {
  UseMethod("feature_names")
}

#' @export
feature_names.FeatureTable <- function(ft) {
  ft$feature_names()
}

#### dim

#' Dimensions of a FeatureTable
#'
#' @description
#' Return the dimensions of the FeatureTable.
#'
#' @param x A FeatureTable object.
#'
#' @return
#' The dimension of the feature_table.
#'
#' @family dimension functions
#'
#' @seealso \code{\link{dim}} for generic method, \code{\link{FeatureTable}} for R6 method.
#'
#' @export
dim.FeatureTable <- function(x) {
  dim(x$data)
}

################################################################################
#### little data utils #########################################################
################################################################################

#' @export
max.FeatureTable <- function(ft, ...) {
  ft$max(...)
}

#' @export
min.FeatureTable <- function(ft, ...) {
  ft$min(...)
}

#' @export
non_zero_min <- function(ft, ...) {
  UseMethod("non_zero_min")
}

#' @export
non_zero_min.FeatureTable <- function(ft, ...) {
  ft$non_zero_min(...)
}

#' @export
size <- function(ft, ...) {
  UseMethod("size")
}

#' @export
size.FeatureTable <- function(ft) {
  ft$size()
}
