as.data.frame.FeatureTable <- function(ft) {
  ft$data
}

is.FeatureTable <- function(ft) {
  inherits(ft, "FeatureTable")
}


# Generic `reduce`
reduce <- function(ft, ...) {
  UseMethod("reduce")
}

reduce.FeatureTable <- function(ft, margin, fn, ...) {
  ft$reduce(margin, fn, ...)
}

reduce_features <- function(ft, ...) {
  UseMethod("reduce_features")
}

reduce_features.FeatureTable <- function(ft, fn, ...) {
  ft$reduce_features(fn, ...)
}

reduce_samples <- function(ft, ...) {
  UseMethod("reduce_samples")
}

reduce_samples.FeatureTable <- function(ft, fn, ...) {
  ft$reduce_samples(fn, ...)
}

reduce_all <- function(ft, ...) {
  UseMethod("reduce_all")
}

reduce_all.FeatureTable <- function(ft, fn, ...) {
  ft$reduce_all(fn, ...)
}

################################################################################
#### map #######################################################################
################################################################################

# TODO accept observations as a valid margin.

#' Apply a function over FeatureTable margins.
#'
#' @description
#' Returns a new FeatureTable obtained by applying a function to margins of the \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable object.
#' @param margin Which margin the function will be applied over.  E.g., \code{1} or \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates columns.
#' @param fn The function to be applied over the specified margin.
#' @param ... Optional arguments to \code{fn} (if \code{fn} is a function).
#'
#' @return A new FeatureTable with the result of applying \code{fn} over \code{margin}.
#'
#' @family mapping functions
#'
#' @export
map <- function(ft, margin, fn, ...) {
  UseMethod("map")
}

map.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map(margin, fn, ...)
}

#' Apply a function over FeatureTable margins.
#'
#' @description
#' Returns a new FeatureTable obtained by applying a function to margins of the \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable object.
#' @param margin Which margin the function will be applied over.  E.g., \code{1} or \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates columns.
#' @param fn The function to be applied over the specified margin.  This function should have at least 2 parameters, the data applied over, and the index of the current row or column.
#' @param ... Optional arguments to \code{fn} (if \code{fn} is a function).
#'
#' @return A new FeatureTable with the result of applying \code{fn} over \code{margin}.
#'
#' @family mapping functions
#'
#' @export
map_with_index <- function(ft, margin, fn, ...) {
  UseMethod("map_with_index")
}

map_with_index.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map_with_index(margin, fn, ...)
}

#' Apply a function over FeatureTable margins.
#'
#' @description
#' Returns a new FeatureTable obtained by applying a function to margins of the \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable object.
#' @param margin Which margin the function will be applied over.  E.g., \code{1} or \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates columns.
#' @param fn The function to be applied over the specified margin.  This function should have at least 2 parameters, the data applied over, and the name of the current row or column.
#' @param ... Optional arguments to \code{fn} (if \code{fn} is a function).
#'
#' @return A new FeatureTable with the result of applying \code{fn} over \code{margin}.
#'
#' @family mapping functions
#'
#' @export
map_with_name <- function(ft, margin, fn, ...) {
  UseMethod("map_with_name")
}

map_with_name.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map_with_name(margin, fn, ...)
}


#### map_features ####

#' Apply a function over features of a FeatureTable
#'
#' @description
#' Returns a new FeatureTable obtained by applying a function to features of the \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable object.
#' @param fn The function to be applied over the features.
#' @param ... Optional arguments to \code{fn} (if \code{fn} is a function).
#'
#' @return A new FeatureTable with the result of applying \code{fn} over features.
#'
#' @family mapping functions
#'
#' @export
map_features <- function(ft, ...) {
  UseMethod("map_features")
}

map_features.FeatureTable <- function(ft, fn, ...) {
  ft$map_features(fn, ...)
}

#' Apply a function over features of a FeatureTable
#'
#' @description
#' Returns a new FeatureTable obtained by applying a function to features of the \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable object.
#' @param fn The function to be applied over the features. This function should have at least 2 parameters, the data applied over, and the index of the current row or column.
#' @param ... Optional arguments to \code{fn} (if \code{fn} is a function).
#'
#' @return A new FeatureTable with the result of applying \code{fn} over features.
#'
#' @family mapping functions
#'
#' @export
map_features_with_index <- function(ft, ...) {
  UseMethod("map_features_with_index")
}

map_features_with_index.FeatureTable <- function(ft, fn, ...) {
  ft$map_features_with_index(fn, ...)
}

#' Apply a function over features of a FeatureTable
#'
#' @description
#' Returns a new FeatureTable obtained by applying a function to features of the \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable object.
#' @param fn The function to be applied over the features. This function should have at least 2 parameters, the data applied over, and the name of the current row or column.
#' @param ... Optional arguments to \code{fn} (if \code{fn} is a function).
#'
#' @return A new FeatureTable with the result of applying \code{fn} over features.
#'
#' @family mapping functions
#'
#' @export
map_features_with_name <- function(ft, ...) {
  UseMethod("map_features_with_name")
}

map_features_with_name.FeatureTable <- function(ft, fn, ...) {
  ft$map_features_with_name(fn, ...)
}

#### map_features ####

#' Apply a function over samples of a FeatureTable
#'
#' @description
#' Returns a new FeatureTable obtained by applying a function to samples of the \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable object.
#' @param fn The function to be applied over the samples
#' @param ... Optional arguments to \code{fn} (if \code{fn} is a function).
#'
#' @return A new FeatureTable with the result of applying \code{fn} over samples
#'
#' @family mapping functions
#'
#' @export
map_samples <- function(ft, ...) {
  UseMethod("map_samples")
}

map_samples.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples(fn, ...)
}

#' Apply a function over samples of a FeatureTable
#'
#' @description
#' Returns a new FeatureTable obtained by applying a function to samples of the \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable object.
#' @param fn The function to be applied over the samples.  This function should have at least 2 parameters, the data applied over, and the index of the current sample.
#' @param ... Optional arguments to \code{fn} (if \code{fn} is a function).
#'
#' @return A new FeatureTable with the result of applying \code{fn} over samples
#'
#' @family mapping functions
#'
#' @export
map_samples_with_index <- function(ft, ...) {
  UseMethod("map_samples_with_index")
}

map_samples_with_index.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples_with_index(fn, ...)
}

#' Apply a function over samples of a FeatureTable
#'
#' @description
#' Returns a new FeatureTable obtained by applying a function to samples of the \code{$data} field of a \code{FeatureTable}.
#'
#' @param ft A FeatureTable object.
#' @param fn The function to be applied over the samples.  This function should have at least 2 parameters, the data applied over, and the name of the current sample.
#' @param ... Optional arguments to \code{fn} (if \code{fn} is a function).
#'
#' @return A new FeatureTable with the result of applying \code{fn} over samples
#'
#' @family mapping functions
#'
#' @export
map_samples_with_name <- function(ft, ...) {
  UseMethod("map_samples_with_name")
}

map_samples_with_name.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples_with_name(fn, ...)
}

################################################################################
#### keep ######################################################################
################################################################################

# TODO allow "observations" as a margin name.
# TODO what if you pass optional arguments to a non-function predicate?

#' Keep samples/observations/rows using a predicate
#'
#' @description
#' Keep samples/observations/rows using a predicate function or expression.
#'
#' @param ft A FeatureTable object.
#' @param margin Subscript which the predicate will be applied over.  E.g., \code{1} or \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates columns.
#' @param predicate The predicate function or expression to be applied.  Only those elements where \code{predicate} is or evaluates to \code{TRUE} will be kept.
#' @param ... Optional arguments to \code{predicate} (if \code{predicate} is a function).
#'
#' @return A new FeatureTable with the elements that were kept.
#'
#' @family filtering functions
#'
#' @export
keep <- function(ft, margin, predicate, ...) {
  UseMethod("keep")
}

keep.FeatureTable <- function(ft, margin, predicate, ...) {
  ft$keep(margin, predicate, ...)
}

#### keep_features ####

#' Keep features/columns using a predicate
#'
#' @description
#' Keep features/columns using a predicate function or expression.
#'
#' @param ft A FeatureTable object.
#' @param predicate The predicate function or expression to be applied.  Only those elements where \code{predicate} is or evaluates to \code{TRUE} will be kept.
#' @param ... Optional arguments to \code{predicate} (if \code{predicate} is a function).
#'
#' @return A new FeatureTable with the elements that were kept.
#'
#' @family filtering functions
#'
#' @export
keep_features <- function(ft, predicate, ...) {
  UseMethod("keep_features")
}

keep_features.FeatureTable <- function(ft, predicate, ...) {
  ft$keep_features(predicate, ...)
}

#### keep_samples ####

#' Keep elements using a predicate
#'
#' @description
#' Keep elements using a predicate function or expression.
#'
#' @param ft A FeatureTable object.
#' @param predicate The predicate function or expression to be applied.  Only those elements where \code{predicate} is or evaluates to \code{TRUE} will be kept.
#' @param ... Optional arguments to \code{predicate} (if \code{predicate} is a function).
#'
#' @return A new FeatureTable with the elements that were kept.
#'
#' @family filtering functions
#'
#' @export
keep_samples <- function(ft, predicate, ...) {
  UseMethod("keep_samples")
}

keep_samples.FeatureTable <- function(ft, predicate, ...) {
  ft$keep_samples(predicate, ...)
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
#' @param ft A FeatureTable object.
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