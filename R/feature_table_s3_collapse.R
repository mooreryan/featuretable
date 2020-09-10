################################################################################
#### collapse ##################################################################
################################################################################

#' @export
collapse <- function(ft, ...) {
  UseMethod("collapse")
}

#' @export
collapse.FeatureTable <- function(ft, ...) {
  ft$collapse(...)
}

#' @export
collapse_features <- function(ft, ...) {
  UseMethod("collapse_features")
}

#' @export
collapse_features.FeatureTable <- function(ft, ...) {
  ft$collapse_features(...)
}

#' @export
collapse_samples <- function(ft, ...) {
  UseMethod("collapse_samples")
}

#' @export
collapse_samples.FeatureTable <- function(ft, ...) {
  ft$collapse_samples(...)
}
