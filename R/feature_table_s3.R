# TODO replace all `is` calls with `inherits`

as.data.frame.FeatureTable <- function(ft) {
  ft$data
}

# Generic `reduce`
reduce <- function(x, ...) {
  UseMethod("reduce")
}

reduce.FeatureTable <- function(ft, margin, fn, ...) {
  ft$reduce(margin, fn, ...)
}

reduce_features <- function(x, ...) {
  UseMethod("reduce_features")
}

reduce_features.FeatureTable <- function(ft, fn, ...) {
  ft$reduce_features(fn, ...)
}

reduce_samples <- function(x, ...) {
  UseMethod("reduce_samples")
}

reduce_samples.FeatureTable <- function(ft, fn, ...) {
  ft$reduce_samples(fn, ...)
}

reduce_all <- function(x, ...) {
  UseMethod("reduce_all")
}

reduce_all.FeatureTable <- function(ft, fn, ...) {
  ft$reduce_all(fn, ...)
}

