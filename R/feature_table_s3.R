# TODO replace all `is` calls with `inherits`

as.data.frame.FeatureTable <- function(ft) {
  ft$data
}

is.FeatureTable <- function(x) {
  inherits(x, "FeatureTable")
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

################################################################################
#### map #######################################################################
################################################################################

map <- function(x, ...) {
  UseMethod("map")
}

map.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map(margin, fn, ...)
}

map_with_index <- function(x, ...) {
  UseMethod("map_with_index")
}

map_with_index.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map_with_index(margin, fn, ...)
}

map_with_name <- function(x, ...) {
  UseMethod("map_with_name")
}

map_with_name.FeatureTable <- function(ft, margin, fn, ...) {
  ft$map_with_name(margin, fn, ...)
}


#### map_features ####

map_features <- function(x, ...) {
  UseMethod("map_features")
}

map_features.FeatureTable <- function(ft, fn, ...) {
  ft$map_features(fn, ...)
}

map_features_with_index <- function(x, ...) {
  UseMethod("map_features_with_index")
}

map_features_with_index.FeatureTable <- function(ft, fn, ...) {
  ft$map_features_with_index(fn, ...)
}

map_features_with_name <- function(x, ...) {
  UseMethod("map_features_with_name")
}

map_features_with_name.FeatureTable <- function(ft, fn, ...) {
  ft$map_features_with_name(fn, ...)
}

#### map_features ####

map_samples <- function(x, ...) {
  UseMethod("map_samples")
}

map_samples.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples(fn, ...)
}

map_samples_with_index <- function(x, ...) {
  UseMethod("map_samples_with_index")
}

map_samples_with_index.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples_with_index(fn, ...)
}

map_samples_with_name <- function(x, ...) {
  UseMethod("map_samples_with_name")
}

map_samples_with_name.FeatureTable <- function(ft, fn, ...) {
  ft$map_samples_with_name(fn, ...)
}

################################################################################
#### keep ######################################################################
################################################################################

keep <- function(x, ...) {
  UseMethod("keep")
}

keep.FeatureTable <- function(ft, margin, fn, ...) {
  ft$keep(margin, fn, ...)
}

#### keep_features ####

keep_features <- function(x, ...) {
  UseMethod("keep_features")
}

keep_features.FeatureTable <- function(ft, fn, ...) {
  ft$keep_features(fn, ...)
}

#### keep_samples ####

keep_samples <- function(x, ...) {
  UseMethod("keep_samples")
}

keep_samples.FeatureTable <- function(ft, fn, ...) {
  ft$keep_samples(fn, ...)
}

################################################################################
#### attribute type things #####################################################
################################################################################

#### num_samples & aliases

num_samples <- function(x) {
  UseMethod("num_samples")
}

num_samples.FeatureTable <- function(ft) {
  ft$num_samples()
}

nsamples <- function(x) {
  UseMethod("nsamples")
}

nsamples.FeatureTable <- function(ft) {
  ft$nsamples()
}

num_observations <- function(x) {
  UseMethod("num_observations")
}

num_observations.FeatureTable <- function(ft) {
  ft$num_observations()
}

nobservations <- function(x) {
  UseMethod("nobservations")
}

nobservations.FeatureTable <- function(ft) {
  ft$nobservations()
}

#### num_features & aliases

num_features <- function(x) {
  UseMethod("num_features")
}

num_features.FeatureTable <- function(ft) {
  ft$num_features()
}

nfeatures <- function(x) {
  UseMethod("nfeatures")
}

nfeatures.FeatureTable <- function(ft) {
  ft$nfeatures()
}
