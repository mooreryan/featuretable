#' Core Microbiome
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
#'
#' @export
core_microbiome <- function(ft, ...) {
  UseMethod("core_microbiome")
}

#' @rdname core_microbiome
#' @export
core_microbiome.FeatureTable <- function(ft,
                                         detection_limit = 1,
                                         min_sample_proportion = NULL,
                                         max_sample_proportion = NULL,
                                         min_samples = NULL,
                                         max_samples = NULL,
                                         ...) {
  ft$core_microbiome(detection_limit = detection_limit,
                     min_sample_proportion = min_sample_proportion,
                     max_sample_proportion = max_sample_proportion,
                     min_samples = min_samples,
                     max_samples = max_samples)
}
