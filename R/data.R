#' A FeatureTable with count-like data.
#'
#' A small FeatureTable with count-like (e.g., OTU-like) data used for
#' examples and to test FeatureTable functions.
#'
#' @format A FeatureTable
#' \describe{
#'   \item{feature_table}{4 sample X 5 feature table with count-like data}
#'   \item{feature_data}{3 variables, \code{Color}, \code{Shape}, and \code{Length}}
#'   \item{sample_data}{3 variables, \code{Location}, \code{Season}, and \code{SnazzyFactor}}
#' }
"ft"

#' Data used for examples and to test the FeatureTable functions.
#'
#' A small dataset for testing and making examples for FeatureTable
#' documentation.
#'
#' @format A list
#' \describe{
#'   \item{nsamples}{the number of samples}
#'   \item{nfeatures}{the number of features}
#'   \item{count_table}{a matrix with counts}
#'   \item{feature_data}{a data frame with info about features}
#'   \item{sample_data}{a data frame with info about samples}
#'   \item{expected_feature_data}{what the feature data looks like after making the FeatureTable}
#'   \item{expected_sample_data}{what the samples data looks like after making the FeatureTable}
#' }
"testdata"
