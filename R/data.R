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

#' A FeatureTable with data from Lee et al. (2015)
#'
#' A FeatureTable with count table, sample data, and taxonomy of an SSU rRNA
#' study of basalts on the Dorado Outcrop.
#'
#' @usage
#' # Import into the global env.
#' data(lee)
#'
#' # Refer to the FeatureTable directly.
#' featuretable::lee
#'
#' @format A FeatureTable
#' \describe{
#'   \item{feature_table}{16 samples X 1490 features table with 16S abundance}
#'   \item{feature_data}{7 variables, taxonomy}
#'   \item{sample_data}{4 variables, \code{Temp}, \code{Type}, \code{Char}, \code{Color}}
#' }
#'
#' @references Lee, Michael D., et al. 2015. “Microbial Communities on Seafloor Basalts at Dorado Outcrop Reflect Level of Alteration and Highlight Global Lithic Clades.” Frontiers in Microbiology 6 (December): 1470.
"lee"

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
