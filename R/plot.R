#' FeatureTable summary plots
#'
#' @details
#' If you don't have \code{ggplot2} installed, you will get an error.
#'
#' @param x A FeatureTable
#' @param num_features How many features to show on the plot? The rest of the
#'   features get put into an "Other" category. "Other" will always be gray if
#'   you use one of the built in palettes.
#' @param other_feature_name Name to use if features need to be combined into
#'   an 'Other' category. By default, will be 'Other'.
#' @param fill If FALSE, don't show the features, if anything else, show the
#'   features.
#' @param palette Which palette to use? ("kelly", "muted", "bright", "vibrant",
#'    "high contrast").  If there are more features than colors in the palette,
#'     the colors will be recycled.
#' @param show_legend Show the legend on the right side?
#' @param legend_title Custom legend title
#' @param plot_title Custom plot title
#' @param xlab Custom xlab
#' @param ylab Custom ylab
#' @param axis.text.x Custom \code{axis.text.x}.  Should be an
#'    \code{element_text}.
#' @param ... Extra args
#'
#' @return A `ggplot2` object.
#'
#' @export
plot.FeatureTable <- function(x, ...) {
  x$plot(...)
}

################################################################################
#### ordination ################################################################
################################################################################

#' PCA Biplot
#'
#' @details
#' If installed, you can use \href{https://github.com/mooreryan/biplotr}{biplotR}
#' for nice looking biplots!  Be sure that if you use_biplotr = TRUE, not to set
#' options for the base::biplot function.
#'
#' See documentation for \code{?biplotr::pca_biplot} for more info.
#'
#' If you don't want to use \href{https://github.com/mooreryan/biplotr}{biplotR},
#' then this function is a wrapper for \code{base::biplot}.
#'
#' @param ft A FeatureTable
#' @param use_biplotr Use biplotR for nice biplots.  Must be installed.
#' @param incude_sample_data Do you want to include sample data?  If \code{TRUE},
#'   then you will have access to lots of nice sample data to help annotate your
#'   plots.  See Examples.
#'
#' @examples
#' # TODO!
#'
#' @export
pca_biplot <- function(ft, use_biplotr = FALSE, include_sample_data = FALSE, ...) {
  UseMethod("pca_biplot")
}

#' @rdname pca_biplot
#' @export
pca_biplot.FeatureTable <- function(ft, use_biplotr = FALSE, include_sample_data = FALSE, ...) {
  ft$pca_biplot(use_biplotr = use_biplotr, include_sample_data = include_sample_data, ...)
}
