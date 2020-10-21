#' FeatureTable summary plots
#'
#' @param ft A FeatureTable
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
plot.FeatureTable <- function(ft, ...) {
  ft$plot(...)
}

################################################################################
#### ordination ################################################################
################################################################################

#' @export
pca_biplot <- function(ft, use_biplotr = FALSE, include_sample_data = FALSE, ...) {
  UseMethod("pca_biplot")
}

#' @export
pca_biplot.FeatureTable <- function(ft, use_biplotr = FALSE, include_sample_data = FALSE, ...) {
  ft$pca_biplot(use_biplotr = use_biplotr, include_sample_data = include_sample_data, ...)
}
