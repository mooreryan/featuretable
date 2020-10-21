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
#### conversion ################################################################
################################################################################

#' Convert FeatureTable to phyloseq object.
#'
#' @description
#' If the 'phyloseq' package is not installed, it raises an Error.
#'
#' @param ft A FeatureTable
#'
#' @return a phyloseq object
#'
#' @export
as.phyloseq <- function(ft) {
  UseMethod("as.phyloseq")
}

#' @export
as.phyloseq.FeatureTable <- function(ft) {
  ft$as_phyloseq()
}

################################################################################
#### querying aspects of FeatureTable ##########################################
################################################################################

#' Is the FeatureTable a count table?
#'
#' @param ft A FeatureTable
#'
#' @return TRUE if \code{feature_table} contains only natural numbers (counting numbers >= 0), FALSE otherwise.
#'
#' @export
is_count_table <- function(ft) {
  UseMethod("is_count_table")
}

#' @export
is_count_table.FeatureTable <- function(ft) {
  ft$is_count_table()
}

################################################################################
#### basic CoDA ################################################################
################################################################################

#' @export
replace_zeros <- function(ft, ...) {
  UseMethod("replace_zeros")
}

#' Replacing zeros.
#'
#' @param ft A FeatureTable
#' @param replacement (Ignored if \code{use_cmultRepl = TRUE})
#' @param tol (Ignored if \code{use_cmultRepl = TRUE})
#' @param use_cmultRepl TRUE/FALSE whether to use \code{cmultRepl} function.
#' @param ... Extra arguments (i.g., passed to \code{cmultRepl})
#'
#' @return A new FeatureTable with the zeros in \code{data} replaced.
#'
#' @export
replace_zeros.FeatureTable <- function(ft,
                                       replacement = 0.05,
                                       tol = .Machine$double.eps ^ 0.5,
                                       use_cmultRepl = FALSE,
                                       ...) {
  ft$replace_zeros(replacement, tol, use_cmultRepl, ...)
}

#' Centered log ratio.
#'
#' @param ft A Feature table.
#' @param base Base of logarithm.
#'
#' @return A FeatureTable with the \code{data} transformed with centered log ratio.
#'
#' @export
clr <- function(ft, base = 2) {
  UseMethod("clr")
}

#' @export
clr.FeatureTable <- function(ft, base = 2) {
  ft$clr(base)
}

################################################################################
#### importing data ############################################################
################################################################################

#' @export
ft_from <- function(x, ...) {
  UseMethod("ft_from")
}

# normally from a phyloseq object, the tax table wouldn't have numeric values.  BUT if it comes from FT => phy => FT it could.  but phyloseq taxtable does NOT play well with numebrs.  So user must specify any numeric feature_data columns manually :(
#' @export
ft_from.phyloseq <- function(x, numeric_feature_data_columns = NULL, ...) {
  factor_to_numeric <- function(f) as.numeric(levels(f))[f]

  ct <-  as.matrix(as.data.frame(
    phyloseq::otu_table(x, phyloseq::taxa_are_rows(x)),
    stringsAsFactors = TRUE
  ))

  # Make sure the samples and features are in the right orientation.
  if (phyloseq::taxa_are_rows(x)) {
    dimnames(ct) <- list(
      Features = rownames(ct),
      Samples = colnames(ct)
    )
  } else {
    dimnames(ct) <- list(
      Samples = rownames(ct),
      Features = colnames(ct)
    )
  }

  # Manage the sample_data.
  sample_dat <- phyloseq::sample_data(x)
  attr(sample_dat, "class") <- "data.frame"
  attr(sample_dat, ".S3Class") <- NULL

  # Manage the tax_table.
  feature_dat <- phyloseq::tax_table(x)
  # This one has to be a matrix.
  attr(feature_dat, "class") <- "matrix"

  # tax_tables are weird in phyloseq.  Need to strip of individual column names attr.
  rownames_feature_dat <- rownames(feature_dat)
  feature_dat <- as.data.frame(
    apply(feature_dat, 2, function(x) {
      attr(x, "names") <- NULL

      x
    }),
    stringsAsFactors = TRUE
  )
  rownames(feature_dat) <- rownames_feature_dat

  # If any of the feature data was numeric (ie it came from FT => phy => FT) then you've got problems.
  if (!is.null(numeric_feature_data_columns)) {
    for (j in numeric_feature_data_columns) {
      # Make sure that j is good.
      is_valid_colname <- j %in% colnames(feature_dat)

      suppressWarnings(
        is_valid_colnum <- 0 < as.numeric(j) && as.numeric(j) <= ncol(feature_dat)
      )

      if (!is_valid_colname && (is.na(is_valid_colnum) || !is_valid_colnum)) {
        rlang::abort(paste(j, "is not a valid column specifier. Check your args."),
                     class = Error$ArgumentError)
      }

      feature_dat[, j] <- factor_to_numeric(feature_dat[, j])
    }
  }

  FeatureTable$new(
    ct,
    feature_data = feature_dat,
    sample_data = sample_dat,
    feature_table_rows_are_samples = !phyloseq::taxa_are_rows(x)
  )
}

################################################################################
#### core microbiome ###########################################################
################################################################################

#' @export
core_microbiome <- function(ft, ...) {
  UseMethod("core_microbiome")
}

#' Core Microbiome
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
#' @details
#' If both \code{min_sample_proportion} and \code{min_samples} are given, an
#'   error will be raised. If \code{min_sample_proportion} is not a proportion,
#'   an error will be raised.  If \code{min_samples} looks like a proportion, a
#'    warning will be given.
#'
#' @export
core_microbiome.FeatureTable <- function(ft,
                                         detection_limit = 1,
                                         min_sample_proportion = NULL,
                                         max_sample_proportion = NULL,
                                         min_samples = NULL,
                                         max_samples = NULL) {
  ft$core_microbiome(detection_limit = detection_limit,
                     min_sample_proportion = min_sample_proportion,
                     max_sample_proportion = max_sample_proportion,
                     min_samples = min_samples,
                     max_samples = max_samples)
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

################################################################################
#### shared data ###############################################################
################################################################################

# TODO docs: if you just have a list of feature names, you don't need these, you'd use keep directly.

#' @export
shared_features <- function(ft, method, other) {
  UseMethod("shared_features")
}

#' @export
shared_features.FeatureTable <- function(ft, method, other) {
  ft$shared_features(method, other)
}

#' @export
keep_shared_features <- function(ft, other) {
  UseMethod("keep_shared_features")
}

#' @export
keep_shared_features.FeatureTable <- function(ft, other) {
  ft$keep_shared_features(other)
}

#' @export
shared_feature_names <- function(ft, other) {
  UseMethod("shared_feature_names")
}

#' @export
shared_feature_names.FeatureTable <- function(ft, other) {
  ft$shared_feature_names(other)
}

################################################################################
#### plotting ##################################################################
################################################################################

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

