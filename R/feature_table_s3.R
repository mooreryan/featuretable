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
map_features <- function(ft, fn, ...) {
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
map_features_with_index <- function(ft, fn, ...) {
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
map_features_with_name <- function(ft, fn, ...) {
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
map_samples <- function(ft, fn, ...) {
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
map_samples_with_index <- function(ft, fn, ...) {
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
map_samples_with_name <- function(ft, fn, ...) {
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
keep <- function(ft, ...) {
  UseMethod("keep")
}

keep.FeatureTable <- function(ft, ...) {
  ft$keep(...)
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

keep_features.FeatureTable <- function(ft, ...) {
  ft$keep_features(...)
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
keep_samples <- function(ft, ...) {
  UseMethod("keep_samples")
}

keep_samples.FeatureTable <- function(ft, ...) {
  ft$keep_samples(...)
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

is_count_table.FeatureTable <- function(ft) {
  ft$is_count_table()
}

################################################################################
#### basic CoDA ################################################################
################################################################################

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

clr.FeatureTable <- function(ft, base = 2) {
  ft$clr(base)
}

################################################################################
#### ordination ################################################################
################################################################################

pca_biplot <- function(ft, use_biplotr = FALSE, include_sample_data = FALSE, ...) {
  UseMethod("pca_biplot")
}

pca_biplot.FeatureTable <- function(ft, use_biplotr = FALSE, include_sample_data = FALSE, ...) {
  ft$pca_biplot(use_biplotr = use_biplotr, include_sample_data = include_sample_data, ...)
}

################################################################################
#### importing data ############################################################
################################################################################

ft_from <- function(x, ...) {
  UseMethod("ft_from")
}

# normally from a phyloseq object, the tax table wouldn't have numeric values.  BUT if it comes from FT => phy => FT it could.  but phyloseq taxtable does NOT play well with numebrs.  So user must specify any numeric feature_data columns manually :(
ft_from.phyloseq <- function(x, numeric_feature_data_columns = NULL, ...) {
  factor_to_numeric <- function(f) as.numeric(levels(f))[f]

  ct <-  as.matrix(as.data.frame(phyloseq::otu_table(x, phyloseq::taxa_are_rows(x))))

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
  feature_dat <- as.data.frame(apply(feature_dat, 2, function(x) {
    attr(x, "names") <- NULL

    x
  }))
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
#### merge #####################################################################
################################################################################

# It seems a bit hackish to reuse merge in a way that is def NOT how the base generic function works.

#' @export
merge.FeatureTable <- function(ft, ...) {
  ft$merge(...)
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