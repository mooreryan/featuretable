################################################################################
#### exporting data ############################################################
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
