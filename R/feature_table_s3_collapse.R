################################################################################
#### collapse ##################################################################
################################################################################

#' Collapse samples/observations/rows or features/columns based on metadata.
#'
#' @description
#' Collapse samples/observations/rows or features/columns based on metadata. For
#' features/samples, any feature/sample with the same metadata for selected
#' category will be grouped.  
#' 
#' @param ft A FeatureTable.
#' @param margin Margin to collapse.  E.g., \code{1} or \code{"samples"} 
#'   indicates rows, \code{2} or \code{"features"} indicates columns.
#' @param by The data column to collapse by.
#' @param keep_na Do you want to group all NAs together (TRUE) or drop them 
#' (FALSE, the defult)?
#' 
#' @return A new FeatureTable with the specified margin collapsed on the
#'   specified metadata column.
#' 
#' @details 
#' Grouping is done by summing counts for each category.
#' 
#' If you to keep features/samples with \code{NA} for the \code{by} category,
#' pass \code{keep_na = TRUE}.  Then the NA will become a new factor in the
#' collapsed data.
#' 
#' Currently, you can only collapse on one metadata column at a time :(
#' 
#' @examples 
#' data(ft)
#' 
#' # You can direcctly access variables in the metadata.
#' ft$collapse("features", Color)
#' ft$collapse_features(Color)
#' 
#' # Or refer to them by strings.
#' ft$collapse("features", "Color")
#' ft$collapse_features("Color")
#' 
#' # You can collapse samples on metadata as well.
#' ft$collapse("samples", Location)
#' ft$collapse_samples(Location)
#' 
#' # And you can use the s3 style functions.
#' collapse(ft, "samples", Location)
#' collapse_samples(ft, Location)
#' 
#' collapse(ft, "features", Shape)
#' collapse_featuse(ft, Shape)
#' 
#' # For now, you can't do more than one variable at a time.  Sorry!
#' \dontrun{
#'   ft$collapse_samples(c("Location", "Season"))
#' }
#' 
#' @export
collapse <- function(ft, ...) {
  UseMethod("collapse")
}

#' @export
collapse.FeatureTable <- function(ft, ...) {
  ft$collapse(...)
}

#' @rdname collapse
#' @export
collapse_features <- function(ft, ...) {
  UseMethod("collapse_features")
}

#' @export
collapse_features.FeatureTable <- function(ft, ...) {
  ft$collapse_features(...)
}

#' @rdname collapse
#' @export
collapse_samples <- function(ft, ...) {
  UseMethod("collapse_samples")
}

#' @export
collapse_samples.FeatureTable <- function(ft, ...) {
  ft$collapse_samples(...)
}
