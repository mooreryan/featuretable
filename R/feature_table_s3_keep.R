################################################################################
#### keep ######################################################################
################################################################################

# FeatureTable s3 functions for `keep`

#' Keep samples/observations/rows or features/columns using a predicate.
#'
#' @description
#' Keep samples/observations/rows or features/columns based on result of a
#' predicate function or expression.
#'
#' @param ft A FeatureTable.
#' @param margin Margin to apply the predicate over.  E.g., \code{1} or
#'   \code{"samples"} indicates rows, \code{2} or \code{"features"} indicates
#'   columns.
#' @param predicate The predicate function or expression to be applied.  Only
#'   those elements where \code{predicate} is \code{TRUE} or evaluates to
#'   \code{TRUE} will be kept.
#' @param ... Optional arguments to \code{predicate} (if \code{predicate} is a
#'   function).
#'
#' @return A new FeatureTable with the elements that were kept.
#' 
#' @details 
#' A \code{predicate} can be a function that evaluates to \code{TRUE}, or
#' \code{FALSE} for each item it is applied to, or it can be a logical vector
#' the same length as the \code{margin} you're working with, or it can be some
#' other expresssion/call that will evaluate to a logical vector.  See Examples
#' for more information.
#' 
#' Note that `query` is currently only available for filtering features.  See 
#' Examples for details.
#' 
#' @examples 
#' data(ft)
#' 
#' #### Filtering based on metadata ####
#' 
#' # First, let me show you how to filter based on metadata (i.e., the stuff in
#' # `$feature_data` or `$sample_data`).  Note that this kind of filtering works
#' # the same for samples.
#'
#' # Here is what the `feature_data` looks like.
#' #
#' # > ft$feature_data
#' #           Color  Shape Length
#' # Feature_1   red square    5.0
#' # Feature_2   red circle    6.0
#' # Feature_3   red square    2.3
#' # Feature_4  blue   <NA>    7.0
#' # Feature_5  blue circle   10.0
#' 
#' # To keep features, you can use either `keep` and specify the correct margin, 
#' # or use the `keep_features` helper function.  For the examples that follow, 
#' # I will show you both ways, but of course, you can pick whichever one you 
#' # prefer!
#'
#' # Keep features that have 'circle' in the `feature_data`.  Note that you can
#' # refer to variables/colnames in the data directly inside the `keep` 
#' # functions!
#' #
#' ft$keep("features", Shape == "circle")
#' ft$keep_features(Shape != "circle")
#' 
#' # You can use a logical vector directly.
#' #
#' ft$keep("features", c(TRUE, TRUE, FALSE, TRUE, TRUE))
#' ft$keep_features(c(TRUE, TRUE, FALSE, TRUE, TRUE))
#' 
#' # Or you can use any expressions that will evaluate to a logical vector.
#' #
#' ft$keep("features", ft$feature_data$Shape == "circle")
#' ft$keep_features(ft$feature_data$Shape == "circle")
#'
#' # Keep features that do NOT have 'circle' in the `feature_data`.  
#' #
#' ft$keep("features", Shape != "circle")
#' ft$keep_features(Shape != "circle")
#' 
#' # You can also have more complicated expressions for predicates.
#' #
#' # Keep the red circles
#' ft$keep("features", Shape == "circle" & Color == "red")
#' ft$keep_features(Shape == "circle" & Color == "red")
#' 
#' # Keep any features that are 'red' OR 'circles'.
#' ft$keep("features", Shape == "circle" | Color == "red")
#' ft$keep_features(Shape == "circle" | Color == "red")
#' 
#' # Of course, you can keep adding on expressions....
#' ft$keep("features", (Shape == "square" & Color == "red") | Length > 5)
#' ft$keep_features((Shape == "square" & Color == "red") | Length > 5)
#' 
#' #### Filtering based on actual data (i.e., in `$data`) ####
#' 
#' # Sometimes you want to filter features or samples based on the actual data 
#' # (i.e., the stuff in `$data`), rather than on the metadata (i.e., the stuff
#' # in `$feature_data` or `$sample_data`).  No problem!  For that, we will use
#' # functions that apply over the margin we want.
#' 
#' # For reference, here is the data:
#' # 
#' # > ft$data
#' #           Features
#' # Samples    Feature_1 Feature_2 Feature_3 Feature_4 Feature_5
#' #   Sample_1         0         0         0         1        10
#' #   Sample_2         0         0         1         2        20
#' #   Sample_3         0         1         2         3        30
#' #   Sample_4         1         2         3         4        40
#'
#' # Keep features whose sum is more than 5.
#' ft$keep("features", function(feature) sum(feature) > 5)
#' ft$keep_features(function(feature) sum(feature) > 5)
#' 
#' # Keep samples whose sum is more than 25.
#' ft$keep("samples", function(sample) sum(sample) > 5)
#' ft$keep_samples(function(sample) sum(sample) > 5)
#' 
#' # Note that there is nothing special about using `sample` or `feature` as a 
#' # parameter to those anonymous functions.  I could have used `x` or anything 
#' # else.
#' 
#' #### Predicates that work on data and metadata together! ####
#' 
#' # What if you want to filter based on both metadata and the actual data?  
#' # For that, you need to use `query`.  It is a special function only 
#' # accessible from inside the keep functions. (Note that currently, you can 
#' # only use `query` from `keep` with margin 'features', or keep_features.)
#' 
#' # Keep features that are circles and also have an abundance of > 5.
#' ft$keep("features", 
#'         query(Shape == "circle") & query(function(feature) sum(feature) > 5))
#' ft$keep_features(
#'   query(Shape == "circle") & query(function(feature) sum(feature) > 5)
#' )
#' 
#' # Notice how I wrapped both expressions in a `query` function.  You can also 
#' # have more than two queries just like before.
#' 
#' # One slightly weird thing is that the expression that evalautes w.r.t. the 
#' # metadata doesn't actually need to be wrapped in a query.  For example, this
#' # will also work:
#' #
#' ft$keep_features(
#'   Shape == "circle" & query(function(feature) sum(feature) > 5)
#' )
#' 
#' # But I would consider that a quirk of the implementation and not rely on it, 
#' # as it may change in the future.
#' 
#' #### Filtering on actual data, feature data, and sample data together ####
#' 
#' # For reference, here is the sample data:
#' # > ft$sample_data
#' #           Location Season SnazzyFactor
#' #  Sample_1    Spain Summer           10
#' #  Sample_2    Spain Summer           12
#' #  Sample_3 Portugal Winter           25
#' #  Sample_4    Spain Winter            3 
#' 
#' # (Note that like the above section, this only works for features currently.)
#' #
#' # Here's something cool.  Let's say you wanted to filter features based on 
#' # their abundance, some bit of metadata about them (e.g., taxonomy) but 
#' # restrict your search to a subset of samples (say, all samples from Summer, 
#' # or all samples from Spain).  You can do that with the `query` function as 
#' # well!  Let me show you what I mean....
#'
#' # Keep features whose abundance is > 5 in 'Winter' samples (i.e., you don't 
#' # care what there abundance in other seasons, as long as they fit the 
#' # criteria for the 'Winter' samples.)
#' 
#' ft$keep("features", query(function(feature) sum(feature) > 5,
#'                           restrict = Season == "Winter"))
#' 
#' # Notice the use of the `restrict` parameter.  It takes expressions that 
#' # evaluate in the context of the sample data.
#' 
#' # And you can make more complicated queries if you want.
#' ft$keep_features(query(Shape == "circle") |
#'                    query(function(feature) sum(feature) > 5,
#'                          restrict = Season == "Winter"))
#' 
#' # Here is something that may trip you up.
#' 
#' \dontrun{
#'   ft$keep_features(query(Shape == "circle", restrict = Season == "Winter"))
#' }
#' 
#' # The above code will raise an `ArgumentError`.  Here's the thing: feature 
#' # metadata does not change based on the sample metadata.  In other words, a 
#' # feature is a 'circle' or a 'square' regardless of whether it is in a 
#' # 'Summer' sample, a 'Winter' sample, or both.  So restricting a metadata 
#' # query to a subset of samples just doesn't make any sense.
#' #
#' # If you find yourself wanting to do that, my guess is what you really want 
#' # are to filter features whose 'Shape' is 'circle' and that are actually 
#' # present in 'Winter' samples.  If that is the case, you want to do something
#' # like this:
#' #
#' ft$keep_features(
#'   query(Shape == "circle") &
#'     query(function(feature) sum(feature) > 0,
#'           restrict = Season == "Winter")
#' )
#'
#' @export
keep <- function(ft, ...) {
  UseMethod("keep")
}

keep.FeatureTable <- function(ft, ...) {
  ft$keep(...)
}

#' @rdname keep
#' @export
keep_features <- function(ft, ...) {
  UseMethod("keep_features")
}

#' @export
keep_features.FeatureTable <- function(ft, ...) {
  ft$keep_features(...)
}

#' @rdname keep
#' @export
keep_samples <- function(ft, ...) {
  UseMethod("keep_samples")
}

#' @export
keep_samples.FeatureTable <- function(ft, ...) {
  ft$keep_samples(...)
}
