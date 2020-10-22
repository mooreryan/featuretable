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

#' @rdname is_count_table
#' @export
is_count_table.FeatureTable <- function(ft) {
  ft$is_count_table()
}

#' Replacing zeros.
#'
#' @details
#' If \code{use_cmultRepl = TRUE} but \code{zCompositions::cmultRepl} is not
#' availabel, it will raise an error.
#'
#' @param ft A FeatureTable
#' @param replacement (Ignored if \code{use_cmultRepl = TRUE})
#' @param tol (Ignored if \code{use_cmultRepl = TRUE})
#' @param use_cmultRepl TRUE/FALSE whether to use \code{cmultRepl} function.
#' @param ... Extra arguments (i.g., passed to \code{cmultRepl})
#'
#' @return A new FeatureTable with the zeros in \code{data} replaced.
#'
#' @seealso \code{\link{cmultRepl}}
#'
#' @export
replace_zeros <- function(ft, ...) {
  UseMethod("replace_zeros")
}

#' @rdname replace_zeros
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
#' @details
#' See \url{https://en.wikipedia.org/wiki/Compositional_data#Center_logratio_transform}.
#'
#' @param ft A Feature table.
#' @param base Base of logarithm.
#'
#' @return
#' A FeatureTable with the \code{data} transformed with centered log ratio.
#'
#' @export
clr <- function(ft, base = 2) {
  UseMethod("clr")
}

#' @rdname clr
#' @export
clr.FeatureTable <- function(ft, base = 2) {
  ft$clr(base)
}
