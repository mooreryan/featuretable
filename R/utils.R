package_available <- function(name) {
  isTRUE(requireNamespace(name, quietly = TRUE))
}

package_unavailable <- Negate(package_available)

# See `?integer` for more info.
is_whole_number <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}

is_natural_number <- function(x, tol = .Machine$double.eps ^ 0.5) {
  x >= 0 & is_whole_number(x, tol)
}

wide_to_long <- function(dat) {
  data.frame(
    Sample = rep(rownames(dat), times = ncol(dat)),
    Feature = as.vector(sapply(colnames(dat), function(name) rep(name, times = nrow(dat)))),
    # TODO pretty sure ft$data should always be a matrix.
    Value = as.vector(as.matrix(dat))
  )
}

# TODO test me
#' @export
relative_abundance <- function(sample, multiplier = 1) {
  sample / sum(sample, na.rm = TRUE) * multiplier
}


#### Wordy helpers

# TODO it may be better to make these local to the keep function, as there will be a lot of them
# and many don't make too much sense outside of that context.

#' @export
present <- function(x) {
  if (any(x < 0, na.rm = TRUE)) {
    rlang::abort("Negative values present. Present/absent functions don't make sense if there are negative values!",
                 class = Error$ArgumentError)
  }
  sum(x, na.rm = TRUE) > 0
}

#' @export
is_present <- present

#' @export
if_present <- present

#' @export
that_are_present <- present

#' @export
that_were_present <- present

#' @export
absent <- Negate(present)

#' @export
is_absent <- absent

#' @export
if_absent <- absent

#' @export
that_are_absent <- absent

#' @export
that_were_absent <- absent
