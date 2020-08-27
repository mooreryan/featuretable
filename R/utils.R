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
