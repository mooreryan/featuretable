package_available <- function(name) {
  isTRUE(requireNamespace(name, quietly = TRUE))
}

package_unavailable <- Negate(package_available)