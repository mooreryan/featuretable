
# Like `apply`, it coerces X to a matrix first.
# FUN should take two arguments, whatever the MARGIN would have, plus the index.
apply_with_wrapper <- function(which, X, MARGIN, FUN, ...) {
  if (length(MARGIN) != 1) {
    rlang::abort("MARGIN must be 1 or 2 (not a vector).", class = Error$ArgumentError)
  }

  if (MARGIN != 1 && MARGIN != 2) {
    rlang::abort("MARGIN must be 1 or 2.", class = Error$ArgumentError)
  }

  if (length(dim(X)) != 2) {
    rlang::abort("dim(X) must be 2. Other stuff not yet implemented!",
                 class = Error$ArgumentError)
  } else {
    X <- as.matrix(X)
  }

  if (MARGIN == 1) {
    # By rows.
    if (which == "index") {
      indices <- seq_len(nrow(X))
    } else if (which == "name") {
      indices <- rownames(X)

      if (is.null(indices)) {
        # Raise error as this is likely NOT what the user intended.
        rlang::abort("Tried to apply_with_name on margin 1 of object with no rownames",
                     class = Error$Error)
      }
    } else {
      rlang::abort("should be impossible", class = Error$ImpossibleConditionError)
    }

    result <- sapply(indices, function(i) {
      dat <- X[i, ]

      FUN(dat, i, ...)
    })

    if (is.null(dim(result))) {
      # vector type result....reducer type function
      if (is.null(names(result)) && length(result) == nrow(X) && !is.null(rownames(X))) {
        names(result) <- rownames(X)
      }
    } else {
      if (nrow(result) == ncol(X) && ncol(result) == nrow(X)) {
        if (!is.null(rownames(X)) && !is.null(colnames(X))) {
          # This bit of code is tricky, but we want to get the same named dimnames if the dimnames are in fact named.
          # Technically, I could just transpose X and get the dimnames of that.

          tmp <- list()

          margin_2_orig_name <- names(dimnames(X)[2])
          if (is.null(margin_2_orig_name)) {
            tmp[[1]] <- dimnames(X)[[2]]
          } else {
            tmp[[margin_2_orig_name]] <- dimnames(X)[[2]]
          }

          margin_1_orig_name <- names(dimnames(X)[1])
          if (is.null(margin_1_orig_name)) {
            tmp[[2]] <- dimnames(X)[[1]]
          } else {
            tmp[[margin_1_orig_name]] <- dimnames(X)[[1]]
          }

          dimnames(result) <- tmp
        }
      } else {
        rlang::abort("should be impossible", class = Error$ImpossibleConditionError)
      }
    }

    result
  } else if (MARGIN == 2) {
    # By cols.
    if (which == "index") {
      indices <- seq_len(ncol(X))
    } else if (which == "name") {
      indices <- colnames(X)

      if (is.null(indices)) {
        # Raise error as this is likely NOT what the user intended.
        rlang::abort("Tried to apply_with_name on margin 2 of object with no colnames",
                     class = Error$Error)
      }
    } else {
      rlang::abort("should be impossible", error = Error$ImpossibleConditionError)
    }

    result <- sapply(indices, function(j) {
      dat <- X[, j]

      FUN(dat, j, ...)
    })

    if (is.null(dim(result))) {
      # vector type result ...reducer type function
      if (is.null(names(result)) && length(result) == ncol(X) && !is.null(colnames(X))) {
        names(result) <- colnames(X)
      }
    } else {
      if (isTRUE(all.equal(dim(result), dim(X)))) {
        dimnames(result) <- dimnames(X)
      }
    }

    result
  } else {
    rlang::abort("MARGIN must be 1 or 2.", class = Error$ArgumentError)
  }
}

apply_with_index <- function(X, MARGIN, FUN, ...) {
  apply_with_wrapper("index", X, MARGIN, FUN, ...)
}

apply_with_name <- function(X, MARGIN, FUN, ...) {
  apply_with_wrapper("name", X, MARGIN, FUN, ...)
}