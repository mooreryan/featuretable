# Like `apply`, it coerces X to a matrix first.
# FUN should take two arguments, whatever the MARGIN would have, plus the index.
apply_with_index <- function(X, MARGIN, FUN, ...) {
  if (length(MARGIN) != 1) {
    stop("MARGIN must be a scaler (1 or 2).")
  }

  if (MARGIN != 1 && MARGIN != 2) {
    stop("MARGIN must be a scaler (1 or 2).")
  }

  if (length(dim(X)) != 2) {
    stop("dim(X) must be 2.  Other stuff not implemented!")
  } else {
    X <- as.matrix(X)
  }

  if (MARGIN == 1) {
    # By rows.
    indices <- seq_len(nrow(X))

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
        rlang::abort("this shouldn't happen", class = Error$ImpossibleConditionError)
      }
    }

    result
  } else if (MARGIN == 2) {
    # By cols.
    indices <- seq_len(ncol(X))

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
    stop("MARGIN must be 1 or 2.")
  }
}
