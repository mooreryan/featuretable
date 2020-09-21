if (isTRUE(requireNamespace("hedgehog", quietly = TRUE))) {
  # TODO include random generation of dimnames
  gen_matrix_of <- function(dims) {
    nrow <- dims[[1]]
    ncol <- dims[[2]]

    hedgehog::gen.structure(
      hedgehog::gen.c(of = nrow * ncol,
                      hedgehog::gen.element(seq(from = -10, to = 10, by = 0.5))),
                  dim = c(nrow, ncol),
                  dimnames = list(Samples = paste0("Sample_", 1:nrow),
                                  Features = paste0("Feature_", 1:ncol))
    )
  }

  gen_no_dimnames_matrix_of <- function(dims) {
    nrow <- dims[[1]]
    ncol <- dims[[2]]

    hedgehog::gen.structure(
      hedgehog::gen.c(of = nrow * ncol,
                      hedgehog::gen.element(seq(from = -10, to = 10, by = 0.5))),
                  dim = c(nrow, ncol)
    )
  }

  gen_dims <- hedgehog::gen.sample(1:8, size = 2, replace = TRUE)

  gen_data_with_dimnames <- hedgehog::gen.and_then(gen_dims, gen_matrix_of)
  gen_data_no_dimnames <- hedgehog::gen.and_then(gen_dims, gen_no_dimnames_matrix_of)

  # This way is SLOW, but actually tells you the assertion that fails.
  basic_tests <- list(
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$data, dat)
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$dim(), dim(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$nrow(), nrow(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$nsamples(), nrow(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$num_samples(), nrow(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$nobservations(), nrow(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$num_observations(), nrow(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$sample_names(), rownames(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$observation_names(), rownames(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$ncol(), ncol(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$nfeatures(), ncol(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$num_features(), ncol(dat))
    },
    function(dat) {
      ft <- FeatureTable$new(dat)
      expect_equal(ft$feature_names(), colnames(dat))
    }
  )

  test_that("basic feature table stuff works", {
    lapply(basic_tests, function(test_fn) {
      hedgehog::forall(gen_data_with_dimnames, function(dat) {
        test_fn(dat)
      })
    })
  })

  test_that("input data with no dimnames works", {
    lapply(basic_tests, function(test_fn) {
      hedgehog::forall(gen_data_no_dimnames, function(dat) {
        test_fn(dat)
      })
    })
  })
}
