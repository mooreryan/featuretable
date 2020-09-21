# FeatureTable

## Dependencies

### Required

- `R6`
- `rlang`

### Suggested

- If you want nice plots, I suggest you also install `ggplot2` and `biplotr`.
- If you want fancy zero replacement, you should install `zCompositions`.
- If you need to import or export `phyloseq` objects, then you need to install `phyloseq`.
- If you want to build the vignettes yourself, you will need `knitr` and `rmarkdown`.

### Test suite dependencies

- If you want to run the test suite locally, then you need to install `testthat`.
- Some of the tests require additional packages.  If you don't have them, those tests should be skipped.
  - Testing plotting functions requires `vdiffr`.
  - If you want to run the coverage report for yourself, then you will need to install `covr`.  
  - Some of the tests compare bespoke implementations of certain functions to their counterparts in `dplyr`, `tibble`, and `tidyr`.
  - Property tests require the `hedgehog` package.