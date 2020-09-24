# FeatureTable

## Dependencies

### Required

- `R6`
- `rlang`

### Suggested

- If you want nice plots, I suggest you also install `ggplot2`, `ggrepel`, and `biplotr`.
- If you want fancy zero replacement, you should install `zCompositions`.
- If you need to import or export `phyloseq` objects, then you need to install `phyloseq`.
- If you want to build the vignettes yourself, you will need `knitr` and `rmarkdown`.  Also, some of the vignettes use `magrittr` to keep things clean.

### Test suite dependencies

- If you want to run the test suite locally, then you need to install `testthat`.
- Some of the tests require additional packages.  If you don't have them, those tests should be skipped.
  - Testing plotting functions requires `vdiffr`.
  - If you want to run the coverage report for yourself, then you will need to install `covr`.  
  - Some of the tests compare bespoke implementations of certain functions to their counterparts in `dplyr`, `tibble`, and `tidyr`.
  - Property tests require the `hedgehog` package.
  
## R v3 and v4 compatibility

- All the unit tests pass on R v3.6.3 (tested with [this](https://hub.docker.com/layers/mooreryan/featuretable_dev/v1--rocker-verse-3.6.3/images/sha256-e67882750a5abadcefe7fb7107ad4dbc924a2014f199a96abdd48c97172131f8?context=repo) Docker image) and v4.0.2 (tested with [this](https://hub.docker.com/layers/mooreryan/featuretable_dev/v1--rocker-verse-4.0.2/images/sha256-08a916b6bda371ffa8571286373a523dc46d14db9129701cc7a1d75d0c209cd6?context=repo) Docker image).
- Literally every time `data.frame` or `as.data.frame` is called in the `FeatureTable` code and tests , `stringsAsFactors = TRUE` is passed in as an argument.  
  - So, if you need a specific stable sorting of your sringly data, you need to set the factor levels yourself.
  - In theory, you should be fine passing in `data.frame`s that don't have their strings as factors, since `FeatureTable` will convert all strings to factors, but as of 2020-09-22, I haven't tested it.
  - This behavior may change in the future though!
- One thing to note is that `as.data.frame.FeatureTable` does not have a `stringsAsFactors` parameter.  This shouldn't be a problem as string-esque data is not allowed in the `data` field anyway.
