# FeatureTable

[![R-CMD-check](https://github.com/mooreryan/featuretable/actions/workflows/check-standard.yaml/badge.svg?branch=main)](https://github.com/mooreryan/featuretable/actions/workflows/check-standard.yaml)

*Note:  I still consider FeatureTable to be in the pre-release stage.  The code itself is well tested and I use it in my own research, but the API has not stabalized and is still subject to breaking changes when I cut a new release.*

## Installing

Go to the [Releases page](https://github.com/mooreryan/featuretable/releases) and click on the latest release.  Follow the instructions there to install.  There are few strictly required dependencies, but some features will only work if you install some extras.  See the [dependencies section of this readme](https://github.com/mooreryan/featuretable#dependencies).

If you are an R developer, feel free to clone the repository and use developer tools to build and use the package.

## Usage

### Vignettes

`FeatureTable` comes with a few detailed vignettes describing its usage.  After installing, you can view them with the following command:

```
browseVignettes("featuretable")
```

### Function docs

Additionally, each function has a lot of info in the help section.  You can run `?featuretable` to get an overview of everything.  Click on the links in there to get to individual function docs.  You can also access function docs with the `?`, e.g., `?keep_samples`.

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

`R CMD CHECK`, which includes unit tests, are run via GitHub CI for the following versions:

  - Latest release
  - 4.1.3
  - 4.0.5
  - 3.6.3
  - 3.5.3

So as long as that is passing (see the badge at the top of the README), then you can be (pretty) sure the package works with both R v3 and v4.

Notes:

- Each time `data.frame` or `as.data.frame` is called in the `FeatureTable` code and tests , `stringsAsFactors = TRUE` is passed in as an argument.  
  - So, if you need a specific stable sorting of your string-like data, you need to set the factor levels yourself.
  - In theory, you should be fine passing in `data.frame`s that don't have their strings as factors, since `FeatureTable` will convert all strings to factors, but I should probably add tests for this.
  - This behavior may change in the future though!
- One thing to note is that `as.data.frame.FeatureTable` does not have a `stringsAsFactors` parameter.  This shouldn't be a problem as string-esque data is not allowed in the `data` field anyway.

## Hacking

- If you make an update to the core R6 class, make sure you regenerate the `.rda` files in `data`, using the scripts in `data-raw`.
  - Note that generating the data files requires the [DivNet](https://github.com/adw96/DivNet) R package.
  - You can use the recipe in the `justfile` for this.

### Default branch is now main

The default branch is now `main`, and the `master` branch no longer exists.

If you have a local clone using `master` as the default branch, you can [update](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-branches-in-your-repository/renaming-a-branch) it by running the following commands.

```
git branch -m master main
git fetch origin
git branch -u origin/main main
git remote set-head origin -a
```

Optionally, run the following command to remove tracking references to the old branch name.

```
git remote prune origin
```
