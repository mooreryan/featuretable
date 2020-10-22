# Version updates and changes

## featuretable v0.0.10

- Remove `reduce` functions.  They were more or less redundant with the `apply` 
  functions.
- Update a *lot* of documentation!

## featuretable v0.0.9

- Fix the S3 function exporting
- Update data and add Lee citation
- Add some microbiome overview, plotting, and R6/S3 function vignettes
- Add `ggrepel` to suggests
- Fix some of the plot unit tests so that it takes in package data rather than depend on `DivNet`

## featuretable v0.0.8

- Update everything so it works with both R versions 3 and 4.
- This works by basically treating all strings as factors within `FeatureTable`
  code and test suite.

## featuretable v0.0.7

- Add property and integration tests
- Add `relative_abundance` helper
- Add vignette about summarizing `FeatureTables` with plots
- Handle lots of edge cases involving factors and NAs in the collapse functions
- Add `keep_hierarchy` parameter to the collapse functions

## featuretable v0.0.6

- Fix some bugs in the `plot` function
- Add `other_feature_name` parameter to `plot` function

## featuretable v0.0.5

- Add some docs and tests
- Fix some bugs, imports, and suggests
- Drop variable prefix in `collapse` function
- Add `wide_to_long` helper function
- Add R6 and s3 plotting functions

## featuretable v0.0.4

- Add `collapse_samples`.
- Add help docs for `collapse` functions.

## featuretable v0.0.3

- Add lots of docs and examples!
- Add another data set to use in examples (available with: `data(ft)`).

## featuretable v0.0.2

- Add some fancy stuff to the `keep` functions: `query` and `restrict`.  These
  are used to mix data and metadata queries into a single predicate, and to
  restrict data queries to subsets of samples.  Currently, they are only
  available in when keeping `features`.
- Add methods to collapse features based on metadata.
- Add various data utilities and helpers.

## featuretable v0.0.1

The first version!
