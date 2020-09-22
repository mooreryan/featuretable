# Version updates and changes

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