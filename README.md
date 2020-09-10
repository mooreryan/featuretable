# FeatureTable

## To do

- [ ] Test general bogus input...like `ft$keep("features", Arstoien == "green")`
- [ ] Accept `observations` as a valid margin.
- [ ] `*_with_data` functions to give apply functions direct access to data
- [ ] most function documentation is only in the s3 functions...add it to the R6
  methods as well
- [ ] hierarchical `feature_data`
- [ ] how to handle collapse for numeric data?
- [ ] handle user manually changing data and getting things in an incosistent state (e.g., if you have a factor, and user changes things around and it's no longer a factor, or if you have a factor and the user changes one of the entries in `feature_data` or `sample_data` and a factor level drops out, `collapse` will break)

### Keep

- [ ] what if you pass optional arguments to a non-function predicate?
- [ ] move most of the exposition out of the `?keep` examples and into a
  vignette
  