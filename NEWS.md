# posterior 1.1.0

### Enhancements

* use `matrixStats` to speed up convergence functions (#190) and 
`rvar` summaries (#200)

### Bug Fixes

* ensure that `as_draws_rvars()` works on lists of lists (#192)
* fix some vector recycling issues with `rvar_rng` (#195)
* ensure that `subset_draws()` respects input variable order, thanks to
Karl Dunkle Werner and Alexey Stukalov (#188)

### Other Changes

* No longer check for constant-per-chain input in effective
sample size diagnostics as this is overly conservative 
especially for `ess_tail`. (#198)


# posterior 1.0.1

* ensure that all unit tests pass on all CRAN environments
* fix a problem that sometimes lead to `rvar`s being unnecessarily slow (#179)


# posterior 1.0.0

* initial CRAN release


# posterior 0.1.0

* beta release
