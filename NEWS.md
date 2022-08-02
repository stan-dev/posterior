# posterior 1.3.0

### Enhancements

* Implement `for_each_draw(x, expr)`, which executes `expr` once for each draw
  of `x`, exposing variables in `x` as arrays of the shape implied by the
  indices in their names (#224).
* Implement `subset_draws()`, `thin_draws()`, and `resample_draws()` for `rvar`s (#225).
* Allow `weights` to be optional in `resample_draws()` (#225).
* Add an implementation of `drop()` for `rvar`s.
* Speed up subsetting of `draws_list` objects. (#229, #250)

### Bug Fixes

* Support remaining modes of `diag()` for `rvar`s (#246).
* Better parsing for named indices in `as_draws_rvars()`, including nested use
of `[`, like `x[y[1],2]` (#243).
* Allow 0-length `rvar`s with `ndraws() > 1` (#242). 
* Ensure 0-length `rvar`s can be cast to `draws` formats (#242).
* Don't treat length-1 `rvar`s with more than 1 dimension as scalars when 
casting to other formats (#248).


# posterior 1.2.2

### Enhancements

* Improve the `mcse_sd` function to not make a normality assumption. (#232)


# posterior 1.2.1

### Bug Fixes

* Correctly transform lists of data.frames into `draws_list` objects.
* Correctly drop variables on assigning `NULL` in `mutate_variables`. (#222)


# posterior 1.2.0

### Enhancements

* support casting to/from `rvar` and `distributional::dist_sample` (#109)

### Bug Fixes

* fix hidden variables in `bind_draws.draws_df` when binding 
more than two objects thanks to Jouni Helske (#204)
* fix output of `pillar::glimpse()` when used on a data frame containing 
`rvar`s (#210)
* drop `"draws"` and `"draws_df"` classes from `draws_df` objects if meta data
columns are removed by a `dplyr` operation (#202)
* fix output of `print.draws_df()` on objects with unrepaired draws (#217)
* ensure `variance()` works properly with `summarise_draws()` (#219)


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
