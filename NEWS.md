# posterior 1.5.0

### Enhancements

* Added support for nested-Rhat via `rhat_nested` (#256)
* Added support for indexing draws in `rvar`s using `rvar`s (#282):
  * `x[i]` or `x[i] <- y` where `i` is a scalar logical `rvar` slices (or
    updates) `x` by its draws. Thus, if `y <- x[i]`, then `y` is the same
    shape as `x` but with `sum(i)` draws.
  * `x[[i]]` or `x[[i]] <- y` where `i` is a scalar numeric rvar slices (or
    updates) `x` by selecting the `i`th element within each corresponding draw.
    Thus, if `y <- x[[i]]`, then `y` is an `rvar` of length 1.
* Added `rvar_ifelse()`, which is a variant of `ifelse()` that accepts (and
  returns) `rvar`s (#282).
* Array broadcasting for `rvar`s has been made faster.

### Bug Fixes

* Ensure `rfun()` works with primitive functions (#290) and dots arguments (#291).
* Provide implementations of `vctrs::vec_proxy_equal()`, 
`vctrs::vec_proxy_compare()`, and `vctrs::vec_proxy_order()`.
* Minor future-proofing of `cbind(<rvar>)`, `rbind(<rvar>)`, and `chol(<rvar>)`
  for R 4.4 (#304).
* Ensure that `bind_draws(<draws_rvars>)` regenerates draw ids when binding along
  chains or draws; this also fixes a bug in `split_chains(<draws_rvars>)` (#300).


# posterior 1.4.1

### Bug Fixes

* Delay applying `tibble::num()` formatting to output from `summarise_draws()` 
  until `print()` is called so that summary output can be easily converted to a 
  vanilla data frame (#275).


# posterior 1.4.0

### Enhancements

* Added new `rvar_factor()` and `rvar_ordered()` subtypes of `rvar()` that work
  analogously to `factor()` and `ordered()` (#149). See the new section on
  `rvar_factor`s in `vignette("rvar")`.
* The `draws_df()`, `draws_list()`, and `draws_rvars()` formats now support
  discrete variables stored as `factors` / `ordered`s (or `rvar_factor`s /
  `rvar_ordered`s). If converted to formats that do not support discrete
  variables with named levels (`draws_matrix()` and `draws_array()`), 
  factor-like variables are converted to `numeric`s.
* Made `match()` and `%in%` generic and added support for `rvar`s to both
  functions.
* Added `modal_category()`, `entropy()`, and `dissent()` functions for
  summarizing discrete draws.
* Allow lists of draws objects to be passed as the first argument to 
  `bind_draws()` (#253).
* Improving formatting of `summarise_draws` output via `tibble::num`.
* `print.rvar()` and `format.rvar()` now default to a smaller number of
  significant digits in more cases, including when printing in data frames.
  This is controlled by the new `"posterior.digits"` option (see 
  `help("posterior-package")`).
* Implemented faster `vec_proxy.rvar()` and `vec_restore.rvar()`, improving
  performance of `rvar`s in `tibble`s (and elsewhere `vctrs` is used).

### Bug Fixes

* Ensure that `as_draws_rvars()` preserves dimensions of length-1 arrays (#265).
* Fix some minor compatibility issues with `rvar`, `vctrs`, `dplyr`, and
  `ggplot2` (#267, #269).


# posterior 1.3.1

* Minor release that fixes some CRAN check failures.


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
