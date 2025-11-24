# Changelog

## posterior 1.6.1

CRAN release: 2025-02-27

#### Bug Fixes

- Fix a test issue that led to an R CMD check failure on R devel.

#### Enhancements

- Convert lists of matrices to `draws_array` objects.
- Improve the documentation in various places.
- Implement
  [`pit()`](https://mc-stan.org/posterior/dev/reference/pit.md) for
  draws and rvar objects. LOO-PIT can be computed using `weights`.

## posterior 1.6.0

CRAN release: 2024-07-03

#### Enhancements

- Add `exclude` option to
  [`subset_draws()`](https://mc-stan.org/posterior/dev/reference/subset_draws.md),
  which can be used to exclude the matched selection.
- Add `are_log_weights` option to
  [`pareto_smooth()`](https://mc-stan.org/posterior/dev/reference/pareto_smooth.md),
  which is necessary for correct Pareto smoothing computation if the
  input vector consists of log weights.
- Add `pareto_smooth` option to
  [`weight_draws()`](https://mc-stan.org/posterior/dev/reference/weight_draws.md),
  to Pareto smooth weights before adding to a draws object.
- Add individual Pareto diagnostic functions
  ([`pareto_khat()`](https://mc-stan.org/posterior/dev/reference/pareto_khat.md),
  [`pareto_khat_threshold()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md),
  [`pareto_min_ss()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md),
  [`pareto_convergence_rate()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md))
- [`thin_draws()`](https://mc-stan.org/posterior/dev/reference/thin_draws.md)
  now automatically thins draws based on ESS by default, and non-integer
  thinning is possible.
- Matrix multiplication of `rvar`s can now be done with the base matrix
  multiplication operator (`%*%`) instead of `%**%` in R \>= 4.3.
- [`variables()`](https://mc-stan.org/posterior/dev/reference/variables.md),
  `variables<-()`,
  [`set_variables()`](https://mc-stan.org/posterior/dev/reference/variables-set.md),
  and
  [`nvariables()`](https://mc-stan.org/posterior/dev/reference/variables.md)
  now support a `with_indices` argument, which determines whether
  variable names are retrieved/set with (`"x[1]"`, `"x[2]"` …) or
  without (`"x"`) indices
  ([\#208](https://github.com/stan-dev/posterior/issues/208)).
- Add
  [`extract_variable_array()`](https://mc-stan.org/posterior/dev/reference/extract_variable_array.md)
  function to extract variables with indices into arrays of iterations x
  chains x any remaining dimensions
  ([\#340](https://github.com/stan-dev/posterior/issues/340)).
- For types that support `factor` variables (`draws_df`, `draws_list`,
  and `draws_rvars`),
  [`extract_variable()`](https://mc-stan.org/posterior/dev/reference/extract_variable.md)
  and
  [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md)
  can now return `factor`s.

## posterior 1.5.0

CRAN release: 2023-10-31

#### Enhancements

- Added support for nested-Rhat via `rhat_nested`
  ([\#256](https://github.com/stan-dev/posterior/issues/256))
- Added support for indexing draws in `rvar`s using `rvar`s
  ([\#282](https://github.com/stan-dev/posterior/issues/282)):
  - `x[i]` or `x[i] <- y` where `i` is a scalar logical `rvar` slices
    (or updates) `x` by its draws. Thus, if `y <- x[i]`, then `y` is the
    same shape as `x` but with `sum(i)` draws.
  - `x[[i]]` or `x[[i]] <- y` where `i` is a scalar numeric rvar slices
    (or updates) `x` by selecting the `i`th element within each
    corresponding draw. Thus, if `y <- x[[i]]`, then `y` is an `rvar` of
    length 1.
- Added
  [`rvar_ifelse()`](https://mc-stan.org/posterior/dev/reference/rvar_ifelse.md),
  which is a variant of [`ifelse()`](https://rdrr.io/r/base/ifelse.html)
  that accepts (and returns) `rvar`s
  ([\#282](https://github.com/stan-dev/posterior/issues/282)).
- Array broadcasting for `rvar`s has been made faster.

#### Bug Fixes

- Ensure [`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md)
  works with primitive functions
  ([\#290](https://github.com/stan-dev/posterior/issues/290)) and dots
  arguments ([\#291](https://github.com/stan-dev/posterior/issues/291)).
- Provide implementations of
  [`vctrs::vec_proxy_equal()`](https://vctrs.r-lib.org/reference/vec_proxy_equal.html),
  [`vctrs::vec_proxy_compare()`](https://vctrs.r-lib.org/reference/vec_proxy_compare.html),
  and
  [`vctrs::vec_proxy_order()`](https://vctrs.r-lib.org/reference/vec_proxy_compare.html).
- Minor future-proofing of `cbind(<rvar>)`, `rbind(<rvar>)`, and
  `chol(<rvar>)` for R 4.4
  ([\#304](https://github.com/stan-dev/posterior/issues/304)).
- Ensure that `bind_draws(<draws_rvars>)` regenerates draw ids when
  binding along chains or draws; this also fixes a bug in
  `split_chains(<draws_rvars>)`
  ([\#300](https://github.com/stan-dev/posterior/issues/300)).

## posterior 1.4.1

CRAN release: 2023-03-14

#### Bug Fixes

- Delay applying
  [`tibble::num()`](https://tibble.tidyverse.org/reference/num.html)
  formatting to output from
  [`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
  until [`print()`](https://rdrr.io/r/base/print.html) is called so that
  summary output can be easily converted to a vanilla data frame
  ([\#275](https://github.com/stan-dev/posterior/issues/275)).

## posterior 1.4.0

CRAN release: 2023-02-22

#### Enhancements

- Added new
  [`rvar_factor()`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
  and
  [`rvar_ordered()`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
  subtypes of
  [`rvar()`](https://mc-stan.org/posterior/dev/reference/rvar.md) that
  work analogously to [`factor()`](https://rdrr.io/r/base/factor.html)
  and [`ordered()`](https://rdrr.io/r/base/factor.html)
  ([\#149](https://github.com/stan-dev/posterior/issues/149)). See the
  new section on `rvar_factor`s in
  [`vignette("rvar")`](https://mc-stan.org/posterior/dev/articles/rvar.md).
- The
  [`draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md),
  [`draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md),
  and
  [`draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)
  formats now support discrete variables stored as `factors` /
  `ordered`s (or `rvar_factor`s / `rvar_ordered`s). If converted to
  formats that do not support discrete variables with named levels
  ([`draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
  and
  [`draws_array()`](https://mc-stan.org/posterior/dev/reference/draws_array.md)),
  factor-like variables are converted to `numeric`s.
- Made [`match()`](https://mc-stan.org/posterior/dev/reference/match.md)
  and `%in%` generic and added support for `rvar`s to both functions.
- Added
  [`modal_category()`](https://mc-stan.org/posterior/dev/reference/modal_category.md),
  [`entropy()`](https://mc-stan.org/posterior/dev/reference/entropy.md),
  and
  [`dissent()`](https://mc-stan.org/posterior/dev/reference/dissent.md)
  functions for summarizing discrete draws.
- Allow lists of draws objects to be passed as the first argument to
  [`bind_draws()`](https://mc-stan.org/posterior/dev/reference/bind_draws.md)
  ([\#253](https://github.com/stan-dev/posterior/issues/253)).
- Improving formatting of `summarise_draws` output via
  [`tibble::num`](https://tibble.tidyverse.org/reference/num.html).
- [`print.rvar()`](https://mc-stan.org/posterior/dev/reference/print.rvar.md)
  and
  [`format.rvar()`](https://mc-stan.org/posterior/dev/reference/print.rvar.md)
  now default to a smaller number of significant digits in more cases,
  including when printing in data frames. This is controlled by the new
  `"posterior.digits"` option (see
  [`help("posterior-package")`](https://mc-stan.org/posterior/dev/reference/posterior-package.md)).
- Implemented faster `vec_proxy.rvar()` and `vec_restore.rvar()`,
  improving performance of `rvar`s in `tibble`s (and elsewhere `vctrs`
  is used).

#### Bug Fixes

- Ensure that
  [`as_draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)
  preserves dimensions of length-1 arrays
  ([\#265](https://github.com/stan-dev/posterior/issues/265)).
- Fix some minor compatibility issues with `rvar`, `vctrs`, `dplyr`, and
  `ggplot2` ([\#267](https://github.com/stan-dev/posterior/issues/267),
  [\#269](https://github.com/stan-dev/posterior/issues/269)).

## posterior 1.3.1

CRAN release: 2022-09-06

- Minor release that fixes some CRAN check failures.

## posterior 1.3.0

CRAN release: 2022-08-15

#### Enhancements

- Implement `for_each_draw(x, expr)`, which executes `expr` once for
  each draw of `x`, exposing variables in `x` as arrays of the shape
  implied by the indices in their names
  ([\#224](https://github.com/stan-dev/posterior/issues/224)).
- Implement
  [`subset_draws()`](https://mc-stan.org/posterior/dev/reference/subset_draws.md),
  [`thin_draws()`](https://mc-stan.org/posterior/dev/reference/thin_draws.md),
  and
  [`resample_draws()`](https://mc-stan.org/posterior/dev/reference/resample_draws.md)
  for `rvar`s
  ([\#225](https://github.com/stan-dev/posterior/issues/225)).
- Allow `weights` to be optional in
  [`resample_draws()`](https://mc-stan.org/posterior/dev/reference/resample_draws.md)
  ([\#225](https://github.com/stan-dev/posterior/issues/225)).
- Add an implementation of [`drop()`](https://rdrr.io/r/base/drop.html)
  for `rvar`s.
- Speed up subsetting of `draws_list` objects.
  ([\#229](https://github.com/stan-dev/posterior/issues/229),
  [\#250](https://github.com/stan-dev/posterior/issues/250))

#### Bug Fixes

- Support remaining modes of
  [`diag()`](https://rdrr.io/r/base/diag.html) for `rvar`s
  ([\#246](https://github.com/stan-dev/posterior/issues/246)).
- Better parsing for named indices in
  [`as_draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md),
  including nested use of `[`, like `x[y[1],2]`
  ([\#243](https://github.com/stan-dev/posterior/issues/243)).
- Allow 0-length `rvar`s with `ndraws() > 1`
  ([\#242](https://github.com/stan-dev/posterior/issues/242)).
- Ensure 0-length `rvar`s can be cast to `draws` formats
  ([\#242](https://github.com/stan-dev/posterior/issues/242)).
- Don’t treat length-1 `rvar`s with more than 1 dimension as scalars
  when casting to other formats
  ([\#248](https://github.com/stan-dev/posterior/issues/248)).

## posterior 1.2.2

CRAN release: 2022-06-09

#### Enhancements

- Improve the `mcse_sd` function to not make a normality assumption.
  ([\#232](https://github.com/stan-dev/posterior/issues/232))

## posterior 1.2.1

CRAN release: 2022-03-07

#### Bug Fixes

- Correctly transform lists of data.frames into `draws_list` objects.
- Correctly drop variables on assigning `NULL` in `mutate_variables`.
  ([\#222](https://github.com/stan-dev/posterior/issues/222))

## posterior 1.2.0

CRAN release: 2022-01-05

#### Enhancements

- support casting to/from `rvar` and
  [`distributional::dist_sample`](https://pkg.mitchelloharawild.com/distributional/reference/dist_sample.html)
  ([\#109](https://github.com/stan-dev/posterior/issues/109))

#### Bug Fixes

- fix hidden variables in `bind_draws.draws_df` when binding more than
  two objects thanks to Jouni Helske
  ([\#204](https://github.com/stan-dev/posterior/issues/204))
- fix output of
  [`pillar::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html)
  when used on a data frame containing `rvar`s
  ([\#210](https://github.com/stan-dev/posterior/issues/210))
- drop `"draws"` and `"draws_df"` classes from `draws_df` objects if
  meta data columns are removed by a `dplyr` operation
  ([\#202](https://github.com/stan-dev/posterior/issues/202))
- fix output of
  [`print.draws_df()`](https://mc-stan.org/posterior/dev/reference/print.draws_df.md)
  on objects with unrepaired draws
  ([\#217](https://github.com/stan-dev/posterior/issues/217))
- ensure
  [`variance()`](https://pkg.mitchelloharawild.com/distributional/reference/variance.html)
  works properly with
  [`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
  ([\#219](https://github.com/stan-dev/posterior/issues/219))

## posterior 1.1.0

CRAN release: 2021-09-09

#### Enhancements

- use `matrixStats` to speed up convergence functions
  ([\#190](https://github.com/stan-dev/posterior/issues/190)) and `rvar`
  summaries ([\#200](https://github.com/stan-dev/posterior/issues/200))

#### Bug Fixes

- ensure that
  [`as_draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)
  works on lists of lists
  ([\#192](https://github.com/stan-dev/posterior/issues/192))
- fix some vector recycling issues with `rvar_rng`
  ([\#195](https://github.com/stan-dev/posterior/issues/195))
- ensure that
  [`subset_draws()`](https://mc-stan.org/posterior/dev/reference/subset_draws.md)
  respects input variable order, thanks to Karl Dunkle Werner and Alexey
  Stukalov ([\#188](https://github.com/stan-dev/posterior/issues/188))

#### Other Changes

- No longer check for constant-per-chain input in effective sample size
  diagnostics as this is overly conservative especially for `ess_tail`.
  ([\#198](https://github.com/stan-dev/posterior/issues/198))

## posterior 1.0.1

CRAN release: 2021-07-14

- ensure that all unit tests pass on all CRAN environments
- fix a problem that sometimes lead to `rvar`s being unnecessarily slow
  ([\#179](https://github.com/stan-dev/posterior/issues/179))

## posterior 1.0.0

CRAN release: 2021-07-13

- initial CRAN release

## posterior 0.1.0

- beta release
