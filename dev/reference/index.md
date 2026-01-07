# Package index

## Overview

Package overview and global options

- [`posterior-package`](https://mc-stan.org/posterior/dev/reference/posterior-package.md)
  [`posterior`](https://mc-stan.org/posterior/dev/reference/posterior-package.md)
  : Tools for working with posterior (and prior) distributions

## Draws objects and formats

Create`draws` objects and convert between supported formats

- [`as_draws()`](https://mc-stan.org/posterior/dev/reference/draws.md)
  [`is_draws()`](https://mc-stan.org/posterior/dev/reference/draws.md) :

  Transform to `draws` objects

- [`as_draws_array()`](https://mc-stan.org/posterior/dev/reference/draws_array.md)
  [`draws_array()`](https://mc-stan.org/posterior/dev/reference/draws_array.md)
  [`is_draws_array()`](https://mc-stan.org/posterior/dev/reference/draws_array.md)
  :

  The `draws_array` format

- [`as_draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md)
  [`draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md)
  [`is_draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md)
  :

  The `draws_df` format

- [`as_draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md)
  [`draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md)
  [`is_draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md)
  :

  The `draws_list` format

- [`as_draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
  [`draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
  [`is_draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
  :

  The `draws_matrix` format

- [`as_draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)
  [`draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)
  [`is_draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)
  :

  The `draws_rvars` format

- [`print(`*`<draws_array>`*`)`](https://mc-stan.org/posterior/dev/reference/print.draws_array.md)
  :

  Print `draws_array` objects

- [`print(`*`<draws_df>`*`)`](https://mc-stan.org/posterior/dev/reference/print.draws_df.md)
  :

  Print `draws_df` objects

- [`print(`*`<draws_list>`*`)`](https://mc-stan.org/posterior/dev/reference/print.draws_list.md)
  :

  Print `draws_list` objects

- [`print(`*`<draws_matrix>`*`)`](https://mc-stan.org/posterior/dev/reference/print.draws_matrix.md)
  :

  Print `draws_matrix` objects

- [`print(`*`<draws_rvars>`*`)`](https://mc-stan.org/posterior/dev/reference/print.draws_rvars.md)
  :

  Print `draws_rvars` objects

- [`print(`*`<draws_summary>`*`)`](https://mc-stan.org/posterior/dev/reference/print.draws_summary.md)
  :

  Print summaries of `draws` objects

- [`print(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/print.rvar.md)
  [`format(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/print.rvar.md)
  [`str(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/print.rvar.md)
  : Print or format a random variable

- [`variables()`](https://mc-stan.org/posterior/dev/reference/variables.md)
  [`nvariables()`](https://mc-stan.org/posterior/dev/reference/variables.md)
  :

  Get variable names from `draws` objects

- [`example_draws()`](https://mc-stan.org/posterior/dev/reference/example_draws.md)
  :

  Example `draws` objects

- [`reserved_variables()`](https://mc-stan.org/posterior/dev/reference/reserved_variables.md)
  : Reserved variables

## Working with draws objects

Functions for modifying `draws` objects and extracting their contents

- [`bind_draws()`](https://mc-stan.org/posterior/dev/reference/bind_draws.md)
  :

  Bind `draws` objects together

- [`extract_list_of_variable_arrays()`](https://mc-stan.org/posterior/dev/reference/extract_list_of_variable_arrays.md)
  : Extract arrays of multiple variables

- [`extract_variable()`](https://mc-stan.org/posterior/dev/reference/extract_variable.md)
  : Extract draws of a single variable

- [`extract_variable_array()`](https://mc-stan.org/posterior/dev/reference/extract_variable_array.md)
  : Extract array of a single (possibly indexed) variable

- [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md)
  : Extract matrix of a single variable

- [`merge_chains()`](https://mc-stan.org/posterior/dev/reference/merge_chains.md)
  :

  Merge chains of `draws` objects

- [`mutate_variables()`](https://mc-stan.org/posterior/dev/reference/mutate_variables.md)
  :

  Mutate variables in `draws` objects

- [`` `variables<-`() ``](https://mc-stan.org/posterior/dev/reference/variables-set.md)
  [`set_variables()`](https://mc-stan.org/posterior/dev/reference/variables-set.md)
  :

  Set variable names in `draws` objects

- [`order_draws()`](https://mc-stan.org/posterior/dev/reference/order_draws.md)
  :

  Order `draws` objects

- [`split_chains()`](https://mc-stan.org/posterior/dev/reference/split_chains.md)
  : Split Chains

- [`subset_draws()`](https://mc-stan.org/posterior/dev/reference/subset_draws.md)
  [`subset(`*`<draws>`*`)`](https://mc-stan.org/posterior/dev/reference/subset_draws.md)
  :

  Subset `draws` objects

- [`iteration_ids()`](https://mc-stan.org/posterior/dev/reference/draws-index.md)
  [`chain_ids()`](https://mc-stan.org/posterior/dev/reference/draws-index.md)
  [`draw_ids()`](https://mc-stan.org/posterior/dev/reference/draws-index.md)
  [`niterations()`](https://mc-stan.org/posterior/dev/reference/draws-index.md)
  [`nchains()`](https://mc-stan.org/posterior/dev/reference/draws-index.md)
  [`ndraws()`](https://mc-stan.org/posterior/dev/reference/draws-index.md)
  :

  Index `draws` objects

- [`rename_variables()`](https://mc-stan.org/posterior/dev/reference/rename_variables.md)
  :

  Rename variables in `draws` objects

- [`repair_draws()`](https://mc-stan.org/posterior/dev/reference/repair_draws.md)
  :

  Repair indices of `draws` objects

- [`resample_draws()`](https://mc-stan.org/posterior/dev/reference/resample_draws.md)
  :

  Resample `draws` objects

- [`thin_draws()`](https://mc-stan.org/posterior/dev/reference/thin_draws.md)
  :

  Thin `draws` objects

- [`weight_draws()`](https://mc-stan.org/posterior/dev/reference/weight_draws.md)
  :

  Weight `draws` objects

- [`weights(`*`<draws>`*`)`](https://mc-stan.org/posterior/dev/reference/weights.draws.md)
  : Extract Weights from Draws Objects

## Summarizing and diagnosing draws objects

Compute summary statistics and convergence diagnostics

- [`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
  [`summarize_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
  [`summary(`*`<draws>`*`)`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
  [`summary(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
  [`default_summary_measures()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
  [`default_convergence_measures()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
  [`default_mcse_measures()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
  :

  Summaries of `draws` objects

- [`diagnostics`](https://mc-stan.org/posterior/dev/reference/diagnostics.md)
  [`convergence`](https://mc-stan.org/posterior/dev/reference/diagnostics.md)
  : List of available convergence diagnostics

- [`ess_basic()`](https://mc-stan.org/posterior/dev/reference/ess_basic.md)
  : Basic version of the effective sample size

- [`ess_bulk()`](https://mc-stan.org/posterior/dev/reference/ess_bulk.md)
  : Bulk effective sample size (bulk-ESS)

- [`ess_mean()`](https://mc-stan.org/posterior/dev/reference/ess_mean.md)
  : Effective sample size for the mean

- [`ess_quantile()`](https://mc-stan.org/posterior/dev/reference/ess_quantile.md)
  [`ess_median()`](https://mc-stan.org/posterior/dev/reference/ess_quantile.md)
  : Effective sample sizes for quantiles

- [`ess_sd()`](https://mc-stan.org/posterior/dev/reference/ess_sd.md) :
  Effective sample size for the standard deviation

- [`ess_tail()`](https://mc-stan.org/posterior/dev/reference/ess_tail.md)
  : Tail effective sample size (tail-ESS)

- [`rhat()`](https://mc-stan.org/posterior/dev/reference/rhat.md) : Rhat
  convergence diagnostic

- [`rhat_basic()`](https://mc-stan.org/posterior/dev/reference/rhat_basic.md)
  : Basic version of the Rhat convergence diagnostic

- [`rhat_nested()`](https://mc-stan.org/posterior/dev/reference/rhat_nested.md)
  : Nested Rhat convergence diagnostic

- [`mcse_mean()`](https://mc-stan.org/posterior/dev/reference/mcse_mean.md)
  : Monte Carlo standard error for the mean

- [`mcse_quantile()`](https://mc-stan.org/posterior/dev/reference/mcse_quantile.md)
  [`mcse_median()`](https://mc-stan.org/posterior/dev/reference/mcse_quantile.md)
  : Monte Carlo standard error for quantiles

- [`mcse_sd()`](https://mc-stan.org/posterior/dev/reference/mcse_sd.md)
  : Monte Carlo standard error for the standard deviation

- [`pareto_diags()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
  [`pareto_khat_threshold()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
  [`pareto_min_ss()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
  [`pareto_convergence_rate()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
  : Pareto smoothing diagnostics

- [`pareto_khat()`](https://mc-stan.org/posterior/dev/reference/pareto_khat.md)
  : Pareto khat diagnostic

- [`pareto_smooth()`](https://mc-stan.org/posterior/dev/reference/pareto_smooth.md)
  : Pareto smoothing

- [`ps_convergence_rate()`](https://mc-stan.org/posterior/dev/reference/ps_convergence_rate.md)
  : Pareto convergence rate

- [`ps_khat_threshold()`](https://mc-stan.org/posterior/dev/reference/ps_khat_threshold.md)
  : Pareto k-hat threshold

- [`ps_min_ss()`](https://mc-stan.org/posterior/dev/reference/ps_min_ss.md)
  : Pareto-smoothing minimum sample-size

- [`ps_tail()`](https://mc-stan.org/posterior/dev/reference/ps_tail.md)
  : Pareto smooth tail function to pareto smooth the tail of a vector.
  Exported for usage in other packages, not by users.

- [`ps_tail_length()`](https://mc-stan.org/posterior/dev/reference/ps_tail_length.md)
  : Pareto tail length

- [`quantile2()`](https://mc-stan.org/posterior/dev/reference/quantile2.md)
  : Compute Quantiles

- [`rstar()`](https://mc-stan.org/posterior/dev/reference/rstar.md) :
  Calculate R\* convergence diagnostic

- [`entropy()`](https://mc-stan.org/posterior/dev/reference/entropy.md)
  : Normalized entropy

- [`dissent()`](https://mc-stan.org/posterior/dev/reference/dissent.md)
  : Dissention

- [`modal_category()`](https://mc-stan.org/posterior/dev/reference/modal_category.md)
  : Modal category

- [`pit()`](https://mc-stan.org/posterior/dev/reference/pit.md) :
  Probability integral transform

## Functionality specific to the rvar datatype

The `draws_rvar` format (a structured list of `rvar` objects) has the
same methods
(e.g. [`bind_draws()`](https://mc-stan.org/posterior/dev/reference/bind_draws.md))
as the other draws formats. For individual `rvar` objects themselves,
however, posterior provides additional functionality.

- [`chol(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/chol.rvar.md)
  : Cholesky decomposition of random matrix

- [`density(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
  [`density(`*`<rvar_factor>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
  [`cdf(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
  [`cdf(`*`<rvar_factor>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
  [`cdf(`*`<rvar_ordered>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
  [`quantile(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
  [`quantile(`*`<rvar_factor>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
  [`quantile(`*`<rvar_ordered>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
  : Density, CDF, and quantile functions of random variables

- [`` `%**%` ``](https://mc-stan.org/posterior/dev/reference/rvar-matmult.md)
  [`matrixOps(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-matmult.md)
  : Matrix multiplication of random variables

- [`` `[[`( ``*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-slice.md)
  [`` `[[<-`( ``*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-slice.md)
  [`` `[`( ``*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-slice.md)
  [`` `[<-`( ``*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-slice.md)
  : Random variable slicing

- [`E()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`mean(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`Pr()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`median(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`min(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`max(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`sum(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`prod(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`all(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`any(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`Summary(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`variance(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`var()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`sd()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`mad()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`range(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`is.finite(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`is.infinite(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`is.nan(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  [`is.na(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
  : Summaries of random variables within array elements, over draws

- [`rvar_mean()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_median()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_sum()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_prod()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_min()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_max()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_sd()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_var()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_mad()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_range()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_quantile()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_all()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  [`rvar_any()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
  : Summaries of random variables over array elements, within draws

- [`rvar()`](https://mc-stan.org/posterior/dev/reference/rvar.md) :
  Random variables of arbitrary dimension

- [`rvar_apply()`](https://mc-stan.org/posterior/dev/reference/rvar_apply.md)
  : Random variable resulting from a function applied over margins of an
  array or random variable

- [`rvar_factor()`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
  [`rvar_ordered()`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
  : Factor random variables of arbitrary dimension

- [`rvar_ifelse()`](https://mc-stan.org/posterior/dev/reference/rvar_ifelse.md)
  : Random variable ifelse

- [`rvar_is_finite()`](https://mc-stan.org/posterior/dev/reference/rvar_is_finite.md)
  [`rvar_is_infinite()`](https://mc-stan.org/posterior/dev/reference/rvar_is_finite.md)
  [`rvar_is_nan()`](https://mc-stan.org/posterior/dev/reference/rvar_is_finite.md)
  [`rvar_is_na()`](https://mc-stan.org/posterior/dev/reference/rvar_is_finite.md)
  : Special value predicates for random variables

- [`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md)
  : Create random variables from existing random number generators

- [`is_rvar()`](https://mc-stan.org/posterior/dev/reference/is_rvar.md)
  :

  Is `x` a random variable?

- [`is_rvar_factor()`](https://mc-stan.org/posterior/dev/reference/is_rvar_factor.md)
  [`is_rvar_ordered()`](https://mc-stan.org/posterior/dev/reference/is_rvar_factor.md)
  :

  Is `x` a factor random variable?

- [`as_rvar()`](https://mc-stan.org/posterior/dev/reference/as_rvar.md)
  [`as_rvar_numeric()`](https://mc-stan.org/posterior/dev/reference/as_rvar.md)
  [`as_rvar_integer()`](https://mc-stan.org/posterior/dev/reference/as_rvar.md)
  [`as_rvar_logical()`](https://mc-stan.org/posterior/dev/reference/as_rvar.md)
  : Coerce to a random variable

- [`as_rvar_factor()`](https://mc-stan.org/posterior/dev/reference/as_rvar_factor.md)
  [`as_rvar_ordered()`](https://mc-stan.org/posterior/dev/reference/as_rvar_factor.md)
  : Coerce to a factor random variable

- [`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md) :
  Execute expressions of random variables

- [`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md) :
  Create functions of random variables

- [`draws_of()`](https://mc-stan.org/posterior/dev/reference/draws_of.md)
  [`` `draws_of<-`() ``](https://mc-stan.org/posterior/dev/reference/draws_of.md)
  : Get/set array of draws underlying a random variable

- [`diag(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/diag-rvar-method.md)
  : Matrix diagonals (including for random variables)

- [`drop(`*`<rvar>`*`)`](https://mc-stan.org/posterior/dev/reference/drop-rvar-method.md)
  : Drop redundant dimensions

- [`for_each_draw()`](https://mc-stan.org/posterior/dev/reference/for_each_draw.md)
  : Loop over draws

- [`match()`](https://mc-stan.org/posterior/dev/reference/match.md)
  [`` `%in%` ``](https://mc-stan.org/posterior/dev/reference/match.md) :
  Value Matching
