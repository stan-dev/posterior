# Tools for working with posterior (and prior) distributions

<https://mc-stan.org/posterior/>

The posterior package is intended to provide useful tools for both users
and developers of packages for fitting Bayesian models or working with
output from Bayesian models. The primary goals of the package are to:

- Efficiently convert between many different useful formats of draws
  (samples) from posterior or prior distributions.

- Provide consistent methods for operations commonly performed on draws,
  for example, subsetting, binding, or mutating draws.

- Provide various summaries of draws in convenient formats.

- Provide lightweight implementations of state of the art posterior
  inference diagnostics.

## Package options

The following options are used to format and print
[`draws`](https://mc-stan.org/posterior/reference/draws.md) objects, as
in
[`print.draws_array()`](https://mc-stan.org/posterior/reference/print.draws_array.md),
[`print.draws_df()`](https://mc-stan.org/posterior/reference/print.draws_df.md),
[`print.draws_list()`](https://mc-stan.org/posterior/reference/print.draws_list.md),
[`print.draws_matrix()`](https://mc-stan.org/posterior/reference/print.draws_matrix.md),
and
[`print.draws_rvars()`](https://mc-stan.org/posterior/reference/print.draws_rvars.md):

- `posterior.max_draws`: Maximum number of draws to print.

- `posterior.max_iterations`: Maximum number of iterations to print.

- `posterior.max_chains`: Maximum number of chains to print.

- `posterior.max_variables`: Maximum number of variables to print.

The following options are used for formatting the output of
[`summarize_draws`](https://mc-stan.org/posterior/reference/draws_summary.md):

- `posterior.num_args`: Arguments passed to
  [num()](https://tibble.tidyverse.org/reference/num.html) for pretty
  printing of summaries.

The following options are used to format and print
[`rvar`](https://mc-stan.org/posterior/reference/rvar.md) objects, as in
[`print.rvar()`](https://mc-stan.org/posterior/reference/print.rvar.md)
and
[`print.draws_rvars()`](https://mc-stan.org/posterior/reference/print.draws_rvars.md):

- `posterior.rvar_summary`: What style of summary to display:
  `"mean_sd"` displays `mean ± sd`, `"median_mad"` displays
  `median ± mad`.

- `posterior.digits`: How many significant digits are displayed. This
  defaults to a smaller value (`2`) than `getOption("digits")` because
  [`rvar`](https://mc-stan.org/posterior/reference/rvar.md)s print two
  numbers (point summary and uncertainty) next to each other.

The following option is used to construct new
[`rvar`](https://mc-stan.org/posterior/reference/rvar.md) objects, as in
[`rfun()`](https://mc-stan.org/posterior/reference/rfun.md) and
[`rdo()`](https://mc-stan.org/posterior/reference/rdo.md):

- `posterior.rvar_ndraws`: The number of draws used to construct new
  random variables when this number cannot be determined from existing
  arguments (e.g., other
  [`rvar`](https://mc-stan.org/posterior/reference/rvar.md)s passed to a
  function).

The following options are used to control warning messages:

- `posterior.warn_on_merge_chains`: (logical) Some operations will
  trigger an automatic merging of chains, for example, because chains do
  not match between two objects involved in a binary operation. Whether
  this causes a warning can be controlled by this option.

## Warning and error classes

Every warning signaled by posterior has the class `posterior_warning`
and one class for its cause. Use these classes to handle or muffle a
specific warning, for example with
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html) and
`invokeRestart("muffleWarning")`.

- `posterior_warning_ess_capped`: An ESS estimate was capped.

- `posterior_warning_superchain_ids_length`: The length of
  `superchain_ids` does not equal the number of chains.

- `posterior_warning_superchain_unequal`: The superchains do not have
  the same number of chains.

- `posterior_warning_non_numeric`: Non-numeric variables (e.g., factors)
  were converted to numeric.

- `posterior_warning_chains_dropped`: Chains were dropped.

- `posterior_warning_no_variables`: No variables with unreserved names
  were found.

- `posterior_warning_odd_iterations`: The number of iterations is not
  even.

- `posterior_warning_pit_above_one`: Some PIT values are larger than 1.

- `posterior_warning_zero_weight`: All draws of a variable have zero
  weight.

- `posterior_warning_nans_produced`: NaNs were produced.

- `posterior_warning_draws_df_dropped`: The `draws_df` class was
  dropped.

- `posterior_warning_pareto_invalid_input`: The input to Pareto
  smoothing contains infinite or `NA` values, is constant, or has a
  constant tail.

- `posterior_warning_pareto_tail_too_short`: The number of tail draws
  was increased to 5.

- `posterior_warning_pareto_tail_exceeds_draws`: The number of tail
  draws is not less than the total number of draws.

- `posterior_warning_pareto_tail_too_long`: The number of tail draws is
  more than half of the total number of draws.

- `posterior_warning_pareto_tail_not_fitted`: The generalized Pareto
  distribution was not fitted because there are fewer than 5 tail draws.

The error signaled when all weights are zero has the class
`posterior_error_degenerate_weights`.

## See also

Useful links:

- <https://mc-stan.org/posterior/>

- <https://discourse.mc-stan.org/>

- Report bugs at <https://github.com/stan-dev/posterior/issues>

## Author

**Maintainer**: Paul-Christian Bürkner <paul.buerkner@gmail.com>

Authors:

- Paul-Christian Bürkner <paul.buerkner@gmail.com>

- Jonah Gabry <jgabry@gmail.com>

- Matthew Kay <mjskay@northwestern.edu>

- Aki Vehtari <Aki.Vehtari@aalto.fi>

Other contributors:

- Måns Magnusson \[contributor\]

- Rok Češnovar \[contributor\]

- Ben Lambert \[contributor\]

- Ozan Adıgüzel \[contributor\]

- Jacob Socolar \[contributor\]

- Noa Kallioinen \[contributor\]

- Teemu Säilynoja \[contributor\]
