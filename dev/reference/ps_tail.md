# Pareto smooth tail function to pareto smooth the tail of a vector. Exported for usage in other packages, not by users.

Pareto smooth tail function to pareto smooth the tail of a vector.
Exported for usage in other packages, not by users.

## Usage

``` r
ps_tail(
  x,
  ndraws_tail,
  smooth_draws = TRUE,
  tail = c("right", "left"),
  are_log_weights = FALSE,
  ...
)
```

## Arguments

- x:

  (multiple options) One of:

  - A matrix of draws for a single variable (iterations x chains). See
    [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md).

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- ndraws_tail:

  (numeric) number of draws for the tail. If `ndraws_tail` is not
  specified, it will be set to `length(x)`.

- smooth_draws:

  (logical) Should the tails be smoothed? Default is `TRUE`. If `FALSE`,
  `k` will be calculated but `x` will remain untouched.

- tail:

  (string) The tail to diagnose/smooth:

  - `"right"`: diagnose/smooth only the right (upper) tail

  - `"left"`: diagnose/smooth only the left (lower) tail

- are_log_weights:

  (logical) Are the draws log weights? Default is `FALSE`. If `TRUE`
  computation will take into account that the draws are log weights, and
  only right tail will be smoothed.

- ...:

  Arguments passed to individual methods (if applicable).

## References

Aki Vehtari, Daniel Simpson, Andrew Gelman, Yuling Yao and Jonah Gabry
(2024). Pareto Smoothed Importance Sampling. *Journal of Machine
Learning Research*, 25(72):1-58.
[PDF](https://jmlr.org/papers/v25/19-556.html)

## See also

[`pareto_smooth`](https://mc-stan.org/posterior/dev/reference/pareto_smooth.md)
for the user-facing function.
