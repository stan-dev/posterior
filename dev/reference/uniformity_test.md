# Uniformity test for PIT values

Tests whether PIT (probability integral transform) values deviate from
uniformity while accounting for dependence among them. LOO-PIT values
are not independent because each LOO predictive distribution is
conditioned on nearly the same data (all but one observation), which
induces correlation. Standard uniformity tests that assume independence
can have inflated Type I error or reduced power. This function
implements dependence-aware procedures from Tesso and Vehtari (2026).

## Usage

``` r
uniformity_test(pit, test)
```

## Arguments

- pit:

  Numeric vector of PIT values in `[0, 1]`.

- test:

  Character string. One of `"POT"`, `"PIET"`, or `"PRIT"`. See details
  above.

## Value

A list with components:

- `pvalue`: Global p-value from the Cauchy combination test.

- `pointwise`: Shapley values (contributions of each PIT value to the
  test statistic). Non-negative values indicate points contributing to
  evidence against uniformity.

## Details

Three test variants are available:

- **POT** (Pointwise Order Tests): Uses beta distributions for order
  statistics. Best for continuous PIT values; recommended as default for
  LOO-PIT model checking. Good power against diverse departures from
  uniformity.

- **PIET** (Pointwise Inverse-CDF Evaluation Tests): Uses a continuous
  reference distribution (exponential) via inverse-CDF transformation.
  Best for detecting tail deviations; maintains valid Type I error
  control under strong dependence.

- **PRIT** (Pointwise Rank-based Individual Tests): Uses binomial
  distributions on scaled ECDF (ranks). Intended for discrete or
  rank-based PIT values.

All procedures compute pointwise p-values, aggregate them via the
(truncated) Cauchy combination test (Liu and Xie, 2020; Chen et al.,
2025), and derive Shapley values to quantify each point's contribution
to the overall test.

## References

Tesso, H., and Vehtari, A. (2026). LOO-PIT predictive model checking.
arXiv preprint arXiv:2603.02928.

Liu, Y., and Xie, J. (2020). Cauchy combination test: a powerful test
with analytic p-value calculation under arbitrary dependency structures.
*Journal of the American Statistical Association*, 115(529), 393-402.

Chen, B., Xu, W., and Gao, X. (2025). Truncated Cauchy combination test:
a robust and powerful p-value combination method with arbitrary
correlations. arXiv preprint arXiv:2506.12489.
