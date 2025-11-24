# Pareto-khat diagnostics

## Introduction

The paper

- Aki Vehtari, Daniel Simpson, Andrew Gelman, Yuling Yao, and Jonah
  Gabry (2024). Pareto smoothed importance sampling. *Journal of Machine
  Learning Research*, 25(72):1-58.

presents Pareto smoothed importance sampling, but also
Pareto-\\\hat{k}\\ diagnostic that can be used when estimating any
expectation based on finite sample. This vignette illustrates the use of
these diagnostics. The individual diagnostic functions are
[`pareto_khat()`](https://mc-stan.org/posterior/dev/reference/pareto_khat.md),
[`pareto_min_ss()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md),
[`pareto_convergence_rate()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
and
[`pareto_khat_threshold()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md).
The function
[`pareto_diags()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
will return all of these.

Additionally, the
[`pareto_smooth()`](https://mc-stan.org/posterior/dev/reference/pareto_smooth.md)
function can be used to transform draws by smoothing the tail(s). \##
Example

``` r
library(posterior)
```

    ## This is posterior version 1.6.1.9000

    ## 
    ## Attaching package: 'posterior'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     mad, sd, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     %in%, match

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
options(pillar.neg = FALSE, pillar.subtle=FALSE, pillar.sigfig=2)
```

### Simulated data

Generate `xn` a simulated MCMC sample with 4 chains each with 1000
iterations using AR process with marginal normal(0,1)

``` r
N <- 1000
phi <- 0.3
set.seed(6534)
dr <- array(data=replicate(4,as.numeric(arima.sim(n = N,
                                                list(ar = c(phi)),
                                                sd = sqrt((1-phi^2))))),
         dim=c(N,4,1)) %>%
  as_draws_df() %>%
  set_variables('xn')
```

Transform `xn` via cdf-inverse-cdf so that we have variables that have
marginally distributions \\t_3\\, \\t\_{2.5}\\, \\t_2\\, \\t\_{1.5}\\,
and \\t_1\\. These all have thick tails. In addition \\t_2\\,
\\t\_{1.5}\\, and \\t_1\\ have infinite variance, and \\t_1\\ (aka
Cauchy) has infinite mean.

``` r
drt <- dr %>%
  mutate_variables(xt3=qt(pnorm(xn), df=3),
                   xt2_5=qt(pnorm(xn), df=2.5),
                   xt2=qt(pnorm(xn), df=2),
                   xt1_5=qt(pnorm(xn), df=1.5),
                   xt1=qt(pnorm(xn), df=1))
```

### MCMC convergence diagnostics

We examine the draws with the default
[`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md).

``` r
drt %>%
  summarise_draws()
```

    ## # A tibble: 6 × 10
    ##   variable  mean  median    sd   mad    q5   q95  rhat ess_bulk ess_tail
    ##   <chr>    <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
    ## 1 xn       0.010 0.00094  0.99  0.99  -1.6   1.6   1.0    2284.    3189.
    ## 2 xt3      0.025 0.0010   1.6   1.1   -2.3   2.3   1.0    2284.    3189.
    ## 3 xt2_5    0.031 0.0010   2.0   1.1   -2.5   2.5   1.0    2284.    3189.
    ## 4 xt2      0.046 0.0011   2.9   1.2   -2.8   2.9   1.0    2284.    3189.
    ## 5 xt1_5    0.092 0.0011   7.6   1.3   -3.5   3.6   1.0    2284.    3189.
    ## 6 xt1      0.33  0.0012  93.    1.5   -5.8   6.1   1.0    2284.    3189.

All the usual convergence diagnostics \\\widehat{R}\\, Bulk-ESS, and
Tail-ESS look good, which is fine as they have been designed to work
also with infinite variance (Vehtari et al., 2020).

If these variables would present variables of interest for which we
would like to estimate means, we would be also interested in Monte Carlo
standard error (MCSE, see case study [How many iterations to run and how
many digits to
report](https://users.aalto.fi/~ave/casestudies/Digits/digits.html)).

``` r
drt %>%
  summarise_draws(mean, sd, mcse_mean, ess_bulk, ess_basic)
```

    ## # A tibble: 6 × 6
    ##   variable  mean    sd mcse_mean ess_bulk ess_basic
    ##   <chr>    <dbl> <dbl>     <dbl>    <dbl>     <dbl>
    ## 1 xn       0.010  0.99     0.021    2284.     2280.
    ## 2 xt3      0.025  1.6      0.033    2284.     2452.
    ## 3 xt2_5    0.031  2.0      0.039    2284.     2584.
    ## 4 xt2      0.046  2.9      0.054    2284.     2903.
    ## 5 xt1_5    0.092  7.6      0.13     2284.     3553.
    ## 6 xt1      0.33  93.       1.5      2284.     3976.

Here MCSE for mean is based on standard deviation and Basic-ESS, but
these assume finite variance. We did sample also from distributions with
infinite variance, but given a finite sample size, the empirical
variance estimates are always finite, and thus we get overoptimistic
MCSE.

## Pareto-\\\hat{k}\\

To diagnose whether our variables of interest may have infinite variance
and even infinite mean, we can use Pareto-\\\hat{k}\\ diagnostic.

``` r
drt %>%
  summarise_draws(mean, sd, mcse_mean, ess_basic, pareto_khat)
```

    ## # A tibble: 6 × 6
    ##   variable  mean    sd mcse_mean ess_basic pareto_khat
    ##   <chr>    <dbl> <dbl>     <dbl>     <dbl>       <dbl>
    ## 1 xn       0.010  0.99     0.021     2280.      -0.072
    ## 2 xt3      0.025  1.6      0.033     2452.       0.33 
    ## 3 xt2_5    0.031  2.0      0.039     2584.       0.41 
    ## 4 xt2      0.046  2.9      0.054     2903.       0.52 
    ## 5 xt1_5    0.092  7.6      0.13      3553.       0.69 
    ## 6 xt1      0.33  93.       1.5       3976.       1.0

\\\hat{k} \leq 0\\ indicates that all moments exist, and the inverse of
positive \\\hat{k}\\ tells estimate for the number of finite
(fractional) moments. Thus, \\\hat{k}\geq 1/2\\ indicates infinite
variance, and \\\hat{k}\geq 1\\ indicates infinite mean. Sometimes very
thick distribution tails may affect also sampling, but assuming sampling
did go well, and we would be interested only in quantiles, infinite
variance and mean are not a problem. But if we are interested in mean,
then we need to care about the number of (fractional) moments. Here we
see \\\hat{k} \geq 1/2\\ for \\t_2\\, \\t\_{1.5}\\, and \\t\_{1}\\, and
we should not trust their `mcse_mean` values. Without trustworthy MCSE
estimate we don’t have good estimate of how accurate the mean estimate
is. Furthermore, as \\\hat{k} \geq 1\\ for \\t\_{1}\\, the mean is not
finite and the mean estimate is not valid.

## Pareto smoothing

If we really do need those mean estimates, we can improve
trustworthiness by Pareto smoothing, which replaces extreme tail draws
with expected ordered statistics of Pareto distribution fitted to the
tails of the distribution. Pareto smoothed mean estimate (computed using
Pareto smoothed draws) has finite variance with a cost of some bias
which we know when it is negligible. As a thumb rule when
\\\hat{k}\<0.7\\, the bias is negligible.

We do Pareto smoothing for all the variables.

``` r
drts <- drt %>% 
  mutate_variables(xt3_s=pareto_smooth(xt3),
                   xt2_5_s=pareto_smooth(xt2_5),
                   xt2_s=pareto_smooth(xt2),
                   xt1_5_s=pareto_smooth(xt1_5),
                   xt1_s=pareto_smooth(xt1)) %>%
  subset_draws(variable="_s", regex=TRUE)
```

    ## Pareto k-hat = 0.33.

    ## Pareto k-hat = 0.41.

    ## Pareto k-hat = 0.52.

    ## Pareto k-hat = 0.69.

    ## Pareto k-hat = 1.03. Mean does not exist, making empirical mean estimate of the draws not applicable.

Now the `mcse_mean` values are more trustworthy when \\\hat{k} \< 0.7\\.
When \\\hat{k}\>0.7\\ both bias and variance grow so fast that Pareto
smoothing rarely helps (see more details in the paper).

``` r
drts %>%
  summarise_draws(mean, mcse_mean, ess_basic, pareto_khat)
```

    ## # A tibble: 5 × 5
    ##   variable  mean mcse_mean ess_basic pareto_khat
    ##   <chr>    <dbl>     <dbl>     <dbl>       <dbl>
    ## 1 xt3_s    0.026     0.033     2439.        0.34
    ## 2 xt2_5_s  0.034     0.038     2538.        0.41
    ## 3 xt2_s    0.053     0.051     2768.        0.51
    ## 4 xt1_5_s  0.13      0.10      3305.        0.68
    ## 5 xt1_s    1.0       0.83      3908.        1.00

## Minimum sample size required

The bias and variance depend on the sample size, and we can use
additional diagnostic `min_ss` which tells the minimum sample size
needed so that `mcse_mean` can be trusted.

``` r
drt %>%
  summarise_draws(mean, mcse_mean, ess_basic,
                  pareto_khat, min_ss=pareto_min_ss)
```

    ## # A tibble: 6 × 6
    ##   variable  mean mcse_mean ess_basic pareto_khat min_ss
    ##   <chr>    <dbl>     <dbl>     <dbl>       <dbl>  <dbl>
    ## 1 xn       0.010     0.021     2280.      -0.072    10 
    ## 2 xt3      0.025     0.033     2452.       0.33     31.
    ## 3 xt2_5    0.031     0.039     2584.       0.41     48.
    ## 4 xt2      0.046     0.054     2903.       0.52    116.
    ## 5 xt1_5    0.092     0.13      3553.       0.69   1746.
    ## 6 xt1      0.33      1.5       3976.       1.0     Inf

Here required `min_ss` is smaller than `ess_basic` for all except
\\t_1\\, for which there is no hope.

## Convergence rate

Given finite variance, the central limit theorem states that to halve
MCSE we need four times bigger sample size. With Pareto smoothing, we
can go further, but the convergence rate decreases when \\\hat{k}\\
increases.

``` r
drt %>%
  summarise_draws(mean, mcse_mean, ess_basic,
                  pareto_khat, min_ss=pareto_min_ss,
                  conv_rate=pareto_convergence_rate)
```

    ## # A tibble: 6 × 7
    ##   variable  mean mcse_mean ess_basic pareto_khat min_ss conv_rate
    ##   <chr>    <dbl>     <dbl>     <dbl>       <dbl>  <dbl>     <dbl>
    ## 1 xn       0.010     0.021     2280.      -0.072    10       1   
    ## 2 xt3      0.025     0.033     2452.       0.33     31.      0.98
    ## 3 xt2_5    0.031     0.039     2584.       0.41     48.      0.95
    ## 4 xt2      0.046     0.054     2903.       0.52    116.      0.86
    ## 5 xt1_5    0.092     0.13      3553.       0.69   1746.      0.60
    ## 6 xt1      0.33      1.5       3976.       1.0     Inf       0

We see that with \\t_2\\, \\t\_{1.5}\\, and \\t_1\\ we need
\\4^{1/0.86}\approx 5\\, \\4^{1/0.60}\approx 10\\, and \\4^{1/0}\approx
\infty\\ times bigger sample sizes to halve MCSE for mean.

## Pareto-\\\hat{k}\\-threshold

The final Pareto diagnostic, \\\hat{k}\\-threshold, is useful for
smaller sample sizes. Here we select only 100 iterations per chain to
get total of 400 draws.

``` r
drt %>%
  subset_draws(iteration=1:100) %>%
  summarise_draws(mean, mcse_mean, ess_basic,
                  pareto_khat, min_ss=pareto_min_ss,
                  khat_thres=pareto_khat_threshold,
                  conv_rate=pareto_convergence_rate)
```

    ## # A tibble: 6 × 8
    ##   variable   mean mcse_mean ess_basic pareto_khat min_ss khat_thres conv_rate
    ##   <chr>     <dbl>     <dbl>     <dbl>       <dbl>  <dbl>      <dbl>     <dbl>
    ## 1 xn        0.038     0.066      244.      -0.015 1  e 1       0.62      1   
    ## 2 xt3       0.054     0.11       237.       0.32  2.9e 1       0.62      0.96
    ## 3 xt2_5     0.057     0.13       237.       0.38  4.1e 1       0.62      0.93
    ## 4 xt2       0.063     0.17       243.       0.47  7.9e 1       0.62      0.86
    ## 5 xt1_5     0.061     0.31       271.       0.63  4.9e 2       0.62      0.68
    ## 6 xt1      -0.26      1.6        344.       0.93  2.0e15       0.62      0.13

With only 400 draws, we can trust the Pareto smoothed result only when
\\\hat{k}\<0.62\\. For \\t\_{1.5}\\ \\\hat{k}\approx 0.64\\, and
`min_ss` reveals we would probably need more than 560 draws to be on the
safe side.

## Pareto diagnostics

We can get all these diagnostics with
[`pareto_diags()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md),
and it’s easy to use it also for derived quantities.

``` r
drt %>%
  mutate_variables(xt2_5_sq=xt2_5^2) %>%
  subset_draws(variable="xt2_5_sq") %>%
  summarise_draws(mean, mcse_mean,
                  pareto_diags)
```

    ## # A tibble: 1 × 7
    ##   variable  mean mcse_mean  khat min_ss khat_threshold convergence_rate
    ##   <chr>    <dbl>     <dbl> <dbl>  <dbl>          <dbl>            <dbl>
    ## 1 xt2_5_sq   3.9      0.56  0.67  1051.           0.72             0.64

## Discussion

All these diagnostics are presented in Section 3 and summarized in Table
1 in PSIS paper (Vehtari et al., 2024).

If you don’t need to estimate means of thick tailed distributions, and
there are no sampling issues due to thick tails, then you don’t need to
check existence of finite variance, and thus there is no need to check
Pareto-\\\hat{k}\\ for all the parameters and derived quantities.

It is possible that the distribution has finite variance, but
pre-asymptotically given a finite sample size the behavior can be
similar to infinite variance. Thus the diagnostic is useful even in
cases where theory guarantees finite variance.

## Reference

Vehtari, A., Simpson, D., Gelman, A., Yao, Y., & Gabry, J. (2024).
Pareto smoothed importance sampling. *Journal of Machine Learning
Research*, 25(72):1-58.

Vehtari A., Gelman A., Simpson D., Carpenter B., & Bürkner P. C. (2020).
Rank-normalization, folding, and localization: An improved Rhat for
assessing convergence of MCMC. *Bayesian Analysis*, 16(2):667-718.
