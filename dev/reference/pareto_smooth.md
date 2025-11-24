# Pareto smoothing

Smooth the tail draws of x by replacing tail draws by order statistics
of a generalized Pareto distribution fit to the tail(s). For further
details see Vehtari et al. (2024).

## Usage

``` r
pareto_smooth(x, ...)

# S3 method for class 'rvar'
pareto_smooth(x, return_k = FALSE, extra_diags = FALSE, ...)

# Default S3 method
pareto_smooth(
  x,
  tail = c("both", "right", "left"),
  r_eff = NULL,
  ndraws_tail = NULL,
  return_k = FALSE,
  extra_diags = FALSE,
  verbose = TRUE,
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

- ...:

  Arguments passed to individual methods (if applicable).

- return_k:

  (logical) Should the Pareto khat be included in output? If `TRUE`,
  output will be a list containing smoothed draws and diagnostics,
  otherwise it will be a numeric of the smoothed draws. Default is
  `FALSE`.

- extra_diags:

  (logical) Should extra Pareto khat diagnostics be included in output?
  If `TRUE`, `min_ss`, `khat_threshold` and `convergence_rate` for the
  estimated k value will be returned. Default is `FALSE`.

- tail:

  (string) The tail to diagnose/smooth:

  - `"right"`: diagnose/smooth only the right (upper) tail

  - `"left"`: diagnose/smooth only the left (lower) tail

  - `"both"`: diagnose/smooth both tails and return the maximum k-hat
    value

  The default is `"both"`.

- r_eff:

  (numeric) relative effective sample size estimate. If `r_eff` is NULL,
  it will be calculated assuming the draws are from MCMC. Default is
  NULL.

- ndraws_tail:

  (numeric) number of draws for the tail. If `ndraws_tail` is not
  specified, it will be calculated as ceiling(3 \* sqrt(length(x) /
  r_eff)) if length(x) \> 225 and length(x) / 5 otherwise (see Appendix
  H in Vehtari et al. (2024)).

- verbose:

  (logical) Should diagnostic messages be printed? If `TRUE`, messages
  related to Pareto diagnostics will be printed. Default is `FALSE`.

- are_log_weights:

  (logical) Are the draws log weights? Default is `FALSE`. If `TRUE`
  computation will take into account that the draws are log weights, and
  only right tail will be smoothed.

## Value

Either a vector `x` of smoothed values or a named list containing the
vector `x` and a named list `diagnostics` containing numeric values:

- `khat`: estimated Pareto k shape parameter, and optionally

- `min_ss`: minimum sample size for reliable Pareto smoothed estimate

- `khat_threshold`: sample size specific khat threshold for reliable
  Pareto smoothed estimates

- `convergence_rate`: Relative convergence rate for Pareto smoothed
  estimates

If any of the draws is non-finite, that is, `NA`, `NaN`, `Inf`, or
`-Inf`, Pareto smoothing will not be performed, and the original draws
will be returned and and diagnostics will be `NA` (numeric).

## References

Aki Vehtari, Daniel Simpson, Andrew Gelman, Yuling Yao and Jonah Gabry
(2024). Pareto Smoothed Importance Sampling. *Journal of Machine
Learning Research*, 25(72):1-58.
[PDF](https://jmlr.org/papers/v25/19-556.html)

## See also

[`pareto_khat`](https://mc-stan.org/posterior/dev/reference/pareto_khat.md)
for only calculating khat, and
[`pareto_diags`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
for additional diagnostics.

## Examples

``` r
mu <- extract_variable_matrix(example_draws(), "mu")
pareto_smooth(mu)
#> Pareto k-hat = 0.19.
#>          chain
#> iteration            1           2           3           4
#>       1    2.005831131   2.9903807  1.79436801  6.45897880
#>       2    1.458316115   8.0694650  5.98637117  9.27455226
#>       3    5.814947304  -1.3572537  2.55720201  0.30799874
#>       4    6.849586239  11.4282511  2.79442522  3.69252884
#>       5    1.805167741  10.1122198  0.11775891  5.48027067
#>       6    3.841242509 -10.9048130  1.06361700  2.37951413
#>       7    5.474272939  -7.6803520  3.67385716 12.38234076
#>       8    1.203061702   1.7910520  3.50583836  4.90456023
#>       9    0.196964953   5.3502540  8.86764651  0.87963558
#>       10   7.172909824   0.8686961  8.92988008  3.81374732
#>       11   0.991031399  10.2332113  1.89644594  3.43243605
#>       12  -1.584871574   7.1784205  4.26930915 -5.20627855
#>       13   0.732818989   7.3039028  0.50650715 -4.54522355
#>       14   5.447505729   0.2349618 -0.30509849  7.37454008
#>       15   6.180503083   7.8728580  0.56675275  2.21244092
#>       16   8.111140275  13.7112499  6.10796214  6.09010450
#>       17   7.404334735  -4.0393964  6.13616693  8.63655395
#>       18   5.065306530   9.3514496  0.93917231 10.81730425
#>       19   6.595009921   7.3376411 -0.25161267  2.29580552
#>       20  -6.141777930  -0.4181885  4.01895927  8.02861731
#>       21  10.998473912   4.8269561  5.36039410  6.59446197
#>       22   8.748750597  -0.3605767  0.65236100  4.77379635
#>       23   8.479223008   0.8569094  2.18232401 -1.71134333
#>       24   9.515087161   0.3431308 -0.47808994  8.38076792
#>       25   1.368600891   8.4293964  2.37151659  1.96883059
#>       26   6.328315654   9.6024486  1.99824582  5.52765102
#>       27   7.799157426   0.2719572  3.00600558  6.10060909
#>       28   5.504497775   3.5261792  2.61449547  5.13685962
#>       29   7.154337317   6.6350413  4.05403279  6.26798609
#>       30   7.154337317   4.8731366  4.72705540  6.21822025
#>       31   4.498041388   5.7494789  2.07701271  2.24644098
#>       32   3.972859301  -0.0552018  1.71709968  1.01563071
#>       33   1.246672116   9.6939981  5.67656775  4.37682520
#>       34   6.715446883   1.2563140  7.35979809 -0.15011826
#>       35   2.593476464   4.6005333  3.34758639  6.85536829
#>       36   4.150923157   2.6221442  3.46540535  0.80866989
#>       37   5.576988102   7.0896455  7.55994315  7.31015271
#>       38   6.479588155   3.2713094  1.07576229  0.70653658
#>       39   6.714914933  10.5019605  0.87317333  3.45283097
#>       40   3.092833896  -3.0056665  8.28687405  6.15700804
#>       41   4.667634250   5.7776049  9.20058491  6.94590217
#>       42   0.443470401   3.3459415  7.76326303  7.69327441
#>       43   7.659139022   4.5800481  3.64745811  5.71361755
#>       44   2.771024107   6.2521502  4.02172175  1.09474532
#>       45  -2.533628504   3.0365350  5.59027281  4.14183957
#>       46   3.623976143   6.3778728  3.77182163  4.65610892
#>       47   4.177536768  -1.1571581  6.76726892  4.83530335
#>       48   5.189133838  10.3626756  6.73120973  2.36025147
#>       49   3.700737411   2.2621509 -2.15804595  5.18949994
#>       50   7.910712779   1.9472608 -0.81877383  6.96445721
#>       51   2.589031398   9.0605670 -3.29425505  0.78386808
#>       52   2.482615981   6.4514427  8.99413915  4.92198428
#>       53   0.907575663   4.7628599  9.99861639  4.12501340
#>       54   0.377394675  -2.7548612  4.85299318  4.38195376
#>       55   7.949271925   8.1971198  0.03388132  0.59589843
#>       56   2.332970667   6.4147976  5.44475101  3.08923892
#>       57   4.332468078   6.0694052  6.49611407  5.61329746
#>       58   1.729912103   3.4303524  2.08284445  5.80087096
#>       59   1.419622625   4.8609672  3.42615485  2.68913918
#>       60   6.577866229   6.7151986  3.86646326  4.23484000
#>       61  11.690832364   7.2758048  1.89767270  0.07643546
#>       62  -2.336123539   6.4285112  6.78704544  6.75807164
#>       63  -1.996144462   3.2423664  3.67681516  5.35730452
#>       64  13.607246151   9.4315314  2.80134400  3.36597040
#>       65  -3.632647487  -0.9790194  1.55813764  3.61442662
#>       66  12.879658954   8.6918646  5.07969585  3.35760729
#>       67   0.888618383   3.7197851  5.33168512  3.62716863
#>       68   0.902805722   5.7473411  4.05880826  0.83301183
#>       69   1.677317438   6.7786534  6.43319154  7.19935256
#>       70   7.727975737   8.8073091  6.14945199 -0.19999135
#>       71   9.790179925   0.4753524  6.75459412  1.54498835
#>       72   5.919052946   9.8915144  5.75546387  8.58272995
#>       73   2.746375368  -1.8479045  4.77007413  0.67972393
#>       74   6.479242789  11.2002490  4.44370854  8.24150349
#>       75   2.768362673   2.9683601  6.27212042  0.99399372
#>       76   7.434553938   7.0137450  3.15152477  2.54345797
#>       77   2.667748017   3.6667249  4.95460833  7.21851228
#>       78   5.528706765   1.4107874  5.29437266  5.12487891
#>       79  12.001222505   6.5853663  3.50990665  2.41520814
#>       80  -0.896870228   3.4569871 -0.54045445  3.81524258
#>       81  -0.101887371   3.9422212 -0.60547524  5.15624656
#>       82   5.276273611   5.8307438 -0.67336840  3.74296853
#>       83  -0.009972747   7.4963181 -0.74437667  0.53696452
#>       84   1.761676222  -1.2541562  1.00657412  7.98856351
#>       85  -1.065626425   7.0098511  3.14241272  7.83568123
#>       86   5.327504662   1.2485226  6.64058798  6.18030063
#>       87   6.328764411   1.8698705  4.78821655  5.81531118
#>       88   2.452525963   5.8221195  2.89403878  3.15575203
#>       89   3.733483884   3.3794860  9.12932297  3.22457344
#>       90   2.791900326   5.5873786  0.41082914  5.44679063
#>       91   6.123445484   5.8921388  6.38032817  1.06898827
#>       92   3.651788529  10.6527701  7.46521057  6.69428679
#>       93   5.425330551   3.9578860  8.33327864  5.16954115
#>       94   4.815524177   2.6667289  6.31708788  0.97997759
#>       95   3.507938034   7.5924910  1.33994762  5.68731206
#>       96   6.775165528   0.7585901  7.62555057  3.28055582
#>       97   1.856798224   8.1536791  3.92688491  5.03958552
#>       98   6.172828280   1.5199011  0.15791566  2.72636415
#>       99   1.548534686   8.5303112  3.17470254  0.62442662
#>       100  7.527890706  -1.4671966  3.57555760  7.04723309

d <- as_draws_rvars(example_draws("multi_normal"))
pareto_smooth(d$Sigma)
#> Pareto k-hat = 0.05.
#> Pareto k-hat = 0.04.
#> Pareto k-hat = 0.05.
#> Pareto k-hat = 0.04.
#> Pareto k-hat = 0.09.
#> Pareto k-hat = 0.07.
#> Pareto k-hat = 0.05.
#> Pareto k-hat = 0.07.
#> Pareto k-hat = -0.09.
#> rvar<100,4>[3,3] mean ± sd:
#>      [,1]          [,2]          [,3]         
#> [1,]  1.28 ± 0.17   0.53 ± 0.21  -0.40 ± 0.29 
#> [2,]  0.53 ± 0.21   3.67 ± 0.45  -2.10 ± 0.49 
#> [3,] -0.40 ± 0.29  -2.10 ± 0.49   8.12 ± 0.96 
```
