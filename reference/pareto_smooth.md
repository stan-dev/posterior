# Pareto smoothing

Smooth the tail draws of x by replacing tail draws by order statistics
of a generalized Pareto distribution fit to the tail(s). For further
details see Vehtari et al. (2024).

## Usage

``` r
pareto_smooth(x, ...)

# S3 method for class 'rvar'
pareto_smooth(x, return_k = FALSE, extra_diags = FALSE, ..., r_eff = NULL)

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
    [`extract_variable_matrix()`](https://mc-stan.org/posterior/reference/extract_variable_matrix.md).

  - An [`rvar`](https://mc-stan.org/posterior/reference/rvar.md).

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

- r_eff:

  Ignored. A value other than `NULL` or 1 gives a warning. Default is
  `NULL`.

- tail:

  (string) The tail to diagnose/smooth:

  - `"right"`: diagnose/smooth only the right (upper) tail

  - `"left"`: diagnose/smooth only the left (lower) tail

  - `"both"`: diagnose/smooth both tails and return the maximum k-hat
    value

  The default is `"both"`.

- ndraws_tail:

  (numeric) number of draws for the tail. If `ndraws_tail` is not
  specified, it is calculated with
  [`ps_tail_length()`](https://mc-stan.org/posterior/reference/ps_tail_length.md).

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

[`pareto_khat`](https://mc-stan.org/posterior/reference/pareto_khat.md)
for only calculating khat, and
[`pareto_diags`](https://mc-stan.org/posterior/reference/pareto_diags.md)
for additional diagnostics.

## Examples

``` r
mu <- extract_variable_matrix(example_draws(), "mu")
pareto_smooth(mu)
#> Pareto k-hat = 0.27.
#>          chain
#> iteration           1            2            3           4
#>       1    2.00583113   2.99038071  1.794368010  6.45897880
#>       2    1.45831611   8.05045285  5.986371174  9.27305004
#>       3    5.81494730  -1.22174775  2.557202015  0.24060801
#>       4    6.84958624  11.45412096  2.794425215  3.69252884
#>       5    1.80516774  10.12198164  0.079374715  5.48027067
#>       6    3.84124251 -10.90481295  1.063616999  2.37951413
#>       7    5.47427294  -7.69861020  3.673857161 12.41860708
#>       8    1.20306170   1.79105196  3.505838358  4.90456023
#>       9    0.14669225   5.35025400  8.860400744  0.87963558
#>       10   7.17290982   0.86869612  8.923524055  3.81374732
#>       11   0.99103140  10.24453637  1.896445937  3.43243605
#>       12  -1.43029660   7.17842051  4.269309153 -5.00404887
#>       13   0.58420595   7.30390277  0.407186018 -4.31747742
#>       14   5.44750573   0.17889068 -0.284502987  7.37927655
#>       15   6.18050308   7.85084814  0.457401859  2.21244092
#>       16   8.09275858  13.71124994  6.107962144  6.09010450
#>       17   7.38492366  -3.80208547  6.136166934  8.62596897
#>       18   5.06530653   9.35101311  0.939172310 10.83594402
#>       19   6.59500992   7.33764114 -0.238063277  2.29580552
#>       20  -6.00017916  -0.38308537  4.018959271  8.00898554
#>       21  11.01930318   4.82695606  5.360394098  6.59446197
#>       22   8.73979376  -0.33279813  0.528484284  4.77379635
#>       23   8.46633300   0.85735481  2.182324011 -1.54704102
#>       24   9.51689732   0.27021328 -0.435516364  8.36642252
#>       25   1.36860089   8.41577112  2.371516590  1.96883059
#>       26   6.32831565   9.60544635  1.998245821  5.52765102
#>       27   7.77601383   0.21018102  3.006005576  6.10060909
#>       28   5.50449777   3.52617924  2.614495475  5.13685962
#>       29   7.15433732   6.63504133  4.054032794  6.26798609
#>       30   7.15433732   4.87313661  4.727055398  6.21822025
#>       31   4.49804139   5.74947888  2.077012713  2.24644098
#>       32   3.97285930  -0.06855225  1.717099685  1.01563071
#>       33   1.24667212   9.69823154  5.676567746  4.37682520
#>       34   6.71544688   1.25631396  7.359798091 -0.15026742
#>       35   2.59347646   4.60053327  3.347586394  6.85536829
#>       36   4.15092316   2.62214416  3.465405352  0.78920171
#>       37   5.57698810   7.08964546  7.533082659  7.31015271
#>       38   6.47958816   3.27130941  1.075762291  0.57330077
#>       39   6.71491493  10.51669922  0.873173334  3.45283097
#>       40   3.09283390  -2.77649511  8.271131451  6.15700804
#>       41   4.66763425   5.77760488  9.198051660  6.94590217
#>       42   0.35447413   3.34594153  7.739565332  7.66849260
#>       43   7.63382670   4.58004814  3.647458111  5.71361755
#>       44   2.77102411   6.25215016  4.021721750  1.09474532
#>       45  -2.32090706   3.03653496  5.590272809  4.14183957
#>       46   3.62397614   6.37787278  3.771821625  4.65610892
#>       47   4.17753677  -1.04009050  6.767268922  4.83530335
#>       48   5.18913384  10.37565527  6.731209730  2.36025147
#>       49   3.70073741   2.26215093 -1.964282145  5.18949994
#>       50   7.88928309   1.94726080 -0.736502722  6.96445721
#>       51   2.58903140   9.05606614 -3.059002790  0.78196021
#>       52   2.48261598   6.45144266  8.988697554  4.92198428
#>       53   0.90757566   4.76285987 10.006895568  4.12501340
#>       54   0.29903564  -2.53341675  4.852993180  4.38195376
#>       55   7.92843176   8.18003324  0.007795561  0.48163830
#>       56   2.33297067   6.41479763  5.444751011  3.08923892
#>       57   4.33246808   6.06940520  6.496114072  5.61329746
#>       58   1.72991210   3.43035241  2.082844455  5.80087096
#>       59   1.41962263   4.86096716  3.426154852  2.68913918
#>       60   6.57786623   6.71519862  3.866463261  4.23484000
#>       61  11.71967401   7.27580476  1.897672704  0.04414761
#>       62  -2.13271591   6.42851118  6.787045436  6.75807164
#>       63  -1.81218299   3.24236645  3.676815157  5.35730452
#>       64  13.65517519   9.43219798  2.801343997  3.36597040
#>       65  -3.39404483  -0.87969979  1.558137640  3.61442662
#>       66  12.92089522   8.68208388  5.079695854  3.35760729
#>       67   0.88861838   3.71978511  5.331685118  3.62716863
#>       68   0.90280572   5.74734105  4.058808262  0.80467777
#>       69   1.67731744   6.77865339  6.433191539  7.19935256
#>       70   7.70373205   8.79919681  6.149451994 -0.19335544
#>       71   9.79570180   0.38115581  6.754594119  1.54498835
#>       72   5.91905295   9.89838277  5.755463874  8.57135928
#>       73   2.74637537  -1.67378926  4.770074134  0.55113633
#>       74   6.47924279  11.22347187  4.443708543  8.22508253
#>       75   2.76836267   2.96836015  6.272120420  0.99399372
#>       76   7.42663877   7.01374501  3.151524775  2.54345797
#>       77   2.66774802   3.66672487  4.954608335  7.21851228
#>       78   5.52870676   1.41078743  5.294372661  5.12487891
#>       79  12.03346954   6.58536629  3.509906652  2.41520814
#>       80  -0.80616168   3.45698706 -0.490259959  3.81524258
#>       81  -0.10869733   3.94222115 -0.547504845  5.15624656
#>       82   5.27627361   5.83074377 -0.607462710  3.74296853
#>       83  -0.02974723   7.50852970 -0.670372039  0.43259264
#>       84   1.76167622  -1.12795356  1.006574119  7.96832252
#>       85  -0.95752048   7.00985107  3.142412719  7.81310014
#>       86   5.32750466   1.24852261  6.640587980  6.18030063
#>       87   6.32876441   1.86987051  4.788216552  5.81531118
#>       88   2.45252596   5.82211954  2.894038781  3.15575203
#>       89   3.73348388   3.37948604  9.125790896  3.22457344
#>       90   2.79190033   5.58737855  0.327111309  5.44679063
#>       91   6.12344548   5.89213877  6.380328169  1.06898827
#>       92   3.65178853  10.66938863  7.482810947  6.69428679
#>       93   5.42533055   3.95788604  8.318227694  5.16954115
#>       94   4.81552418   2.66672890  6.317087875  0.97997759
#>       95   3.50793803   7.56613956  1.339947622  5.68731206
#>       96   6.77516553   0.76577293  7.599715114  3.28055582
#>       97   1.85679822   8.13593909  3.926884909  5.03958552
#>       98   6.17282828   1.51990114  0.113537566  2.72636415
#>       99   1.54853469   8.51817249  3.174702542  0.50532515
#>       100  7.51674351  -1.32222817  3.575557598  7.04723309

d <- as_draws_rvars(example_draws("multi_normal"))
pareto_smooth(d$Sigma)
#> Pareto k-hat = 0.02.
#> Pareto k-hat = 0.03.
#> Pareto k-hat = 0.06.
#> Pareto k-hat = 0.03.
#> Pareto k-hat = 0.07.
#> Pareto k-hat = 0.15.
#> Pareto k-hat = 0.06.
#> Pareto k-hat = 0.15.
#> Pareto k-hat = -0.07.
#> rvar<100,4>[3,3] mean ± sd:
#>      [,1]          [,2]          [,3]         
#> [1,]  1.28 ± 0.17   0.53 ± 0.21  -0.40 ± 0.29 
#> [2,]  0.53 ± 0.21   3.67 ± 0.45  -2.10 ± 0.49 
#> [3,] -0.40 ± 0.29  -2.10 ± 0.49   8.12 ± 0.96 
```
