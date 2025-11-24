# Autocovariance estimates

Compute autocovariance estimates for every lag for the specified input
sequence using a fast Fourier transform approach. The estimate for lag t
is scaled by N-t where N is the length of the sequence.

## Usage

``` r
autocovariance(x)
```

## Arguments

- x:

  (numeric vector) A sequence of values.

## Value

A numeric vector of autocovariances at every lag (scaled by N-lag).
