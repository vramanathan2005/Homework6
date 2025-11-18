# Standardize a sparse_numeric vector

Standardizes a `sparse_numeric` vector by subtracting the mean and
dividing by the standard deviation. Implicit zeros are included in the
calculations by converting to a dense numeric vector and then back to
`sparse_numeric`.

## Usage

``` r
standardize(x, ...)
```

## Arguments

- x:

  A `sparse_numeric` vector.

- ...:

  Ignored.

## Value

A new standardized `sparse_numeric` vector.
