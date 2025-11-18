# Mean for sparse_numeric

Compute the mean of a `sparse_numeric` vector, counting all implicit
zeros. For other object types, this falls back to
[`base::mean()`](https://rdrr.io/r/base/mean.html).

## Usage

``` r
mean(x, ...)
```

## Arguments

- x:

  A `sparse_numeric` vector or any numeric-like object.

- ...:

  Additional arguments passed to
  [`base::mean()`](https://rdrr.io/r/base/mean.html) for non-sparse
  input.

## Value

A numeric scalar: the mean of all elements.
