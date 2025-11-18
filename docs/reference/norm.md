# Norm for sparse_numeric

Compute the Euclidean (L2) norm of a `sparse_numeric` vector. For other
object types, this falls back to
[`base::norm()`](https://rdrr.io/r/base/norm.html).

## Usage

``` r
norm(x, ...)
```

## Arguments

- x:

  A `sparse_numeric` vector or another object suitable for
  [`base::norm()`](https://rdrr.io/r/base/norm.html).

- ...:

  Additional arguments passed to
  [`base::norm()`](https://rdrr.io/r/base/norm.html) for non-sparse
  input.

## Value

A numeric scalar giving the Euclidean norm.
