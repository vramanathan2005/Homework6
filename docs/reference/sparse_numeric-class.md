# Sparse numeric vector class

An S4 class for storing sparse numeric vectors using positions and
values.

## Usage

``` r
# S4 method for class 'sparse_numeric,sparse_numeric'
e1 + e2

# S4 method for class 'sparse_numeric,sparse_numeric'
e1 - e2

# S4 method for class 'sparse_numeric,sparse_numeric'
e1 * e2
```

## Arguments

- e1, e2:

  `sparse_numeric` vectors used as left and right operands for
  arithmetic operators like `+`, `-`, and `*`.

## Functions

- `e1 + e2`: Element-wise addition (+) for sparse_numeric vectors

- `e1 - e2`: Element-wise subtraction (-) for sparse_numeric vectors

- `e1 * e2`: Element-wise multiplication (\*) for sparse_numeric vectors

## Slots

- `value`:

  Numeric vector of non-zero values.

- `pos`:

  Integer vector of positions (1-based) for the non-zero values.

- `length`:

  Integer giving the full length of the underlying dense vector.
