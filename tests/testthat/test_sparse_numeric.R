test_that("numeric <-> sparse_numeric round trip works", {
  x <- c(0, 5, 0, -2, 0, 3)

  sx <- as(x, "sparse_numeric")
  expect_s4_class(sx, "sparse_numeric")
  expect_equal(sx@length, length(x))

  x_back <- as(sx, "numeric")
  expect_equal(x_back, x)
})

test_that("sparse_add, sparse_sub, sparse_mult, sparse_crossprod behave like dense ops", {
  x <- c(0, 5, 0, -2, 0, 3)
  y <- c(1, 0, 4, -2, 0, 0)

  sx <- as(x, "sparse_numeric")
  sy <- as(y, "sparse_numeric")

  # add
  s_add <- sparse_add(sx, sy)
  expect_equal(as(s_add, "numeric"), x + y)

  # sub
  s_sub <- sparse_sub(sx, sy)
  expect_equal(as(s_sub, "numeric"), x - y)

  # elementwise mult
  s_mul <- sparse_mult(sx, sy)
  expect_equal(as(s_mul, "numeric"), x * y)

  # crossprod (dot product)
  s_cp <- sparse_crossprod(sx, sy)
  expect_equal(s_cp, sum(x * y))
})

test_that("mean for sparse_numeric matches dense mean", {
  x <- c(0, 5, 0, 5)
  sx <- as(x, "sparse_numeric")

  expect_equal(mean(sx), mean(x))
})

test_that("norm for sparse_numeric matches Euclidean norm", {
  x <- c(0, 3, 4, 0)
  sx <- as(x, "sparse_numeric")

  expect_equal(norm(sx), sqrt(sum(x^2)))
})

test_that("standardize gives mean ~ 0 and sd ~ 1", {
  x  <- c(0, 5, 0, 5)
  sx <- as(x, "sparse_numeric")

  s_std <- standardize(sx)
  v_std <- as(s_std, "numeric")

  expect_equal(length(v_std), length(x))
  expect_equal(mean(v_std), 0, tolerance = 1e-8)
  expect_equal(sd(v_std),   1, tolerance = 1e-8)
})

test_that("errors are thrown for invalid operations", {
  # length-0 vector: mean and standardize should error
  empty <- new(
    "sparse_numeric",
    value  = numeric(0),
    pos    = integer(0),
    length = 0L
  )

  expect_error(mean(empty))
  expect_error(standardize(empty))

  # mismatched lengths in arithmetic
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(1, 0, 2, 3), "sparse_numeric")

  expect_error(sparse_add(x, y))
  expect_error(sparse_sub(x, y))
  expect_error(sparse_mult(x, y))
  expect_error(sparse_crossprod(x, y))
})

test_that("constructor stores values and slots correctly", {
  x <- new(
    "sparse_numeric",
    value  = c(5, 0, -2),
    pos    = c(1L, 4L, 7L),
    length = 10L
  )

  expect_s4_class(x, "sparse_numeric")
  expect_equal(x@value,  c(5, 0, -2))
  expect_equal(x@pos,    c(1L, 4L, 7L))
  expect_equal(x@length, 10L)
})

test_that("constructor / validity catches bad inputs", {
  # value and pos lengths differ
  expect_error(
    new("sparse_numeric",
        value  = 1:3,
        pos    = 1:2,
        length = 5L),
    "Lengths of 'value' and 'pos' must match"
  )

  # positions out of range
  expect_error(
    new("sparse_numeric",
        value  = 1:2,
        pos    = c(0L, 6L),
        length = 5L),
    "'pos' values must be within 1 and 'length'"
  )

  # duplicated positions
  expect_error(
    new("sparse_numeric",
        value  = c(1, 2),
        pos    = c(2L, 2L),
        length = 5L),
    "'pos' cannot contain duplicates"
  )
})

test_that("as(\"numeric\") correctly reconstructs dense vector", {
  x <- new(
    "sparse_numeric",
    value  = c(10, -3),
    pos    = c(2L, 5L),
    length = 6L
  )
  expect_equal(as(x, "numeric"), c(0, 10, 0, 0, -3, 0))
})

test_that("arithmetic operations work on overlapping / non-overlapping entries", {
  x <- new(
    "sparse_numeric",
    value  = c(1, 2),
    pos    = c(1L, 3L),
    length = 5L
  )
  y <- new(
    "sparse_numeric",
    value  = c(3, -1),
    pos    = c(3L, 5L),
    length = 5L
  )

  expect_equal(as(x + y, "numeric"), c(1, 0, 5, 0, -1))
  expect_equal(as(x - y, "numeric"), c(1, 0, -1, 0, 1))
  expect_equal(as(x * y, "numeric"), c(0, 0, 6, 0, 0))
})

test_that("arithmetic throws error when lengths differ", {
  x <- new(
    "sparse_numeric",
    value  = 1,
    pos    = 1L,
    length = 5L
  )
  y <- new(
    "sparse_numeric",
    value  = 2,
    pos    = 1L,
    length = 7L
  )

  expect_error(x + y, "Vectors must have the same length")
  expect_error(x - y, "Vectors must have the same length")
  expect_error(x * y, "Vectors must have the same length")
})

test_that("mean handles zero, negative, and all-zero vectors", {
  # all implicit zeros
  x0 <- new(
    "sparse_numeric",
    value  = numeric(0),
    pos    = integer(0),
    length = 10L
  )
  expect_equal(mean(x0), 0)

  # positive + negative values
  x1 <- new(
    "sparse_numeric",
    value  = c(-5, 5),
    pos    = c(2L, 4L),
    length = 4L
  )
  expect_equal(mean(x1), 0)

  # single non-zero
  x2 <- new(
    "sparse_numeric",
    value  = 10,
    pos    = 4L,
    length = 5L
  )
  expect_equal(mean(x2), 10 / 5)
})

test_that("norm computes Euclidean norm correctly, including empty case", {
  x <- new(
    "sparse_numeric",
    value  = c(3, 4),
    pos    = c(1L, 5L),
    length = 5L
  )
  expect_equal(norm(x), 5)  # sqrt(3^2 + 4^2)

  empty <- new(
    "sparse_numeric",
    value  = numeric(0),
    pos    = integer(0),
    length = 8L
  )
  expect_equal(norm(empty), 0)
})

test_that("standardize matches dense scale() for non-degenerate data", {
  dense <- c(0, 4, 0, 8, 0)
  sx    <- as(dense, "sparse_numeric")

  sx_std    <- standardize(sx)
  dense_std <- scale(dense)[, 1]

  expect_equal(
    as(sx_std, "numeric"),
    as.numeric(dense_std),
    tolerance = 1e-8
  )
})

test_that("standardize errors on zero-variance vector", {
  # all-implicit-zero vector => sd = 0
  y <- new(
    "sparse_numeric",
    value  = numeric(0),
    pos    = integer(0),
    length = 5L
  )

  expect_error(
    standardize(y),
    "standard deviation is zero",
    fixed = TRUE
  )
})

test_that("sparse_crossprod matches dense dot product and handles no overlap", {
  x <- new(
    "sparse_numeric",
    value  = c(1, 2, 3),
    pos    = c(1L, 3L, 5L),
    length = 6L
  )
  y <- new(
    "sparse_numeric",
    value  = c(4, -1),
    pos    = c(3L, 5L),
    length = 6L
  )

  dense_x <- as(x, "numeric")
  dense_y <- as(y, "numeric")

  expect_equal(
    sparse_crossprod(x, y),
    sum(dense_x * dense_y)
  )

  # no overlapping positions → dot product should be 0
  a <- new(
    "sparse_numeric",
    value  = 1,
    pos    = 1L,
    length = 4L
  )
  b <- new(
    "sparse_numeric",
    value  = 2,
    pos    = 4L,
    length = 4L
  )
  expect_equal(sparse_crossprod(a, b), 0)
})

test_that("validity catches non-integer positions", {
  expect_error(
    new("sparse_numeric",
        value = 1:3,
        pos   = c(1.5, 2.5, 3.5),
        length = 5L)
  )
})

test_that("validity catches positions out of range (extra test)", {
  expect_error(
    new("sparse_numeric",
        value = 1:2,
        pos   = c(0L, 6L),
        length = 5L)
  )
})

test_that("arithmetic with empty sparse vectors works", {
  a <- new("sparse_numeric",
           value  = numeric(0),
           pos    = integer(0),
           length = 5L)
  b <- new("sparse_numeric",
           value  = 3,
           pos    = 2L,
           length = 5L)

  expect_equal(as(a + b, "numeric"), c(0, 3, 0, 0, 0))
  expect_equal(as(b - a, "numeric"), c(0, 3, 0, 0, 0))
  expect_equal(as(a * b, "numeric"), c(0, 0, 0, 0, 0))
})

test_that("sparse_crossprod works for identical sparse vectors", {
  x <- new(
    "sparse_numeric",
    value  = c(2, -1),
    pos    = c(2L, 4L),
    length = 6L
  )

  expect_equal(
    sparse_crossprod(x, x),
    sum(c(0, 2, 0, -1, 0, 0)^2)
  )
})

test_that("show() prints basic summary for sparse_numeric", {
  x <- as(c(0, 5, 0, -2), "sparse_numeric")

  out <- capture.output(show(x))
  expect_true(any(grepl("Sparse numeric vector of length", out)))
  expect_true(any(grepl("Non-zero positions", out)))
  expect_true(any(grepl("Values:", out)))
})

test_that("plot() works for two sparse_numeric vectors", {
  x <- as(c(0, 5, 0, -2), "sparse_numeric")
  y <- as(c(1, 0, 4, 0), "sparse_numeric")

  # Just check that it runs without error
  expect_error(plot(x, y), NA)
})

test_that("norm() falls back to base::norm for non-sparse input", {
  z <- c(3, 4)
  # This should go through the `else base::norm(x, ...)` branch
  expect_equal(norm(z, type = "2"), base::norm(z, type = "2"))
})

test_that("standardize() errors on non-sparse input", {
  z <- c(1, 2, 3)
  expect_error(
    standardize(z),
    "standardize\\(\\) is only implemented for sparse_numeric objects\\."
  )
})
