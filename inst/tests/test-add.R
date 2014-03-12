context("add two SparseSignals")

test_that("adding nonsense gives errors", {
  x <- SparseSignal(c(12L, 10L, 14L),
                    c(13L, 11L, 16L),
                    c( 2,  1,  3))
  y <- SparseSignal(c(10L, 12L, 15L, 1000L),
                    c(11L, 13L, 17L, 1001L),
                    c( 1, -2,  4,    0))
  expect_that(addSparseSignals(x, y), throws_error("positions decreasing"))
})

test_that("adding gives the right answer", {
  x <- data.frame(1L, 2L, 5.5)
  two <- addSparseSignals(x, data.frame(4L, 10L, 3.8))
  expect_that(two$first, equals(c(1, 4)))
  expect_that(two$after, equals(c(2, 10)))
  expect_that(two$value, equals(c(5.5, 3.8)))

  neg <- data.frame(1L, 2L, -5.5)
  none <- addSparseSignals(neg, x)
  expect_equal(length(none$first), 0)
  expect_equal(length(none$after), 0)
  expect_equal(length(none$value), 0)

  three <-
    addSparseSignals(data.frame(1L, 5L, 5.5),
                     data.frame(3L, 10L, 3.3))
  expect_that(three$first, equals(c(1, 3, 5)))
  expect_that(three$after, equals(c(3, 5, 10)))
  expect_that(three$value, equals(c(5.5, 8.8, 3.3)))

  x <- data.frame(c(10L, 12L, 14L),
                  c(11L, 13L, 16L),
                  c(  1,   2,   3))
  y <- data.frame(c(10L, 12L, 15L),
                  c(11L, 13L, 17L),
                  c( 1, -2,    4))
  z <- addSparseSignals(x, y)
  expect_equal(z$first, c(10, 14, 15, 16))
  expect_equal(z$after, c(11, 15, 16, 17))
  expect_equal(z$value, c( 2,  3,  7,  4))
})
