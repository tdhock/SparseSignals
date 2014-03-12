context("methods")

test_that("adding nonsense gives errors", {
  x <- SparseSignal(c(10L, 12L, 14L),
                    c(11L, 13L, 16L),
                    c(  1,   2,   3))
  expect_error(x + c(1,2))
  expect_error(x - c(1,2))
  for(nonsense in list(list(), function(x)x)){
    expect_error(x + nonsense, 
                 "addition only defined when y is numeric or SparseSignal")
    expect_error(x - nonsense,
                 "subtraction only defined when y is numeric or SparseSignal")
  }
})

test_that("SparseSignal + vector gives the right answer", {
  x <- SparseSignal(c(10L, 12L, 14L),
                    c(11L, 13L, 16L),
                    c(  1,   2,   3))
  plus.scalar <- x + 1
  expect_that(plus.scalar$value, equals(2:4))
  plus.vector <- x + 3:1
  expect_that(plus.vector$value, equals(c(4, 4, 4)))
  minus <- -x$value
  expect_true(nrow(x + minus) == 0)
})

test_that("SparseSignal - vector gives the right answer", {
  x <- SparseSignal(c(10L, 12L, 14L),
                    c(11L, 13L, 16L),
                    c(  1,   2,   3))
  minus.scalar <- x - 10
  expect_that(minus.scalar$value, equals((-9):(-7)))
  minus.vector <- x - c(2, 3, 4)
  expect_that(minus.vector$value, equals(c(-1, -1, -1)))
  expect_true(nrow(x - x$value) == 0)
})
