context("SparseSignal")

test_that("indices can be integers or numeric",{
  ss.int <- SparseSignal(1L, 5L, 3.56)
  ss.num <- SparseSignal(1, 5, 3.56)
  expect_that(ss.int, is_a("SparseSignal"))
  expect_that(ss.num, is_a("SparseSignal"))
  for(ss in list(ss.int, ss.num)){
    expect_that(ss$first, is_a("integer"))
    expect_that(ss$after, is_a("integer"))
    expect_that(ss$value, is_a("numeric"))
  }
})

test_that("zeros are ignored",{
  ss <- SparseSignal(c(10, 12, 15, 1000),
                     c(11, 13, 17, 1001),
                     c( 1, -2,  4,    0))
  expect_true(!0 %in% ss$value)
})

test_that("SparseSignal stops on bad input", {
  expect_that(SparseSignal(list(), 5, 3.56), throws_error())
})
