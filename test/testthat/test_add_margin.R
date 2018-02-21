context("add_margin")
test_that("the sample size in additive margin", {
  expect_equal( round(add.margin(0.02, 0.04)$control, 3) , 1569.776)
  expect_equal( round(add.margin(0.09, 0.05, 0.06)$eff, 3), 1.002)
  expect_error( add.margin(-0.02, 20, 10))
  expect_error( add.margin(0.05, -0.04))
})
