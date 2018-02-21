context("")
test_that("the sample size for multiplicative margin", {
  expect_equal(round(multi.margin(1.2, 2)$control, 2), 863.38)
  expect_equal(round(multi.margin(1.15, 3, 2)$control, 2), 2029.17)
  expect_error(multi.margin(0.5, 20, 20))
  expect_error(multi.margin(1.5, -10, 10))
})
