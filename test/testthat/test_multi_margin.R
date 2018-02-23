context("")
test_that("the sample size for multiplicative margin", {
  expect_equal(round(multi.margin(1.2, 10, 2, 2)$control, 2), 8.63)
  expect_equal(round(multi.margin(1.15, 2.2, 3, 2)$control, 2), 419.25)
  expect_error(multi.margin(0.5, 20, 20, 10))
  expect_error(multi.margin(1.5, -10, -10, 10))
})
