context("new aggregation method (2019)")

new_c = c(56, 86, 98, 50, 45, 56, 96, 25,
          15, 55, 85, 69, 12, 52, 25, 56,
          32, 25, 68, 98, 58, 66, 56, 58,
          15, 55, 85, 69, 12, 52, 25, 56)
m = matrix(new_c, ncol = 8)

new_c = c(56, 86, 98, 50, 45, 56, 96, 25,
          15, 55, 85, 69, 12, 52, 25, 56,
          32, 25, 68, 98, 58, 66, 56, 58)

lg = matrix(new_c, nrow = 3, ncol = 8, byrow = TRUE)

test_that("test correctness of new aggregation method", {

  mr = get_boltzmann(m, relative = TRUE, method = "aggregation")
  ma = get_boltzmann(m, relative = FALSE, method = "aggregation")

  expect_equal(20.986, mr, tolerance = 1e-3)
  expect_equal(25.84, ma, tolerance = 1e-3)
})

test_that("test dimensions for the aggregation method", {

  expect_error(get_boltzmann(lg, method = "aggregation"))
})
