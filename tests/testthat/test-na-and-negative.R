context("entropy calc on data with na and data with negative values")

set_1 = matrix(c(9, 0, 9, 0, 9, 0, 9, 0, 0, 9,
                 9, 0, 0, 9, 0, 0, 9, 9, 9, 0,
                 0, 0, 0, 0, 9, 0, 9, 0, 0, 0,
                 9, 0, 9, 9, 9, 9, 9, 9, 9, 9,
                 9, 0, 0, 9, 0, 0, 9, 0, 0, 0,
                 9, 9, 9, 9, 9, 9, 0, 9, 0, 9,
                 0, 9, 0, 9, 0, 9, 0, 0, 9, 0,
                 9, 0, 0, 9, 9, 9, 0, 9, 0, 9,
                 9, 0, 0, 0, 0, 0, 0, 0, 9, 0,
                 0, 9, 9, 0, 9, 0, 9, 9, 0, 9),
               ncol = 10)

set_2 = matrix(c(9, 0, 9, 0, 9, 0, 9, 0, 0, 9,
                 9, 0, 0, 9, 0, 0, 9, 9, 9, 0,
                 0, 0, 0, 0, 9, 0, 9, 0, 0, 0,
                 9, 0, 9, 9, 9, 9, 9, 9, 9, 9,
                 9, 0, 0, 9, 0, NA, 9, 0, 0, 0,
                 9, 9, 9, 9, 9, 9, 0, 9, 0, 9,
                 0, 9, 0, 9, 0, 9, 0, 0, 9, 0,
                 9, 0, 0, 9, 9, 9, 0, 9, 0, 9,
                 9, 0, 0, 0, 0, 0, 0, 0, 9, 0,
                 0, 9, 9, 0, 9, 0, 9, 9, 0, 9),
               ncol = 10)

ver_1 = matrix(c(1, 2, 3, 4), ncol = 2)
ver_2 = matrix(c(1, 2, 3, NA), ncol = 2)
ver_3 = matrix(c(1, 2, NA, NA), ncol = 2)
ver_4 = matrix(c(1, NA, NA, NA), ncol = 2)
ver_5 = matrix(c(NA, NA, NA, NA), ncol = 2)

ver_1b = matrix(c(-1, 0, 1, 2), ncol = 2)
ver_2b = matrix(c(-1, 0, 1, NA), ncol = 2)

# tests
test_that("entropy calc with NA gives lower values", {

  gb1 = get_boltzmann(set_1, relative = FALSE, method = "hierarchy", base = "log")
  gb2 = get_boltzmann(set_2, relative = FALSE, method = "hierarchy", base = "log")
  expect_true(gb1 > gb2)

})

test_that("entropy calc is correct on data with NA", {

  gb_v3 = get_boltzmann(ver_3, na_adjust = FALSE)
  gb_v4 = get_boltzmann(ver_4, na_adjust = FALSE)
  gb_v5 = get_boltzmann(ver_5, na_adjust = FALSE)

  expect_equal(gb_v3, 0.301, tolerance = 1e-3)
  expect_equal(gb_v4, 0, tolerance = 1e-3)
  expect_equal(gb_v5, 0, tolerance = 1e-3)

})

test_that("entropy calc is correct on data with negative values", {

  gb_v1a = get_boltzmann(ver_1)
  gb_v2a = get_boltzmann(ver_2)
  gb_v1b = get_boltzmann(ver_1b)
  gb_v2b = get_boltzmann(ver_2b)

  expect_equal(gb_v1a, gb_v1b)
  expect_equal(gb_v2a, gb_v2b)

})

