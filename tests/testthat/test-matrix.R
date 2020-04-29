context("entropy calc on matrix/array")
# data prep ---------------------------------------------------------------
# software example
new_c = c(56, 86, 98, 50, 45, 56, 96, 25,
          15, 55, 85, 69, 12, 52, 25, 56,
          32, 25, 68, 98, 58, 66, 56, 58)

lg = matrix(new_c, nrow = 3, ncol = 8, byrow = TRUE)

# Gao, Peichao, Hong Zhang, and Zhilin Li. "A hierarchy-based solution to
# calculate the configurational entropy of landscape gradients."
# Landscape Ecology 32.6 (2017): 1133-1146.
set1a = matrix(c(rep(0, 18), rep(2, 18)), ncol = 6)
set1b = matrix(c(rep(0, 32), rep(2, 32)), ncol = 8)
set1c = matrix(c(rep(0, 50), rep(2, 50)), ncol = 10)
set2d = matrix(c(6, rep(2, 99)), ncol = 10)
set2e = matrix(c(rep(2, 22), 6, rep(2, 77)), ncol = 10)
set2f = matrix(c(rep(2, 44), 6, rep(2, 55)), ncol = 10)
set3g = matrix(c(rep(2, 45), 10, rep(2, 54)), ncol = 10)
set3h = matrix(c(rep(2, 45), 20, rep(2, 54)), ncol = 10)
set3i = matrix(c(rep(2, 45), 30, rep(2, 54)), ncol = 10)
set4j = matrix(c(rep(0, 50), rep(1, 50)), ncol = 10)
set4k = matrix(c(rep(0, 50), rep(4, 50)), ncol = 10)
set4l = matrix(c(rep(0, 50), rep(7, 50)), ncol = 10)
set5m = matrix(c(rep(0, 50), rep(9, 50)), ncol = 10)
set5n = matrix(c(rep(0, 20), rep(9, 10), rep(0, 20),
                 rep(9, 10), rep(0, 10), rep(9, 30)),
               ncol = 10)
set5o = matrix(c(9, 0, 9, 0, 9, 0, 9, 0, 0, 9,
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

set_results = c(18.7, 34.5, 54.9,
                1.2, 17.3, 41.0,
                50.8, 91.2, 110.8,
                27.2, 102.4, 141.5,
                167.6, 247.0, 282.2)

set_list = list(set1a, set1b, set1c,
                set2d, set2e, set2f,
                set3g, set3h, set3i,
                set4j, set4k, set4l,
                set5m, set5n, set5o)

# Gao, Peichao, Hong Zhang, and Zhilin Li. "An efficient
# analytical method for computing the Boltzmann entropy of a landscape
# gradient." Transactions in GIS (2018).
case1 = matrix(c(4, 4, 4, 4), ncol = 2)
case2 = matrix(c(4, 5, 4, 5), ncol = 2)
case3 = matrix(c(2, 2, 2, 8), ncol = 2)
case4 = matrix(c(4, 5, 4, 9), ncol = 2)
case5 = matrix(c(2, 5, 5, 8), ncol = 2)
case6 = matrix(c(1, 5, 4, 8), ncol = 2)
case7 = matrix(c(2, 5, 5, 9), ncol = 2)
case8 = matrix(c(1, 5, 4, 9), ncol = 2)

case_results = c(0, 0.7782, 0.6021, 1.0792,
                 1.8195, 1.8921, 1.8573, 1.9243)

case_array = array(c(case1, case2, case3, case4,
                     case5, case6, case7, case8),
                   dim = c(2, 2, 8))

# negative example
new_c2 = c(56, 86, 98, 50, 45, 56, 96, 25,
          15, 55, -85, 69, 12, 52, 25, 56,
          32, 25, 68, 98, 58, 66, 56, 58)

lg2 = matrix(new_c2, nrow = 3, ncol = 8, byrow = TRUE)

# na example
new_c3 = c(56, 86, 98, 50, 45, 56, 96, 25,
           15, 55, NA, 69, 12, 52, 25, 56,
           32, 25, 68, 98, 58, 66, 56, 58)

lg3 = matrix(new_c3, nrow = 3, ncol = 8, byrow = TRUE)

# tests
test_that("relative entropy calc is correct on matrix", {

  # calculations are correct #1
  expect_equal(get_boltzmann(lg, relative = TRUE, method = "hierarchy", base = "log"), 81.745, tolerance = 1e-3)
  expect_equal(get_boltzmann(lg, relative = TRUE, method = "hierarchy", base = "log2"), 117.934, tolerance = 1e-3)
  expect_equal(get_boltzmann(lg, relative = TRUE, method = "hierarchy", base = "log10"), 35.501, tolerance = 1e-3)

  expect_equal(get_boltzmann(lg2, relative = TRUE, method = "hierarchy", base = "log10"), 36.699, tolerance = 1e-3)

  # warnings
})

test_that("absolute entropy calc is correct on matrix", {

  # calculations are correct #1
  expect_equal(get_boltzmann(lg, relative = FALSE, method = "hierarchy", base = "log"), 111.519, tolerance = 1e-3)
  expect_equal(get_boltzmann(lg, relative = FALSE, method = "hierarchy", base = "log2"), 160.889, tolerance = 1e-3)
  expect_equal(get_boltzmann(lg, relative = FALSE, method = "hierarchy", base = "log10"), 48.432, tolerance = 1e-3)

  expect_equal(get_boltzmann(lg2, relative = FALSE, method = "hierarchy", base = "log10"), 49.375, tolerance = 1e-3)


  # calculations are correct #2
  set_outputs = unlist(lapply(set_list, get_boltzmann, relative = FALSE, method = "hierarchy"))
  expect_equal(set_results, set_outputs, tolerance = 1e-3)

  # bad inputs
  # warnings
})

test_that("relative entropy calc is correct on array", {

  # calculations are correct #3
  case_outputs = get_boltzmann(case_array, relative = TRUE, method = "hierarchy")
  expect_equal(case_results, case_outputs, tolerance = 1e-3)

  # bad inputs
  # warnings
})


