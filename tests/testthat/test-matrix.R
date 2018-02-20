context("entropy calc on matrix")
# data create
new_c = c(56, 86, 98, 50, 45, 56, 96, 25,
          15, 55, 85, 69, 12, 52, 25, 56,
          32, 25, 68, 98, 58, 66, 56, 58)

lg = matrix(new_c, nrow = 3, ncol = 8, byrow = TRUE)

test_that("relative entropy calc is correct on matrix", {

  # calculations are correct
  expect_equal(get_boltzmann(lg, relative = TRUE), 81.745, tolerance = 1e-3)
  expect_equal(get_boltzmann(lg, relative = TRUE, base = "log2"), 117.934, tolerance = 1e-3)
  expect_equal(get_boltzmann(lg, relative = TRUE, base = "log10"), 35.501, tolerance = 1e-3)

  # bad inputs
  # warnings
})

test_that("absolute entropy calc is correct on matrix", {

  # calculations are correct
  # expect_equal(get_boltzmann(lg, relative = FALSE), 111.519, tolerance = 1e-3)
  # expect_equal(get_boltzmann(lg, relative = FALSE, base = "log2"), 160.889, tolerance = 1e-3)
  # expect_equal(get_boltzmann(lg, relative = FALSE, base = "log10"), 48.432, tolerance = 1e-3)

  # bad inputs
  # warnings
})
