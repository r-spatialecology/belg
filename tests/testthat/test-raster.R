context("entropy calc on Raster*")

test_that("entropy calc on a RasterLayer is correct", {

  # calculations are correct
  simple_output = get_boltzmann(simple_land, relative = FALSE)
  expect_equal(18.382, simple_output, tolerance = 1e-3)

  # bad inputs
  # warnings
})

test_that("entropy calc on a RasterStack is correct", {

  stack_land = stack(simple_land, complex_land)

  # calculations are correct
  stack_output = get_boltzmann(stack_land, relative = FALSE, base = "log2")
  expect_equal(c(61.063, 160.889), stack_output, tolerance = 1e-3)

  # bad inputs
  # warnings
})
