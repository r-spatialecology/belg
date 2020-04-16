context("entropy calc on Raster*")

library(raster)
library(stars)
stack_land = stack(simple_land, complex_land)
brick_land = brick(simple_land, complex_land)

# raster
simple_output = get_boltzmann(simple_land, relative = FALSE, method = "hierarchy")
stack_output = get_boltzmann(stack_land, relative = FALSE, method = "hierarchy", base = "log2")
brick_output = get_boltzmann(brick_land, relative = FALSE, method = "hierarchy", base = "log2")

# stars
stars_output1 = get_boltzmann(st_as_stars(simple_land), relative = FALSE, method = "hierarchy")
stars_output2 = get_boltzmann(st_as_stars(stack_land), relative = FALSE, method = "hierarchy", base = "log2")
stars_output3 = get_boltzmann(st_as_stars(brick_land), relative = FALSE, method = "hierarchy", base = "log2")

test_that("entropy calc on a RasterLayer is correct", {

  # calculations are correct
  expect_equal(104.858, simple_output, tolerance = 1e-3)

  # bad inputs
  # warnings
})

test_that("entropy calc on a RasterStack is correct", {

  # calculations are correct
  expect_equal(c(348.331, 635.008), stack_output, tolerance = 1e-3)

  # bad inputs
  # warnings
})

test_that("entropy calc on a RasterBrick is correct", {

  # calculations are correct
  expect_equal(c(348.331, 635.008), brick_output, tolerance = 1e-3)

  # bad inputs
  # warnings
})

test_that("entropy calc on stars objects are correct", {

  # calculations are correct
  expect_equal(simple_output, stars_output1)
  expect_equal(stack_output, stars_output2)
  expect_equal(brick_output, stars_output3)

  # bad inputs
  # warnings
  expect_warning(get_boltzmann(c(st_as_stars(simple_land),
                                 st_as_stars(simple_land)),
                                 relative = FALSE,
                               method = "hierarchy"))

})
