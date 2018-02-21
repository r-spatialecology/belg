# data prep ---------------------------------------------------------------
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

# compare the results -----------------------------------------------------
library(purrr)
library(testthat)
set_list = list(set1a, set1b, set1c,
                set2d, set2e, set2f,
                set3g, set3h, set3i,
                set4j, set4k, set4l,
                set5m, set5n, set5o)

set_outputs = set_list %>%
  map_dbl(get_boltzmann, relative = FALSE)

tibble::tibble(expected = set_results,
               obtained = set_outputs)

expect_equal(set_results, round(set_outputs, 1))

# get_boltzmann2(set2e, relative = FALSE)
# get_boltzmann2(set2f, relative = FALSE)
