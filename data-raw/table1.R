# data prep ---------------------------------------------------------------
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

# compare the results -----------------------------------------------------
library(purrr)
library(testthat)
case_list = list(case1, case2, case3, case4,
                 case5, case6, case7, case8)

case_outputs = case_list %>%
  map_dbl(get_boltzmann)

tibble::tibble(expected = case_results,
               obtained = case_outputs)

expect_equal(case_results, round(case_outputs, 4))
