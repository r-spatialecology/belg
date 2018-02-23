
<!-- README.md is generated from README.Rmd. Please edit that file -->
belg
====

Boltzmann entropy (also called configurational entropy) has been recently applied to analyze entropy of landscape gradients (Gao et al. (2017), Gao et al.(2018)). The goal of **belg** is to provide an efficient implementation (in C++) of the method for calculation of the landscape gradient's Boltzmann entropy in R.

Installation
------------

<!-- You can install the released version of belg from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("belg") -->
<!-- ``` -->
You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Nowosad/belg")
```

Examples
--------

As an example, we could use two small raster - `complex_land` representing a complex landscape and `simple_land` representing a simple landscape:

``` r
library(raster)
library(belg)
plot(stack(complex_land, simple_land))
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

The main function in this package, `get_boltzmann`, calculates the Boltzmann entropy of a landscape gradient:

``` r
get_boltzmann(complex_land)
#> [1] 48.43241
get_boltzmann(simple_land)
#> [1] 18.3818
```

This function accepts a `RasterLayer`, `RasterStack`, `RasterBrick`, `matrix`, or `array` object as an input. It also allows for calculation of the relative (the `relative` argument equal to `TRUE`) and absolute Boltzmann entropy of a landscape gradient. As a default, it uses a logarithm of base 10 (`log10`), however `log` and `log2` are also available options for the `base` argument.

``` r
get_boltzmann(complex_land, base = "log")
#> [1] 111.5198
get_boltzmann(complex_land, relative = TRUE)
#> [1] 35.50168
get_boltzmann(complex_land, base = "log2", relative = TRUE)
#> [1] 117.934
```

References
----------

-   Gao, Peichao, Hong Zhang, and Zhilin Li. "An efficient analytical method for computing the Boltzmann entropy of a landscape gradient." Transactions in GIS (2018).
-   Gao, Peichao, Hong Zhang, and Zhilin Li. "A hierarchy-based solution to calculate the configurational entropy of landscape gradients." Landscape Ecology 32(6) (2017): 1133-1146.
