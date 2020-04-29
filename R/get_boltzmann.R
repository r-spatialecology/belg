#' Boltzmann entropy of a landscape gradient
#'
#' Calculates the Boltzmann entropy of a landscape gradient
#'
#' @param x stars, RasterLayer, RasterStack, RasterBrick, matrix, or array.
#' @param base A logarithm base ("log", "log2" or "log10").
#' @param relative Should a relative or absolute entopy be calculated? TRUE or FALSE (default).
#' @param method A method used. Either "hierarchy" for
#' the hierarchy-based method (Gao et al., 2017) or "aggregation" (default)
#' for the aggregation-based method (Gao et al., 2019).
#' @param na_adjust Should the output value be adjusted to the proportion of not missing cells? Either TRUE (default) or FALSE
#'
#' @return a numeric vector
#'
#' @details The method for computing the Boltzmann entropy of a landscape
#' gradient works on integer values that are either positive or equals to zero.
#' This function automatically rounds values to the nearest integer value
#' (rounding halfway cases away from zero) and negative values are shifted to
#' positive values.
#'
#' @references Gao, Peichao, Hong Zhang, and Zhilin Li. "A hierarchy-based
#' solution to calculate the configurational entropy of landscape gradients."
#' Landscape Ecology 32.6 (2017): 1133-1146.
#'
#' @references Gao, Peichao, Hong Zhang, and Zhilin Li. "An efficient
#' analytical method for computing the Boltzmann entropy of a landscape
#' gradient." Transactions in GIS (2018).
#'
#' @references Gao, Peichao and Zhilin Li. "Aggregation-based method
#' for computing absolute Boltzmann entropy of landscape gradient
#' with full thermodynamic consistency"
#' Landscape Ecology (2019)
#'
#' @examples
#' new_c = c(56, 86, 98, 50, 45, 56, 96, 25,
#'           15, 55, 85, 69, 12, 52, 25, 56,
#'           32, 25, 68, 98, 58, 66, 56, 58)
#'
#'
#' lg = matrix(new_c, nrow = 3, ncol = 8, byrow = TRUE)
#' get_boltzmann(lg, relative = FALSE, method = "hierarchy", base = "log10")
#' get_boltzmann(lg, relative = TRUE, method = "hierarchy", base = "log2")
#' get_boltzmann(lg, relative = TRUE, method = "hierarchy", base = "log")
#'
#' @name get_boltzmann
#' @export
get_boltzmann = function(x, method = "aggregation", na_adjust = TRUE, base = "log10", relative = FALSE) UseMethod("get_boltzmann")

#' @name get_boltzmann
#' @export
get_boltzmann.default = function(x, method = "aggregation", na_adjust = TRUE, base = "log10", relative = FALSE){
  if (method == "hierarchy"){
    result = get_boltzmann_default(x, base, relative)
  } else if (method == "aggregation"){
    result = get_boltzmann_aggregation(x, base, relative)
  }
  # if (!missing(resolution)){
  #   if (length(resolution) == 1){
  #     resolution = resolution^2
  #   } else {
  #     resolution = resolution[1] * resolution[2]
  #   }
  # } else {
  #   resolution = 1
  # }
  # if (scale == "no_of_cells"){
  #   result = (result) / (ncol(x) * nrow(x))
  # } else if (scale == "resolution"){
  #   result = (result) / (resolution) #* 1000000
  # } else if (scale == "all"){
  #   result = (result) / (ncol(x) * nrow(x) * resolution)
  # }
  if (na_adjust){
    result = (result) / (not_na_prop(x))
  }
  return(result)
}

##' @name get_boltzmann
##' @export
get_boltzmann.matrix = function(x, method = "aggregation", na_adjust = TRUE, base = "log10", relative = FALSE){
  if (method == "hierarchy"){
    result = get_boltzmann_default(x, base, relative)
  } else if (method == "aggregation"){
    result = get_boltzmann_aggregation(x, base, relative)
  }
  # if (!missing(resolution)){
  #   if (length(resolution) == 1){
  #     resolution = resolution^2
  #   } else {
  #     resolution = resolution[1] * resolution[2]
  #   }
  # } else {
  #   resolution = 1
  # }
  # if (scale == "no_of_cells"){
  #   result = (result) / (ncol(x) * nrow(x))
  # } else if (scale == "resolution"){
  #   result = (result) / (resolution) #* 1000000
  # } else if (scale == "all"){
  #   result = (result) / (ncol(x) * nrow(x) * resolution)
  # }
  if (na_adjust){
    result = (result) / (not_na_prop(x))
  }
  return(result)
}

#' @name get_boltzmann
#' @export
get_boltzmann.array = function(x, method = "aggregation", na_adjust = TRUE, base = "log10", relative = FALSE){
  if (method == "hierarchy"){
    result = apply(x, MARGIN = 3, get_boltzmann_default, base, relative)
  } else if (method == "aggregation"){
    result = apply(x, MARGIN = 3, get_boltzmann_aggregation, base, relative)
  }
  # if (!missing(resolution)){
  #   if (length(resolution) == 1){
  #     resolution = resolution^2
  #   } else {
  #     resolution = resolution[1] * resolution[2]
  #   }
  # } else {
  #   resolution = 1
  # }
  # if (scale == "no_of_cells"){
  #   result = (result) / (ncol(x) * nrow(x))
  # } else if (scale == "resolution"){
  #   result = (result) / (resolution) #* 1000000
  # } else if (scale == "all"){
  #   result = (result) / (ncol(x) * nrow(x) * resolution)
  # }
  if (na_adjust){
    result = (result) / apply(x, MARGIN = 3, not_na_prop)
  }
  return(result)
}

#' @name get_boltzmann
#' @export
get_boltzmann.RasterLayer = function(x, method = "aggregation", na_adjust = TRUE, base = "log10", relative = FALSE){
  if (!requireNamespace("sp", quietly = TRUE))
    stop("Package sp required, please install it first", call. = FALSE)
  if (!requireNamespace("raster", quietly = TRUE))
    stop("Package raster required, please install it first", call. = FALSE)
  # if (missing(resolution)){
  #   resolution = c(raster::xres(x),
  #                  raster::yres(x))
  # }
  get_boltzmann(raster::as.matrix(x), base = base, relative = relative, method = method, na_adjust = na_adjust)
}

#' @name get_boltzmann
#' @export
get_boltzmann.RasterStack = function(x, method = "aggregation", na_adjust = TRUE, base = "log10", relative = FALSE){
  if (!requireNamespace("sp", quietly = TRUE))
    stop("Package sp required, please install it first", call. = FALSE)
  if (!requireNamespace("raster", quietly = TRUE))
    stop("Package raster required, please install it first", call. = FALSE)
  # if (missing(resolution)){
  #   resolution = c(raster::xres(x),
  #                  raster::yres(x))
  # }
  get_boltzmann(raster::as.array(x), base = base, relative = relative, method = method, na_adjust = na_adjust)
}

#' @name get_boltzmann
#' @export
get_boltzmann.RasterBrick = function(x, method = "aggregation", na_adjust = FALSE, base = "log10", relative = FALSE){
  if (!requireNamespace("sp", quietly = TRUE))
    stop("Package sp required, please install it first", call. = FALSE)
  if (!requireNamespace("raster", quietly = TRUE))
    stop("Package raster required, please install it first", call. = FALSE)
  # if (missing(resolution)){
  #   resolution = c(raster::xres(x),
  #                  raster::yres(x))
  # }
  get_boltzmann(raster::as.array(x), base = base, relative = relative, method = method, na_adjust = na_adjust)
}

#' @name get_boltzmann
#' @export
get_boltzmann.stars = function(x, method = "aggregation", na_adjust = TRUE, base = "log10", relative = FALSE){
  if (!requireNamespace("stars", quietly = TRUE))
    stop("Package stars required, please install it first", call. = FALSE)
  if (length(x) > 1){
    warning("The input stars object has more than one attribute. \nBoltzmann entropy is calculated for the first attribute in the stars object only", call. = FALSE)
  }
  # if (missing(resolution)){
  #   resolution = c(abs(stars::st_dimensions(x)$x$delta),
  #                  abs(stars::st_dimensions(x)$y$delta))
  # }
  get_boltzmann(x[[1]], base = base, relative = relative, method = method, na_adjust = na_adjust)
}
