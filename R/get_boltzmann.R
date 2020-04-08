#' Boltzmann entropy of a landscape gradient
#'
#' Calculates the Boltzmann entropy of a landscape gradient
#'
#' @param x RasterLayer, RasterStack, RasterBrick, matrix, or array
#' @param base A logarithm base ("log", "log2" or "log10")
#' @param relative TRUE/FALSE
#' @param method A method used. Either "hierarchy" (default) for
#' the hierarchy-based method (Gao et al., 2017) or "aggregation"
#' for the aggregation-based method (Gao et al., 2019)
#' @param scale TRUE/FALSE
#' @param resolution Resolution
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
#' get_boltzmann(lg, relative = FALSE, base = "log10")
#' get_boltzmann(lg, relative = TRUE, base = "log2")
#' get_boltzmann(lg, relative = TRUE, base = "log")
#'
#' @name get_boltzmann
#' @export
get_boltzmann = function(x, base = "log10", relative = FALSE, method = "hierarchy", scale = FALSE, resolution) UseMethod("get_boltzmann")

#' @name get_boltzmann
#' @export
get_boltzmann.default = function(x, base = "log10", relative = FALSE, method = "hierarchy", scale = FALSE, resolution){
  if (method == "hierarchy"){
    result = get_boltzmann_default(x, base, relative)
  } else if (method == "aggregation"){
    result = get_boltzmann_aggregation(x, base, relative)
  }
  if (scale){
    if (!missing(resolution)){
      result = (result) / (ncol(x) * nrow(x) * not_na_prop(x) * resolution) #* 1000000
    } else {
      result = (result) / (ncol(x) * nrow(x) * not_na_prop(x))
    }
  }
  return(result)
}

##' @name get_boltzmann
##' @export
get_boltzmann.matrix = function(x, base = "log10", relative = FALSE, method = "hierarchy", scale = FALSE, resolution){
  if (method == "hierarchy"){
    result = get_boltzmann_default(x, base, relative)
  } else if (method == "aggregation"){
    result = get_boltzmann_aggregation(x, base, relative)
  }
  if (scale){
    if (!missing(resolution)){
      result = (result) / (ncol(x) * nrow(x) * not_na_prop(x) * resolution) #* 1000000
    } else {
      result = (result) / (ncol(x) * nrow(x) * not_na_prop(x))
    }
  }
  return(result)
}

#' @name get_boltzmann
#' @export
get_boltzmann.array = function(x, base = "log10", relative = FALSE, method = "hierarchy", scale = FALSE, resolution){
  if (method == "hierarchy"){
    result = apply(x, MARGIN = 3, get_boltzmann_default, base, relative)
  } else if (method == "aggregation"){
    result = apply(x, MARGIN = 3, get_boltzmann_aggregation, base, relative)
  }
  if (scale){
    if (!missing(resolution)){
      result = (result) / (ncol(x) * nrow(x) * not_na_prop(x) * resolution) #* 1000000
    } else {
      result = (result) / (ncol(x) * nrow(x) * not_na_prop(x))
    }
  }
  return(result)
}

#' @name get_boltzmann
#' @export
get_boltzmann.RasterLayer = function(x, base = "log10", relative = FALSE, method = "hierarchy", scale = FALSE, resolution){
  if (!requireNamespace("sp", quietly = TRUE))
    stop("Package sp required, please install it first", call. = FALSE)
  if (!requireNamespace("raster", quietly = TRUE))
    stop("Package raster required, please install it first", call. = FALSE)
  if (missing(resolution)){
    resolution = raster::xres(x) * raster::yres(x)
  }
  get_boltzmann(raster::as.matrix(x), base = base, relative = relative, method = method, scale = scale, resolution = resolution)
}

#' @name get_boltzmann
#' @export
get_boltzmann.RasterStack = function(x, base = "log10", relative = FALSE, method = "hierarchy", scale = FALSE, resolution){
  if (!requireNamespace("sp", quietly = TRUE))
    stop("Package sp required, please install it first", call. = FALSE)
  if (!requireNamespace("raster", quietly = TRUE))
    stop("Package raster required, please install it first", call. = FALSE)
  if (missing(resolution)){
    resolution = raster::xres(x) * raster::yres(x)
  }
  get_boltzmann(raster::as.array(x), base = base, relative = relative, method = method, scale = scale, resolution = resolution)
}

#' @name get_boltzmann
#' @export
get_boltzmann.RasterBrick = function(x, base = "log10", relative = FALSE, method = "hierarchy", scale = FALSE, resolution){
  if (!requireNamespace("sp", quietly = TRUE))
    stop("Package sp required, please install it first", call. = FALSE)
  if (!requireNamespace("raster", quietly = TRUE))
    stop("Package raster required, please install it first", call. = FALSE)
  if (missing(resolution)){
    resolution = raster::xres(x) * raster::yres(x)
  }
  get_boltzmann(raster::as.array(x), base = base, relative = relative, method = method, scale = scale, resolution = resolution)
}

#' @name get_boltzmann
#' @export
get_boltzmann.stars = function(x, base = "log10", relative = FALSE, method = "hierarchy", scale = FALSE, resolution){
  if (!requireNamespace("stars", quietly = TRUE))
    stop("Package stars required, please install it first", call. = FALSE)
  if (length(x) > 1){
    warning("The input stars object has more than one attribute. \nBoltzmann entropy is calculated for the first attribute in the stars object only", call. = FALSE)
  }
  if (missing(resolution)){
    resolution = abs(stars::st_dimensions(x)$x$delta) * abs(stars::st_dimensions(x)$y$delta)
  }
  get_boltzmann(x[[1]], base = base, relative = relative, method = method, scale = scale, resolution = resolution)
}
