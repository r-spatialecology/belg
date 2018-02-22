#' Boltzmann entropy of a landscape gradient
#'
#' Read a text output of the geoPAT 2.0 functions into R
#'
#' @param x A matrix.
#' @param base A logarithm base ("log", "log2" or "log10")
#' @param relative TRUE/FALSE
#'
#' @return a numeric vector
#'
#' @references Gao, Peichao, Hong Zhang, and Zhilin Li. "A hierarchy-based
#' solution to calculate the configurational entropy of landscape gradients."
#' Landscape Ecology 32.6 (2017): 1133-1146.
#'
#' @references Gao, Peichao, Hong Zhang, and Zhilin Li. "An efficient
#' analytical method for computing the Boltzmann entropy of a landscape
#' gradient." Transactions in GIS (2018).
#'
#' @examples
#' new_c = c(56, 86, 98, 50, 45, 56, 96, 25,
#'           15, 55, 85, 69, 12, 52, 25, 56,
#'           32, 25, 68, 98, 58, 66, 56, 58)
#'
#' lg = matrix(new_c, nrow = 3, ncol = 8, byrow = TRUE)
#' get_boltzmann(lg, relative = FALSE, base = "log10")
#' get_boltzmann(lg, relative = TRUE, base = "log2")
#' get_boltzmann(lg, relative = TRUE, base = "log")
#'
#' @name get_boltzmann
#' @export
get_boltzmann = function(x, base = "log10", relative = FALSE) UseMethod("get_boltzmann")

#' @name get_boltzmann
#' @export
get_boltzmann.default = function(x, base = "log10", relative = FALSE){
  get_boltzmann_default(x, base, relative)
}

#' @name get_boltzmann
#' @export
get_boltzmann.Raster = function(x, base = "log10", relative = FALSE){
  if (!requireNamespace("sp", quietly = TRUE))
    stop("package sp required, please install it first")
  if (!requireNamespace("raster", quietly = TRUE))
    stop("package raster required, please install it first")
  get_boltzmann(raster::as.matrix(x), base = base, relative = relative)
}
