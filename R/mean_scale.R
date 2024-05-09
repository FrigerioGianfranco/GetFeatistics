
#' Mean-scale values
#'
#' From a vector of numbers, it subtracts the mean to each element.
#'
#' @param x a numeric vector.
#' @param na.rm logical. Should missing value be stripped before the computation? (Same as for mean)
#'
#'
#' @return a numeric vector with the scaled values.
#'
#' @export
mean_scale <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {stop("x must be a numeric vector")}
  if (length(na.rm)!=1) {stop("na.rm must be exclusively TRUE or FALSE")} else if (!is.logical(na.rm)) {stop("na.rm must be exclusively TRUE or FALSE")} else if (is.na(na.rm)) {stop("na.rm must be exclusively TRUE or FALSE, not a missing value")}
  
  x - mean(x, na.rm = na.rm)
}

