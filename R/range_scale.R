
#' Range-scale values
#'
#' From a vector of numbers, to each element it subtracts the mean and divide by the difference between the maximum and the minimum values.
#'
#' @param x a numeric vector.
#' @param na.rm logical. Should missing value be stripped before the computation? (Same as for mean, max and min)
#'
#'
#' @return a numeric vector with the scaled values.
#'
#' @export
range_scale <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {stop("x must be a numeric vector")}
  if (length(na.rm)!=1) {stop("na.rm must be exclusively TRUE or FALSE")} else if (!is.logical(na.rm)) {stop("na.rm must be exclusively TRUE or FALSE")} else if (is.na(na.rm)) {stop("na.rm must be exclusively TRUE or FALSE, not a missing value")}
  if (!is.na(max(x, na.rm)-min(x, na.rm))) {
    if (max(x, na.rm)-min(x, na.rm) == 0) {
      warning("the difference between min and max is zero, thus NaN will be introduced")
    }
  }
  
  (x - mean(x, na.rm = na.rm)) / (max(x, na.rm)-min(x, na.rm))
}

