
#' Cutting the P-value if < 0.001
#'
#' Returns "< 0.001" if a number is less than 0.001.
#'
#' @param x a single number, which must be between 0 and 1.
#'
#' @return a single character
#'
#' @export
cutP <- function(x) {
  if (length(x)!=1) {stop("x must be a single numeric value or a missing value")}
  if (is.na(x)) {x <- as.numeric(NA)}
  if (!is.numeric(x)) {stop("x must be a single numeric value")}
  
  if (is.na(x)) {
    return(as.character(NA))
  } else if (x>1 | x<0) {
    stop("the P-value must be between 0 and 1!")
  } else if (x < 0.001) {
    return("<0.001")
  } else {
    return(format(round(x,3),nsmall=3))
  }
}

