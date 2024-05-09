
#' Round a number to defined digital places
#'
#' If the number is less than 10, it returns it rounded with 3 digits,
#' if it's less than 100 with 2 digits, if it's less than 1000 with 1 digit, otherwise with 0 digits.
#'
#' @param x a single number.
#'
#' @return a single number.
#'
#' @export
round_the_result <- function(x) {
  if (!is.numeric(x)) {stop("x must be a single numeric value")}
  if (length(x)!=1) {stop("x must be a single numeric value")}
  if (is.na(x)) {
    return(as.numeric(NA))
  } else {
    ifelse(x<10, round(x, digits = 3),
           ifelse(x<100, round(x, digits = 2),
                  ifelse(x<1000, round(x, digits = 1),
                         round(x, digits = 0))))
  }
}

