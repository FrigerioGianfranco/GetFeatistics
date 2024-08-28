
#' Fix names
#'
#' It removes special characters from a character vector, useful to modify column names. Spaces and hyphens will be replaced with the underscore,
#'
#' @param x a character vector.
#' @param if_start_with_number_add character of length 1. If the name start with a number, a character indicated in this argument will be added at the beginning.
#'
#' @return a single character with special character replaced.
#'
#' @export
fix_names <- function(x, if_start_with_number_add = "m") {
  
  if (!is.character(x)) {stop("x must be a character vector")}
  if (length(if_start_with_number_add) != 1) {stop("if_start_with_number_add must be a character vector of length 1")}
  if (is.na(if_start_with_number_add)) {stop("if_start_with_number_add must be a character vector of length 1, and not a missing value")}
  if (!is.character(if_start_with_number_add)) {stop("if_start_with_number_add must be a character vector of length 1")}
  
  
  x_mod <- x %>%
    str_replace_all("_", "UNLIKELYTHISISPRESENT") %>%
    str_replace_all("-", "UNLIKELYTHISISPRESENT") %>%
    str_replace_all("[:space:]", "UNLIKELYTHISISPRESENT") %>%
    str_replace_all("[^[:alnum:]]", ".") %>%
    str_replace_all("UNLIKELYTHISISPRESENT", "_")
  
  
  if (length(x_mod) > 0) {
    for (i in which(grepl("^\\d", x_mod))) {
      x_mod[i] <- paste0(if_start_with_number_add, x_mod[i])
    }
    
    for (i in which(grepl("^[[:punct:]][0-9]", x_mod))) {
      x_mod[i] <- paste0(substr(x_mod[i], start = 1, stop = 1), if_start_with_number_add, substr(x_mod[i], start = 2, stop = nchar(x_mod[i])))
    }
  }
  
  return(x_mod)
}

