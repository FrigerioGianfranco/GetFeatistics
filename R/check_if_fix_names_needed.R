
#' Check if names can be fixed
#'
#' It checks if names contain spaces or special characters or if start with number
#'
#' @param x a character vector.
#'
#' @return logical vector, indicating which element contains special characters or start with a number
#'
#' @export
check_if_fix_names_needed <- function(x) {
  
  if (!is.character(x)) {stop("x must be a character vector")}
  
  final_logic <- logical(length = length(x))
  
  if (length(x) > 0) {
    for (i in 1:length(x)) {
      final_logic[i] <- x[i] != fix_names(x[i])
    }
  }
  
  return(final_logic)
}

