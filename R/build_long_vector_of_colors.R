

#' Build a vector with colors.
#'
#' It creates a vector containing 112 different colors from the pals package; in particular from the following palettes, in this order: trubetskoy, glasbey, kelly, polychrome.
#'
#' @return A character vectors with 112 different colors.
#'
#' @export
build_long_vector_of_colors <- function() {
  
  long_colors_vector <- c(pals::trubetskoy(), pals::glasbey(), pals::kelly(), pals::polychrome())
  names(long_colors_vector) <- NULL
  
  return(long_colors_vector)
  
}