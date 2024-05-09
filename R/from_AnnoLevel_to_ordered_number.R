

#' Get an numeric vector from a factor vector of annotation levels.
#'
#' Given a factor vector of annotation levels, it creates a numeric vector. Useful for ranking.
#'
#' @param AnnoLevel_vector a factor vector, containing annotation levels.
#'
#'
#' @return A numeric vector ranking the annotation levels.
#'
#' @export
from_AnnoLevel_to_ordered_number <- function(AnnoLevel_vector) {
  if (!is.factor(AnnoLevel_vector)) {stop("provide Annolevel as a factor!")}
  AnnoLevel_as_ordered_number <- as.character(AnnoLevel_vector)
  
  for (l in 1:length(AnnoLevel_as_ordered_number)) {
    AnnoLevel_as_ordered_number[l] <- which(levels(AnnoLevel_vector) == AnnoLevel_vector[l])
  }
  
  return(as.numeric(AnnoLevel_as_ordered_number))
}
