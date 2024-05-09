

#' Get a factor vector of annotation levels from a numeric vector.
#'
#' Given a numeric vector, it creates a factor vector of annotation levels. It's the opposite of from_AnnoLevel_to_ordered_number.
#'
#' @param AnnoLevel_as_ordered_number a numeric vector, containing annotation levels converted as numeric.
#' @param ident_levels character. The levels of the factor vector to be created.
#'
#'
#' @return A factor vector of annotation levels.
#'
#' @export
from_ordered_number_to_AnnoLevel <- function(AnnoLevel_as_ordered_number, ident_levels = c("1", "2a", "2b", "3a", "3b", "3c", "4a", "5")) {
  if (!is.numeric(AnnoLevel_as_ordered_number)) {stop("AnnoLevel_as_ordered_number must be a numeric vector")}
  if (!is.character(ident_levels)) {stop("ident_levels must be a character vecotr")}
  if (any(is.na(ident_levels))) {stop("ident_levels must be a character vecotr, with no missing values")}
  if (any(duplicated(ident_levels))) {stop("ident_levels must be a character vecotr, with no duplicated")}
  
  if (any(AnnoLevel_as_ordered_number > length(ident_levels))) {stop("ordered number is too high")}
  
  AnnoLevel_vector <- factor(rep(NA, length(AnnoLevel_as_ordered_number)), levels = ident_levels)
  
  for (o in 1:length(AnnoLevel_as_ordered_number)) {
    AnnoLevel_vector[o] <- ident_levels[AnnoLevel_as_ordered_number[o]]
  }
  
  return(AnnoLevel_vector)
}

