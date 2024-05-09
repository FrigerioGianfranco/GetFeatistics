
#' Fix duplicate names
#'
#' It adds sequential numbers to solve duplicates in a character vector.
#'
#' @param x a character vector.
#' @param zeros logical. Should leading zero be added?
#' @param NA_as_character logical. If FALSE, the missing values will not be affected by this function, if TRUE, any missing values duplicated will be considered as a character "NA".
#'
#' @return a single character with duplicated solved, thus with only unique elements.
#'
#' @export
fix_duplicated <- function(x, zeros = TRUE, NA_as_character = FALSE) {
  
  if (!is.character(x)) {stop("x must be a character vector")}
  if (length(zeros) != 1) {stop("zeros must be only TRUE or FALSE")}
  if (is.na(zeros)) {stop("zeros must be only TRUE or FALSE")}
  if (!is.logical(zeros)) {stop("zeros must be only TRUE or FALSE")}
  if (length(NA_as_character) != 1) {stop("NA_as_character must be only TRUE or FALSE")}
  if (is.na(NA_as_character)) {stop("NA_as_character must be only TRUE or FALSE")}
  if (!is.logical(NA_as_character)) {stop("NA_as_character must be only TRUE or FALSE")}
  
  x_mod <- x
  
  
  if (any(duplicated(x))) {
    names_duplicated <- unique(x[which(duplicated(x))])
    
    samples_names_w_duplID <- tibble(samples = x) %>%
      group_by(samples) %>%
      mutate(duplicateID = row_number()) %>%
      ungroup()
    
    if (zeros) {max_zeros <- ceiling(log10(max(samples_names_w_duplID$duplicateID)+0.1))}
    
    for (i in which(x %in% names_duplicated)) {
      
      if (is.na(x[i]) & (NA_as_character==FALSE)) {
        x_mod[i] <- x[i]
      } else {
        this_duplicateID <- samples_names_w_duplID$duplicateID[i]
        if (zeros) {this_zero_position <- ceiling(log10(this_duplicateID+0.1))}
        if (zeros) {this_number_of_zeros <- max_zeros-this_zero_position}
        
        if (zeros) {
          x_mod[i] <- paste0(x[i],
                             "_",
                             paste(rep("0", this_number_of_zeros), collapse = ""),
                             this_duplicateID)
        } else {
          x_mod[i] <- paste0(x[i],
                             "_",
                             this_duplicateID)
        }
      }
    }
  }
  
  return(x_mod)
}

