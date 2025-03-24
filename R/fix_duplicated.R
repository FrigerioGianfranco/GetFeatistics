
#' Fix duplicate names
#'
#' It adds sequential numbers to solve duplicates in a character vector.
#'
#' @param x a character vector.
#' @param zeros logical. Should leading zero be added?
#' @param define_highest_for_zeros NULL or an numeric integer of length 1. If zeros is TRUE, you can decide the highest number to define the number of zeros. If this argument is NULL, the highest number will be defined based on the highest number of duplicated.
#' @param start_with_zero logical. If TRUE, the first number added for solving the duplicated will be 0, if FALSE will be 1.
#' @param exclude_the_first logical. If TRUE, the suffix will not be added only from the second duplicated on.
#' @param NA_as_character logical. If FALSE, the missing values will not be affected by this function, if TRUE, any missing values duplicated will be considered as a character "NA".
#'
#' @return a single character with duplicated solved, thus with only unique elements.
#'
#' @export
fix_duplicated <- function(x, zeros = TRUE, define_highest_for_zeros = NULL, start_with_zero = FALSE, exclude_the_first = TRUE, NA_as_character = FALSE) {
  
  if (!is.character(x)) {stop("x must be a character vector")}
  if (length(zeros) != 1) {stop("zeros must be only TRUE or FALSE")}
  if (is.na(zeros)) {stop("zeros must be only TRUE or FALSE")}
  if (!is.logical(zeros)) {stop("zeros must be only TRUE or FALSE")}
  if (zeros) {
    if (!is.null(define_highest_for_zeros)) {
      if (length(define_highest_for_zeros) != 1) {stop("if not NULL, define_highest_for_zeros must be a numeric integer of length 1")}
      if (is.na(define_highest_for_zeros)) {stop("if not NULL, define_highest_for_zeros must not be NA")}
      if (!is.numeric(define_highest_for_zeros) & !is.integer(define_highest_for_zeros)) {stop("if not NULL, define_highest_for_zeros must be a numeric integer")}
      if (is.numeric(define_highest_for_zeros)) {
        if (define_highest_for_zeros != as.integer(define_highest_for_zeros)) {
          stop("if not NULL, define_highest_for_zeros must be a numeric integer")
        } else {
          define_highest_for_zeros <- as.integer(define_highest_for_zeros)
        }
      }
    }
  }
  if (length(start_with_zero) != 1) {stop("start_with_zero must be only TRUE or FALSE")}
  if (is.na(start_with_zero)) {stop("start_with_zero must be only TRUE or FALSE")}
  if (!is.logical(start_with_zero)) {stop("start_with_zero must be only TRUE or FALSE")}
  if (length(exclude_the_first) != 1) {stop("exclude_the_first must be only TRUE or FALSE")}
  if (is.na(exclude_the_first)) {stop("exclude_the_first must be only TRUE or FALSE")}
  if (!is.logical(exclude_the_first)) {stop("exclude_the_first must be only TRUE or FALSE")}
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
    if (start_with_zero) {
      samples_names_w_duplID$duplicateID <- samples_names_w_duplID$duplicateID-1
    }
    
    
    if (zeros) {
      if (is.null(define_highest_for_zeros)) {
        samples_names_w_duplID <- mutate(samples_names_w_duplID,
                                         zeros_duplicateID = zero_prefixing(duplicateID))
      } else {
        samples_names_w_duplID <- mutate(samples_names_w_duplID,
                                         zeros_duplicateID = zero_prefixing(duplicateID, highest = define_highest_for_zeros))
      }
    }
    
    for (i in which(x %in% names_duplicated)) {
      
      if (is.na(x[i]) & (NA_as_character==FALSE)) {
        x_mod[i] <- x[i]
      } else {
        this_duplicateID <- samples_names_w_duplID$duplicateID[i]
        
        go_on <- TRUE
        if (exclude_the_first & start_with_zero) {
          if (this_duplicateID == 0) {
            go_on <- FALSE
          }
        } else if (exclude_the_first & !start_with_zero) {
          if (this_duplicateID == 1) {
            go_on <- FALSE
          }
        }
        
        if (go_on) {
          
          if (zeros) {
            x_mod[i] <- paste0(x[i],
                               "_",
                               samples_names_w_duplID$zeros_duplicateID[i])
          } else {
            x_mod[i] <- paste0(x[i],
                               "_",
                               this_duplicateID)
          }
        }
      }
    }
  }
  
  return(x_mod)
}

