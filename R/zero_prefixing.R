#' Zero prefixing numbers
#'
#' It converts a number to a character with zeros as prefix, useful for having them ordered by name correctly.
#'
#' @param numbers numeric. Numbers to be converted.
#' @param highest numeric of length 1. The highest number, used to define the numbers of zeros. By default, it uses the max of the vector passed to numbers.
#' @param additional_prefix NULL or character of length 1. An additional prefix to add at the beginning of each number.
#' @param additional_suffix NULL or character of length 1. An additional suffix to add at the beginning of each number.
#'
#' @return A character vector with the added zeros as prefix.
#'
#' @export
zero_prefixing <- function(numbers, highest = max(numbers), additional_prefix = NULL, additional_suffix = NULL) {
  
  if (length(numbers) < 1) {
    return(character())
  } else {
    
    if (!is.numeric(numbers)) {stop("numbers must be numeric")}
    if (any(is.na(numbers))) {stop("numbers must not contain missing values")}
    
    if (length(highest)!=1) {stop("highest must a numeric of length 1")}
    if (is.na(highest)) {stop("highest must be numeric, not a missing value")}
    if (!is.numeric(highest)) {stop("highest must be numeric")}
    
    if (highest < max(numbers)) {warning("there are some numbers that are actually higher than the value you have passed to highest..!")}
    
    if (!is.null(additional_prefix)) {
      if (length(additional_prefix)!=1) {stop("additional_prefix must a character of length 1")}
      if (is.na(additional_prefix)) {stop("additional_prefix must be a character, not a missing value")}
      if (!is.character(additional_prefix)) {stop("additional_prefix must be a character")}
    }
    
    if (!is.null(additional_suffix)) {
      if (length(additional_suffix)!=1) {stop("additional_suffix must a character of length 1")}
      if (is.na(additional_suffix)) {stop("additional_suffix must be a character, not a missing value")}
      if (!is.character(additional_suffix)) {stop("additional_suffix must be a character")}
    }
    
    
    total_wanted_char <- nchar(floor(abs(highest)))
    
    output_vector <- as.character(numbers)
    
    for (i in 1:length(output_vector)) {
      
      this_number_of_zeros <- total_wanted_char - nchar(floor(abs(numbers[i])))
      
      if (this_number_of_zeros<0) {this_number_of_zeros <- 0}
      
      output_vector[i] <- paste0(paste0(rep("0", this_number_of_zeros), collapse = ""), numbers[i])
      
      if (!is.null(additional_prefix)) {
        output_vector[i] <- paste0(additional_prefix, output_vector[i])
      }
      
      if (!is.null(additional_suffix)) {
        output_vector[i] <- paste0(output_vector[i], additional_suffix)
      }
    }
    
    return(output_vector)
    
  }
}
