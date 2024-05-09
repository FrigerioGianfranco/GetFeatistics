

#' Get a table of feature intensities from the MSDIAL export file.
#'
#' Given a table exported from MSDIAL (exporting the Area from MS-Dial 5.1.230912), it creates a table with only the name of features in the first column (same names created in the one of the featINFO) and the intensities of features in each other column.
#'
#' @param MSDIAL_raw_table a dataframe. Load on R the exported table using write_tsv or write_csv, then directly pass it in this argument.
#' @param n_last_coloums_to_delete numercial of length 1. Usually in the exported file from MSDIAL, the last columns are for descriptive statistics of the features. Pass here the numner of the last columns that you need to remove.
#'
#'
#' @return A tibble with the feature intensities.
#'
#' @export
get_feat_table_from_MSDial <- function(MSDIAL_raw_table, n_last_coloums_to_delete = 2) {
  
  check_integer <- function(x) {x == round(x)}
  if (length(n_last_coloums_to_delete)!=1) { stop("n_last_coloums_to_delete must contain a whole number indicating the number of last column from the table to remove") }
  if (!check_integer(n_last_coloums_to_delete)) { stop("n_last_coloums_to_delete must contain a whole number indicating the number of last column from the table to remove") }
  if (is.na(n_last_coloums_to_delete)) {stop("n_last_coloums_to_delete must contain a whole number indicating the number of last column from the table to remove")}
  
  MSDIAL_raw_table_selected <- MSDIAL_raw_table[,-c(2:which(colnames(MSDIAL_raw_table)== "Class"))]
  MSDIAL_raw_table_selected_better <- MSDIAL_raw_table_selected
  
  for (lc in 1:n_last_coloums_to_delete) {
    MSDIAL_raw_table_selected_better <- MSDIAL_raw_table_selected_better[, -length(colnames(MSDIAL_raw_table_selected_better))]
  }
  
  the_next_colnames <- as.character(MSDIAL_raw_table_selected_better[4,])
  
  the_next_colnames <- str_replace_all(the_next_colnames, "[// //)]", "_")
  the_next_colnames <- str_replace_all(the_next_colnames, "[/////)]", "")
  the_next_colnames <- str_replace_all(the_next_colnames, "[//(//)]", ".")
  the_next_colnames <- str_replace_all(the_next_colnames, "[//)//)]", ".")
  the_next_colnames <- str_replace_all(the_next_colnames, "[//%//)]", "perc")
  the_next_colnames[grepl("^[[:digit:]]+", the_next_colnames)] <- paste0("X", the_next_colnames[grepl("^[[:digit:]]+", the_next_colnames)])
  
  colnames(MSDIAL_raw_table_selected_better) <- the_next_colnames
  
  MSDIAL_raw_table_selected_even_better <- MSDIAL_raw_table_selected_better[-c(1:4),]
  
  add_feat <- function(x) {
    
    x <- as.numeric(x)
    
    x_final <- as.character(rep(NA, length(x)))
    
    for (i in 1:length(x)) {
      zeros <- if (x[i] > 10000000) {
        stop("too many features!! More than 10 milions!!! XD")
      } else if (x[i] >= 1000000) {
        ""
      } else if (x[i] >= 100000) {
        "0"
      } else if (x[i] >= 10000) {
        "00"
      } else if (x[i] >= 1000) {
        "000"
      } else if (x[i] >= 100) {
        "0000"
      } else if (x[i] >= 10) {
        "00000"
      } else {
        "000000"
      }
      
      x_final[i] <- paste0("feat", zeros, x[i])
      
    }
    
    return(x_final)
  }
  
  MSDIAL_raw_table_selected_even_better$Alignment_ID <- add_feat(MSDIAL_raw_table_selected_even_better$Alignment_ID)
  
  for (a in colnames(MSDIAL_raw_table_selected_even_better)) {
    if (all(grepl("^\\d*\\.?\\d+$", pull(MSDIAL_raw_table_selected_even_better, a)) | is.na(pull(MSDIAL_raw_table_selected_even_better, a)))) {
      MSDIAL_raw_table_selected_even_better[,a] <- as.numeric(pull(MSDIAL_raw_table_selected_even_better, a))
    }
  }
  
  MSDIAL_raw_table_selected_even_better[MSDIAL_raw_table_selected_even_better == 0] <- NA
  
  return(MSDIAL_raw_table_selected_even_better)
  
}


