

#' Get a featINFO table from the MSDIAL export file.
#'
#' Given a table exported from MSDIAL (exporting the Area from MS-Dial 5.1.230912), it creates the featINFO table, i.e. the table with the full information on each feature.
#'
#' @param MSDIAL_raw_table a dataframe. Inport on R the exported table using write_tsv or write_csv, then directly pass it in this argument.
#' @param add_AnnoLevels logical. If TRUE, an additional column named "AnnoLevels" will be added, and the annotation levels will be calculated considering the cut-offs reported in https://doi.org/10.1007/s00216-022-04207-z.
#'
#' @return A tibble with the information for each feature.
#'
#' @export
get_feat_info_from_MSDial <- function(MSDIAL_raw_table, add_AnnoLevels = FALSE) {
  
  if (length(add_AnnoLevels)!=1) {stop("add_AnnoLevels must be exclusively TRUE or FALSE")}
  if (!is.logical(add_AnnoLevels)) {stop("add_AnnoLevels must be exclusively TRUE or FALSE")}
  if (is.na(add_AnnoLevels)) {stop("add_AnnoLevels must be exclusively TRUE or FALSE")}
  
  
  MSDIAL_raw_table_selected <- MSDIAL_raw_table[,1:which(colnames(MSDIAL_raw_table)== "Class")]
  
  the_next_colnames <- as.character(MSDIAL_raw_table_selected[4,])
  
  the_next_colnames <- str_replace_all(the_next_colnames, "[// //)]", "_")
  the_next_colnames <- str_replace_all(the_next_colnames, "[/////)]", "")
  the_next_colnames <- str_replace_all(the_next_colnames, "[//(//)]", ".")
  the_next_colnames <- str_replace_all(the_next_colnames, "[//)//)]", ".")
  the_next_colnames <- str_replace_all(the_next_colnames, "[//%//)]", "perc")
  
  colnames(MSDIAL_raw_table_selected) <- the_next_colnames
  
  MSDIAL_raw_table_selected_better <- MSDIAL_raw_table_selected[-c(1:4),]
  
  
  add_feat <- function(x) {
    
    x <- as.numeric(x)
    
    x_final <- as.character(rep(NA, length(x)))
    
    if (length(x) > 10000000) {stop("too many features!! More than 10 milions!!! XD")}
    
    for (i in 1:length(x)) {
      zeros <- if (x[i] >= 1000000) {
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
  
  MSDIAL_raw_table_selected_better$Alignment_ID <- add_feat(MSDIAL_raw_table_selected_better$Alignment_ID)
  
  for (a in colnames(MSDIAL_raw_table_selected_better)) {
    if (any(pull(MSDIAL_raw_table_selected_better, a) == "null")) {
      for (i in which(pull(MSDIAL_raw_table_selected_better, a) == "null")) {
        MSDIAL_raw_table_selected_better[i,a] <- NA
      }
    }
    
    if (all(grepl("true", pull(MSDIAL_raw_table_selected_better, a), ignore.case = TRUE) | grepl("false", pull(MSDIAL_raw_table_selected_better, a), ignore.case = TRUE) | is.na(pull(MSDIAL_raw_table_selected_better, a)))) {
      MSDIAL_raw_table_selected_better[,a] <- as.logical(toupper(pull(MSDIAL_raw_table_selected_better, a)))
    } else if (all(grepl("^\\d*\\.?\\d+$", pull(MSDIAL_raw_table_selected_better, a)) | is.na(pull(MSDIAL_raw_table_selected_better, a)))) {
      MSDIAL_raw_table_selected_better[,a] <- as.numeric(pull(MSDIAL_raw_table_selected_better, a))
    }
  }
  
  feat_info_output <- MSDIAL_raw_table_selected_better
  
  if (add_AnnoLevels) {
    feat_info_output <- get_AnnoLevels_MSDial(feat_info_output)
  }
  
  return(feat_info_output)
}

