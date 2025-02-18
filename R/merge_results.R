
#' Combining results from different chromatographic/polarity sequences.
#'
#' Given the final output/statistical tables from different chromatographics/polarities analyses of the same study (e.g.: HILIC POS, RPLC NEG), it combines together the results.
#'
#' @param results_list a list, each element should be a data frame with the results (e.g. from the statistical analyses) we would combine together. Ideally, each row should be an annotated compound, each column can contain any information about it, for example results of statistics, chemical information, feature intensities, etc..
#' @param strings_in_colnames_to_remove a character vector of length 1 or of the same length of the data frames in results_list. For each of those, this string will be removed to consider together the samples. A typical example: c("_HILIC_pos", "_RP_neg", "_RP_pos").
#' @param by_column NULL or a character vector of length 1 indicating a column name of the data frames. This column will be the reference used to treat duplicated, which will be ranked based on the if_duplicated_consider_columns argument.
#' @param if_duplicated_consider_columns NULL or a character vector with column names of the data frames. Those columns must contain numerical or factor variables, and will serve to prioritize the row to keep in the combined data frame in case of duplicated in the column passed in by_column. The first column of if_duplicated_consider_columns will be considered, in case of tie, the second one, and so on.
#' @param decreasing a logical vector of length 1 or of the same length of if_duplicated_consider_columns. For each of the columns passed in if_duplicated_consider_columns, the ranking order will be decreasing if TRUE, or increasing if FALSE.
#' @param name_new_column character of length 1. A new column will be created in the merged table with this name, indicating the original data frame.
#' 
#' @return A tibble with the combined results.
#'
#' @export
merge_results <- function(results_list, strings_in_colnames_to_remove = "", by_column = NULL, if_duplicated_consider_columns = NULL, decreasing = FALSE, name_new_column = "chromatographic_run") {
  
  if (!is.list(results_list)) {stop("results_list must be a list")}
  if (!all(map_lgl(results_list, is.data.frame))) {stop("every element of results_list must be a data frame")}
  if (any(map_lgl(results_list, \(x) any(duplicated(colnames(x)))))) {stop("There are some columns in the data frames in results_list that have duplicated names. Please, avoid it")}
  if (is.null(names(results_list))) {
    for (i in 1:length(results_list)) {
      names(results_list)[i] <- paste0("table_", i)
    }
  }
  
  if (length(by_column) >=1 ) {
    if (length(by_column)!=1) {stop("if by_column is specified, it must a character of length 1")}
    if (is.na(by_column)) {stop("if by_column is specified, by_column must be a character of length 1, not a NA")}
    if (!is.character(by_column)){stop("if by_column is specified, by_column must be a character of length 1")}
    if (!all(map_lgl(results_list, \(x) by_column %in% colnames(x)))) {stop("the column passed in by_column must be present in each data.frame of results_list")}
  }
  
  
  if (!is.character(strings_in_colnames_to_remove)) {stop("strings_in_colnames_to_remove must be a character vector")}
  if (length(strings_in_colnames_to_remove)==1) {
    strings_in_colnames_to_remove <- rep(strings_in_colnames_to_remove, length(results_list))
  }
  if (length(strings_in_colnames_to_remove) != length(results_list)) stop("The length of strings_in_colnames_to_remove must be 1 or the same length of results_list")
  if (any(is.na(strings_in_colnames_to_remove))) {
    strings_in_colnames_to_remove[which(is.na(strings_in_colnames_to_remove))] <- ""
  }
  
  if (length(if_duplicated_consider_columns) >= 1) {
    if (any(is.na(if_duplicated_consider_columns))) {stop("if_duplicated_consider_columns must not contain NAs")}
    if (!is.character(if_duplicated_consider_columns)) {stop("if_duplicated_consider_columns must be a charcter vector")}
    
    if (!all(map_lgl(results_list, \(x) all(if_duplicated_consider_columns %in% colnames(x))))) {stop("the columns passed in if_duplicated_consider_columns must be present in each data.frame of results_list")}
    for (idcc in if_duplicated_consider_columns) {
      if (!all(map_lgl(results_list, \(x) is.numeric(pull(x, idcc)) | is.factor(pull(x, idcc))))) {stop(" the columns passed in if_duplicated_consider_columns must be numeric or factor variables")}
    }
    
    if (length(if_duplicated_consider_columns)==1) {
      if (length(decreasing)!=1) {stop("decreasing must be a logical vector of the same lenth of if_duplicated_consider_columns, or of length 1")}
    } else {
      if (length(decreasing) == 1) {
        decreasing <- rep(decreasing, length(if_duplicated_consider_columns))
      } else if (length(decreasing) != length(if_duplicated_consider_columns)) {
        stop("decreasing must be a logical vector of the same lenth of if_duplicated_consider_columns, or of length 1")
      }
    }
    if (any(is.na(decreasing))) {stop("decreasing must not contain NAs")}
    if (!is.logical(decreasing)) {stop("decreasing must be a logical vector")}
  }
  
  
  if (length(name_new_column)!=1) {stop("name_new_column must be a character of length 1")}
  if (is.na(name_new_column)) {stop("name_new_column must be a character of length 1, not a NA")}
  if (!is.character(name_new_column)){stop("name_new_column must be a character of length 1")}
  
  
  
  
  
  for (rl in 1:length(results_list)) {
    
    if (strings_in_colnames_to_remove[rl] != "") {
      if (!any(grepl(strings_in_colnames_to_remove[rl], colnames(results_list[[rl]])))) {warning(paste0(strings_in_colnames_to_remove[rl], " was not found in any column names of ", names(results_list)[rl]))}
      
      colnames(results_list[[rl]]) <- str_remove_all(colnames(results_list[[rl]]), strings_in_colnames_to_remove[rl])
    }
  }
  
  
  # adjusting the variable type if needed:
  type_list <- vector(mode = "list", length = length(results_list))
  names(type_list) <- names(results_list)
  
  for (dfn in 1:length(results_list)) {
    type_list[[dfn]] <- as_tibble(t(map_chr(results_list[[dfn]], typeof)))
  }
  
  combined_type_table <- bind_rows(type_list, .id = name_new_column)
  
  for (cct in colnames(combined_type_table)[-which(colnames(combined_type_table)==name_new_column)])  {
    vector_noNA <- pull(combined_type_table, cct)[!is.na(pull(combined_type_table, cct))]
    if (length(vector_noNA) > 0) {
      if (!all(vector_noNA==vector_noNA[1])) {
        for (dfnc in 1:length(results_list)) {
          if (cct %in% colnames(results_list[[dfnc]])) {
            results_list[[dfnc]][,cct] <- as.character(pull(results_list[[dfnc]], cct))
          }
        }
      }
    }
  }
  #
  
  
  combined_table <- bind_rows(results_list, .id = name_new_column)
  
  if (length(by_column) != 1) {
    cat("\n")
    cat("since by_column was not specified, the datasets has just been combined without filtering anything")
  } else {
    if (length(if_duplicated_consider_columns) < 1) {
      cat("\n")
      cat("since if_duplicated_consider_columns was not specified, the datasets has just been combined without filtering anything")
    } else {
      
      for(o in rev(if_duplicated_consider_columns)) {
        combined_table <- combined_table[order(pull(combined_table, o), decreasing = decreasing[which(if_duplicated_consider_columns==o)]),]
      }
      
      
      duplicated_elements <- unique(pull(combined_table, by_column)[which(duplicated(pull(combined_table, by_column)))])
      
      for (de in duplicated_elements) {
        
        statement <- paste0(de, " was present ")
        
        combined_table_fil <- combined_table[which(pull(combined_table, by_column) == de), c(by_column, name_new_column)]
        
        for (cr in names(results_list)[which(names(results_list) %in% unique(pull(combined_table_fil, name_new_column)))]) {
          
          if (which(cr==names(results_list)[which(names(results_list) %in% unique(pull(combined_table_fil, name_new_column)))]) != 1) {
            statement <- paste0(statement, ", ")
          }
          
          times_in_this_cs <- length(which(pull(combined_table_fil, name_new_column)==cr))
          
          statement <- paste0(statement, times_in_this_cs, " times in ", cr)
          
        }
        
        statement <- paste0(statement, ". Now kept only one from ", pull(combined_table_fil, name_new_column)[1], ".")
        
        cat("\n")
        cat("\n")
        cat(statement)
      }
      
      combined_table <- combined_table[which(!duplicated(pull(combined_table, by_column))), ]
      
    }
  }
  
  
  return(combined_table)
  
}



