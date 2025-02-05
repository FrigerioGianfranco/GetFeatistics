

#' Add the information from featINFO to a table.
#'
#' Given a table containing data related to features (for example the output of a statistical analyses) and the related featINFO table, it combines everything in a single table.
#'
#' @param df1 dataframe. The table containing some results (e.g. from statistical analyses) for each feature.
#' @param colfeat_df1 character of length 1. It is the name of the column in df1 that contains the unique names of features.
#' @param dfINFO dataframe. The featINFO table.
#' @param colfeat_dfINFO character of length 1. It is the name of the column in dfINFO that contains the unique names of features.
#' @param add_char_to_INFO logical. Should a string of character to be added to each column from the dfINFO?
#' @param char_to_add_to_INFO character of length 1. If add_char_to_INFO is TRUE, pass here the string of character that will be added to each column from dfINFO.
#'
#'
#' @return A tibble. The combined table containing also the information on each feature.
#'
#' @export
addINFO_to_table <- function(df1, colfeat_df1, dfINFO, colfeat_dfINFO, add_char_to_INFO = FALSE, char_to_add_to_INFO = "_INFO") {
  if (!is.data.frame(df1)) {stop("df1 must be a data frame!")}
  if (!is.character(colfeat_df1)) {stop("colfeat_df1 must be a character string")}
  if (length(colfeat_df1) != 1) {stop("colfeat_df1 must be a character string of length 1")}
  if (is.na(colfeat_df1)) {stop("colfeat_df1 must be a character string of length 1, and not a missing value!")}
  if (!colfeat_df1 %in% colnames(df1)) {stop("colfeat_df1 must indicating the name of the coloumn of the first dataframe containing the unique names of features!")}
  
  if (!is.data.frame(dfINFO)) {stop("dfINFO must be a data frame!")}
  if (!is.character(colfeat_dfINFO)) {stop("colfeat_dfINFO must be a character string")}
  if (length(colfeat_dfINFO) != 1) {stop("colfeat_dfINFO must be a character string of length 1")}
  if (is.na(colfeat_dfINFO)) {stop("colfeat_dfINFO must be a character string of length 1, and not a missing value!")}
  if (!colfeat_dfINFO %in% colnames(dfINFO)) {stop("colfeat_dfINFO must indicating the name of the coloumn of the INFO dataframe containing the unique names of features!")}
  if (any(duplicated(pull(dfINFO, colfeat_dfINFO)))) stop("there are duplicated in the colfeat of dfINFO")
  
  if (length(add_char_to_INFO)!=1) {stop("add_char_to_INFO must be exclusively TRUE or FALSE")} else if (!is.logical(add_char_to_INFO)) {stop("add_char_to_INFO must be exclusively TRUE or FALSE")} else if (is.na(add_char_to_INFO)) {stop("add_char_to_INFO must be exclusively TRUE or FALSE")}
  if (!is.character(char_to_add_to_INFO)) {stop("char_to_add_to_INFO must be a character string")}
  if (length(char_to_add_to_INFO) != 1) {stop("char_to_add_to_INFO must be a character string of length 1")}
  if (is.na(char_to_add_to_INFO)) {stop("char_to_add_to_INFO must be a character string of length 1, and not a missing value!")}
  
  if (any(is.na(colnames(df1))) | any(is.na(colnames(dfINFO)))) {stop("There are some columns with missing name")}
  
  if (add_char_to_INFO == FALSE & (any(colnames(df1) %in% colnames(dfINFO)) | any(colnames(dfINFO) %in% colnames(df1)))) {
    stop("There are some columns that have the same name in both dataframes. To fix this issue, you could set the argument add_char_to_INFO to TRUE and indicate the characters to add to each coloumn of the dfINFO with the argument char_to_add_to_INFO")
  }
  
  if (add_char_to_INFO == TRUE) {
    colnames(dfINFO) <- paste0(colnames(dfINFO), char_to_add_to_INFO)
    
    if (any(colnames(df1) %in% colnames(dfINFO)) | any(colnames(dfINFO) %in% colnames(df1))) {
      stop("There are still some columns that have the same name in both dataframes. To fix this issue, try to change char_to_add_to_INFO")
    }
  }
  
  dfINFO_empty <- dfINFO[0,]
  dfINFO_empty_withNA <- dfINFO_empty
  if (length(pull(df1, colfeat_df1)) > 0) {
    dfINFO_empty_withNA[1:length(pull(df1, colfeat_df1)),] <- NA
  }
  
  df_final <- cbind(dfINFO_empty_withNA, df1)
  
  if (length(pull(df_final, 1)) > 0) {
    for (i in 1:length(pull(df_final, 1))) {
      
      index_in_featINFO <- which(pull(dfINFO, colfeat_dfINFO) == pull(df1, colfeat_df1)[i])
      
      if (length(index_in_featINFO) > 1) {
        stop("something wrong: more than one correspondence..")
      } else if (length(index_in_featINFO) == 0) {
        warning(paste0("no correspondence for ", pull(df1, colfeat_df1)[i], ", which is the row number ", i, " of the df1"))
      } else if (length(index_in_featINFO) == 1) {
        df_final[i, colnames(dfINFO)] <- dfINFO[index_in_featINFO, ]
      } else {
        stop("something very wired..")
      }
    }
  } else {
    structure(df_final[, colnames(dfINFO)], class = class(dfINFO))
  }
  
  df_final <- as_tibble(df_final)
  return(df_final)
}
