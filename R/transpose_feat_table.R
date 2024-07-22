
#' Transpose features table.
#'
#' Given a feature table, it transposes it, so it is more suitable for following elaborations and statistical analyses.
#'
#' @param feat_table a dataframe: first column the featname, each other column is a sample with feature intensities. Each row is a feature. It can be obtained with the get_feat_table_from_MSDial function of the present package, but ideally it underwent the cleaning with QCs using the QCs_process function.
#' @param name_first_column character of length 1. You can specify here the name of the first column, which will be referred to the column of samples.
#'
#' @return A tibble with samples as rows and features as columns.
#'
#' @export
transpose_feat_table <- function(feat_table, name_first_column = "samples") {
  if (!is.data.frame(feat_table)) {stop("feat_table must be a data frame!")}
  if (any(duplicated(pull(feat_table, 1)))) {stop(paste0("There are some duplicated in the first coplumn of feat_table. PLease avoid. In particular the duplicates are: ", paste0(unique(pull(feat_table, 1)[which(duplicated(pull(feat_table, 1)))]), collapse = ", ")))}
  if (!(all(map_lgl(feat_table[,-1], is.numeric)))) {stop("all columns of feat_table, besides the first one, must contain numeric data")}
  
  if (length(name_first_column) != 1) {stop("name_first_column must be a character vector of length 1")}
  if (is.na(name_first_column)) {stop("name_first_column must be a character vector of length 1, not a missing value!")}
  if (!is.character(name_first_column)) {stop("name_first_column must be a character vector of length 1")}
  
  samplesnames <- colnames(feat_table)[-1]
  featnames <- as.character(pull(feat_table, 1))
  
  matrix_data <- as.matrix(feat_table[,-1])
  matrix_data_t <- t(matrix_data)
  
  colnames(matrix_data_t) <- featnames
  rownames(matrix_data_t) <- NULL
  
  df_output <- as_tibble(matrix_data_t) %>%
    add_column(First_column_GF = samplesnames, .before = 1)
  
  colnames(df_output)[1] <- name_first_column
  
  return(df_output)
}


