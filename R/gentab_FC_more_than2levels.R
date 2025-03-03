

#' Generate a Table with Fold Change even if the factor has more than 2 levels
#'
#' Given a dataframe and a set of numerical variables of that dataframe, it performs fold changes analysis for each pair of levels of a desired variable and creates a new table.
#'
#' @param df a dataframe.
#' @param v a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values.
#' @param f character vector of length 1. Name of the column of df containing the factor variable considered for performing the Fold Change analysis.
#' @param second_to_first_ratio logical. If TRUE the second group/first group ratio will be computed, if FALSE the first group/second group ratio will be computed.
#' @param paired logical. If FALSE it performs FC on mean of the two groups. If TRUE it performs FC for each pair and then compute the mean.
#' @param are_log_transf logical. If you really need to perform this FC analysis on already log-transformed data, specify here as TRUE, and the subtraction will be performed instead of the ratio.
#' @param log_base numeric of length 1. Specify here the base of the logarithm to calculate the logFC or, if are_log_transf is TRUE; the base of the logarithm that were used to transform the data.
#' @param only_on_positive logical. If TRUE, FC analysis will be performed only if all values are positive (if paired), or if all means are positive (if not paired). The rest will be left as NA.
#'
#' @return A tibble the results of the Fold Change analysis for each pair of levels of the factor variables.
#'
#' @export
gentab_FC_more_than2levels <- function(df, v, f, second_to_first_ratio = TRUE, paired = FALSE, are_log_transf = FALSE, log_base = 2, only_on_positive = FALSE) {
  
  
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  
  if (length(f) !=1) {stop("f must be a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the Fold Change analysis")}
  if (is.na(f)) {stop("f must be only a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the Fold Change analysis")}
  if (!is.character(f)) {stop("f must be only a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the Fold Change analysis")}
  if (!f%in%colnames(df)) {stop("f must be only a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the Fold Change analysis")}
  if (!is.factor(pull(df,f))) {stop("f must be the column name of a factor variable!")}
  if (length(levels(pull(df,f))) < 2) {stop("f must be the column name of a factor variable with at least two levels!")}
  
  if (!is.character(v)) {stop("v must be a vector containing the names of the coloumns with data that you want to apply the Fold Change analysis to")}
  if (any(is.na(v))) {stop("v must be a vector containing the names of the coloumns with data that you want to apply the Fold Change analysis to")}
  if (!all(v %in% colnames(df))) {stop("v must be a vector containing the names of the coloumns with data that you want to apply the Fold Change analysis to")}
  if (mean(map_lgl(select(df, all_of(v)), is.numeric)) != 1) {stop("all coloumn passed with v must be numeric!")}
  if (any(map_lgl(select(df, all_of(v)), ~ any(is.na(.))))) {stop("there are some missing values in the data")}
  
  
  
  f_and_v  <- c(f, v)
  if (any(check_if_fix_names_needed(f_and_v))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                               paste0("'", paste0(f_and_v[which(check_if_fix_names_needed(f_and_v))], collapse = "', '"), "'")))}
  
  if (length(second_to_first_ratio)!=1) {stop("second_to_first_ratio must be exclusively TRUE or FALSE")}
  if (!is.logical(second_to_first_ratio)) {stop("second_to_first_ratio must be exclusively TRUE or FALSE")}
  if (is.na(second_to_first_ratio)) {stop("second_to_first_ratio must be exclusively TRUE or FALSE")}
  
  if (length(paired)!=1) {stop("paired must be exclusively TRUE or FALSE")}
  if (!is.logical(paired)) {stop("paired must be exclusively TRUE or FALSE")}
  if (is.na(paired)) {stop("paired must be exclusively TRUE or FALSE")}
  
  if (length(are_log_transf)!=1) {stop("are_log_transf must be exclusively TRUE or FALSE")}
  if (!is.logical(are_log_transf)) {stop("are_log_transf must be exclusively TRUE or FALSE")}
  if (is.na(are_log_transf)) {stop("are_log_transf must be exclusively TRUE or FALSE")}
    
  if (length(log_base)!=1) {stop("log_base must be a number")}
  if (is.na(log_base)) {stop("log_base must be a number")}
  if (!is.numeric(log_base)) {stop("log_base must be a number")}
  
  if (length(only_on_positive)!=1) {stop("only_on_positive must be exclusively TRUE or FALSE")}
  if (!is.logical(only_on_positive)) {stop("only_on_positive must be exclusively TRUE or FALSE")}
  if (is.na(only_on_positive)) {stop("only_on_positive must be exclusively TRUE or FALSE")}
  
  df_fv <- select(df, all_of(c(f, v)))
  
  pair_matrix <- combn(levels(pull(df, f)), 2)
  
  df_final <- tibble(variables = v)
  
  for (i in 1:ncol(pair_matrix)) {
    
    THIS_LEV1 <- pair_matrix[1,i]
    
    THIS_LEV2 <- pair_matrix[2,i]
      
      
    df_fil <- df_fv[which(pull(df_fv, f) %in% c(THIS_LEV1, THIS_LEV2)),]
    
    df_fil[,f] <- droplevels(pull(df_fil, f))
    
    
    if (only_on_positive) {
      
      this_v_to_actually_consider <- character()
      
      if (paired) {
        
        for (vn in v) {
          if (all(pull(df_fil, vn) > 0)) {
            this_v_to_actually_consider <- c(this_v_to_actually_consider, vn)
          }  
        }
        
        
      } else {
        
        for (vn in v) {
          if (mean(pull(df_fil[which(pull(df_fil, f) == THIS_LEV1), ], vn)) > 0 &
              mean(pull(df_fil[which(pull(df_fil, f) == THIS_LEV2), ], vn)) > 0) {
            this_v_to_actually_consider <- c(this_v_to_actually_consider, vn)
          }  
        }
        
      }
      
    } else {
      
      this_v_to_actually_consider <- v
      
    }
    
    
    FC_this_results <- gentab_FC(df = df_fil,
                                 v = this_v_to_actually_consider,
                                 f = f,
                                 second_to_first_ratio = second_to_first_ratio,
                                 paired = paired,
                                 are_log_transf = are_log_transf,
                                 log_base = log_base,
                                 filter_sign = FALSE)
    
    
    df_final <- add_column(df_final,
                           this_new_col_FC = as.numeric(NA),
                           this_new_col_logFC = as.numeric(NA))
    
    
    
    for (u in 1:length(pull(FC_this_results, 1))) {
      
      df_final[which(df_final$variables == FC_this_results$variables[u]), "this_new_col_FC"] <- FC_this_results$FC[u]
      df_final[which(df_final$variables == FC_this_results$variables[u]), "this_new_col_logFC"] <- FC_this_results$logFC[u]
      
    }
    
    colnames(df_final)[which(colnames(df_final) == "this_new_col_FC")] <- paste0(THIS_LEV1, "_vs_", THIS_LEV2, "_FC")
    colnames(df_final)[which(colnames(df_final) == "this_new_col_logFC")] <- paste0(THIS_LEV1, "_vs_", THIS_LEV2, "_logFC")
    
  }
  
  
  return(df_final)
  
}
