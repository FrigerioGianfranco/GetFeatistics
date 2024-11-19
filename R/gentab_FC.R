

#' Generate a Table with Fold Change
#'
#' Given a dataframe and a set of numerical variables of that dataframe, it performs fold changes analysis to each desired variable and creates a new table with the p-values. Please, use data before log-transformation and scaling for this analysis!
#'
#' @param df a dataframe.
#' @param v a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values.
#' @param f character vector of length 1. Name of the column of df containing the factor variable (that must have exactly 2 levels) considered for performing the Fold Change analysis.
#' @param second_to_first_ratio logical. If TRUE the second group/first group ratio will be computed, if FALSE the first group/second group ratio will be computed.
#' @param paired logical. If FALSE it performs FC on mean of the two groups. If TRUE it performs FC for each pair and then compute the mean.
#' @param are_log_transf logical. If you really need to perform this FC analysis on already log-transformed data, specify here as TRUE, and the subtraction will be performed instead of the ration.
#' @param log_base numeric of length 1. Specify here the base of the logarithm to calculate the logFC or, if are_log_transf is TRUE; the base of the logarithm the that were used to transform the data.
#' @param filter_sign logical. If TRUE, the table will be filtered and only those that passed the FCcutoff will be retained.
#' @param FCcutoff numeric of length 1. If filter_sign is TRUE, the value of the FCcutoff to consider a feature difference as significant.
#'
#' @return A tibble the results of the Fold Change analysis
#'
#' @export
gentab_FC <- function(df, v, f, second_to_first_ratio = TRUE, paired = FALSE, are_log_transf = FALSE, log_base = 2, filter_sign = FALSE, FCcutoff = 2) {
  
  
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  
  if (length(f) !=1) {stop("f must be a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the Fold Change analysis")}
  if (is.na(f)) {stop("f must be only a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the Fold Change analysis")}
  if (!is.character(f)) {stop("f must be only a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the Fold Change analysis")}
  if (!f%in%colnames(df)) {stop("f must be only a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the Fold Change analysis")}
  if (!is.factor(pull(df,f))) {stop("f must be the column name of a factor variable!")}
  if (length(levels(pull(df,f))) != 2) {stop("f must be the column name of a factor variable with exactly two levels!")}
  
  if (!is.character(v)) {stop("v must be a vector containing the names of the coloumns with data that you want to apply the Fold Change analysis to")}
  if (any(is.na(v))) {stop("v must be a vector containing the names of the coloumns with data that you want to apply the Fold Change analysis to")}
  if (!all(v %in% colnames(df))) {stop("v must be a vector containing the names of the coloumns with data that you want to apply the Fold Change analysis to")}
  if (mean(map_lgl(select(df, all_of(v)), is.numeric)) != 1) {stop("all coloumn passed with v must be numeric!")}
  if (any(map_lgl(select(df, all_of(v)), ~ any(is.na(.))))) {warning("there are some missing values in the data")}
  
  
  
  f_and_v  <- c(f, v)
  if (any(check_if_fix_names_needed(f_and_v))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                               paste0("'", paste0(f_and_v[which(check_if_fix_names_needed(f_and_v))], collapse = "', '"), "'")))}
  
  if (length(second_to_first_ratio)!=1) {stop("second_to_first_ratio must be exclusively TRUE or FALSE")}
  if (!is.logical(second_to_first_ratio)) {stop("second_to_first_ratio must be exclusively TRUE or FALSE")}
  if (is.na(second_to_first_ratio)) {stop("second_to_first_ratio must be exclusively TRUE or FALSE")}
  
  if (length(paired)!=1) {stop("paired must be exclusively TRUE or FALSE")}
  if (!is.logical(paired)) {stop("paired must be exclusively TRUE or FALSE")}
  if (is.na(paired)) {stop("paired must be exclusively TRUE or FALSE")}
  
  if (paired) {
    length_vect_fact1 <- length(which(pull(df, f) == levels(pull(df, f))[1]))
    length_vect_fact2 <- length(which(pull(df, f) == levels(pull(df, f))[2]))
    
    if (length_vect_fact1 != length_vect_fact2) {stop("You cannot perform a paired Fold Change analysis on groups that don't have the same number of observations")}
  }
  
  if (length(are_log_transf)!=1) {stop("are_log_transf must be exclusively TRUE or FALSE")}
  if (!is.logical(are_log_transf)) {stop("are_log_transf must be exclusively TRUE or FALSE")}
  if (is.na(are_log_transf)) {stop("are_log_transf must be exclusively TRUE or FALSE")}
    
  if (are_log_transf== FALSE) {
    if (any(map_lgl(df[,v], ~ any(.x <= 0)))) {warning("There are some negative values in the data, this will give you wired results, not suitable for a FC analysis!!")}
    if (any(map_lgl(df[,v], ~ any(.x == 0)))) {warning("There are some zeros in the data")}
  }
  
  if (length(log_base)!=1) {stop("log_base must be a number")}
  if (is.na(log_base)) {stop("log_base must be a number")}
  if (!is.numeric(log_base)) {stop("log_base must be a number")}
  
  
  if (length(filter_sign)!=1) {stop("filter_sign must be exclusively TRUE or FALSE")}
  if (!is.logical(filter_sign)) {stop("filter_sign must be exclusively TRUE or FALSE")}
  if (is.na(filter_sign)) {stop("filter_sign must be exclusively TRUE or FALSE")}
  
  
  if (filter_sign) {
    if (length(FCcutoff)!=1) {stop("pcutoff must be a single number between 0 and 1")}
    if (is.na(FCcutoff)) {stop("pcutoff must be a single number between 0 and 1, and not a missing value")}
    if (!is.numeric(FCcutoff)) {stop("pcutoff must be a single number between 0 and 1")}
    if (FCcutoff<1) {stop("pcutoff must be a single greather than or equal to 1")}
  }
  
  
  df_fil1 <- select(df, all_of(f), all_of(v))
  df_fil1_lev1 <- df_fil1[which(pull(df_fil1, f) == levels(pull(df_fil1, f))[1]),]
  df_fil1_lev2 <- df_fil1[which(pull(df_fil1, f) == levels(pull(df_fil1, f))[2]),]
  
  
  
  df_FCresults <- tibble(variables = character(), FC = numeric(), logFC = numeric())
  
  if (paired == FALSE & are_log_transf == FALSE) {
    
    for (m in v) {
      
      mean_lev1 <- mean(pull(df_fil1_lev1, m), na.rm = TRUE)
      mean_lev2 <- mean(pull(df_fil1_lev2, m), na.rm = TRUE)
      
      df_FCresults_new_row <- tibble(variables = m,
                                     FC = ifelse(second_to_first_ratio, mean_lev2/mean_lev1, mean_lev1/mean_lev2),
                                     logFC = as.numeric(NA))
      df_FCresults_new_row$logFC <- log(df_FCresults_new_row$FC, base = log_base)
      
      
      df_FCresults <- rbind(df_FCresults, df_FCresults_new_row)
    }
    
  } else if (paired == TRUE & are_log_transf == FALSE) {
    
    for (m in v) {
      
      ratios <- numeric()
      for (i in 1:length(pull(df_fil1_lev1, m))) {
        
        if (second_to_first_ratio) {
          this_ratio <- pull(df_fil1_lev2, m)[i] / pull(df_fil1_lev1, m)[i]
        } else {
          this_ratio <- pull(df_fil1_lev1, m)[i] / pull(df_fil1_lev2, m)[i]
        }
        
        ratios <- c(ratios, this_ratio)
      }
      
      
      df_FCresults_new_row <- tibble(variables = m,
                                     FC = mean(ratios),
                                     logFC = as.numeric(NA))
      df_FCresults_new_row$logFC <- log(df_FCresults_new_row$FC, base = log_base)
      
      
      df_FCresults <- rbind(df_FCresults, df_FCresults_new_row)
    }
  
  } else if (paired == FALSE & are_log_transf == TRUE) {
    
    for (m in v) {
      mean_lev1 <- mean(pull(df_fil1_lev1, m), na.rm = TRUE)
      mean_lev2 <- mean(pull(df_fil1_lev2, m), na.rm = TRUE)
      
      df_FCresults_new_row <- tibble(variables = m,
                                     FC = as.numeric(NA),
                                     logFC = ifelse(second_to_first_ratio, mean_lev2-mean_lev1, mean_lev1-mean_lev2))
      df_FCresults_new_row$FC <- log_base^(df_FCresults_new_row$logFC)
      
      
      df_FCresults <- rbind(df_FCresults, df_FCresults_new_row)
    }
    
    
  } else if (paired == TRUE & are_log_transf == TRUE) {
    
    
    
    for (m in v) {
      
      ratios <- numeric()
      for (i in 1:length(pull(df_fil1_lev1, m))) {
        
        if (second_to_first_ratio) {
          this_ratio <- pull(df_fil1_lev2, m)[i] - pull(df_fil1_lev1, m)[i]
        } else {
          this_ratio <- pull(df_fil1_lev1, m)[i] - pull(df_fil1_lev2, m)[i]
        }
        
        ratios <- c(ratios, this_ratio)
      }
      
      
      df_FCresults_new_row <- tibble(variables = m,
                                     FC = as.numeric(NA),
                                     logFC = mean(ratios))
      df_FCresults_new_row$FC <- log_base^(df_FCresults_new_row$logFC)
      
      
      df_FCresults <- rbind(df_FCresults, df_FCresults_new_row)
    }
  }
  
  
  
  if (filter_sign) {
    df_final <- filter(df_FCresults, FC <= (1/FCcutoff) | FC >= FCcutoff)
  } else {
    df_final <- df_FCresults
  }
  
  return(df_final)
  
}
