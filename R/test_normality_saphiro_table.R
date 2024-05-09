

#' Test normality with a Shapiro–Wilk test
#'
#' Given a dataframe and a set of numerical variables of that dataframe, it tests the normality of those data by performing a Shapiro–Wilk test to each desired variable.
#'
#' @param df a dataframe.
#' @param v a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values.
#' @param pvalcutoff a numeric of length 1, must be between 0 and 1, indicating the p-value cut-off.
#' @param cutpval logical. Do you want to cut the P-value using the function cutP of the present package?
#'
#'
#' @return A tibble with a number of rows equal to elements specified in v. For each of those, it reports the results of the Saphiro test and whether those data are normally distributed considering the given p-value cutoff.
#'
#' @export
test_normality_saphiro_table <- function(df, v, pvalcutoff = 0.05, cutpval = FALSE) {
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  if (mean(v %in% colnames(df)) != 1) {stop("v must be a vector containing the names of the coloumns of df that you want to apply the descriptive statistics")}
  if (mean(map_lgl(select(df, all_of(v)), is.numeric)) != 1) {stop("all coloumn passed in v must be numeric!")}
  
  if (any(check_if_fix_names_needed(v))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                         paste0("'", paste0(v[which(check_if_fix_names_needed(v))], collapse = "', '"), "'")))}
  
  
  if (length(pvalcutoff) != 1) {stop("pvalcutoff must be a numeric of length 1")}
  if (is.na(pvalcutoff)) {stop("pvalcutoff must be a numeric of length 1, and not a missing value!")}
  if (!is.numeric(pvalcutoff)) {stop("pvalcutoff must be a numeric of length 1")}
  if (pvalcutoff<0 | pvalcutoff>1) {stop("pvalcutoff must be a number between 0 and 1")}
  
  if (length(cutpval)!=1) {stop("cutpval must be exclusively TRUE or FALSE")}
  if (!is.logical(cutpval)) {stop("cutpval must be exclusively TRUE or FALSE")}
  if (is.na(cutpval)) {stop("cutpval must be exclusively TRUE or FALSE")}
  
  shapiro_final_table <- tibble(variable = as.character(rep(NA, times = length(v))),
                                shap_test_result = as.numeric(rep(NA, times = length(v))),
                                shap_test_pvalue = as.numeric(rep(NA, times = length(v))),
                                normally_distributed = as.logical(rep(NA, times = length(v))))
  
  if(cutpval == TRUE) {
    shapiro_final_table <- add_column(shapiro_final_table,
                                      shap_test_pvalue_cut = as.character(rep(NA, times = length(v))),
                                      .after = "shap_test_pvalue")
  }
  
  
  for (i in 1:length(v)) {
    
    shap <- shapiro.test(pull(df, v[i]))
    
    shapiro_final_table$variable[i] <- v[i]
    shapiro_final_table$shap_test_result[i] <- shap[["statistic"]][["W"]]
    shapiro_final_table$shap_test_pvalue[i] <- shap[["p.value"]]
    shapiro_final_table$normally_distributed[i] <- ifelse(shapiro_final_table$shap_test_pvalue[i] > pvalcutoff, TRUE, FALSE)
    
    if(cutpval == TRUE) {
      shapiro_final_table$shap_test_pvalue_cut[i] <- cutP(shapiro_final_table$shap_test_pvalue[i])
    }
  }
  
  return(shapiro_final_table)
  
}


