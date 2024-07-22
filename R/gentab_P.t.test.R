

#' Generate a Table with P-values from a T-test
#'
#' Given a dataframe and a set of numerical variables of that dataframe, it performs t-test to each desired variable and creates a new table with the p-values.
#'
#' @param df a dataframe.
#' @param v a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values. Moreover, missing values are not allowed (if any, consider before replacing them using the function transf_data of the present package).
#' @param f character vector of length 1. Name of the column of df containing the factor variable (that must have exactly 2 levels) considered for performing th t-tests.
#' @param paired logical. If FALSE it performs non-paired t-tests. If TRUE it performs paired t-tests.
#' @param FDR logical. If TRUE, after performing the t-tests, it also correct p-values across the different variables with a false discovery rate multiple comparison correction (method "fdr" of the function p.adjust).
#' @param cutPval logical. If TRUE, it cut the p-values using the cutP function of the present package.
#' @param groupdiff logical. Do you also what to add an additional column indicating which group is higher?
#' @param pcutoff a numeric of length 1, must be between 0 and 1. If groupdiff is TRUE, the difference between groups will be reported only if the p-values is below the cut-off reported here.
#' @param filter_sign logical. If TRUE, the table will be filtered and only the p-values lower than the value specified in pcutoff will be considered.
#'
#' @return A tibble the results of the t-tests.
#'
#' @export
gentab_P.t.test <- function(df, v, f, paired = FALSE, FDR = FALSE, cutPval = FALSE, groupdiff = TRUE, pcutoff = 0.05, filter_sign = FALSE) {
  
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  
  if (length(f) !=1) {stop("f must be a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the t-test")}
  if (is.na(f)) {stop("f must be only a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the t-test")}
  if (!is.character(f)) {stop("f must be only a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the t-test")}
  if (!f%in%colnames(df)) {stop("f must be only a character of length 1 that is the name of the coloumn that you want to consider as factor variable for the t-test")}
  if (!is.factor(pull(df,f))) {stop("f must be the column name of a factor variable!")}
  if (length(levels(pull(df,f))) != 2) {stop("f must be the column name of a factor variable with exactly two levels!")}
  
  if (!is.character(v)) {stop("v must be a vector containing the names of the coloumns with data that you want to apply the t-test to")}
  if (any(is.na(v))) {stop("v must be a vector containing the names of the coloumns with data that you want to apply the t-test to")}
  if (!all(v %in% colnames(df))) {stop("v must be a vector containing the names of the coloumns with data that you want to apply the t-test to")}
  if (mean(map_lgl(select(df, all_of(v)), is.numeric)) != 1) {stop("all coloumn passed with v must be numeric!")}
  if (any(map_lgl(select(df, all_of(v)), ~ any(is.na(.))))) {stop("there are some missing values in the data. Please, input or remove those missing values. Consider using the function transf_data of the present package")}
  if (any(map_lgl(df[,v], ~ any(.x == 0)))) {warning("There are some zeros in the data, isn't it better to replace them with NAs?")}
  
  f_and_v  <- c(f, v)
  if (any(check_if_fix_names_needed(f_and_v))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                               paste0("'", paste0(f_and_v[which(check_if_fix_names_needed(f_and_v))], collapse = "', '"), "'")))}
  
  if (length(paired)!=1) {stop("paired must be exclusively TRUE or FALSE")} else if (!is.logical(paired)) {stop("paired must be exclusively TRUE or FALSE")} else if (is.na(paired)) {stop("paired must be exclusively TRUE or FALSE")}
  
  if (paired) {
    length_vect_fact1 <- length(which(pull(df, f) == levels(pull(df, f))[1]))
    length_vect_fact2 <- length(which(pull(df, f) == levels(pull(df, f))[2]))
    
    if (length_vect_fact1 != length_vect_fact2) {stop("You cannot perform a paired t-test on groups that don't have the same number of observations")}
  }
  
  
  if (length(FDR)!=1) {stop("FDR must be exclusively TRUE or FALSE")}
  if (!is.logical(FDR)) {stop("FDR must be exclusively TRUE or FALSE")}
  if (is.na(FDR)) {stop("FDR must be exclusively TRUE or FALSE")}
  
  if (length(cutPval)!=1) {stop("cutPval must be exclusively TRUE or FALSE")}
  if (!is.logical(cutPval)) {stop("cutPval must be exclusively TRUE or FALSE")}
  if (is.na(cutPval)) {stop("cutPval must be exclusively TRUE or FALSE")}
  
  if (length(groupdiff)!=1) {stop("groupdiff must be exclusively TRUE or FALSE")}
  if (!is.logical(groupdiff)) {stop("groupdiff must be exclusively TRUE or FALSE")}
  if (is.na(groupdiff)) {stop("groupdiff must be exclusively TRUE or FALSE")}
  
  if (length(filter_sign)!=1) {stop("filter_sign must be exclusively TRUE or FALSE")}
  if (!is.logical(filter_sign)) {stop("filter_sign must be exclusively TRUE or FALSE")}
  if (is.na(filter_sign)) {stop("filter_sign must be exclusively TRUE or FALSE")}
  
  
  if (groupdiff | filter_sign) {
    if (length(pcutoff)!=1) {stop("pcutoff must be a single number between 0 and 1")}
    if (is.na(pcutoff)) {stop("pcutoff must be a single number between 0 and 1, and not a missing value")}
    if (!is.numeric(pcutoff)) {stop("pcutoff must be a single number between 0 and 1")}
    if (pcutoff>1 | pcutoff<0) {stop("pcutoff must be a single number below or equal to 1")}
  }
  
  
  #function for extract P-value from a non paired T-test
  gett.test.P <- function (v, f) {
    Pvalue <- t.test(v~f)$p.value
  }
  
  #function for extract P-value from a paired T-test
  gett.test.P_paired <- function (v, f) {
    Pvalue <- t.test(v~f, paired=TRUE)$p.value
  }
  
  
  df_fil1 <- select(df, all_of(f), all_of(v))
  df_fil1_lev1 <- df_fil1[which(pull(df_fil1, f) == levels(pull(df_fil1, f))[1]),]
  df_fil1_lev2 <- df_fil1[which(pull(df_fil1, f) == levels(pull(df_fil1, f))[2]),]
  
  df_fil2 <- select(df, all_of(v))
  
  if (paired == FALSE) {Pvalues_t.test <- map_dbl(df_fil2, gett.test.P, pull(df,f))} else if (paired == TRUE) {Pvalues_t.test <- map_dbl(df_fil2, gett.test.P_paired, pull(df,f))}
  if (FDR) {Pvalues_t.test_FDR <- p.adjust(Pvalues_t.test, method ="fdr")}
  
  df_P <- as.data.frame(Pvalues_t.test)
  df_P_final <- tibble(variables = rownames(df_P), Pvalues = df_P$Pvalues_t.test)
  P_to_consider <- "Pvalues"
  if (FDR) {
    df_P_final <- mutate(df_P_final, PvaluesFDR = Pvalues_t.test_FDR)
    P_to_consider <- "PvaluesFDR"
    }
  
  if(groupdiff == TRUE) {
    df_P_final <- mutate(df_P_final, group_diff = as.character(NA))
    for(i in 1:length(pull(df_P_final, P_to_consider))) {
      if (pull(df_P_final, P_to_consider)[i] < pcutoff) {
        if (mean(pull(df_fil1_lev1, df_P_final$variables[i])) > mean(pull(df_fil1_lev2, df_P_final$variables[i]))) {
          df_P_final[i, "group_diff"] <- paste0(levels(pull(df_fil1, f))[1], " > ", levels(pull(df_fil1, f))[2])
        } else if (mean(pull(df_fil1_lev1, df_P_final$variables[i])) < mean(pull(df_fil1_lev2, df_P_final$variables[i]))) {
          df_P_final[i, "group_diff"] <- paste0(levels(pull(df_fil1, f))[2], " > ", levels(pull(df_fil1, f))[1])
        } else {
          df_P_final[i, "group_diff"] <- paste0(levels(pull(df_fil1, f))[1], " = ", levels(pull(df_fil1, f))[2])
          warning(paste0("It's wired that it's the same mean, since the p-value is significative, for  ", df_P_final$variables[i]))
        }
      }
    }
  }
  
  
  if (filter_sign == TRUE) {
    if (FDR == FALSE) {
      df_P_final <- filter(df_P_final, Pvalues < pcutoff)
    } else if (FDR) {
      df_P_final <- filter(df_P_final, PvaluesFDR < pcutoff)
    }
  }
  
  if (cutPval == TRUE) {
    df_P_final$Pvalues <- map_chr(df_P_final$Pvalues, cutP)
    if (FDR) {
      df_P_final$PvaluesFDR <- map_chr(df_P_final$PvaluesFDR, cutP)
    }
  }
  
  return(df_P_final)
}


