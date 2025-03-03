

#' Generate a Table with P-values from two-way ANOVA with TurkeyHSD posthoc
#'
#' Given a dataframe and a set of numerical variables of that dataframe, it performs 2-way ANOVA, with also TurkeyHSD posthoc tests for between groups comparison, to each desired variable and creates a new table with the p-values.
#'
#' @param DF a dataframe.
#' @param v a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values. Moreover, missing values are not allowed (if any, consider before replacing them using the function transf_data of the present package).
#' @param f character vector of length 2. Name of the two columns of df containing the factor variables for performing the two-way ANOVA.
#' @param interact logical. Do you also want to perform interactions?
#' @param FDR logical. If TRUE, after performing the ANOVA, it also correct p-values across the different variables with a false discovery rate multiple comparison correction (method "fdr" of the function p.adjust).
#' @param groupdiff logical. Do you also what to add an additional column indicating which group is higher?
#' @param pcutoff a numeric of length 1, must be between 0 and 1. If groupdiff is TRUE, the difference between groups will be reported only if the p-values is below the cut-off reported here.
#' @param filter_sign logical. If TRUE, the table will be filtered and only the p-values lower than the value specified in pcutoff will be considered.
#' @param cutPval logical. If TRUE, it cut the p-values using the cutP function of the present package.
#'
#' @return A tibble the results of the t-tests.
#'
#' @export
gentab_P.2wayANOVA_posthocTurkeyHSD <- function(DF, v, f, interact = FALSE, FDR = FALSE, groupdiff = FALSE, pcutoff = 0.05, filter_sign = FALSE, cutPval = FALSE) {
  if (!is.data.frame(DF)) {stop("DF must be a data frame!")}
  if (!is.character(v)) {stop("v must be a character")}
  if (length(v) == 0) {stop("v must contain at least one element!")}
  if (any(is.na(v))) {stop("v must not contain mising values")}
  if (any(duplicated(v))) {stop("v must not contain duplicates")}
  if (!all(v %in% colnames(DF))) {stop("the names you indicate in v must correspond to names of columns in DF")}
  if (!all(map_lgl(DF[,v], is.numeric))) {stop("in DF, the columns chosed with v must contain numerical values")}
  if (any(map_lgl(DF[,v], ~ any(is.na(.x))))) {stop("in DF, the columns chosen with v must not contain missing values")}
  
  if (!is.character(f)) {stop("f must be a character")}
  if (length(f) != 2) {stop("f must contain the names of two coloumn of DF")}
  if (any(is.na(f))) {stop("f must not contain mising values")}
  if (!all(f %in% colnames(DF))) {stop("the names you indicate in f must correspond to names of columns in DF")}
  if (!all(map_lgl(DF[,f], is.factor))) {stop("in the df, the columns chosen with f must contain factor variables")}
  if (any(map_lgl(DF[,f], ~ any(is.na(.x))))) {stop("in the df, the columns chosen with f must not contain missing values")}
  if (any(levels(pull(DF, f[1])) %in% levels(pull(DF, f[2]))) | any(levels(pull(DF, f[2])) %in% levels(pull(DF, f[1])))) {stop("please, use different names for the factor levels of the two factors")}
  
  f_and_v  <- c(f, v)
  if (any(check_if_fix_names_needed(f_and_v))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                               paste0("'", paste0(f_and_v[which(check_if_fix_names_needed(f_and_v))], collapse = "', '"), "'")))}
  
  if (length(interact)!=1) {stop("interact must be exclusively TRUE or FALSE")} else if (!is.logical(interact)) {stop("interact must be exclusively TRUE or FALSE")} else if (is.na(interact)) {stop("interact must be exclusively TRUE or FALSE")}
  if (length(FDR)!=1) {stop("FDR must be exclusively TRUE or FALSE")} else if (!is.logical(FDR)) {stop("FDR must be exclusively TRUE or FALSE")} else if (is.na(FDR)) {stop("FDR must be exclusively TRUE or FALSE")}
  if (length(groupdiff)!=1) {stop("groupdiff must be exclusively TRUE or FALSE")} else if (!is.logical(groupdiff)) {stop("groupdiff must be exclusively TRUE or FALSE")} else if (is.na(groupdiff)) {stop("groupdiff must be exclusively TRUE or FALSE")}
  if (length(filter_sign)!=1) {stop("filter_sign must be exclusively TRUE or FALSE")} else if (!is.logical(filter_sign)) {stop("filter_sign must be exclusively TRUE or FALSE")} else if (is.na(filter_sign)) {stop("filter_sign must be exclusively TRUE or FALSE")}
  
  if (groupdiff | filter_sign) {
    if (length(pcutoff)!=1) {stop("pcutoff must be a number between 0 and 1")}
    if (is.na(pcutoff)) {stop("pcutoff must be a number between 0 and 1")}
    if (!is.numeric(pcutoff)) {stop("pcutoff must be a number between 0 and 1")}
    if (pcutoff>1 | pcutoff<0) {stop("pcutoff must be a number between 0 and 1")}
  }
  
  if (length(cutPval)!=1) {stop("cutPval must be exclusively TRUE or FALSE")}
  if (is.na(cutPval)) {stop("cutPval must be exclusively TRUE or FALSE")}
  if (!is.logical(cutPval)) {stop("cutPval must be exclusively TRUE or FALSE")}
  
  if (interact == FALSE) {
    
    two.way_first <- aov(data = DF, as.formula(paste0(v[1], " ~ ", paste0(f, collapse = " + "))))
    
    TURKEY_first <- TukeyHSD(two.way_first)
    
    final_tab_colnames <- c("Dependent", paste0(f, "_Pvalue"))
    for (ind in f) {
      final_tab_colnames <- c(final_tab_colnames, paste0(str_replace_all(rownames(TURKEY_first[[ind]]), "-", "_vs_"), "_Pvalue"))
    }
    
    
    final_tab <- matrix(NA, nrow = 0, ncol = length(final_tab_colnames))
    colnames(final_tab) <- final_tab_colnames
    final_tab <- as_tibble(final_tab)
    
    col_Pvalues <- colnames(final_tab)[which(grepl("_Pvalue", colnames(final_tab)))]
    col_Pvalues_comparisons <- colnames(final_tab)[which(grepl("_Pvalue", colnames(final_tab)) & grepl("_vs_", colnames(final_tab)))]
    
    if (FDR == TRUE) {
      col_PvaluesFDR <- paste0(col_Pvalues, "FDR")
      col_Pvalues_comparisonsFDR <- paste0(col_Pvalues_comparisons, "FDR")
      final_tab[, col_PvaluesFDR] <- as.character(NA)
    }
    
    
    if (groupdiff == TRUE) {
      groupdiff_colnames <- str_remove_all(col_Pvalues_comparisons, "_Pvalue")
      final_tab[, groupdiff_colnames] <- as.character(NA)
    }
    
    starting_new_row <- final_tab[1,]
    
    for (a in v) {
      two.way <- aov(data = DF, as.formula(paste0(a, " ~ ", paste0(f, collapse = " + "))))
      
      TURKEY <- TukeyHSD(two.way)
      
      new_row_tab <- starting_new_row
      
      new_row_tab[1,1] <- a
      new_row_tab[1,col_Pvalues] <- as.list(c(as.vector(summary(two.way)[[1]][["Pr(>F)"]])[1:length(f)],
                                              as.vector(TURKEY[[f[1]]][,"p adj"]),
                                              as.vector(TURKEY[[f[2]]][,"p adj"])))
      
      
      if (groupdiff == TRUE & FDR == FALSE) {
        
        tab_TURKEY_matrix <- rbind(TURKEY[[f[1]]], TURKEY[[f[2]]])
        tab_TURKEY_df <- bind_cols(tibble(comparisons = rownames(tab_TURKEY_matrix)),
                                   as_tibble(tab_TURKEY_matrix))
        
        for (m in col_Pvalues_comparisons) {
          if (!is.na(pull(new_row_tab, m)[1])) {
            if (pull(new_row_tab, m)[1] < pcutoff) {
              
              elem1 <- strsplit(str_remove_all(m, "_Pvalue"), "_vs_")[[1]][1]
              elem2 <- strsplit(str_remove_all(m, "_Pvalue"), "_vs_")[[1]][2]
              
              if (length(which(pull(tab_TURKEY_df, comparisons) == str_remove_all(str_replace_all(m, "_vs_", "-"), "_Pvalue"))) != 1) { stop("something wrong")}
              
              if (pull(tab_TURKEY_df, "diff")[which(pull(tab_TURKEY_df, comparisons) == str_remove_all(str_replace_all(m, "_vs_", "-"), "_Pvalue"))] > 0) {
                new_row_tab[1, paste0(elem1, "_vs_", elem2)] <- paste0(elem1, " > ", elem2)
              } else if (pull(tab_TURKEY_df, "diff")[which(pull(tab_TURKEY_df, comparisons) == str_remove_all(str_replace_all(m, "_vs_", "-"), "_Pvalue"))] < 0) {
                new_row_tab[1, paste0(elem1, "_vs_", elem2)] <- paste0(elem2, " > ", elem1)
              } else {
                new_row_tab[1, paste0(elem1, "_vs_", elem2)] <- paste0(elem1, " = ", elem2)
                warning(paste0(a, " it's wired: if it's statistically different, it should not be equal!"))
              }
            }
          }
        }
      }
      
      final_tab <- rbind(final_tab, new_row_tab)
      
    }
    
    if (FDR == TRUE) {
      for (pv in col_Pvalues) {
        final_tab[, paste0(pv, "FDR")] <- p.adjust(pull(final_tab, pv), method ="fdr")
      }
    }
    
    significant_v <- character()
    
    if (FDR == FALSE) {
      for (a in v) {
        if (sum(map_lgl(filter(final_tab, Dependent == a)[, col_Pvalues], ~ . < pcutoff), na.rm = TRUE) >0) {
          significant_v <- c(significant_v, a)
        }
      }
    } else if (FDR == TRUE) {
      for (a in v) {
        if (sum(map_lgl(filter(final_tab, Dependent == a)[, col_PvaluesFDR], ~ . < pcutoff), na.rm = TRUE) >0) {
          significant_v <- c(significant_v, a)
        }
      }
    }
    
    if (groupdiff == TRUE & FDR == TRUE) {
      for (s in significant_v) {
        two.way <- aov(data = DF, as.formula(paste0(s, " ~ ", paste0(f, collapse = " + "))))
        
        TURKEY <- TukeyHSD(two.way)
        
        tab_TURKEY_matrix <- rbind(TURKEY[[f[1]]], TURKEY[[f[2]]])
        tab_TURKEY_df <- bind_cols(tibble(comparisons = rownames(tab_TURKEY_matrix)),
                                   as_tibble(tab_TURKEY_matrix))
        for (m in col_Pvalues_comparisonsFDR) {
          
          if (!is.na(pull(final_tab, m)[which(final_tab$Dependent == s)])) {
            if (pull(final_tab, m)[which(final_tab$Dependent == s)] < pcutoff) {
              
              elem1 <- strsplit(str_remove_all(m, "_PvalueFDR"), "_vs_")[[1]][1]
              elem2 <- strsplit(str_remove_all(m, "_PvalueFDR"), "_vs_")[[1]][2]
              
              if (length(which(pull(tab_TURKEY_df, "comparisons") == str_remove_all(str_replace_all(m, "_vs_", "-"), "_PvalueFDR"))) != 1) { stop("something wrong")}
              
              if (pull(tab_TURKEY_df, "diff")[which(pull(tab_TURKEY_df, "comparisons") == str_remove_all(str_replace_all(m, "_vs_", "-"), "_PvalueFDR"))] > 0) {
                final_tab[which(final_tab$Dependent == s), paste0(elem1, "_vs_", elem2)] <- paste0(elem1, " > ", elem2)
              } else if (pull(tab_TURKEY_df, "diff")[which(pull(tab_TURKEY_df, "comparisons") == str_remove_all(str_replace_all(m, "_vs_", "-"), "_PvalueFDR"))] < 0) {
                final_tab[which(final_tab$Dependent == s), paste0(elem1, "_vs_", elem2)] <- paste0(elem2, " > ", elem1)
              } else {
                final_tab[which(final_tab$Dependent == s), paste0(elem1, "_vs_", elem2)] <- paste0(elem1, " = ", elem2)
                warning(paste0(s, " it's wired: if it's statistically different, it should not be equal!"))
              }
            }
          }
        }
      }
    }
    
    if (filter_sign == TRUE) {
      final_tab <- filter(final_tab, Dependent %in% significant_v)
    }
  }
  
  if (interact == TRUE) {
    
    two.way_first <- aov(data = DF, as.formula(paste0(v[1], " ~ ", paste0(f, collapse = " * "))))
    
    TURKEY_first <- TukeyHSD(two.way_first)
    
    final_tab_colnames <- c("Dependent", paste0(f, "_Pvalue"), paste0(paste0(f, collapse = ":"), "_Pvalue"))
    for (ind in c(f, paste0(f, collapse = ":"))) {
      if(!is.null(TURKEY_first[[ind]])) {
        final_tab_colnames <- c(final_tab_colnames, paste0(str_replace_all(rownames(TURKEY_first[[ind]]), "-", "_vs_"), "_Pvalue"))
      }
    }
    
    
    final_tab <- matrix(NA, nrow = 0, ncol = length(final_tab_colnames))
    colnames(final_tab) <- final_tab_colnames
    final_tab <- as_tibble(final_tab)
    
    col_Pvalues <- colnames(final_tab)[which(grepl("_Pvalue", colnames(final_tab)))]
    col_Pvalues_comparisons <- colnames(final_tab)[which(grepl("_Pvalue", colnames(final_tab)) & grepl("_vs_", colnames(final_tab)))]
    
    if (FDR == TRUE) {
      col_PvaluesFDR <- paste0(col_Pvalues, "FDR")
      col_Pvalues_comparisonsFDR <- paste0(col_Pvalues_comparisons, "FDR")
      final_tab[, col_PvaluesFDR] <- as.character(NA)
    }
    
    if (groupdiff == TRUE) {
      groupdiff_colnames <- str_remove_all(col_Pvalues_comparisons, "_Pvalue")
      final_tab[, groupdiff_colnames] <- as.character(NA)
    }
    
    starting_new_row <- final_tab[1,]
    
    for (a in v) {
      two.way <- aov(data = DF, as.formula(paste0(a, " ~ ", paste0(f, collapse = " * "))))
      
      TURKEY <- TukeyHSD(two.way)
      
      new_row_tab <- starting_new_row
      
      new_row_tab[1,1] <- a
      new_row_tab[1,col_Pvalues] <- as.list(c(as.vector(summary(two.way)[[1]][["Pr(>F)"]])[1:3],
                                              as.vector(TURKEY[[f[1]]][,"p adj"]),
                                              as.vector(TURKEY[[f[2]]][,"p adj"]),
                                              as.vector(TURKEY[[paste0(f, collapse = ":")]][,"p adj"])))
      
      
      if (groupdiff == TRUE & FDR == FALSE) {
        
        tab_TURKEY_matrix <- rbind(TURKEY[[f[1]]], TURKEY[[f[2]]], TURKEY[[paste0(f, collapse = ":")]])
        tab_TURKEY_df <- bind_cols(tibble(comparisons = rownames(tab_TURKEY_matrix)),
                                   as_tibble(tab_TURKEY_matrix))
        
        for (m in col_Pvalues_comparisons) {
          
          if(!is.na(pull(new_row_tab, m)[1])) {
            if (pull(new_row_tab, m)[1] < pcutoff) {
              
              elem1 <- strsplit(str_remove_all(m, "_Pvalue"), "_vs_")[[1]][1]
              elem2 <- strsplit(str_remove_all(m, "_Pvalue"), "_vs_")[[1]][2]
              
              if (length(which(pull(tab_TURKEY_df, comparisons) == str_remove_all(str_replace_all(m, "_vs_", "-"), "_Pvalue"))) != 1) { stop("something wrong")}
              
              if (pull(tab_TURKEY_df, "diff")[which(pull(tab_TURKEY_df, comparisons) == str_remove_all(str_replace_all(m, "_vs_", "-"), "_Pvalue"))] > 0) {
                new_row_tab[1, paste0(elem1, "_vs_", elem2)] <- paste0(elem1, " > ", elem2)
              } else if (pull(tab_TURKEY_df, "diff")[which(pull(tab_TURKEY_df, comparisons) == str_remove_all(str_replace_all(m, "_vs_", "-"), "_Pvalue"))] < 0) {
                new_row_tab[1, paste0(elem1, "_vs_", elem2)] <- paste0(elem2, " > ", elem1)
              } else {
                new_row_tab[1, paste0(elem1, "_vs_", elem2)] <- paste0(elem1, " = ", elem2)
                warning(paste0(a, " it's wired: if it's statistically different, it should not be equal!"))
              }
            }
          }
        }
      }
      
      final_tab <- rbind(final_tab, new_row_tab)
      
    }
    
    if (FDR == TRUE) {
      for (pv in col_Pvalues) {
        final_tab[, paste0(pv, "FDR")] <- p.adjust(pull(final_tab, pv), method ="fdr")
      }
    }
    
    significant_v <- character()
    
    if (FDR == FALSE) {
      for (a in v) {
        if (sum(map_lgl(filter(final_tab, Dependent == a)[, col_Pvalues], ~ . < pcutoff), na.rm = TRUE) >0) {
          significant_v <- c(significant_v, a)
        }
      }
    } else if (FDR == TRUE) {
      for (a in v) {
        if (sum(map_lgl(filter(final_tab, Dependent == a)[, col_PvaluesFDR], ~ . < pcutoff), na.rm = TRUE) >0) {
          significant_v <- c(significant_v, a)
        }
      }
    }
    
    
    
    if (groupdiff == TRUE & FDR == TRUE) {
      
      for (s in significant_v) {
        two.way <- aov(data = DF, as.formula(paste0(s, " ~ ", paste0(f, collapse = " * "))))
        
        TURKEY <- TukeyHSD(two.way)
        
        tab_TURKEY_matrix <- rbind(TURKEY[[f[1]]], TURKEY[[f[2]]], TURKEY[[paste0(f, collapse = ":")]])
        tab_TURKEY_df <- bind_cols(tibble(comparisons = rownames(tab_TURKEY_matrix)),
                                   as_tibble(tab_TURKEY_matrix))
        for (m in col_Pvalues_comparisonsFDR) {
          
          if (!is.na(pull(final_tab, m)[which(final_tab$Dependent == s)])) {
            if (pull(final_tab, m)[which(final_tab$Dependent == s)] < pcutoff) {
              
              elem1 <- strsplit(str_remove_all(m, "_PvalueFDR"), "_vs_")[[1]][1]
              elem2 <- strsplit(str_remove_all(m, "_PvalueFDR"), "_vs_")[[1]][2]
              
              if (length(which(pull(tab_TURKEY_df, "comparisons") == str_remove_all(str_replace_all(m, "_vs_", "-"), "_PvalueFDR"))) != 1) { stop("something wrong")}
              
              if (pull(tab_TURKEY_df, "diff")[which(pull(tab_TURKEY_df, "comparisons") == str_remove_all(str_replace_all(m, "_vs_", "-"), "_PvalueFDR"))] > 0) {
                final_tab[which(final_tab$Dependent == s), paste0(elem1, "_vs_", elem2)] <- paste0(elem1, " > ", elem2)
              } else if (pull(tab_TURKEY_df, "diff")[which(pull(tab_TURKEY_df, "comparisons") == str_remove_all(str_replace_all(m, "_vs_", "-"), "_PvalueFDR"))] < 0) {
                final_tab[which(final_tab$Dependent == s), paste0(elem1, "_vs_", elem2)] <- paste0(elem2, " > ", elem1)
              } else {
                final_tab[which(final_tab$Dependent == s), paste0(elem1, "_vs_", elem2)] <- paste0(elem1, " = ", elem2)
                warning(paste0(s, " it's wired: if it's statistically different, it should not be equal!"))
              }
            }
          }
        }
      }
    }
    
    if (filter_sign == TRUE) {
      final_tab <- filter(final_tab, Dependent %in% significant_v)
    }
  }
  
  if (cutPval == TRUE) {
    for (cln in col_Pvalues) {
      final_tab[, cln] <- map_chr(pull(final_tab, cln), cutP)
    }
    
    if (FDR == TRUE) {
      for (cln in col_PvaluesFDR) {
        final_tab[, cln] <- map_chr(pull(final_tab, cln), cutP)
      }
    }
  }
  
  return(final_tab)
}

