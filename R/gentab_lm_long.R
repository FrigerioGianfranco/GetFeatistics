

#' Generate a long table with complete results of linear regression models
#'
#' Given a dataframe and a set of dependent and independent variables from that dataframe, it generates a linear regression model for each single dependent variable and creates a new table with all slopes and p-values.
#'
#' @param df a dataframe.
#' @param dep a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values.
#' @param form_ind character vector of length 1. Write here the part of the formual of the linear model the ~ (for example: for the linear model dependent ~ independent1 + independent2 + independent3, you should pass here: "independent1 + independent2 + independent3").
#' @param mdl one of the following: "lm", "lmer", or "tobit". For linear model with fixed effects choose "lm" (it uses the function lm); for linear model with mixed effects choose "lmer" (function lmer of the lmerTest package);  for linear model with censored variable choose "tobit" (function tobit of the AER package)
#' @param left_cens NULL or a named numeric vector. If mdl is "tobit", indicate here the left-censored values (e.g.: the limit of detection values). In particular, this argument should be set to NULL (no left-censored values) or a named numeric vector whose names must correspond to variables passed in dep.
#' @param right_cens NULL or a named numeric vector. If mdl is "tobit", indicate here the right-censored values (e.g.: the upper limit of detection values). In particular, this argument should be set to NULL (no right-censored values) or a named numeric vector whose names must correspond to variables passed in dep.
#' @param var_perc logical. Besides the beta slopes, do you also want to know the variation percentage? (if so, please, ensure your data are log-transformormed and scaled; consider using the function data_transf of the present package to do this)
#' @param base numerical of length one. If var_perc is TRUE, it is the base of the logarithm used to log-transform the dependent variables.
#' @param FDR logical. If TRUE, after performing the ANOVA, it also correct p-values across the different variables with a false discovery rate multiple comparison correction (method "fdr" of the function p.adjust).
#' @param filter_sign  logical. If TRUE, the table will be filtered and only the p-values lower than the value specified in pcutoff will be considered.
#' @param pcutoff a numeric of length 1, must be between 0 and 1. If filter_sign is TRUE, cut-off value of the p-values.
#' @param cutPval logical. If TRUE, it cut the p-values using the cutP function of the present package.
#'
#'
#' @return A tibble the results of the t-tests.
#'
#' @export
gentab_lm_long <- function(df, dep, form_ind, mdl = "lm", left_cens = NULL, right_cens = NULL, var_perc = FALSE, base = exp(1), FDR = FALSE, filter_sign = FALSE, pcutoff = 0.05, cutPval = FALSE) {
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  if (!is.character(dep)) {stop("dep must be a character")}
  if (length(dep) == 0) {stop("dep must contain at least one element!")}
  if (any(is.na(dep))) {stop("dep must not contain mising values")}
  if (any(duplicated(dep))) {stop("dep must not contain duplicates")}
  if (!all(dep %in% colnames(df))) {stop("the names you indicate in dep must correspond to names of columns in df")}
  
  #if (any(map_lgl(df[,dep], ~ any(is.na(.x))))) {stop("in df, the columns chosen with dep must not contain missing values")}
  #if (any(map_lgl(df[,dep], ~ any(.x == 0)))) {warning("There are some zeros in the data, isn't it better to replace them with NAs?")}
  
  
  
  if (length(form_ind)!=1) {stop("form_ind must be a character of lenght 1, containing the formula of the linear model after the ~")}
  if (is.na(form_ind))  {stop("form_ind must be a character of lenght 1, containing the formula of the linear model after the ~, not a missing value")}
  if (!is.character(form_ind)) {stop("form_ind must be a character of lenght 1, containing the formula of the linear model after the ~")}
  
  if (any(check_if_fix_names_needed(dep))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                           paste0("'", paste0(dep[which(check_if_fix_names_needed(dep))], collapse = "', '"), "'")))}
  
  if (length(mdl) != 1) {stop('mdl must be one of the following: "lm", "lmer", or "tobit"')}
  if (is.na(mdl)) {stop('mdl must be one of the following: "lm", "lmer", or "tobit"')}
  if (!is.character(mdl)) {stop('mdl must be one of the following: "lm", "lmer", or "tobit"')}
  if (!mdl%in%c("lm", "lmer", "tobit")) {stop('mdl must be one of the following: "lm", "lmer", or "tobit"')}
  
  if (mdl == "tobit") {
    if (!is.null(left_cens)) {
      if (length(left_cens) == 0) {stop("left_cens must be NULL or a named numeric vector with the values for left-censoring")}
      if (!is.numeric(left_cens)) {stop("left_cens must be NULL or a named numeric vector with the values for left-censoring")}
      if (is.null(names(left_cens))) {stop("left_cens must be NULL or a named numeric vector with the values for left-censoring")}
      if (any(duplicated(names(left_cens)))) {stop("names of left_cens must not contain duplicated names")}
      if (!all(dep%in%names(left_cens))) {stop("the names of left_cens must correspond to names indicated in dep")}
    }
    
    if (!is.null(right_cens)) {
      if (length(right_cens) == 0) {stop("right_cens must be NULL or a named numeric vector with the values for right-censoring")}
      if (!is.numeric(right_cens)) {stop("right_cens must be NULL or a named numeric vector with the values for right-censoring")}
      if (is.null(names(right_cens))) {stop("right_cens must be NULL or a named numeric vector with the values for right-censoring")}
      if (any(duplicated(names(right_cens)))) {stop("names of right_cens must not contain duplicated names")}
      if (!all(dep%in%names(right_cens))) {stop("the names of right_cens must correspond to names indicated in dep")}
    }
  }
  
  if (length(var_perc)!=1) {stop("var_perc must be exclusively TRUE or FALSE")}
  if (is.na(var_perc)) {stop("var_perc must be exclusively TRUE or FALSE")}
  if (!is.logical(var_perc)) {stop("var_perc must be exclusively TRUE or FALSE")}
  
  if (var_perc) {
    if (length(base)!=1) {"base must be a numeric of length 1"}
    if (is.na(base)) {"base must be a numeric of length 1, and not a missing value"}
    if (!is.numeric(base)) {"base must be a numeric of length 1"}
  }
  
  if (length(FDR)!=1) {stop("FDR must be exclusively TRUE or FALSE")}
  if (is.na(FDR)) {stop("FDR must be exclusively TRUE or FALSE")}
  if (!is.logical(FDR)) {stop("FDR must be exclusively TRUE or FALSE")}
  
  if (length(filter_sign)!=1) {stop("filter_sign must be exclusively TRUE or FALSE")}
  if (is.na(filter_sign)) {stop("filter_sign must be exclusively TRUE or FALSE")}
  if (!is.logical(filter_sign)) {stop("filter_sign must be exclusively TRUE or FALSE")}
  
  if (filter_sign) {
    if (length(pcutoff)!=1) {stop("pcutoff must be a number between 0 and 1")}
    if (is.na(pcutoff)) {stop("pcutoff must be a number between 0 and 1")}
    if (!is.numeric(pcutoff)) {stop("pcutoff must be a number between 0 and 1")}
    if (pcutoff>1 | pcutoff<0) {stop("pcutoff must be a number between 0 and 1")}
  }
  
  if (length(cutPval)!=1) {stop("cutPval must be exclusively TRUE or FALSE")}
  if (is.na(cutPval)) {stop("cutPval must be exclusively TRUE or FALSE")}
  if (!is.logical(cutPval)) {stop("cutPval must be exclusively TRUE or FALSE")}
  
  
  
  if (mdl == "lm") {
    
    TAB_FINAL1 <- tibble(Dependent = character(),
                         Independent = character(),
                         N_observations = double(),
                         beta = double(),
                         beta_95confint_lower = double(),
                         beta_95confint_upper = double(),
                         SE = double(),
                         adj_R_sqrd = double(),
                         Pvalue = double())
    
    for (a in dep) {
      FIT <- lm(data = df, as.formula(paste0(a, " ~ ", form_ind)))
      SUMMARY_FIT <- summary(FIT)
      CONFINT_FIT <- confint(FIT)
      
      TAB_TEMP <- tibble(Dependent = rep(a, length(names(FIT[["coefficients"]]))),
                         Independent = names(FIT[["coefficients"]]),
                         N_observations = rep(nobs(FIT), length(names(FIT[["coefficients"]]))),
                         beta = as.numeric(rep(NA, length(names(FIT[["coefficients"]])))),
                         beta_95confint_lower = as.numeric(rep(NA, length(names(FIT[["coefficients"]])))),
                         beta_95confint_upper = as.numeric(rep(NA, length(names(FIT[["coefficients"]])))),
                         SE = as.numeric(rep(NA, length(names(FIT[["coefficients"]])))),
                         adj_R_sqrd = as.numeric(rep(NA, length(names(FIT[["coefficients"]])))),
                         Pvalue = as.numeric(rep(NA, length(names(FIT[["coefficients"]])))))
      
      if ((length(names(FIT[["coefficients"]])) == length(rownames(SUMMARY_FIT[["coefficients"]]))) & (length(rownames(SUMMARY_FIT[["coefficients"]])) == length(rownames(CONFINT_FIT)))) {
        if (all(names(FIT[["coefficients"]]) == rownames(SUMMARY_FIT[["coefficients"]])) &
            all(names(FIT[["coefficients"]]) == rownames(CONFINT_FIT)) &
            all(rownames(SUMMARY_FIT[["coefficients"]]) == rownames(CONFINT_FIT))) {
          TABOK <- TRUE
        } else {
          TABOK <- FALSE
        }
      } else {
        TABOK <- FALSE
      }
      
      if (TABOK == TRUE) {
        TAB_TEMP <- mutate(TAB_TEMP,
                           beta = SUMMARY_FIT[["coefficients"]][,"Estimate"],
                           beta_95confint_lower = CONFINT_FIT[,"2.5 %"],
                           beta_95confint_upper = CONFINT_FIT[,"97.5 %"],
                           SE = SUMMARY_FIT[["coefficients"]][,"Std. Error"],
                           adj_R_sqrd = rep(SUMMARY_FIT$adj.r.squared, length(SUMMARY_FIT[["coefficients"]][,"Estimate"])),
                           Pvalue = SUMMARY_FIT[["coefficients"]][,"Pr(>|t|)"])
      } else if (TABOK == FALSE) {
        for (vrb in names(FIT[["coefficients"]])) {
          THIS_ROW_INDEX_SUMMARY_FIT <- which(rownames(SUMMARY_FIT[["coefficients"]]) == vrb)
          
          if (length(THIS_ROW_INDEX_SUMMARY_FIT) > 1) {
            stop("something wired, probably some duplicate names of independent variables was created..")
          } else if (length(THIS_ROW_INDEX_SUMMARY_FIT) == 1) {
            TAB_TEMP[which(TAB_TEMP$Independent == vrb), "beta"] <- SUMMARY_FIT[["coefficients"]][THIS_ROW_INDEX_SUMMARY_FIT, "Estimate"]
            TAB_TEMP[which(TAB_TEMP$Independent == vrb), "SE"] <- SUMMARY_FIT[["coefficients"]][THIS_ROW_INDEX_SUMMARY_FIT, "Std. Error"]
            TAB_TEMP[which(TAB_TEMP$Independent == vrb), "adj_R_sqrd"] <- SUMMARY_FIT$adj.r.squared
            TAB_TEMP[which(TAB_TEMP$Independent == vrb), "Pvalue"] <- SUMMARY_FIT[["coefficients"]][THIS_ROW_INDEX_SUMMARY_FIT, "Pr(>|t|)"]
          }
          
          THIS_ROW_INDEX_CONFINT_FIT <- which(rownames(CONFINT_FIT) == vrb)
          
          if (length(THIS_ROW_INDEX_CONFINT_FIT) > 1) {
            stop("something wired, probably some duplicate names of independent variables was created..")
          } else if (length(THIS_ROW_INDEX_CONFINT_FIT) == 1) {
            TAB_TEMP[which(TAB_TEMP$Independent == vrb), "beta_95confint_lower"] <- CONFINT_FIT[THIS_ROW_INDEX_CONFINT_FIT, "2.5 %"]
            TAB_TEMP[which(TAB_TEMP$Independent == vrb), "beta_95confint_upper"] <- CONFINT_FIT[THIS_ROW_INDEX_CONFINT_FIT, "97.5 %"]
          }
        }
      }
      
      TAB_FINAL1 <- rbind(TAB_FINAL1, TAB_TEMP)
    }
    
    
    if (FDR == TRUE) {
      TAB_FINAL2 <- tibble(Dependent = character(),
                           Independent = character(),
                           N_observations = double(),
                           beta = double(),
                           beta_95confint_lower = double(),
                           beta_95confint_upper = double(),
                           SE = double(),
                           adj_R_sqrd = double(),
                           Pvalue = double(),
                           FDR_Pvalue = double())
      
      for (b in unique(TAB_FINAL1$Independent)) {
        TAB_TEMP <- TAB_FINAL1 %>%
          filter(Independent == b) %>%
          mutate(FDR_Pvalue = p.adjust(Pvalue, method ="fdr"))
        
        TAB_FINAL2 <- rbind(TAB_FINAL2, TAB_TEMP)
      }
      
      TAB_FINAL2 <- mutate(TAB_FINAL2,
                           negative_log10p = -log10(Pvalue),
                           negative_log10fdr = -log10(FDR_Pvalue))
      
    } else if (FDR == FALSE) {
      TAB_FINAL2 <- mutate(TAB_FINAL1,
                           negative_log10p = -log10(Pvalue))
    }
    
    if (var_perc == TRUE) {
      TAB_FINAL2 <- mutate(TAB_FINAL2,
                           variation_perc = ((base^beta)-1)*100)
    }
    
    
    
  } else if (mdl == "tobit") {
    TAB_FINAL1 <- tibble(Dependent = character(),
                         Independent = character(),
                         N_observations = double(),
                         N_left_censored = double(),
                         N_uncensored = double(),
                         N_right_censored = double(),
                         beta = double(),
                         beta_95confint_lower = double(),
                         beta_95confint_upper = double(),
                         SE = double(),
                         Pvalue = double())
    
    for (a in dep) {
      if (is.null(left_cens)) {
        this_left <- -Inf
      } else {
        if (is.na(left_cens[a])) {
          this_left <- -Inf
        } else {
          this_left <- left_cens[a]
        }
      }
      
      if (is.null(right_cens)) {
        this_right <- Inf
      } else {
        if (is.na(right_cens[a])) {
          this_right <- Inf
        } else {
          this_right <- right_cens[a]
        }
      }
      
      
      TBFIT <- tobit(formula = as.formula(paste0(a, " ~ ", form_ind)), left = this_left, right = this_right, data = df)
      SUMMARY_TBFIT <- summary(TBFIT)  # if there are NAs in the coefficient, this function here generates an error
      COEFF_TBFIT <- SUMMARY_TBFIT[["coefficients"]][-which(rownames(SUMMARY_TBFIT[["coefficients"]])=="Log(scale)"),]
      CONFINT_TBFIT <- confint(TBFIT)
      
      if (length(COEFF_TBFIT[,"Estimate"]) != length(CONFINT_TBFIT[,"2.5 %"])) {stop("Different lenght between summary and confint - maybe because of some NA generated within the coefficients of the model?")}
      TAB_TEMP <- tibble(Dependent = rep(a, length(COEFF_TBFIT[,"Estimate"])),
                         Independent = names(COEFF_TBFIT[,"Estimate"]),
                         N_observations = rep(SUMMARY_TBFIT[["n"]]["Total"], length(COEFF_TBFIT[,"Estimate"])),
                         N_left_censored = rep(SUMMARY_TBFIT[["n"]]["Left-censored"], length(COEFF_TBFIT[,"Estimate"])),
                         N_uncensored = rep(SUMMARY_TBFIT[["n"]]["Uncensored"], length(COEFF_TBFIT[,"Estimate"])),
                         N_right_censored = rep(SUMMARY_TBFIT[["n"]]["Right-censored"], length(COEFF_TBFIT[,"Estimate"])),
                         beta = COEFF_TBFIT[,"Estimate"],
                         beta_95confint_lower = CONFINT_TBFIT[,"2.5 %"],
                         beta_95confint_upper = CONFINT_TBFIT[,"97.5 %"],
                         SE = COEFF_TBFIT[,"Std. Error"],
                         Pvalue = COEFF_TBFIT[,"Pr(>|z|)"])
      TAB_FINAL1 <- rbind(TAB_FINAL1, TAB_TEMP)
    }
    
    
    if (FDR == TRUE) {
      TAB_FINAL2 <- tibble(Dependent = character(),
                           Independent = character(),
                           N_observations = double(),
                           N_left_censored = double(),
                           N_uncensored = double(),
                           N_right_censored = double(),
                           beta = double(),
                           beta_95confint_lower = double(),
                           beta_95confint_upper = double(),
                           SE = double(),
                           Pvalue = double(),
                           FDR_Pvalue = double())
      
      for (b in unique(TAB_FINAL1$Independent)) {
        TAB_TEMP <- TAB_FINAL1 %>%
          filter(Independent == b) %>%
          mutate(FDR_Pvalue = p.adjust(Pvalue, method ="fdr"))
        
        TAB_FINAL2 <- rbind(TAB_FINAL2, TAB_TEMP)
      }
      
      TAB_FINAL2 <- mutate(TAB_FINAL2,
                           negative_log10p = -log10(Pvalue),
                           negative_log10fdr = -log10(FDR_Pvalue))
      
    } else if (FDR == FALSE) {
      TAB_FINAL2 <- mutate(TAB_FINAL1,
                           negative_log10p = -log10(Pvalue))
    }
    
    if (var_perc == TRUE) {
      TAB_FINAL2 <- mutate(TAB_FINAL2,
                           variation_perc = ((base^beta)-1)*100)
    }
    
  } else if (mdl == "lmer") {
    
    TAB_FINAL1 <- tibble(Dependent = character(),
                         Independent = character(),
                         N_observations = double(),
                         N_groups = double(),
                         beta = double(),
                         beta_95confint_lower = double(),
                         beta_95confint_upper = double(),
                         SE = double(),
                         adj_R_sqrd_marginal = double(),
                         adj_R_sqrd_conditional = double(),
                         Pvalue = double())
    
    for (a in dep) {
      FIT <- lmerTest::lmer(data = df, as.formula(paste0(a, " ~ ", form_ind)))
      SUMMARY_FIT <- summary(FIT)
      CONFINT_FIT <- confint(FIT)
      
      if(length(SUMMARY_FIT[["ngrps"]]) != 1) {stop("you would need to update this forumla to include more than one random effect")}
      
      TAB_TEMP <- tibble(Dependent = rep(a, length(SUMMARY_FIT[["coefficients"]][,"Estimate"])),
                         Independent = names(SUMMARY_FIT[["coefficients"]][,"Estimate"]),
                         N_observations = rep(nobs(FIT), length(SUMMARY_FIT[["coefficients"]][,"Estimate"])),
                         N_groups = rep(SUMMARY_FIT[["ngrps"]], length(SUMMARY_FIT[["coefficients"]][,"Estimate"])),
                         beta = SUMMARY_FIT[["coefficients"]][,"Estimate"],
                         beta_95confint_lower = CONFINT_FIT[rownames(SUMMARY_FIT[["coefficients"]]),"2.5 %"],
                         beta_95confint_upper = CONFINT_FIT[rownames(SUMMARY_FIT[["coefficients"]]),"97.5 %"],
                         SE = SUMMARY_FIT[["coefficients"]][,"Std. Error"],
                         adj_R_sqrd_marginal = rep(r.squaredGLMM(FIT)[,"R2m"], length(SUMMARY_FIT[["coefficients"]][,"Estimate"])),
                         adj_R_sqrd_conditional = rep(r.squaredGLMM(FIT)[,"R2c"], length(SUMMARY_FIT[["coefficients"]][,"Estimate"])),
                         Pvalue = SUMMARY_FIT[["coefficients"]][,"Pr(>|t|)"])
      
      TAB_FINAL1 <- rbind(TAB_FINAL1, TAB_TEMP)
    }
    
    
    if (FDR == TRUE) {
      TAB_FINAL2 <- tibble(Dependent = character(),
                           Independent = character(),
                           N_observations = double(),
                           N_groups = double(),
                           beta = double(),
                           beta_95confint_lower = double(),
                           beta_95confint_upper = double(),
                           SE = double(),
                           adj_R_sqrd_marginal = double(),
                           adj_R_sqrd_conditional = double(),
                           Pvalue = double(),
                           FDR_Pvalue = double())
      
      for (b in unique(TAB_FINAL1$Independent)) {
        TAB_TEMP <- TAB_FINAL1 %>%
          filter(Independent == b) %>%
          mutate(FDR_Pvalue = p.adjust(Pvalue, method ="fdr"))
        
        TAB_FINAL2 <- rbind(TAB_FINAL2, TAB_TEMP)
      }
      TAB_FINAL2 <- mutate(TAB_FINAL2,
                           negative_log10p = -log10(Pvalue),
                           negative_log10fdr = -log10(FDR_Pvalue))
      
    } else if (FDR == FALSE) {
      TAB_FINAL2 <- mutate(TAB_FINAL1,
                           negative_log10p = -log10(Pvalue))
    }
    
    if (var_perc == TRUE) {
      TAB_FINAL2 <- mutate(TAB_FINAL2,
                           variation_perc = ((base^beta)-1)*100)
    }
  }
  
  if (FDR == TRUE) {
    TAB_FINAL2 <- arrange(TAB_FINAL2, Independent, FDR_Pvalue)
  } else {
    TAB_FINAL2 <- arrange(TAB_FINAL2, Independent, Pvalue)
  }
  
  
  if (filter_sign == TRUE & FDR == TRUE) {
    TAB_FINAL2 <- filter(TAB_FINAL2,
                         FDR_Pvalue < pcutoff)
  } else if (filter_sign == TRUE & FDR == FALSE) {
    TAB_FINAL2 <- filter(TAB_FINAL2,
                         Pvalue < pcutoff)
  }
  
  if (cutPval == TRUE & FDR == TRUE) {
    TAB_FINAL2[, "FDR_Pvalue"] <- map_chr(pull(TAB_FINAL2, "FDR_Pvalue"), cutP)
    TAB_FINAL2[, "Pvalue"] <- map_chr(pull(TAB_FINAL2, "Pvalue"), cutP)
  } else if (cutPval == TRUE & FDR == FALSE) {
    TAB_FINAL2[, "Pvalue"] <- map_chr(pull(TAB_FINAL2, "Pvalue"), cutP)
  }
  
  return(TAB_FINAL2)
}


