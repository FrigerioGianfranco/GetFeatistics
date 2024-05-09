
###! da capire se tolgiere anche queste e lasciare solo quelle col Turkey

##### attualmente NON esportata!


###!!! -> function to generate a Table of P-values from a DF applying an ANOVA
#f is the name (with "") of the factor variable,
#v is a char vector (with "") of all variable to apply ANOVA

gentab_P.ANOVA <- function(df, f, v, FDR = FALSE, cutPval = TRUE) {
  
  #function for extract P-value from ANOVA
  getANOVA.P <- function (v, f) {
    PvalueANOVA <- unlist(summary(aov(v ~ f)))["Pr(>F)1"]
    names(PvalueANOVA) <- NULL
    PvalueANOVA
  }
  
  df_fil1 <- select(df, all_of(f), all_of(v))
  df_fil2 <- select(df, all_of(v))
  Pvalues_ANOVA <- map_dbl(df_fil2, getANOVA.P, pull(df,f))
  if (FDR == TRUE) {Pvalues_ANOVA <- p.adjust(Pvalues_ANOVA, method ="fdr")}
  
  if (cutPval == TRUE) {Pvalues_ANOVA <- map_chr(Pvalues_ANOVA, cutP)}
  
  df_P <- tibble(Variables = v,
                 Anova_P = Pvalues_ANOVA)
  if (FDR == TRUE) {names(df_P) <- c("Variables", "Anova_P_FDR")}
  
  return(df_P)
}

