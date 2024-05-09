

######!!!!! -> function to generate a Table of P-values from a DF applying ANOVA with post-hoc   ----  #the next step is to put where it is higher and the generation of box-plot only for those statistically significant!!
#f is the name (with "") of the factor variable. MUST BE FROM 3 TO 6 LEVELS!
#v is a char vector (with "") of all variable to apply ANOVA
#posthoc is the posthoc method (as from the p.adj function)
#FDR can be TRUE or FALSE to perform the false discovery rate P.value correction
#cutPval can be TRUE or FALSE to perform the better subsetting of resutls ("with <0.001")


### ATTUALMENTE NON ESPORTATA!!


gentab_P.ANOVA_posthoc <- function(df, f, v, posthoc = "bonf", FDR = FALSE, cutPval = TRUE) {
  
  #function for extract P-value from ANOVA
  getANOVA.P <- function (v, f) {
    PvalueANOVA <- unlist(summary(aov(v ~ f)))["Pr(>F)1"]
    names(PvalueANOVA) <- NULL
    PvalueANOVA
  }
  
  # Function to generate a vector of P-values from pairwise T.test. Factor MUST have 3 levels
  getPairposthoc.P_3lev <- function(v,f, posthoc = "bonf") {
    if (length(levels(f))!=3) {stop("FACTOR LEVELS MUST BE 3!")} else {
      factlev <- levels(f)
      PvaluesTab <- pairwise.t.test(v, f, p.adj = posthoc)$p.value
      
      PvaluesVect <- c(PvaluesTab[factlev[2],factlev[1]], PvaluesTab[factlev[3],factlev[1]], PvaluesTab[factlev[3],factlev[2]])
      
      
      names(PvaluesVect) <- c(paste(factlev[1],"vs",factlev[2]),paste(factlev[1],"vs",factlev[3]),paste(factlev[2],"vs",factlev[3]))
      PvaluesVect_withANOVA <- c(ANOVA=getANOVA.P(v,f),PvaluesVect)
      PvaluesVect_withANOVA
    }}
  
  
  ###!!! -> function to generate a Table of P-values from a DF applying ANOVA with post-hoc
  #f is the name (with "") of the factor variable. MUST BE 3 LEVELS!
  #v is a char vector (with "") of all variable to apply ANOVA
  gentab_P.ANOVA_posthoc_3lev <- function(df, f, v, posthoc = "bonf", FDR = FALSE, cutPval = TRUE) {
    if (length(levels(pull(df,f)))!=3) {stop("FACTOR LEVELS MUST BE 3!")} else {
      df_fil1 <- select(df, all_of(f), all_of(v))
      df_fil2 <- select(df, all_of(v))
      Pvalues_post <- t(sapply(df_fil2, getPairposthoc.P_3lev, pull(df,f), posthoc))
      Pvalues_post_df <- as_tibble(Pvalues_post)
      
      if (FDR == TRUE) {Pvalues_post_df <- mutate_all(Pvalues_post_df, function(x) {p.adjust(x, method ="fdr")})}
      
      if (cutPval == TRUE) {Pvalues_post_df <- mutate_all(Pvalues_post_df, function(x) {map_chr(x, cutP)})}
      
      Pvalues_post_df2 <- cbind(rownames(Pvalues_post), Pvalues_post_df)
      
      names(Pvalues_post_df2)[1] <- "Variables"
      
      Pvalues_post_df2
    }}
  
  
  
  # Function to generate a vector of P-values from pairwise T.test. Factor MUST have 4 levels
  getPairposthoc.P_4lev <- function(v,f, posthoc = "bonf") {
    if (length(levels(f))!=4) {stop("FACTOR LEVELS MUST BE 4!")} else {
      factlev <- levels(f)
      PvaluesTab <- pairwise.t.test(v, f, p.adj = posthoc)$p.value
      
      PvaluesVect <- c(PvaluesTab[factlev[2],factlev[1]], PvaluesTab[factlev[3],factlev[1]], PvaluesTab[factlev[4],factlev[1]], PvaluesTab[factlev[3],factlev[2]], PvaluesTab[factlev[4],factlev[2]], PvaluesTab[factlev[4],factlev[3]])
      
      names(PvaluesVect) <- c(paste(factlev[1],"vs",factlev[2]), paste(factlev[1],"vs",factlev[3]),     paste(factlev[1],"vs",factlev[4]),        paste(factlev[2],"vs",factlev[3]),       paste(factlev[2],"vs",factlev[4]),        paste(factlev[3],"vs",factlev[4]))
      PvaluesVect_withANOVA <- c(ANOVA=getANOVA.P(v,f),PvaluesVect)
      PvaluesVect_withANOVA
    }}
  
  ###!!! -> function to generate a Table of P-values from a DF applying ANOVA with post-hoc Bonferoni
  #f is the name (with "") of the factor variable. MUST BE 4 LEVELS!
  #v is a char vector (with "") of all variable to apply ANOVA
  gentab_P.ANOVA_posthoc_4lev <- function(df, f, v, posthoc = "bonf", FDR = FALSE, cutPval = TRUE) {
    if (length(levels(pull(df,f)))!=4) {stop("FACTOR LEVELS MUST BE 4!")} else {
      df_fil1 <- select(df, all_of(f), all_of(v))
      df_fil2 <- select(df, all_of(v))
      Pvalues_post <- t(sapply(df_fil2, getPairposthoc.P_4lev, pull(df,f), posthoc))
      Pvalues_post_df <- as_tibble(Pvalues_post)
      
      if (FDR == TRUE) {Pvalues_post_df <- mutate_all(Pvalues_post_df, function(x) {p.adjust(x, method ="fdr")})}
      
      if (cutPval == TRUE) {Pvalues_post_df <- mutate_all(Pvalues_post_df, function(x) {map_chr(x, cutP)})}
      
      Pvalues_post_df2 <- cbind(rownames(Pvalues_post), Pvalues_post_df)
      
      names(Pvalues_post_df2)[1] <- "Variables"
      
      Pvalues_post_df2
    }}
  
  # Function to generate a vector of P-values from pairwise T.test. Factor MUST have 5 levels
  getPairposthoc.P_5lev <- function(v,f, posthoc = "bonf") {
    if (length(levels(f))!=5) {stop("FACTOR LEVELS MUST BE 5!")} else {
      factlev <- levels(f)
      PvaluesTab <- pairwise.t.test(v, f, p.adj = posthoc)$p.value
      PvaluesVect <- c(PvaluesTab[factlev[2],factlev[1]], PvaluesTab[factlev[3],factlev[1]], PvaluesTab[factlev[4],factlev[1]], PvaluesTab[factlev[5],factlev[1]], PvaluesTab[factlev[3],factlev[2]], PvaluesTab[factlev[4],factlev[2]], PvaluesTab[factlev[5],factlev[2]], PvaluesTab[factlev[4],factlev[3]], PvaluesTab[factlev[5],factlev[3]], PvaluesTab[factlev[5],factlev[4]])
      
      names(PvaluesVect) <- c(paste(factlev[1],"vs",factlev[2]), paste(factlev[1],"vs",factlev[3]),     paste(factlev[1],"vs",factlev[4]),        paste(factlev[1],"vs",factlev[5]),       paste(factlev[2],"vs",factlev[3]),        paste(factlev[2],"vs",factlev[4]),        paste(factlev[2],"vs",factlev[5]),        paste(factlev[3],"vs",factlev[4]),        paste(factlev[3],"vs",factlev[5]),        paste(factlev[4],"vs",factlev[5]))
      PvaluesVect_withANOVA <- c(ANOVA=getANOVA.P(v,f),PvaluesVect)
      PvaluesVect_withANOVA
    }}
  
  ###!!! -> function to generate a Table of P-values from a DF applying ANOVA with post-hoc Bonferoni
  #f is the name (with "") of the factor variable. MUST BE 5 LEVELS!
  #v is a char vector (with "") of all variable to apply ANOVA
  gentab_P.ANOVA_posthoc_5lev <- function(df, f, v, posthoc = "bonf", FDR = FALSE, cutPval = TRUE) {
    if (length(levels(pull(df,f)))!=5) {stop("FACTOR LEVELS MUST BE 5!")} else {
      df_fil1 <- select(df, all_of(f), all_of(v))
      df_fil2 <- select(df, all_of(v))
      Pvalues_post <- t(sapply(df_fil2, getPairposthoc.P_5lev, pull(df,f), posthoc))
      Pvalues_post_df <- as_tibble(Pvalues_post)
      
      if (FDR == TRUE) {Pvalues_post_df <- mutate_all(Pvalues_post_df, function(x) {p.adjust(x, method ="fdr")})}
      
      if (cutPval == TRUE) {Pvalues_post_df <- mutate_all(Pvalues_post_df, function(x) {map_chr(x, cutP)})}
      
      Pvalues_post_df2 <- cbind(rownames(Pvalues_post), Pvalues_post_df)
      
      names(Pvalues_post_df2)[1] <- "Variables"
      
      Pvalues_post_df2
      
    }}
  
  
  
  # Function to generate a vector of P-values from pairwise T.test. Factor MUST have 6 levels
  getPairposthoc.P_6lev <- function(v,f, posthoc = "bonf") {
    if (length(levels(f))!=6) {stop("FACTOR LEVELS MUST BE 6!")} else {
      factlev <- levels(f)
      PvaluesTab <- pairwise.t.test(v, f, p.adj = posthoc)$p.value
      PvaluesVect <- c(PvaluesTab[factlev[2],factlev[1]], PvaluesTab[factlev[3],factlev[1]], PvaluesTab[factlev[4],factlev[1]], PvaluesTab[factlev[5],factlev[1]], PvaluesTab[factlev[6],factlev[1]], PvaluesTab[factlev[3],factlev[2]], PvaluesTab[factlev[4],factlev[2]], PvaluesTab[factlev[5],factlev[2]], PvaluesTab[factlev[6],factlev[2]], PvaluesTab[factlev[4],factlev[3]], PvaluesTab[factlev[5],factlev[3]], PvaluesTab[factlev[6],factlev[3]], PvaluesTab[factlev[5],factlev[4]], PvaluesTab[factlev[6],factlev[4]], PvaluesTab[factlev[6],factlev[5]])
      
      names(PvaluesVect) <- c(paste(factlev[1],"vs",factlev[2]), paste(factlev[1],"vs",factlev[3]),     paste(factlev[1],"vs",factlev[4]),        paste(factlev[1],"vs",factlev[5]),       paste(factlev[1],"vs",factlev[6]),        paste(factlev[2],"vs",factlev[3]),        paste(factlev[2],"vs",factlev[4]),        paste(factlev[2],"vs",factlev[5]),        paste(factlev[2],"vs",factlev[6]),        paste(factlev[3],"vs",factlev[4]),        paste(factlev[3],"vs",factlev[5]),     paste(factlev[3],"vs",factlev[6]),    paste(factlev[4],"vs",factlev[5]),     paste(factlev[4],"vs",factlev[6]),       paste(factlev[5],"vs",factlev[6]))
      PvaluesVect_withANOVA <- c(ANOVA=getANOVA.P(v,f),PvaluesVect)
      PvaluesVect_withANOVA
    }}
  
  ###!!! -> function to generate a Table of P-values from a DF applying ANOVA with post-hoc Bonferoni
  #f is the name (with "") of the factor variable. MUST BE 6 LEVELS!
  #v is a char vector (with "") of all variable to apply ANOVA
  gentab_P.ANOVA_posthoc_6lev <- function(df, f, v, posthoc = "bonf", FDR = FALSE, cutPval = TRUE) {
    if (length(levels(pull(df,f)))!=6) {stop("FACTOR LEVELS MUST BE 6!")} else {
      df_fil1 <- select(df, all_of(f), all_of(v))
      df_fil2 <- select(df, all_of(v))
      Pvalues_post <- t(sapply(df_fil2, getPairposthoc.P_6lev, pull(df,f)))
      
      Pvalues_post_df <- as_tibble(Pvalues_post)
      
      if (FDR == TRUE) {Pvalues_post_df <- mutate_all(Pvalues_post_df, function(x) {p.adjust(x, method ="fdr")})}
      
      if (cutPval == TRUE) {Pvalues_post_df <- mutate_all(Pvalues_post_df, function(x) {map_chr(x, cutP)})}
      
      Pvalues_post_df2 <- cbind(rownames(Pvalues_post), Pvalues_post_df)
      
      names(Pvalues_post_df2)[1] <- "Variables"
      
      Pvalues_post_df2
    }}
  
  
  if (length(levels(pull(df,f)))==3) {
    gentab_P.ANOVA_posthoc_3lev(df, f, v, posthoc = posthoc, FDR = FDR, cutPval = cutPval)
  } else if (length(levels(pull(df,f)))==4) {
    gentab_P.ANOVA_posthoc_4lev(df, f, v, posthoc = posthoc, FDR = FDR, cutPval = cutPval)
  } else if (length(levels(pull(df,f)))==5) {
    gentab_P.ANOVA_posthoc_5lev(df, f, v, posthoc = posthoc, FDR = FDR, cutPval = cutPval)
  } else if (length(levels(pull(df,f)))==6) {
    gentab_P.ANOVA_posthoc_6lev(df, f, v, posthoc = posthoc, FDR = FDR, cutPval = cutPval)
  } else {
    stop("FACTOR LEVELS MUST BE FROM 3 TO 6!")
  }
}
