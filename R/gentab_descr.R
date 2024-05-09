

#' Generate a table with descriptive statistics
#'
#' Given a dataframe and a set of numerical variables of that dataframe, it generate a table with descriptive statistics for each of those variables.
#'
#' @param df a dataframe.
#' @param v a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values.
#' @param f character of length 1 or a missing value. If you pass here a character, that must be a column of the df containing a factor: if so, the function will perform the descriptive statistics grouped by that factor.
#' @param type one of the following: "mean", "mean SD", "median", "median (min; max)", "median (5th; 95th percentile)", or "median (25th; 75th percentile)".
#' @param ROUND logical. Should the values be rounded? if TRUE, you could indicate the number of decimal places in the dig argument.
#' @param dig numeric of length 1 or a missing value. If ROUND is TRUE, you can indicate here the number of decimal places; if missing, the function round_the_result of the present package will be used.
#' @param unit_mes character of length 1 or a missing value. You can specify here a unit of measure to add to each cell of the table (I'd suggest to do it only if is actually the same for each variable indicated with v).
#' @param missing one of the following: "always", "never", or "only if >0". Do you also want to report in the table how many missing values are present?
#'
#'
#' @return A tibble (a data frame) in which the first row is the number of observations, each other row reports the desired descriptive statistics for each considered variable.
#'
#' @export
gentab_descr <- function(df, v, f = NA, type = "mean", ROUND = FALSE, dig = NA, unit_mes = NA, missing = "only if >0") {
  
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  if (!all(v %in% colnames(df))) {stop("v must be a vector containing the names of the coloumns in df that you want to apply the descriptive statistics")}
  if (!all((map_lgl(select(df, all_of(v)), is.numeric)))) {stop("all coloumns of df passed with v must be numeric!")}
  
  if (length(f) !=1) {stop("f must be a character of lenght 1 or a missing value")}
  if (!is.na(f)) {
    if (!f%in%colnames(df)) {stop("f must be the name of the coloumn that you want to consider as factor variable")}
    if (!is.factor(pull(df,f))) {stop("f must be the column name in df of a factor variable!")}
    
    f_and_v  <- c(f, v)
    if (any(check_if_fix_names_needed(f_and_v))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                                 paste0("'", paste0(f_and_v[which(check_if_fix_names_needed(f_and_v))], collapse = "', '"), "'")))}
  } else {
    if (any(check_if_fix_names_needed(v))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                           paste0("'", paste0(v[which(check_if_fix_names_needed(v))], collapse = "', '"), "'")))}
  }
  
  if (!is.character(type)) {stop('type must be a character of length 1 containing one of the following: "mean", "mean SD", "median", "median (min; max)", "median (5th; 95th percentile)", or "median (25th; 75th percentile)"')}
  if (length(type) != 1) {stop('type must be a character of length 1 containing one of the following: "mean", "mean SD", "median", "median (min; max)", "median (5th; 95th percentile)", or "median (25th; 75th percentile)"')}
  if (!type%in%c("mean", "mean SD", "median", "median (min; max)", "median (5th; 95th percentile)", "median (25th; 75th percentile)")) {stop('type must be a character of length 1 containing one of the following: "mean", "mean SD", "median", "median (min; max)", "median (5th; 95th percentile)", or "median (25th; 75th percentile)"')}
  
  if (length(ROUND)!=1) {stop("ROUND must be exclusively TRUE or FALSE")} else if (!is.logical(ROUND)) {stop("ROUND must be exclusively TRUE or FALSE")} else if (is.na(ROUND)) {stop("ROUND must be exclusively TRUE or FALSE")}
  
  if (ROUND == TRUE) {
    if (length(dig)==1) {
      if (!is.na(dig)) {
        if (dig != round(dig)) {stop("dig must contain integer numbers indicating the digits you want to round your data")}
        dig <- rep(dig, length(v))
      } else if (length(dig) > 1) {
        if (mean(dig == round(dig)) != 1) {stop("dig must contain integer numbers indicating the digits you want to round your data")}
        if (length(v) != length(dig)) {stop("dig and v must have the same length or dig must be of length 1!")}
      }
    }
  }
  
  if (!is.na(unit_mes)) {
    if (!is.character(unit_mes)) {stop("unit_mes must be a character vector of length 1")}
    if (length(unit_mes) != 1) {stop("unit_mes must be a character vector of length 1")}
  }
  
  
  if (!is.character(missing)) {stop('missing must be a character of length 1 containing one of the following: "always", "never", "only if >0"')}
  if (length(missing) != 1) {stop('missing must be a character of length 1 containing one of the following: "always", "never", "only if >0"')}
  if (!missing%in%c("always", "never", "only if >0")) {stop('missing must be a character of length 1 containing one of the following: "always", "never", "only if >0"')}
  
  if (!is.na(f)) {
    Tabfinal_matrix  <- matrix(rep(NA, times = (1+length(levels(pull(df,f))))*(1+length(v))),
                               ncol = 1+length(levels(pull(df,f))),
                               nrow = 1+length(v))
    
    
    colnames(Tabfinal_matrix) <- c("Variables", paste(levels(pull(df,f)), type))
    
    numerosity <- df %>%
      group_by(df[f]) %>%
      summarize(N=n()) %>%
      pull(N)
  } else {
    Tabfinal_matrix  <- matrix(rep(NA, times = 2*(1+length(v))),
                               ncol = 2,
                               nrow = 1+length(v))
    
    colnames(Tabfinal_matrix) <- c("Variables", type)
  }
  
  if (!is.na(unit_mes)) {
    colnames(Tabfinal_matrix)[-1] <- paste0(colnames(Tabfinal_matrix)[-1], " (", unit_mes, ")")
  }
  
  
  
  for (i in 1:nrow(Tabfinal_matrix)) {
    for (u in 1:ncol(Tabfinal_matrix)) {
      
      rounding <- function(x) {
        if (ROUND == FALSE) {
          x
        } else if (length(dig)==1) {
          if (is.na(dig)) {
            round_the_result(x)
          }
        } else if (length(dig)>1) {
          if (is.na(dig[i-1])) {
            round_the_result(x)
          } else {
            round(x, digits = dig[i-1])
          }
        }
      }
      
      
      if(i == 1 & u == 1) {
        Tabfinal_matrix[i,u] <- "n"
        
        if (!is.na(f)) {
          if (missing == "always" | (missing == "only if >0" & sum(is.na(pull(df, f)))>0)) {
            Tabfinal_matrix[i,u] <- paste0("n", "; missing = ", sum(is.na(pull(df, f))), " (", round(mean(is.na(pull(df, f)))*100, digits = 2), "%)")
          }
        }
        
        
      } else if (i == 1) {
        Tabfinal_matrix[1,u] <-  ifelse(!is.na(f), numerosity[u-1], length(pull(df, v[1])))
      } else if (i > 1 & u == 1) {
        Tabfinal_matrix[-1,1] <- v
      } else if (i > 1 & u > 1) {
        if (!is.na(f)) {df_fil <- df[which(pull(df,f) == levels(pull(df,f))[u-1]),]} else {df_fil <- df}
        if (!is.numeric(pull(df, v[i-1]))) {
          Tabfinal_matrix[i,u] <- "non numeric data!"
        } else if (type == "mean") {
          Tabfinal_matrix[i,u] <- rounding(mean(pull(df_fil, v[i-1]), na.rm = TRUE))
        } else if (type == "mean SD") {
          Tabfinal_matrix[i,u] <- paste0(rounding(mean(pull(df_fil, v[i-1]), na.rm = TRUE)), " ? ", rounding(sd(pull(df_fil, v[i-1]), na.rm = TRUE)))
        } else if (type == "median") {
          Tabfinal_matrix[i,u] <- rounding(median(pull(df_fil, v[i-1])))
        } else if (type == "median (min; max)") {
          Tabfinal_matrix[i,u] <- paste0(rounding(median(pull(df_fil, v[i-1]), na.rm = TRUE)), " (", rounding(min(pull(df_fil, v[i-1]), na.rm = TRUE)), "; ", rounding(max(pull(df_fil, v[i-1]), na.rm = TRUE)), ")")
        } else if (type == "median (5th; 95th percentile)") {
          Tabfinal_matrix[i,u] <- paste0(rounding(median(pull(df_fil, v[i-1]), na.rm = TRUE)), " (", rounding(quantile(pull(df_fil, v[i-1]), 0.05, na.rm = TRUE)), "; ", rounding(quantile(pull(df_fil, v[i-1]), 0.95, na.rm = TRUE)), ")")
        } else if (type == "median (25th; 75th percentile)") {
          Tabfinal_matrix[i,u] <- paste0(rounding(median(pull(df_fil, v[i-1]), na.rm = TRUE)), " (", rounding(quantile(pull(df_fil, v[i-1]), 0.25, na.rm = TRUE)), "; ", rounding(quantile(pull(df_fil, v[i-1]), 0.75, na.rm = TRUE)), ")")
        }
        
        if (missing == "always" | (missing == "only if >0" & sum(is.na(pull(df, v[i-1])))>0)) {
          Tabfinal_matrix[i,u] <- paste0(Tabfinal_matrix[i,u], "; missing = ", sum(is.na(pull(df, v[i-1]))), " (", mean(is.na(pull(df, v[i-1])))*100, "%)")
        }
      }
    }
  }
  
  
  Tabfinal_df <- as_tibble(Tabfinal_matrix)
  
  if (type == "mean" | type == "median") {
    if (missing == "never" | (missing == "only if >0" & mean(map_lgl(select(df, all_of(v)), ~ any(is.na(.))))==0)) {
      
      for (coln in colnames(Tabfinal_df)[-1]) {
        Tabfinal_df[, coln] <- as.numeric(pull(Tabfinal_df, coln))
      }
    }
  }
  
  return(Tabfinal_df)
  
}


