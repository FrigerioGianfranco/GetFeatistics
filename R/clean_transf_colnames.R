
#' Clean colnames after transformation
#'
#' Right after the transf_data function, column names are added with additional names such as "_transf_mr_ln_paretosc", (which means transformed missing values replaced, natural-log-transformed, pareto-scaled). With this function you clean that and reduce the dataframe in order to contain only transformed variable of interest.
#'
#' @param df a dataframa after the transf_data function.
#' @param v a character vector. Each element must correspond to original column names of the df and ideally this is the same that you passed previously to the function transf_data.
#' @param suffix_to_consider character of length 1. The suffix of the columns of the variable to use.
#'
#'
#' @return df with only transformed column of interest, with cleaned names.
#'
#' @export
clean_transf_colnames <- function(df, v, suffix_to_consider = "_mr_ln_paretosc") {
  
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  if (!is.character(v)) {stop("v must be a character")}
  if (any(is.na(v))) {stop("v must not contain mising values")}
  
  if (length(suffix_to_consider)!=1) {stop("suffix_to_consider must be a character of length 1")}
  if (is.na(suffix_to_consider)) {stop("suffix_to_consider must not be NA")}
  if (!is.character(suffix_to_consider)) {stop("suffix_to_consider must be a character")}
  
  if (!grepl("_mr", suffix_to_consider) &
      !grepl("_ln", suffix_to_consider) &
      !grepl("_Log", suffix_to_consider) &
      !grepl("_log", suffix_to_consider) &
      !grepl("_meansc", suffix_to_consider) &
      !grepl("_autosc", suffix_to_consider) &
      !grepl("_paretosc", suffix_to_consider) &
      !grepl("_rangesc", suffix_to_consider)) stop("suffix_to_consider must be a suffix as generated from the funciton transf_data")
  
  columns_to_keep <- paste0(v, suffix_to_consider)
  
  if (!all(columns_to_keep %in% colnames(df))) {stop(paste0("not all the columns v added with the suffix passed to suffix_to_consider are present in df. In particular these are not present: ",
                                                            paste0(columns_to_keep[which(!columns_to_keep %in% colnames(df))], collapse = ", ")))}
 
  
  columns_to_remove <- character()
  
  for (a in v) {
    if (a %in% colnames(df)) {
      columns_to_remove <- c(columns_to_remove, a)
    }
    if (any(grepl("_mr", colnames(df)) & grepl(a, colnames(df)))) {
      columns_to_remove <- c(columns_to_remove, colnames(df)[which(grepl("_mr", colnames(df)) & grepl(a, colnames(df)))])
    }
    if (any(grepl("_ln", colnames(df)) & grepl(a, colnames(df)))) {
      columns_to_remove <- c(columns_to_remove, colnames(df)[which(grepl("_ln", colnames(df)) & grepl(a, colnames(df)))])
    }
    if (any(grepl("_Log", colnames(df)) & grepl(a, colnames(df)))) {
      columns_to_remove <- c(columns_to_remove, colnames(df)[which(grepl("_Log", colnames(df)) & grepl(a, colnames(df)))])
    }
    if (any(grepl("_log", colnames(df)) & grepl(a, colnames(df)))) {
      columns_to_remove <- c(columns_to_remove, colnames(df)[which(grepl("_log", colnames(df)) & grepl(a, colnames(df)))])
    }
    if (any(grepl("_meansc", colnames(df)) & grepl(a, colnames(df)))) {
      columns_to_remove <- c(columns_to_remove, colnames(df)[which(grepl("_meansc", colnames(df)) & grepl(a, colnames(df)))])
    }
    if (any(grepl("_autosc", colnames(df)) & grepl(a, colnames(df)))) {
      columns_to_remove <- c(columns_to_remove, colnames(df)[which(grepl("_autosc", colnames(df)) & grepl(a, colnames(df)))])
    }
    if (any(grepl("_paretosc", colnames(df)) & grepl(a, colnames(df)))) {
      columns_to_remove <- c(columns_to_remove, colnames(df)[which(grepl("_paretosc", colnames(df)) & grepl(a, colnames(df)))])
    }
    if (any(grepl("_rangesc", colnames(df)) & grepl(a, colnames(df)))) {
      columns_to_remove <- c(columns_to_remove, colnames(df)[which(grepl("_rangesc", colnames(df)) & grepl(a, colnames(df)))])
    }
  }
  
  columns_to_remove <- unique(columns_to_remove)
  
  columns_to_remove <- columns_to_remove[which(!columns_to_remove%in%columns_to_keep)]
  
  
  df_output <- df[,which(!colnames(df)%in%columns_to_remove)]
  
  colnames(df_output)[which(colnames(df_output)%in%columns_to_keep)] <- v
  
  return(df_output)
  
}

