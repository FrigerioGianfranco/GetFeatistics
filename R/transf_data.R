

#' Transform the data
#'
#' Given a dataframe and a set of numerical variables of that dataframe, it can replace missing values, log-transform, and/or scale the data.
#'
#' @param df a dataframe.
#' @param v a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values.
#' @param missing_replace logical. Should missing value be replaced? if TRUE, each missing value will be replaced with the minimum value of that variable, modified with the operation indicated in the missing_repl_type and missing_repl_value arguments. If a variable have all missing values, it will not processed further.
#' @param missing_repl_type one of the following: "divide", "multiply", or "exponentiate". If missing_replace is TRUE, each missing values will be replaced by the minimum value of that variable divided by ("divide"), multiplied by ("multiply"), or raised to power of ("exponentiate") the value indicated in the missing_repl_value argument. Suggestion: if you want to replace missing values with the squared root of the minimum value, select here "exponentiate" and in missing_repl_value put 1/2 (raising to the power of 1/2 is equal to applying the square root).
#' @param missing_repl_value numerical of length 1. If missing_replace is TRUE, this is the value of the operation indicated in missing_repl_type that will be applied to the the minimum values, the result of which will replace the missing values.
#' @param log_transf logical. Should log-transformation be applied to each data?
#' @param log_base numerical of length 1. If log_transf is TRUE, this is the base of the logarithm applied to transform the data.
#' @param scaling logical. Should the data be scaled? If TRUE, set the scaling_type argument properly.
#' @param scaling_type one of the following: "mean_scale", "auto_scale", "pareto_scale", or "range_scale". If scaling is TRUE, for each variable, data will be subtracted by the mean ("mean_scale"), subtracted by the mean and divided by the standard deviation ("auto_scale"), subtracted by the mean and divided by the squared root of the standard deviation ("pareto_scale"), or subtracted by the mean and divided by the difference between the maximum and the minimum values ("range_scale").
#' @param vect_names_transf logical. If TRUE, it also create in the global environment character vectors containing the names of the variables transformed. It is useful to pass these names to the v arguments of furhter statistical functions of this package.
#' @param name_vect_names character of length 1. If vect_names_transf is TRUE, specify here the main name of the character vectors containing the names of the transformed variables.
#'
#' @return df with new columns containing the transformed values. In particular: the new columns that end in "_mr" refer to missing values replaced, "_ln", "_Log", or "_log" to log-transformed, "_meansc", "_autosc", "_paretosc", or "_rangesc" to the scaling. As example, the new columns that end with "_mr_ln_paretosc" refer to data missing value replaced, natural-logarithm transformed, and pareto scaled. Check also the name of those column by setting vect_names_transf as TRUE and check in the global environment the objects whose names start with what you set in name_vect_names.
#'
#' @export
transf_data <- function(df, v, missing_replace = TRUE, missing_repl_type = "divide", missing_repl_value = 5 , log_transf = TRUE, log_base = exp(1), scaling = TRUE, scaling_type = "pareto_scale", vect_names_transf = FALSE, name_vect_names = "vars_ready") {
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  if (mean(v %in% colnames(df)) != 1) {stop("v must be a vector containing the names of the coloumns of df that you want to apply the descriptive statistics")}
  if (mean(map_lgl(select(df, all_of(v)), is.numeric)) != 1) {stop("all coloumn passed in v must be numeric!")}
  
  
  if (any(check_if_fix_names_needed(v))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                         paste0("'", paste0(v[which(check_if_fix_names_needed(v))], collapse = "', '"), "'")))}
  
  
  logic_checkzero <- logical(length = length(v))
  for (ii in 1:length(v)) {
    if (any(pull(df, v[ii])[which(!is.na(pull(df, v[ii])))] == 0)) {
      logic_checkzero[ii] <- TRUE
    } else {
      logic_checkzero[ii] <- FALSE
    }
  }
  if (any(logic_checkzero)) {warning("There are some zeros in the data, isn't it better to replace them with NAs?")}
  
  
  if (length(missing_replace)!=1) {stop("missing_replace must be exclusively TRUE or FALSE")}
  if (!is.logical(missing_replace)) {stop("missing_replace must be exclusively TRUE or FALSE")}
  if (is.na(missing_replace)) {stop("missing_replace must be exclusively TRUE or FALSE")}
  
  if (missing_replace) {
    if (length(missing_repl_type) != 1) {stop('missing_repl_type must be a character of length 1 containing one of the following: "divide", "multiply", "exponentiate"')}
    if (!is.character(missing_repl_type)) {stop('missing_repl_type must be a character of length 1 containing one of the following: "divide", "multiply", "exponentiate"')}
    if (!missing_repl_type%in%c("divide", "multiply", "exponentiate")) {stop('missing_repl_type must be a character of length 1 containing one of the following: "divide", "multiply", "exponentiate"')}
    
    if (length(missing_repl_value) != 1) {stop("missing_repl_value must be a numeric of length 1")}
    if (is.na(missing_repl_value)) {stop("missing_repl_value must be a numeric of length 1, and not a missing value!")}
    if (!is.numeric(missing_repl_value)) {stop("missing_repl_value must be a numeric of length 1")}
  }
  
  if (length(log_transf)!=1) {stop("log_transf must be exclusively TRUE or FALSE")}
  if (!is.logical(log_transf)) {stop("log_transf must be exclusively TRUE or FALSE")}
  if (is.na(log_transf)) {stop("log_transf must be exclusively TRUE or FALSE")}
  
  if (log_transf) {
    if (length(log_base) != 1) {stop("log_base must be a numeric of length 1")}
    if (is.na(log_base)) {stop("log_base must be a numeric of length 1, and not a missing value!")}
    if (!is.numeric(log_base)) {stop("log_base must be a numeric of length 1")}
    
    logic_checkzeroless <- logical(length = length(v))
    for (ii in 1:length(v)) {
      if (any(pull(df, v[ii])[which(!is.na(pull(df, v[ii])))] <= 0)) {
        logic_checkzeroless[ii] <- TRUE
      } else {
        logic_checkzeroless[ii] <- FALSE
      }
    }
    if (any(logic_checkzeroless)) {warning("The data contains some values that are zero or less. Computing the logharitm will give NaN")}
  }
  
  if (length(scaling)!=1) {stop("scaling must be exclusively TRUE or FALSE")}
  if (!is.logical(scaling)) {stop("scaling must be exclusively TRUE or FALSE")}
  if (is.na(scaling)) {stop("scaling must be exclusively TRUE or FALSE")}
  
  if (scaling) {
    if (!is.character(scaling_type)) {stop('scaling_type must be a character of length 1 containing one of the following: "mean_scale", "auto_scale", "pareto_scale", or "range_scale"')}
    if (length(scaling_type) != 1) {stop('scaling_type must be a character of length 1 containing one of the following: ""mean_scale", "auto_scale", "pareto_scale", or "range_scale"')}
    if (!scaling_type%in%c("mean_scale", "auto_scale", "pareto_scale", "range_scale")) {stop('scaling_type must be a character of length 1 containing one of the following: "mean_scale", "auto_scale", "pareto_scale", or "range_scale"')}
  }
  
  if (length(vect_names_transf)!=1) {stop("vect_names_transf must be exclusively TRUE or FALSE")}
  if (!is.logical(vect_names_transf)) {stop("vect_names_transf must be exclusively TRUE or FALSE")}
  if (is.na(vect_names_transf)) {stop("vect_names_transf must be exclusively TRUE or FALSE")}
  
  if (vect_names_transf) {
    if (length(name_vect_names) != 1) {stop('name_vect_names must be a character of length 1')}
    if (is.na(name_vect_names)) {stop('name_vect_names must be a character of length 1, and not a missing value!')}
    if (!is.character(name_vect_names)) {stop('name_vect_names must be a character of length 1')}
  }
  
  
  tranf_df <- df
  
  var_to_normalise <- v
  
  
  if (missing_replace == TRUE) {
    if (missing_repl_type == "divide") {
      
      var_to_normalise_nofullNA <- character()
      
      for(a in var_to_normalise) {
        if(mean(is.na(pull(df, a))) != 1) {
          var_to_normalise_nofullNA <- c(var_to_normalise_nofullNA, a)
        }
      }
      
      
      replacing_function <- function(the_numeric_vector) {
        
        the_min_divided <- min(the_numeric_vector, na.rm = TRUE)/missing_repl_value
        the_output <- the_numeric_vector
        
        for (i in which(is.na(the_numeric_vector))) {
          the_output[i] <- the_min_divided
        }
        return(the_output)
      }
      
      
      tranf_df <- mutate_at(tranf_df, var_to_normalise_nofullNA, list(mr = replacing_function))
      
      col_names_missing_replaced <- paste0(var_to_normalise_nofullNA, "_mr")
      
    } else if (missing_repl_type == "multiply") {
      var_to_normalise_nofullNA <- character()
      
      for(a in var_to_normalise) {
        if(mean(is.na(pull(df, a))) != 1) {
          var_to_normalise_nofullNA <- c(var_to_normalise_nofullNA, a)
        }
      }
      
      
      replacing_function <- function(the_numeric_vector) {
        
        the_min_multiplied <- min(the_numeric_vector, na.rm = TRUE)*missing_repl_value
        the_output <- the_numeric_vector
        
        for (i in which(is.na(the_numeric_vector))) {
          the_output[i] <- the_min_multiplied
        }
        return(the_output)
      }
      
      
      tranf_df <- mutate_at(tranf_df, var_to_normalise_nofullNA, list(mr = replacing_function))
      
      col_names_missing_replaced <- paste0(var_to_normalise_nofullNA, "_mr")
      
    } else if (missing_repl_type == "exponentiate") {
      var_to_normalise_nofullNA <- character()
      
      for(a in var_to_normalise) {
        if(mean(is.na(pull(df, a))) != 1) {
          var_to_normalise_nofullNA <- c(var_to_normalise_nofullNA, a)
        }
      }
      
      
      replacing_function <- function(the_numeric_vector) {
        
        the_min_exponentiated <- min(the_numeric_vector, na.rm = TRUE)^missing_repl_value
        the_output <- the_numeric_vector
        
        for (i in which(is.na(the_numeric_vector))) {
          the_output[i] <- the_min_exponentiated
        }
        return(the_output)
      }
      
      
      tranf_df <- mutate_at(tranf_df, var_to_normalise_nofullNA, list(mr = replacing_function))
      
      col_names_missing_replaced <- paste0(var_to_normalise_nofullNA, "_mr")
    }
    
    if (log_transf == TRUE) {
      tranf_df <- mutate_at(tranf_df, col_names_missing_replaced, list(TYPE_OF_LOG = ~log(., base = log_base)))
      
      if (scaling == TRUE) {
        if (scaling_type == "mean_scale") {tranf_df <- mutate_at(tranf_df, paste0(col_names_missing_replaced, "_TYPE_OF_LOG"), list(meansc = ~mean_scale(., na.rm = TRUE)))}
        if (scaling_type == "auto_scale") {tranf_df <- mutate_at(tranf_df, paste0(col_names_missing_replaced, "_TYPE_OF_LOG"), list(autosc = ~auto_scale(., na.rm = TRUE)))}
        if (scaling_type == "pareto_scale") {tranf_df <- mutate_at(tranf_df, paste0(col_names_missing_replaced, "_TYPE_OF_LOG"), list(paretosc = ~pareto_scale(., na.rm = TRUE)))}
        if (scaling_type == "range_scale") {tranf_df <- mutate_at(tranf_df, paste0(col_names_missing_replaced, "_TYPE_OF_LOG"), list(rangesc = ~range_scale(., na.rm = TRUE)))}
      }
      
      colnames(tranf_df) <- str_replace_all(string = colnames(tranf_df), pattern = "_TYPE_OF_LOG", replacement = ifelse(log_base == exp(1), "_ln",
                                                                                                                        ifelse(log_base == 10, "_Log",
                                                                                                                               paste0("_log", log_base))))
      
    } else {
      if (scaling == TRUE) {
        if (scaling_type == "mean_scale") {tranf_df <- mutate_at(tranf_df, col_names_missing_replaced, list(meansc = mean_scale))}
        if (scaling_type == "auto_scale") {tranf_df <- mutate_at(tranf_df, col_names_missing_replaced, list(autosc = auto_scale))}
        if (scaling_type == "pareto_scale") {tranf_df <- mutate_at(tranf_df, col_names_missing_replaced, list(paretosc = pareto_scale))}
        if (scaling_type == "range_scale") {tranf_df <- mutate_at(tranf_df, col_names_missing_replaced, list(rangesc = range_scale))}
      }
    }
  } else {
    if (log_transf == TRUE) {
      tranf_df <- mutate_at(tranf_df, var_to_normalise, list(TYPE_OF_LOG = ~log(., base = log_base)))
      
      if (scaling == TRUE) {
        if (scaling_type == "mean_scale") {tranf_df <- mutate_at(tranf_df, paste0(var_to_normalise, "_TYPE_OF_LOG"), list(meansc = ~mean_scale(., na.rm = TRUE)))}
        if (scaling_type == "auto_scale") {tranf_df <- mutate_at(tranf_df, paste0(var_to_normalise, "_TYPE_OF_LOG"), list(autosc = ~auto_scale(., na.rm = TRUE)))}
        if (scaling_type == "pareto_scale") {tranf_df <- mutate_at(tranf_df, paste0(var_to_normalise, "_TYPE_OF_LOG"), list(paretosc = ~pareto_scale(., na.rm = TRUE)))}
        if (scaling_type == "range_scale") {tranf_df <- mutate_at(tranf_df, paste0(var_to_normalise, "_TYPE_OF_LOG"), list(rangesc = ~range_scale(., na.rm = TRUE)))}
      }
      
      colnames(tranf_df) <- str_replace_all(string = colnames(tranf_df), pattern = "_TYPE_OF_LOG", replacement = ifelse(log_base == exp(1), "_ln",
                                                                                                                        ifelse(log_base == 10, "_Log",
                                                                                                                               paste0("_log", log_base))))
      
    } else {
      if (scaling == TRUE) {
        if (scaling_type == "mean_scale") {tranf_df <- mutate_at(tranf_df, var_to_normalise, list(meansc = mean_scale))}
        if (scaling_type == "auto_scale") {tranf_df <- mutate_at(tranf_df, var_to_normalise, list(autosc = auto_scale))}
        if (scaling_type == "pareto_scale") {tranf_df <- mutate_at(tranf_df, var_to_normalise, list(paretosc = pareto_scale))}
        if (scaling_type == "range_scale") {tranf_df <- mutate_at(tranf_df, var_to_normalise, list(rangesc = range_scale))}
      }
    }
  }
  
  
  if (vect_names_transf == TRUE) {
    
    if (missing_replace == TRUE) {
      
      assign(paste0(name_vect_names, "_mr"),
             col_names_missing_replaced,
             envir = .GlobalEnv)
      
      if (log_transf == TRUE) {
        assign(paste0(name_vect_names, "_mr", ifelse(log_base == exp(1), "_ln", ifelse(log_base == 10, "_Log", paste0("_log", log_base)))),
               paste0(col_names_missing_replaced,  ifelse(log_base == exp(1), "_ln", ifelse(log_base == 10, "_Log", paste0("_log", log_base)))),
               envir = .GlobalEnv)
      }
      
      if (log_transf == FALSE & scaling == TRUE) {
        assign(paste0(name_vect_names, "_mr", ifelse(scaling_type == "mean_scale", "_meansc", ifelse(scaling_type == "auto_scale", "_autosc", ifelse(scaling_type == "pareto_scale", "_paretosc" , ifelse(scaling_type == "range_scale", "_rangesc", "_"))))),
               paste0(col_names_missing_replaced, ifelse(scaling_type == "mean_scale", "_meansc", ifelse(scaling_type == "auto_scale", "_autosc", ifelse(scaling_type == "pareto_scale", "_paretosc" , ifelse(scaling_type == "range_scale", "_rangesc", "_"))))),
               envir = .GlobalEnv)
        
      }
      
      if (log_transf == TRUE & scaling == TRUE) {
        
        assign(paste0(name_vect_names, "_mr", ifelse(log_base == exp(1), "_ln", ifelse(log_base == 10, "_Log", paste0("_log", log_base))), ifelse(scaling_type == "mean_scale", "_meansc", ifelse(scaling_type == "auto_scale", "_autosc", ifelse(scaling_type == "pareto_scale", "_paretosc" , ifelse(scaling_type == "range_scale", "_rangesc", "_"))))),
               paste0(col_names_missing_replaced, ifelse(log_base == exp(1), "_ln", ifelse(log_base == 10, "_Log", paste0("_log", log_base))), ifelse(scaling_type == "mean_scale", "_meansc", ifelse(scaling_type == "auto_scale", "_autosc", ifelse(scaling_type == "pareto_scale", "_paretosc" , ifelse(scaling_type == "range_scale", "_rangesc", "_"))))),
               envir = .GlobalEnv)
      }
    } else {
      if (log_transf == TRUE) {
        assign(paste0(name_vect_names, ifelse(log_base == exp(1), "_ln", ifelse(log_base == 10, "_Log", paste0("_log", log_base)))),
               paste0(var_to_normalise, ifelse(log_base == exp(1), "_ln", ifelse(log_base == 10, "_Log", paste0("_log", log_base)))),
               envir = .GlobalEnv)
      }
      
      if (log_transf == FALSE & scaling == TRUE) {
        assign(paste0(name_vect_names, ifelse(scaling_type == "mean_scale", "_meansc", ifelse(scaling_type == "auto_scale", "_autosc", ifelse(scaling_type == "pareto_scale", "_paretosc" , ifelse(scaling_type == "range_scale", "_rangesc", "_"))))),
               paste0(var_to_normalise, ifelse(scaling_type == "mean_scale", "_meansc", ifelse(scaling_type == "auto_scale", "_autosc", ifelse(scaling_type == "pareto_scale", "_paretosc" , ifelse(scaling_type == "range_scale", "_rangesc", "_"))))),
               envir = .GlobalEnv)
      }
      
      if (log_transf == TRUE & scaling == TRUE) {
        
        assign(paste0(name_vect_names, ifelse(log_base == exp(1), "_ln", ifelse(log_base == 10, "_Log", paste0("_log", log_base))), ifelse(scaling_type == "mean_scale", "_meansc", ifelse(scaling_type == "auto_scale", "_autosc", ifelse(scaling_type == "pareto_scale", "_paretosc" , ifelse(scaling_type == "range_scale", "_rangesc", "_"))))),
               paste0(var_to_normalise, ifelse(log_base == exp(1), "_ln", ifelse(log_base == 10, "_Log", paste0("_log", log_base))), ifelse(scaling_type == "mean_scale", "_meansc", ifelse(scaling_type == "auto_scale", "_autosc", ifelse(scaling_type == "pareto_scale", "_paretosc" , ifelse(scaling_type == "range_scale", "_rangesc", "_"))))),
               envir = .GlobalEnv)
      }
    }
  }
  
  return(tranf_df)
  
}

