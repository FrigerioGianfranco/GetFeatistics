

#' Elaborating results of targeted analyses
#'
#' Given a tables containing data from targeted analyses, including a calibration curve, it calculates the concentrations in unknown samples. and moooore
#'
#' @param data_intensity a dataframe containing the intensity area of each peak, with each row corresponding to a sample, and each column to an analysed molecule. The first column must contain the name of the samples, all other columns the intensities.
#' @param data_legend a dataframe in which the first column must be equal to the first column of data_intensity; the second column contains the samply_type, i.e.: "blank", "curve", "qc", or "unknown"; the following columns contain, for each targeted compound, the actual values for the curve and qc.
#' @param compound_legend either NULL or a dataframe with up to four columns: the first column contains the targeted molecules; the second column contains the relative matched internal standard (or NAs); the third column contains the weighting factor, i.e. either "none" (or NA) or the operation to perform to X and/or Y for calculating the weighting factors for the linear models, X and Y must be in uppercase (examples: "1/X", "1/(X^2)", "1/Y", "1/sqrt(Y)"); the fourth column contains the unit of mesure of the concentrations (only for reference and for later visualisation, not used for any calculation).
#'
#' @return a list with 5 tibbles and 1 list: results_concentrations, the table with the results; results_accuracy, the table with the accuracies (expressed as % of the theoretical value); cv_internal_standards, the table with the variation cofficients (%) of the internal standards; compound_legend, the same table provided in the argument compound_legend; summary_regression_models, a table with the slopes, intercepts, r.squared, and adj.r.squared; regression_models, a list containing all the computed linear models.
#'
#' @export
get_targeted_elaboration <- function(data_intensity, data_legend, compound_legend = NULL) {
  if (!is.data.frame(data_intensity)) {stop("data_intensity must be a data frame!")}
  if (length(colnames(data_intensity))<2) {stop("data_intensity must have 2 or more columns!")}
  if (!is.character(pull(data_intensity, 1))) {stop("the first column of data_intensity must be characters")}
  if (!all(map_lgl(data_intensity[,-1], is.numeric))) {stop("besides the first one, all other columns of data_intensity must contain numeric data")}
  
  if (!is.data.frame(data_legend)) {stop("data_legend must be a data frame!")}
  if (length(colnames(data_legend))<3) {stop("data_legend must have 3 or more columns!")}
  if (length(pull(data_legend, 1)) != length(pull(data_intensity, 1))) {stop("data_intensity and data_legend must have the same number of rows!")}
  if (!all(pull(data_intensity, 1) == pull(data_legend, 1))) {stop("the first column of data_intensity and data_legend must be identical!")}
  
  samples_names <- pull(data_intensity, 1)
  
  if (any(is.na(samples_names))) {stop("please, no missing values in the first column of data_intensity and data_legend")}
  
  sample_names_fixed <- samples_names %>% fix_duplicated()
  data_intensity[, 1] <- sample_names_fixed
  data_legend[, 1] <- sample_names_fixed
  
  
  colnames(data_intensity) <- colnames(data_intensity) %>% fix_names()
  colnames(data_legend) <- colnames(data_legend) %>% fix_names()
  
  if (any(duplicated(colnames(data_intensity)))) {
    stop(paste0("The column names of data_intensity must not contain duplicated. ",
                ifelse(any(check_if_fix_names_needed(colnames(data_intensity))), "Please also note that some names have been fixed. ", ""),
                "The column names not unique are: ",
                paste0(unique(colnames(data_intensity)[which(duplicated(colnames(data_intensity)))]), collapse = ", ")))
  }
  if (any(duplicated(colnames(data_legend)))) {
    stop(paste0("The column names of data_legend must not contain duplicated. ",
                ifelse(any(check_if_fix_names_needed(colnames(data_legend))), "Please also note that some names have been fixed. ", ""),
                "The column names not unique are: ",
                paste0(unique(colnames(data_legend)[which(duplicated(colnames(data_legend)))]), collapse = ", ")))
  }
  
  
  
  if (!("CURVE" %in% toupper(pull(data_legend, 2)))) { stop('in the second column of data_legend, there must be some "curve" elements')}
  if (!all(map_lgl(data_legend[,-c(1,2)], is.numeric))) {stop("besides the first one and second one, all other columns of data_legend must contain numeric data")}
  if (!(all(colnames(data_legend)[-c(1,2)] %in% colnames(data_intensity)[-1]))) {stop("besides the first one and second one, all other column names of data_legend must be also column names of data_intensity")}
  
  
  if (is.null(compound_legend)) {
    
    data_intensity_touse <- select(data_intensity, 1, all_of(colnames(data_intensity)[which(colnames(data_intensity) %in% colnames(data_legend))]))
    
    weight_curve <- rep("none", length(colnames(data_intensity_touse)[-1]))
    names(weight_curve) <- colnames(data_intensity_touse)[-1]
    
  } else {
    if (!is.data.frame(compound_legend)) {stop("compound_legend must be NULL or a data frame!")}
    
    compound_legend[, 1] <- fix_names(pull(compound_legend, 1))
    
    if (any(duplicated(pull(compound_legend, 1)))) {stop("The first column of compound_legend must not contain duplicates")}
    if (!(all(map_lgl(pull(compound_legend, 1), ~(is.na(.) | .%in% colnames(data_intensity)))))) {stop("The first column of compound_legend must contain compounds that are column names in data_intensity")}
    
    if (length(colnames(compound_legend)) >= 2) {
      
      compound_legend[, 2] <- fix_names(pull(compound_legend, 2))
      if (!(all(map_lgl(pull(compound_legend, 2), ~(is.na(.) | .%in% colnames(data_intensity)))))) {stop("The second column of compound_legend must contain compounds that are column names in data_intensity")}
      
      data_intensity_touse <- select(data_intensity, 1, all_of(colnames(data_intensity)[which(colnames(data_intensity) %in% colnames(data_legend))]))
      
      for (a in colnames(data_intensity_touse)[-1]) {
        if (a %in% pull(compound_legend, 1)) {
          this_matched_IS <- pull(compound_legend, 2)[which(pull(compound_legend, 1) == a)]
          if (!is.na(this_matched_IS)) {
            if (any(pull(data_intensity, this_matched_IS) == 0)) {warning(paste0("There are some zeros in the intensities of the internal standards ", this_matched_IS, ", so NaN will be introduced"))}
            for (i in 1:length(pull(data_intensity_touse,1))) {
              data_intensity_touse[i, a] <- pull(data_intensity, a)[i] / pull(data_intensity, this_matched_IS)[i]
            }
          }
        }
      }
    }
    
    weight_curve <- rep("none", length(colnames(data_intensity_touse)[-1]))
    names(weight_curve) <- colnames(data_intensity_touse)[-1]
    
    if (length(compound_legend)>=3) {
      
      if (!all(is.na(pull(compound_legend, 3)))) {
        if (!is.character(pull(compound_legend, 3))) {stop('The thrid column of compound_legend, if present, must be a character vector with the the weighting factor, i.e. either "none" (or NA) or the operation to perform to X and/or Y for calculating the weighting factors for the linear models, X and Y must be in uppercase (examples: "1/X", "1/(X^2)", "1/Y", "1/sqrt(Y)")')}
        
        for (u in which(!is.na(pull(compound_legend, 3)))) {
          if (pull(compound_legend, 1)[u] %in% names(weight_curve)) {
            weight_curve[which(names(weight_curve)==pull(compound_legend, 1)[u])] <- pull(compound_legend, 3)[u]
          }
        }
      }
    }
  }
  
  
  
  
  data_intensity_touse_curve <- data_intensity_touse[which(toupper(pull(data_legend, 2)) == "CURVE"), ]
  
  data_legend_curve <- data_legend[which(toupper(pull(data_legend, 2)) == "CURVE"), ]
  
  
  data_output_results <- bind_cols(data_legend[, 1:2],
                                   as_tibble(matrix(data = as.numeric(rep(NA, length(colnames(data_intensity_touse)[-1])*length(pull(data_intensity_touse, 1)))),
                                                    ncol = length(colnames(data_intensity_touse)[-1]),
                                                    nrow = length(pull(data_intensity_touse, 1)),
                                                    dimnames = list(NULL,
                                                                    colnames(data_intensity_touse)[-1]))))
  
  data_output_accuracy <- data_output_results
  
  
  list_models <- vector("list", length(colnames(data_intensity_touse)[-1]))
  names(list_models) <- colnames(data_intensity_touse)[-1]
  
  tab_summary_lm <- tibble(parameter = c("slope", "intercept", "r.squared", "adj.r.squared"))
  tab_summary_lm[, colnames(data_intensity_touse)[-1]] <- as.numeric(NA)
  
  
  cv_is <- tibble(sample_type = c("all", unique(pull(data_legend, 2))))
  colnames(cv_is)[1] <- colnames(data_legend)[2]
  
  if (!is.null(compound_legend)) {
    if (length(colnames(compound_legend)) >=2) {
      cv_is[, unique(pull(compound_legend, 2)[which(!is.na(pull(compound_legend, 2)))])] <- as.numeric(NA)
    }
  }
  
  
  for (a in colnames(data_intensity_touse)[-1]) {
    
    Y <- pull(data_intensity_touse_curve, a)
    X <- pull(data_legend_curve, a)
    
    this_weighting <- weight_curve[which(names(weight_curve) == a)]
    
    if (toupper(this_weighting) == "NONE") {
      this_MODEL <- lm(formula = Y ~ X)
    } else {
      weightings <- eval(parse(text = this_weighting))
      
      if (!any((length(weightings) ==1) | (length(weightings) == length(X)) | (length(weightings) == length(Y)))) stop(paste("The length of the created weightings factor is not 1 nor the lenght of the obesrvation of the calibration curve for ", a, ". Please check as the weighinted linear regression cannot be performed with these data and with this weighting factor"))
      if (!is.numeric(weightings)) stop(paste0("The created weightings are not numeric for ", a, " Please check as the weighinted linear regression cannot be performed with these data and with this weighting factor"))
      
      if (any(is.na(weightings))) {stop(paste0("Some NAs have been introduced in the weightings for ", a, ". Therefore, the weighinted linear regression cannot be performed with these data and with this weighting factor"))}
      if (any(is.infinite(weightings))) {stop(paste0("Some Inf have been introduced in the weightings for ", a, ". This happens, as example, if you divide for zero. The weighinted linear regression cannot be performed with these data and with this weighting factor"))}
      if (any(is.nan(weightings))) {stop(paste0("Some NaN have been introduced in the weightings for ", a, ". This happens, as example, if you calculate the root of a negative number. The weighinted linear regression cannot be performed with these data and with this weighting factor"))}
      if (any(weightings<0)) {stop(paste0("Some negative numbers have been introduced in the weightings for ", a, ". Therefore, the weighinted linear regression cannot be performed with these data and with this weighting factor"))}
      if (any(weightings==0)) {warning(paste0("Some zeros have been introduced in the weightings for ", a, ". The weighinted linear regression is still performed"))}
      
      this_MODEL <- lm(formula = Y ~ X, weights = weightings)
    }
    
    list_models[[a]] <- this_MODEL
    
    
    for (i in 1:length(pull(data_intensity_touse, 1))) {
      
      data_output_results[i, a] <- (pull(data_intensity_touse, a)[i]-this_MODEL[["coefficients"]][["(Intercept)"]])/this_MODEL[["coefficients"]][["X"]]
      
      if (!is.na(pull(data_legend, a)[i])) {
        data_output_accuracy[i, a] <- (data_output_results[i, a]/data_legend[i, a])*100
      }
    }
    
    tab_summary_lm[1, a] <- this_MODEL[["coefficients"]][["X"]]
    tab_summary_lm[2, a] <- this_MODEL[["coefficients"]][["(Intercept)"]]
    tab_summary_lm[3, a] <- summary(this_MODEL)[["r.squared"]]
    tab_summary_lm[4, a] <- summary(this_MODEL)[["adj.r.squared"]]
    
    
  }
  
  if (!is.null(compound_legend)) {
    if (length(colnames(compound_legend))>=2) {
      for (IS in unique(pull(compound_legend, 2)[which(!is.na(pull(compound_legend, 2)))])) {
        
        cv_is[1, IS] <- (sd(pull(data_intensity, IS))/mean(pull(data_intensity, IS)))*100
        
        for (u in unique(pull(data_legend, 2))) {
          cv_is[which(pull(cv_is, 1) == u), IS] <- (sd(pull(data_intensity, IS)[which(pull(data_legend, 2) == u)])/mean(pull(data_intensity, IS)[which(pull(data_legend, 2) == u)]))*100
        }
      }
    }
  }
  
  
  final_list <- list(results_concentrations = data_output_results,
                     results_accuracy = data_output_accuracy,
                     cv_internal_standards = cv_is,
                     compound_legend = as_tibble(compound_legend),
                     summary_regression_models = tab_summary_lm,
                     regression_models = list_models)
  
  
  return(final_list)
}


