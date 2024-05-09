

#' Check molecules in the feature table.
#'
#' Given a featINFO, a feature intensities table, and a table with a list of molecules with known mass, it looks for those molecule in the feature table.
#'
#' @param featmatrix a dataframe: first column the featname, each other column is a sample with feature intensities. Each row is a feature. It can be obtained with the get_feat_table_from_MSDial function of the present package.
#' @param featinfo a dataframe. A table with at least three columns, in this order: feature names, retention times, and masss-to-charge ratios. It can be obtained with the get_feat_info_from_MSDial function of the present package.
#' @param molecules_list a dataframe containing the molecules to search. The first column must contain the names of the molecule. ALso it should contain at least another column with the m/z to look for.
#' @param mz_to_search character vector of length 1. It is the column name of the molecules_list data-frame with the m/z values to look for (it not provided the third column will be used).
#' @param error numeric of length 1. Is is the tolerance for the search of m/z in Da or ppm (specified in the error_type argument)
#' @param error_type one of the following: "ppm", or "Da". It is referred to the value provided in the error argument.
#' @param check_rt logical. Should also the retention times be checked?
#' @param rt_to_search if check_rt is TRUE, it is the column name of the molecules_list data-frame with the rt values to look for. (it not provided the second column will be used)
#' @param rt_window numeric of length 1. It is the retention time window to consider centered to the known molecules to check the experimental ones. Please check that the unit (seconds or minutes) is the same in all tha tables.
#'
#'
#' @return A tibble with the matching feature intensities.
#'
#' @export
checkmolecules_in_feat_table <- function(featmatrix, featinfo,
                                         molecules_list, mz_to_search = NULL,
                                         error = 20, error_type = "ppm",
                                         check_rt = FALSE, rt_to_search = NULL, rt_window = 12) {
  
  if (!is.data.frame(featmatrix)) {stop("featmatrix must be a data-frame")}
  if (!is.data.frame(featinfo)) {stop("featinfo must be a data-frame")}
  if (any(is.na(pull(featmatrix, colnames(featmatrix)[1]))) | any(is.na(pull(featinfo, colnames(featinfo)[1])))) {stop("the first column of featmatrix and featinfo must be identical and contains the names of the features: it seems that there is a missing values somewhere..")}
  if (any(duplicated(pull(featmatrix, colnames(featmatrix)[1]))) | any(duplicated(pull(featinfo, colnames(featinfo)[1])))) stop("There are some duplicated names in the feature names")
  if (length(pull(featmatrix, colnames(featmatrix)[1])) != length(pull(featinfo, colnames(featinfo)[1]))) {stop("the first column of featmatrix and featinfo must be identical and contains the names of the features")}
  if (mean(pull(featmatrix, colnames(featmatrix)[1]) == pull(featinfo, colnames(featinfo)[1]))!=1) {stop("the first column of featmatrix and featinfo must be identical and contains the names of the features")}
  
  if (mean(map_lgl(featmatrix[,-1], is.numeric)) != 1) {stop("featmatrix, from the second column on, must contain only numeric values, i.e.: the intensities of features")}
  if (mean(map_lgl(featinfo[,2:3], is.numeric)) != 1) {stop("featinfo, in the second and third column, must contain only numeric values, i.e.: the retention times and the m/Z values")}
  if (any(is.na(pull(featinfo, colnames(featinfo)[3])))) {stop("featinfo, in the third column, must contain the m/Z values without missing values")}
  if (check_rt == TRUE) { if (any(is.na(pull(featinfo, colnames(featinfo)[2])))) {stop("featinfo, in the second column, must contain the rt values without missing values")}}
  
  if (!is.data.frame(molecules_list)) {stop("molecules_list must be a data-frame")}
  if(any(duplicated(pull(molecules_list, colnames(molecules_list)[1])))) {
    conteggio <- 1
    original_names <- pull(molecules_list, colnames(molecules_list)[1])
    repeat {
      conteggio <- conteggio +1
      for (i in which(duplicated(pull(molecules_list, colnames(molecules_list)[1])))) {
        molecules_list[i, colnames(molecules_list)[1]] <- paste0(original_names[i],"_", conteggio)
      }
      if(!any(duplicated(pull(molecules_list, colnames(molecules_list)[1]))))
        break
    }
  }
  
  if (is.null(mz_to_search)) {
    if (!is.numeric(pull(molecules_list, colnames(molecules_list)[3]))) {stop("Since you did not indicate anything in the mz_to_search argument, the third column of the molecules_list data-frame is considered, and it must contain only the numercic values of the m/z you want to search")} else if (any(is.na(pull(molecules_list, colnames(molecules_list)[3])))) {stop("Since you did not indicate anything in the mz_to_search argument, the third column of the molecules_list data-frame is considered, and it must contain only the values of the m/z you want to search, with no missing values!")} else {
      mz_to_search <- colnames(molecules_list)[3]
    }
  } else {
    if (!is.character(mz_to_search)) {
      stop('mz_to_search must be a character of length 1')
    } else if (length(mz_to_search) != 1) {
      stop('mz_to_search must be a character of length 1')
    } else if (is.na(mz_to_search)) {
      stop('mz_to_search must be a character of length 1, and not a missing value!')
    }
    if (!mz_to_search %in% colnames(molecules_list)) {stop("molecules_list must be the name of the column of the mz_to_search data-frame containing the m/z values")}
    if (!is.numeric(pull(molecules_list, mz_to_search))) {stop("The column of the molecules_list data-frame named as the mz_to_search argument must contain the numercic values of the m/z you want to search")} else if (any(is.na(pull(molecules_list, mz_to_search)))) {stop("The column of the molecules_list data-frame named as the mz_to_search argument must contain the values of the m/z you want to search, with no missing values!")}
  }
  
  if (length(error)!=1) {stop("error must be a numeric of length 1")} else if (!is.numeric(error)) {stop("error must be a numeric of length 1")} else if (is.na(error)) {stop("error must not be NA")}
  
  if (!is.character(error_type)) {
    stop('error_type must be a character of length 1 containing one of the following: "ppm" or "Da"')
  } else if (length(error_type) != 1) {
    stop('error_type must be a character of length 1 containing one of the following: "ppm" or "Da"')
  } else if (!error_type%in%c("ppm", "Da")) {
    stop('error_type must be a character of length 1 containing one of the following: "ppm" or "Da"')
  }
  
  if (length(check_rt)!=1) {stop("check_rt must be exclusively TRUE or FALSE")} else if (!is.logical(check_rt)) {stop("check_rt must be exclusively TRUE or FALSE")} else if (is.na(check_rt)) {stop("check_rt must be exclusively TRUE or FALSE")}
  
  if (check_rt == TRUE) {
    if (is.null(rt_to_search)) {
      if (!is.numeric(pull(molecules_list, colnames(molecules_list)[2]))) {stop("Since you did not indicate anything in the rt_to_search argument, the second column of the molecules_list data-frame is considered, and it must contain only the numercic values of the rt you want to search")} else if (any(is.na(pull(molecules_list, colnames(molecules_list)[2])))) {stop("Since you did not indicate anything in the rt_to_search argument, the second column of the molecules_list data-frame is considered, and it must contain only the values of the rt you want to search, with no missing values!")} else {
        rt_to_search <- colnames(molecules_list)[2]
      }
    } else {
      if (!is.character(rt_to_search)) {
        stop('rt_to_search must be a character of length 1')
      } else if (length(rt_to_search) != 1) {
        stop('rt_to_search must be a character of length 1')
      } else if (is.na(rt_to_search)) {
        stop('rt_to_search must be a character of length 1, and not a missing value!')
      }
      if (!rt_to_search %in% colnames(molecules_list)) {stop("rt_to_search must be the name of the column of the molecules_list data-frame containing the rt values")}
      if (!is.numeric(pull(molecules_list, rt_to_search))) {stop("The column of the molecules_list data-frame named as the rt_to_search argument must contain the numercic values of the rt you want to search")} else if (any(is.na(pull(molecules_list, rt_to_search)))) {stop("The column of the molecules_list data-frame named as the rt_to_search argument must contain the values of the rt you want to search, with no missing values!")}
    }
    
    if (length(rt_window)!=1) {stop("rt_window must be a numeric of length 1")} else if (!is.numeric(rt_window)) {stop("rt_window must be a numeric of length 1")} else if (is.na(rt_window)) {stop("rt_window must not be NA")}
  }
  
  final_table_one_row <-  matrix(NA, nrow = 1, ncol = length(colnames(featmatrix))+3)
  colnames(final_table_one_row) <-  c("molecule", colnames(featinfo)[1], colnames(featinfo)[2], colnames(featinfo)[3], colnames(featmatrix)[-1])
  final_table_one_row <-  as_tibble(final_table_one_row)
  
  final_table <-  matrix(NA, nrow = 0, ncol = length(colnames(featmatrix))+3)
  colnames(final_table) <-  c("molecule", colnames(featinfo)[1], colnames(featinfo)[2], colnames(featinfo)[3], colnames(featmatrix)[-1])
  final_table <-  as_tibble(final_table)
  
  
  if (error_type == "ppm") {
    for (i in 1:length(pull(molecules_list, colnames(molecules_list)[1]))) {
      minus_ppm <- -error
      plus_ppm <- error
      theor_mass <- pull(molecules_list, mz_to_search)[i]
      low_mass <- minus_ppm*theor_mass/1000000 + theor_mass
      high_mass <- plus_ppm*theor_mass/1000000 + theor_mass
      
      for (u in 1:length(pull(featinfo,colnames(featinfo)[3]))) {
        
        if (pull(featinfo,colnames(featinfo)[3])[u] > low_mass & pull(featinfo,colnames(featinfo)[3])[u] < high_mass) {
          
          new_row_tibble <- final_table_one_row
          new_row_tibble[1,1] <- pull(molecules_list, colnames(molecules_list)[1])[i]
          new_row_tibble[1,2] <- pull(featinfo,colnames(featinfo)[1])[u]
          new_row_tibble[1,3] <- pull(featinfo,colnames(featinfo)[2])[u]
          new_row_tibble[1,4] <- pull(featinfo,colnames(featinfo)[3])[u]
          new_row_tibble[1,5:(length(colnames(featmatrix))+3)] <- featmatrix[u,2:length(colnames(featmatrix))]
          
          final_table <- rbind(final_table, new_row_tibble)
        }
      }
      
      if (!pull(molecules_list, colnames(molecules_list)[1])[i] %in% final_table$molecule) {
        new_row_tibble <- final_table_one_row
        new_row_tibble[1,1] <- pull(molecules_list, colnames(molecules_list)[1])[i]
        new_row_tibble[1,2] <- "NOT_FOUND"
        
        final_table <- rbind(final_table, new_row_tibble)
      }
    }
  } else if (error_type == "Da") {
    for (i in 1:length(pull(molecules_list, colnames(molecules_list)[1]))) {
      theor_mass <- pull(molecules_list, mz_to_search)[i]
      low_mass <- theor_mass - error
      high_mass <- theor_mass + error
      
      for (u in 1:length(pull(featinfo,colnames(featinfo)[3]))) {
        
        if (pull(featinfo,colnames(featinfo)[3])[u] > low_mass & pull(featinfo,colnames(featinfo)[3])[u] < high_mass) {
          
          new_row_tibble <- final_table_one_row
          new_row_tibble[1,1] <- pull(molecules_list, colnames(molecules_list)[1])[i]
          new_row_tibble[1,2] <- pull(featinfo,colnames(featinfo)[1])[u]
          new_row_tibble[1,3] <- pull(featinfo,colnames(featinfo)[2])[u]
          new_row_tibble[1,4] <- pull(featinfo,colnames(featinfo)[3])[u]
          new_row_tibble[1,5:(length(colnames(featmatrix))+3)] <- featmatrix[u,2:length(colnames(featmatrix))]
          
          final_table <- rbind(final_table, new_row_tibble)
        }
      }
      
      if (!pull(molecules_list, colnames(molecules_list)[1])[i] %in% final_table$molecule) {
        new_row_tibble <- final_table_one_row
        new_row_tibble[1,1] <- pull(molecules_list, colnames(molecules_list)[1])[i]
        new_row_tibble[1,2] <- "NOT_FOUND"
        
        final_table <- rbind(final_table, new_row_tibble)
      }
    }
  }
  
  
  if (check_rt == TRUE) {
    
    final_table <- add_column(final_table,
                              rt_confirmed = as.logical(rep(NA, length(pull(final_table, colnames(final_table)[1])))),
                              .after = colnames(final_table)[2])
    
    for(i in 1:length(pull(final_table, colnames(final_table)[1]))) {
      if (pull(final_table, colnames(final_table)[2])[i] != "NOT_FOUND") {
        rt_lower_limit <- pull(molecules_list, rt_to_search)[which(pull(molecules_list, colnames(molecules_list)[1])==pull(final_table, colnames(final_table)[1])[i])] - (rt_window/2)
        rt_upper_limit <- pull(molecules_list, rt_to_search)[which(pull(molecules_list, colnames(molecules_list)[1])==pull(final_table, colnames(final_table)[1])[i])] + (rt_window/2)
        
        if(pull(final_table, colnames(final_table)[4])[i] <= rt_upper_limit & pull(final_table, colnames(final_table)[4])[i] >= rt_lower_limit) {
          final_table$rt_confirmed[i] <- TRUE
        } else {
          final_table$rt_confirmed[i] <- FALSE
        }
      }
    }
  }
  return(final_table)
}

