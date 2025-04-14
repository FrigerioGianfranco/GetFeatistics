

#' Check molecules in the feature table.
#'
#' Given a featINFO, a feature intensities table, and a table with a list of molecules with known mass, it looks for those molecule in the feature table.
#'
#' @param featmatrix a dataframe: first column the featname, each other column is a sample with feature intensities. Each row is a feature. (It can be obtained with the get_feat_table_from_MSDial or get_feat_table_from_patRoon function).
#' @param featinfo a dataframe. A table with at least three columns, in this order: feature names, retention times, and mass-to-charge ratios. (It can be obtained with the get_feat_info_from_MSDial or get_feat_info_from_patRoon function).
#' @param molecules_list a dataframe containing the molecules to search. The first column must contain the names of the molecule. Also, it should contain at least another column with the m/z to look for.
#' @param mz_to_search character vector of length 1. It is the column name of the molecules_list data-frame with the m/z values to look for (If not provided the third column will be used).
#' @param error numeric of length 1. Is is the tolerance for the search of m/z in Da or ppm (specified in the error_type argument)
#' @param error_type one of the following: "ppm", or "Da". It is referred to the value provided in the error argument.
#' @param check_rt logical. Should also the retention times be checked?
#' @param rt_to_search if check_rt is TRUE, it is the column name of the molecules_list data-frame with the rt values to look for. (If not provided the second column will be used)
#' @param rt_window numeric of length 1. It is the retention time window to consider centered to the known molecules to check the experimental ones. Please check that the unit (seconds or minutes) is the same in all the tables.
#' @param return_as_featinfo_lev1 logical. If TRUE, and if also check_rt is TRUE, the function will return the featinfo with the found molecules as level 1 in the AnnoLevel column.
#'
#'
#' @return A tibble with the matching feature intensities. If return_as_featinfo_lev1 is TRUE, the output is instead the featinfo with the compounds found as level 1 in the AnnoLevel column.
#'
#' @export
checkmolecules_in_feat_table <- function(featmatrix, featinfo,
                                         molecules_list, mz_to_search = NULL,
                                         error = 20, error_type = "ppm",
                                         check_rt = FALSE, rt_to_search = NULL, rt_window = 12,
                                         return_as_featinfo_lev1 = FALSE) {
  
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
  
  if (length(return_as_featinfo_lev1)!=1) {stop("return_as_featinfo_lev1 must be exclusively TRUE or FALSE")}
  if (!is.logical(return_as_featinfo_lev1)) {stop("return_as_featinfo_lev1 must be exclusively TRUE or FALSE")}
  if (is.na(return_as_featinfo_lev1)) {stop("return_as_featinfo_lev1 must be exclusively TRUE or FALSE")}
  if (return_as_featinfo_lev1 & !check_rt) {stop("if return_as_featinfo_lev1 is TRUE, also check_rt must be TRUE as level 1 compounds can only be confirmed with the retention time!")}
  
  if (any(c("mz_molecule_list", "rt_molecule_list") %in% colnames(featmatrix))) {stop('please, just do not name in advance any column of featmatrix "mz_molecule_list" or "rt_molecule_list"')}
  if (any(c("mz_molecule_list", "rt_molecule_list") %in% colnames(featinfo))) {stop('please, just do not name in advance any column of featinfo "mz_molecule_list" or "rt_molecule_list"')}
  if (any(c("mz_molecule_list", "rt_molecule_list") %in% colnames(molecules_list))) {
    if (is.null(mz_to_search)) {stop('please, just do not name in advance any column of molecules_list "mz_molecule_list" or "rt_molecule_list", unless they are the columns used for the search')}
    if (is.null(rt_to_search)) {stop('please, just do not name in advance any column of molecules_list "mz_molecule_list" or "rt_molecule_list", unless they are the columns used for the search')}
    if (mz_to_search!= "mz_molecule_list" | rt_to_search!="rt_molecule_list") {stop('please, just do not name in advance any column of molecules_list "mz_molecule_list" or "rt_molecule_list", unless they are the columns used for the search')}
  }
  
  molecules_list_to_add <- molecules_list
  colnames(molecules_list_to_add)[which(colnames(molecules_list_to_add) == mz_to_search)] <- "mz_molecule_list"
  colnames(molecules_list_to_add)[which(colnames(molecules_list_to_add) == rt_to_search)] <- "rt_molecule_list"
  
  final_table_one_row <-  matrix(NA, nrow = 1, ncol = length(colnames(molecules_list_to_add))+length(colnames(featmatrix))+4)
  colnames(final_table_one_row) <- c(colnames(molecules_list_to_add), colnames(featinfo)[1], colnames(featinfo)[2], "rt_shift", colnames(featinfo)[3], "mz_shift", colnames(featmatrix)[-1])
  final_table_one_row <-  as_tibble(final_table_one_row)
  
  final_table <-  final_table_one_row[0,]
  
  
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
          new_row_tibble[1, colnames(molecules_list_to_add)] <- molecules_list_to_add[i,]
          new_row_tibble[1,colnames(featinfo)[1]] <- pull(featinfo, colnames(featinfo)[1])[u]
          new_row_tibble[1,colnames(featinfo)[2]] <- pull(featinfo, colnames(featinfo)[2])[u]
          if (!is.null(rt_to_search)) {new_row_tibble[1,"rt_shift"] <- pull(featinfo,colnames(featinfo)[2])[u] - pull(molecules_list, rt_to_search)[i]}
          new_row_tibble[1,colnames(featinfo)[3]] <- pull(featinfo,colnames(featinfo)[3])[u]
          new_row_tibble[1,"mz_shift"] <- pull(featinfo,colnames(featinfo)[3])[u] - theor_mass
          new_row_tibble[1, colnames(featmatrix)[-1]] <- featmatrix[u, colnames(featmatrix)[-1]]
          
          final_table <- rbind(final_table, new_row_tibble)
        }
      }
      
      if (!pull(molecules_list, colnames(molecules_list)[1])[i] %in% pull(final_table, colnames(molecules_list)[1])) {
        new_row_tibble <- final_table_one_row
        new_row_tibble[1, colnames(molecules_list_to_add)] <- molecules_list_to_add[i,]
        new_row_tibble[1,colnames(featinfo)[1]] <- "NOT_FOUND"
        
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
          new_row_tibble[1, colnames(molecules_list_to_add)] <- molecules_list_to_add[i,]
          new_row_tibble[1,colnames(featinfo)[1]] <- pull(featinfo, colnames(featinfo)[1])[u]
          new_row_tibble[1,colnames(featinfo)[2]] <- pull(featinfo, colnames(featinfo)[2])[u]
          if (!is.null(rt_to_search)) {new_row_tibble[1,"rt_shift"] <- pull(featinfo,colnames(featinfo)[2])[u] - pull(molecules_list, rt_to_search)[i]}
          new_row_tibble[1,colnames(featinfo)[3]] <- pull(featinfo,colnames(featinfo)[3])[u]
          new_row_tibble[1,"mz_shift"] <- pull(featinfo,colnames(featinfo)[3])[u] - theor_mass
          new_row_tibble[1, colnames(featmatrix)[-1]] <- featmatrix[u, colnames(featmatrix)[-1]]
          
          final_table <- rbind(final_table, new_row_tibble)
        }
      }
      
      if (!pull(molecules_list, colnames(molecules_list)[1])[i] %in% pull(final_table, colnames(molecules_list)[1])) {
        new_row_tibble <- final_table_one_row
        new_row_tibble[1, colnames(molecules_list_to_add)] <- molecules_list_to_add[i,]
        new_row_tibble[1,colnames(featinfo)[1]] <- "NOT_FOUND"
        
        final_table <- rbind(final_table, new_row_tibble)
      }
    }
  }
  
  
  if (check_rt == TRUE) {
    
    final_table <- add_column(final_table,
                              rt_confirmed = as.logical(rep(NA, length(pull(final_table, colnames(final_table)[1])))),
                              .before = colnames(featinfo)[1])
    
    for(i in 1:length(pull(final_table, colnames(final_table)[1]))) {
      if (pull(final_table, colnames(featinfo)[1])[i] != "NOT_FOUND") {
        rt_lower_limit <- pull(molecules_list, rt_to_search)[which(pull(molecules_list, colnames(molecules_list)[1])==pull(final_table, colnames(final_table)[1])[i])] - (rt_window/2)
        rt_upper_limit <- pull(molecules_list, rt_to_search)[which(pull(molecules_list, colnames(molecules_list)[1])==pull(final_table, colnames(final_table)[1])[i])] + (rt_window/2)
        
        if(pull(final_table, colnames(featinfo)[2])[i] <= rt_upper_limit & pull(final_table, colnames(featinfo)[2])[i] >= rt_lower_limit) {
          final_table$rt_confirmed[i] <- TRUE
        } else {
          final_table$rt_confirmed[i] <- FALSE
        }
      }
    }
  }
  
  if (!return_as_featinfo_lev1) {
    return(final_table)
  } else {
    
    final_table_ret_conf <- filter(final_table, rt_confirmed)
    featinfo_output <- featinfo
    
    
    if (length(which(pull(featinfo, 1) %in% pull(final_table_ret_conf, colnames(featinfo)[1])))>0) {
      
      for (i in which(pull(featinfo, 1) %in% pull(final_table_ret_conf, colnames(featinfo)[1]))) {
        
        this_feat <- pull(featinfo, 1)[i]
        
        final_table_ret_conf_this_feat <- final_table_ret_conf[which(pull(final_table_ret_conf, colnames(featinfo)[1]) == this_feat),]
        
        if (nrow(final_table_ret_conf_this_feat) == 0) {
          stop("an enexpected error (error n.001) happened, please ask Gianfranco to solve... (it should not happen)")
        }
        if (nrow(final_table_ret_conf_this_feat) > 1) {
          
          if (!all(pull(final_table_ret_conf_this_feat, colnames(featinfo)[1]) == pull(final_table_ret_conf_this_feat, colnames(featinfo)[1])[1])) {
            stop("an enexpected error (error n.004) happened, please ask Gianfranco to solve... (it should not happen)")
          }
          
          cat(paste0("\nCompounds '", paste0(pull(final_table_ret_conf_this_feat, colnames(molecules_list)[1]), collapse = "', '"), "' were all matched to '", pull(final_table_ret_conf_this_feat, colnames(featinfo)[1])[1], "'"))
          
          final_table_ret_conf_this_feat <- arrange(final_table_ret_conf_this_feat, abs(rt_shift), abs(mz_shift))
          
          final_table_ret_conf_this_feat <- final_table_ret_conf_this_feat[1,]
          
          cat(paste0("\n - '", pull(final_table_ret_conf_this_feat, colnames(molecules_list)[1]), "' was chosen (likely because of better rt shift or m/z shift)\n"))
          
        }
        if (nrow(final_table_ret_conf_this_feat) == 1) {
          
          this_row_of_molecules_list_to_add <- which(pull(molecules_list_to_add, 1) == pull(final_table_ret_conf_this_feat, 1)[1])
          
          if (length(this_row_of_molecules_list_to_add)!=1) {
            stop("an enexpected error (error n.002) happened, please ask Gianfranco to solve... (it should not happen)")
          }
          
          featinfo_output[i, colnames(molecules_list_to_add)] <- molecules_list_to_add[this_row_of_molecules_list_to_add,]
          featinfo_output[i, "AnnoLevel"] <- "1"
          featinfo_output[i, "rt_shift"] <- pull(final_table_ret_conf_this_feat, "rt_shift")[1]
          featinfo_output[i, "mz_shift"] <- pull(final_table_ret_conf_this_feat, "mz_shift")[1]
          
        } else {
          stop("an enexpected error (error n.003) happened, please ask Gianfranco to solve... (it should not happen)")
        }
      }
    }
    
    
    return(featinfo_output)
  }
}

