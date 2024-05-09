
#' Combining three tables of feature intensities in a combined table.
#'
#' Given three different tables of feature intensities and the related featINFO tables, it combines the feature intensities in a single table.
#'
#' @param featTable1 a dataframe, containing feature intensities.
#' @param featINFO1 a dataframe, containing the featINFO related to featTable1.
#' @param featTable2 a dataframe, containing feature intensities.
#' @param featINFO2  a dataframe, containing the featINFO related to featTable2.
#' @param featTable3 a dataframe, containing feature intensities.
#' @param featINFO3 a dataframe, containing the featINFO related to featTable3.
#' @param mz_widow numeric of length 1. The mass-to-charge ratio window within which features could be grouped as the same feature.
#' @param rt_window numeric of length 1. The retention time window within within features could be grouped as the same feature.
#'
#'
#' @return A tibble. The combined table of feature intensities.
#'
#' @export
GetCombined_featTable_MSDial <- function(featTable1, featINFO1, name1 = "table1",
                                         featTable2, featINFO2, name2 = "table2",
                                         featTable3, featINFO3, name3 = "table3",
                                         mz_widow = 0.005, rt_window = 2) {
  
  if (!is.data.frame(featTable1)) {stop("featTable1 must be a data-frame!")}
  if (!is.data.frame(featINFO1)) {stop("featINFO1 must be a data-frame!")}
  if (!is.numeric(pull(featINFO1, colnames(featINFO1)[2]))) {stop ("the second coloumn of featINFO1 must contain retention times, so numeric values with no missing values")}
  if (any(is.na(pull(featINFO1, colnames(featINFO1)[2])))) {stop ("the second coloumn of featINFO1 must contain retention times, so numeric values with no missing values")}
  if (!is.numeric(pull(featINFO1, colnames(featINFO1)[3]))) {stop ("the second coloumn of featINFO1 must contain m/z, so numeric values with no missing values")}
  if (any(is.na(pull(featINFO1, colnames(featINFO1)[3])))) {stop ("the second coloumn of featINFO1 must contain m/z, so numeric values with no missing values")}
  if (!"INCHIKEY" %in% colnames(featINFO1)) {stop("INCHIKEY must be present as a coulumn in featINFO1")}
  if (!"Formula" %in% colnames(featINFO1)) {stop("Formula must be present as a coulumn in featINFO1")}
  if (!"AnnoLevel" %in% colnames(featINFO1)) {stop("AnnoLevel must be present as a coulumn in featINFO1")}
  if (!all(pull(featTable1, colnames(featTable1)[1]) %in% pull(featINFO1, colnames(featINFO1)[1]))) {stop("the features names of featTable1 are not contained in featINFO1")}
  
  if (!is.data.frame(featTable2)) {stop("featTable2 must be a data-frame!")}
  if (!is.data.frame(featINFO2)) {stop("featINFO2 must be a data-frame!")}
  if (!is.numeric(pull(featINFO2, colnames(featINFO2)[2]))) {stop ("the second coloumn of featINFO2 must contain retention times, so numeric values with no missing values")}
  if (any(is.na(pull(featINFO2, colnames(featINFO2)[2])))) {stop ("the second coloumn of featINFO2 must contain retention times, so numeric values with no missing values")}
  if (!is.numeric(pull(featINFO2, colnames(featINFO2)[3]))) {stop ("the second coloumn of featINFO2 must contain m/z, so numeric values with no missing values")}
  if (any(is.na(pull(featINFO2, colnames(featINFO2)[3])))) {stop ("the second coloumn of featINFO2 must contain m/z, so numeric values with no missing values")}
  if (!"INCHIKEY" %in% colnames(featINFO2)) {stop("INCHIKEY must be present as a coulumn in featINFO2")}
  if (!"Formula" %in% colnames(featINFO2)) {stop("Formula must be present as a coulumn in featINFO2")}
  if (!"AnnoLevel" %in% colnames(featINFO2)) {stop("AnnoLevel must be present as a coulumn in featINFO2")}
  if (!all(pull(featTable2, colnames(featTable2)[1]) %in% pull(featINFO2, colnames(featINFO2)[1]))) {stop("the features names of featTable2 are not contained in featINFO2")}
  
  if (!is.data.frame(featTable3)) {stop("featTable3 must be a data-frame!")}
  if (!is.data.frame(featINFO3)) {stop("featINFO3 must be a data-frame!")}
  if (!is.numeric(pull(featINFO3, colnames(featINFO3)[2]))) {stop ("the second coloumn of featINFO3 must contain retention times, so numeric values with no missing values")}
  if (any(is.na(pull(featINFO3, colnames(featINFO3)[2])))) {stop ("the second coloumn of featINFO3 must contain retention times, so numeric values with no missing values")}
  if (!is.numeric(pull(featINFO3, colnames(featINFO3)[3]))) {stop ("the second coloumn of featINFO3 must contain m/z, so numeric values with no missing values")}
  if (any(is.na(pull(featINFO3, colnames(featINFO3)[3])))) {stop ("the second coloumn of featINFO3 must contain m/z, so numeric values with no missing values")}
  if (!"INCHIKEY" %in% colnames(featINFO3)) {stop("INCHIKEY must be present as a coulumn in featINFO3")}
  if (!"Formula" %in% colnames(featINFO3)) {stop("Formula must be present as a coulumn in featINFO3")}
  if (!"AnnoLevel" %in% colnames(featINFO3)) {stop("AnnoLevel must be present as a coulumn in featINFO3")}
  if (!all(pull(featTable3, colnames(featTable3)[1]) %in% pull(featINFO3, colnames(featINFO3)[1]))) {stop("the features names of featTable3 are not contained in featINFO3")}
  
  if (!is.character(name1)) {stop('name1 must be a character of length 1')} else if (length(name1) != 1) {stop('name1 must be a character of length 1')} else if (is.na(name1)) {stop('name1 must be a character of length 1, and not a missing value!')}
  if (!is.character(name2)) {stop('name2 must be a character of length 1')} else if (length(name2) != 1) {stop('name2 must be a character of length 1')} else if (is.na(name2)) {stop('name2 must be a character of length 1, and not a missing value!')}
  if (!is.character(name3)) {stop('name3 must be a character of length 1')} else if (length(name3) != 1) {stop('name3 must be a character of length 1')} else if (is.na(name3)) {stop('name3 must be a character of length 1, and not a missing value!')}
  
  if (length(mz_widow)!=1) {stop("mz_widow must be a numeric of length 1")} else if (!is.numeric(mz_widow)) {stop("mz_widow must be a numeric of length 1")} else if (is.na(mz_widow)) {stop("mz_widow must not be NA")}
  if (length(rt_window)!=1) {stop("rt_window must be a numeric of length 1")} else if (!is.numeric(rt_window)) {stop("rt_window must be a numeric of length 1")} else if (is.na(rt_window)) {stop("rt_window must not be NA")}
  
  if(any(colnames(featTable1)[-1] %in% colnames(featTable2)[-1]) | any(colnames(featTable2)[-1] %in% colnames(featTable3)[-1]) | any(colnames(featTable1)[-1] %in% colnames(featTable3)[-1])) {
    stop("please, consider analyses with different names in the coloumn names of featTable1, featTable2, and featTable3")
  }
  
  
  
  featTable1[,1] <- paste0(pull(featTable1, colnames(featTable1)[1]), "_", name1)
  featINFO1[,1] <- paste0(pull(featINFO1, colnames(featINFO1)[1]), "_", name1)
  featINFO1_QCfil <- featINFO1[which(pull(featINFO1, colnames(featINFO1)[1]) %in% pull(featTable1, colnames(featTable1)[1])),]
  if (!all(pull(featTable1, 1) == pull(featINFO1_QCfil, 1))) {stop( "something wrong")}
  
  featTable2[,1] <- paste0(pull(featTable2, colnames(featTable2)[1]), "_", name2)
  featINFO2[,1] <- paste0(pull(featINFO2, colnames(featINFO2)[1]), "_", name2)
  featINFO2_QCfil <- featINFO2[which(pull(featINFO2, colnames(featINFO2)[1]) %in% pull(featTable2, colnames(featTable2)[1])),]
  if (!all(pull(featTable2, 1) == pull(featINFO2_QCfil, 1))) {stop( "something wrong")}
  
  featTable3[,1] <- paste0(pull(featTable3, colnames(featTable3)[1]), "_", name3)
  featINFO3[,1] <- paste0(pull(featINFO3, colnames(featINFO3)[1]), "_", name3)
  featINFO3_QCfil <- featINFO3[which(pull(featINFO3, colnames(featINFO3)[1]) %in% pull(featTable3, colnames(featTable3)[1])),]
  if (!all(pull(featTable3, 1) == pull(featINFO3_QCfil, 1))) {stop( "something wrong")}
  
  
  
  potential_featTable2_pairings <- tibble(feat2 = pull(featINFO2_QCfil, 1),
                                          pairing_with_feat1 = as.character(rep(NA, length(pull(featINFO2_QCfil, 1)))))
  
  for (i in 1:length(pull(featINFO2_QCfil, 1))) {
    
    this_feature_name <- pull(featINFO2_QCfil, 1)[i]
    this_INCHIKEY <- pull(featINFO2_QCfil, "INCHIKEY")[i]
    this_Formula <- pull(featINFO2_QCfil, "Formula")[i]
    this_AnnoLevel <- pull(featINFO2_QCfil, "AnnoLevel")[i]
    
    this_min_rt <- pull(featINFO2_QCfil, 2)[i] - (rt_window/2)
    this_max_rt <- pull(featINFO2_QCfil, 2)[i] + (rt_window/2)
    this_min_mz <- pull(featINFO2_QCfil, 3)[i] - (mz_widow/2)
    this_max_mz <- pull(featINFO2_QCfil, 3)[i] + (mz_widow/2)
    
    this_match_with_featINFO1_QCfil <- as.character(NA)
    
    this_featINFO1_QCfil_FIL <- featINFO1_QCfil[pull(featINFO1_QCfil,2) > this_min_rt & pull(featINFO1_QCfil,2) < this_max_rt & pull(featINFO1_QCfil,3) > this_min_mz & pull(featINFO1_QCfil,3) < this_max_mz,]
    
    if (length(pull(this_featINFO1_QCfil_FIL, 1)) != 0) {
      if (!is.na(this_INCHIKEY)) {
        this_featINFO1_QCfil_moreFIL <- this_featINFO1_QCfil_FIL[which(pull(this_featINFO1_QCfil_FIL, "INCHIKEY") == this_INCHIKEY),]
        if (length(pull(this_featINFO1_QCfil_moreFIL, 1)) == 1) {
          potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_moreFIL, 1)
        } else if (length(pull(this_featINFO1_QCfil_moreFIL, 1)) > 1) {
          closest_index <- which.min(abs(pull(this_featINFO1_QCfil_moreFIL, 2) - pull(featINFO2_QCfil, 2)[i]))
          potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_moreFIL, 1)[closest_index]
        } else {
          if (length(pull(this_featINFO1_QCfil_FIL, 1)) == 1) {
            potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)
          } else {
            closest_index <- which.min(abs(pull(this_featINFO1_QCfil_FIL, 2) - pull(featINFO2_QCfil, 2)[i]))
            potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)[closest_index]
          }
        }
      } else if (!is.na(this_Formula)) {
        this_featINFO1_QCfil_moreFIL <- this_featINFO1_QCfil_FIL[which(pull(this_featINFO1_QCfil_FIL, "Formula") == this_Formula),]
        if (length(pull(this_featINFO1_QCfil_moreFIL, 1)) == 1) {
          potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_moreFIL, 1)
        } else if (length(pull(this_featINFO1_QCfil_moreFIL, 1)) > 1) {
          closest_index <- which.min(abs(pull(this_featINFO1_QCfil_moreFIL, 2) - pull(featINFO2_QCfil, 2)[i]))
          potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_moreFIL, 1)[closest_index]
        } else {
          if (length(pull(this_featINFO1_QCfil_FIL, 1)) == 1) {
            potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)
          } else {
            closest_index <- which.min(abs(pull(this_featINFO1_QCfil_FIL, 2) - pull(featINFO2_QCfil, 2)[i]))
            potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)[closest_index]
          }
        }
      } else {
        if (length(pull(this_featINFO1_QCfil_FIL, 1)) == 1) {
          potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)
        } else {
          closest_index <- which.min(abs(pull(this_featINFO1_QCfil_FIL, 2) - pull(featINFO2_QCfil, 2)[i]))
          potential_featTable2_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)[closest_index]
        }
      }
    }
  }
  
  duplicated_potential_paring <- unique(potential_featTable2_pairings$pairing_with_feat1[which(!is.na(potential_featTable2_pairings$pairing_with_feat1) & duplicated(potential_featTable2_pairings$pairing_with_feat1))])
  
  potential_featTable2_pairings_updt <- potential_featTable2_pairings
  
  for(a in duplicated_potential_paring) {
    duplicated_potential_paring_fil <- filter(potential_featTable2_pairings, pairing_with_feat1 == a)
    
    this_featINFO2_QCfil_fil <- filter(featINFO2_QCfil, Alignment_ID %in% duplicated_potential_paring_fil$feat2)
    
    this_min_rt <- min(pull(this_featINFO2_QCfil_fil, 2)) - (rt_window/2)
    this_max_rt <- max(pull(this_featINFO2_QCfil_fil, 2)) + (rt_window/2)
    this_min_mz <- min(pull(this_featINFO2_QCfil_fil, 3)) - (mz_widow/2)
    this_max_mz <- max(pull(this_featINFO2_QCfil_fil, 3)) + (mz_widow/2)
    
    this_featINFO1_QCfil_fil <- featINFO1_QCfil[pull(featINFO1_QCfil,2) > this_min_rt & pull(featINFO1_QCfil,2) < this_max_rt & pull(featINFO1_QCfil,3) > this_min_mz & pull(featINFO1_QCfil,3) < this_max_mz,]
    
    if (length(pull(this_featINFO1_QCfil_fil, 1)) == length(pull(this_featINFO2_QCfil_fil, 1))) {
      
      sorted_indices1 <- order(pull(this_featINFO1_QCfil_fil, 2))
      sorted_indices2 <- order(pull(this_featINFO2_QCfil_fil, 2))
      
      new_pairing_little_tab <- tibble(feat2 = pull(this_featINFO2_QCfil_fil, 1)[sorted_indices2],
                                       pairing_with_feat1 = pull(this_featINFO1_QCfil_fil, 1)[sorted_indices1])
      
      potential_featTable2_pairings_updt[which(pull(potential_featTable2_pairings_updt, "feat2") %in% pull(new_pairing_little_tab, "feat2")),] <- new_pairing_little_tab
      
    } else if (length(pull(this_featINFO1_QCfil_fil, 1)) < length(pull(this_featINFO2_QCfil_fil, 1))) {
      
      if (all(pull(this_featINFO2_QCfil_fil, "AnnoLevel") == pull(this_featINFO2_QCfil_fil, "AnnoLevel")[1])) {
        
        short <- pull(this_featINFO1_QCfil_fil, 2)
        long_original <- pull(this_featINFO2_QCfil_fil, 2)
        long_reduced <- long_original
        
        indices_to_remove_from_long_original <- numeric()
        
        repeat {
          indices_to_remove_from_long_original <- c(indices_to_remove_from_long_original, which(long_original==long_reduced[which.max(abs(long_reduced - mean(short)))]))
          
          long_reduced <- long_reduced[-which.max(abs(long_reduced - mean(short)))]
          
          if(length(long_reduced) == length(short)) {
            break
          }
        }
        
        potential_featTable2_pairings_updt[which(pull(potential_featTable2_pairings_updt, "feat2") %in% pull(this_featINFO2_QCfil_fil, 1)[indices_to_remove_from_long_original]),"pairing_with_feat1"] <- NA
        
        this_featINFO2_QCfil_fil_reduced <- this_featINFO2_QCfil_fil[-indices_to_remove_from_long_original,]
        
        
        sorted_indices1 <- order(pull(this_featINFO1_QCfil_fil, 2))
        sorted_indices2 <- order(pull(this_featINFO2_QCfil_fil_reduced, 2))
        
        new_pairing_little_tab <- tibble(feat2 = pull(this_featINFO2_QCfil_fil_reduced, 1)[sorted_indices2],
                                         pairing_with_feat1 = pull(this_featINFO1_QCfil_fil, 1)[sorted_indices1])
        
        potential_featTable2_pairings_updt[which(pull(potential_featTable2_pairings_updt, "feat2") %in% pull(new_pairing_little_tab, "feat2")),] <- new_pairing_little_tab
        
      } else {
        
        short <- pull(this_featINFO1_QCfil_fil, 2)
        long_original <- pull(this_featINFO2_QCfil_fil, 2)
        levels_original <- pull(this_featINFO2_QCfil_fil, "AnnoLevel")
        long_reduced <- long_original
        levels_reduced <- levels_original
        
        indices_to_remove_from_long_original <- numeric()
        
        repeat {
          
          lowest_level_here <- max(from_AnnoLevel_to_ordered_number(levels_reduced)) %>% from_ordered_number_to_AnnoLevel()
          
          this_long_reduced <- long_reduced[which(levels_reduced == lowest_level_here)]
          
          indices_to_remove_from_long_original <- c(indices_to_remove_from_long_original, which(long_original==this_long_reduced[which.max(abs(this_long_reduced - mean(short)))]))
          
          levels_reduced <- levels_reduced[-which(long_reduced==long_original[which(long_original==this_long_reduced[which.max(abs(this_long_reduced - mean(short)))])])]
          long_reduced <- long_reduced[-which(long_reduced==long_original[which(long_original==this_long_reduced[which.max(abs(this_long_reduced - mean(short)))])])]
          
          
          if(length(long_reduced) == length(short)) {
            break
          }
        }
        
        potential_featTable2_pairings_updt[which(pull(potential_featTable2_pairings_updt, "feat2") %in% pull(this_featINFO2_QCfil_fil, 1)[indices_to_remove_from_long_original]),"pairing_with_feat1"] <- NA
        
        this_featINFO2_QCfil_fil_reduced <- this_featINFO2_QCfil_fil[-indices_to_remove_from_long_original,]
        
        
        sorted_indices1 <- order(pull(this_featINFO1_QCfil_fil, 2))
        sorted_indices2 <- order(pull(this_featINFO2_QCfil_fil_reduced, 2))
        
        new_pairing_little_tab <- tibble(feat2 = pull(this_featINFO2_QCfil_fil_reduced, 1)[sorted_indices2],
                                         pairing_with_feat1 = pull(this_featINFO1_QCfil_fil, 1)[sorted_indices1])
        
        potential_featTable2_pairings_updt[which(pull(potential_featTable2_pairings_updt, "feat2") %in% pull(new_pairing_little_tab, "feat2")),] <- new_pairing_little_tab
        
      }
      
    } else if (length(pull(this_featINFO1_QCfil_fil, 1)) > length(pull(this_featINFO2_QCfil_fil, 1))) {
      
      short <- pull(this_featINFO2_QCfil_fil, 2)
      long_original <- pull(this_featINFO1_QCfil_fil, 2)
      long_reduced <- long_original
      
      indices_to_remove_from_long_original <- numeric()
      
      repeat {
        indices_to_remove_from_long_original <- c(indices_to_remove_from_long_original, which(long_original==long_reduced[which.max(abs(long_reduced - mean(short)))]))
        
        long_reduced <- long_reduced[-which.max(abs(long_reduced - mean(short)))]
        
        if(length(long_reduced) == length(short)) {
          break
        }
      }
      
      this_featINFO1_QCfil_fil_reduced <- this_featINFO1_QCfil_fil[-indices_to_remove_from_long_original,]
      
      sorted_indices1 <- order(pull(this_featINFO1_QCfil_fil_reduced, 2))
      sorted_indices2 <- order(pull(this_featINFO2_QCfil_fil, 2))
      
      new_pairing_little_tab <- tibble(feat2 = pull(this_featINFO2_QCfil_fil, 1)[sorted_indices2],
                                       pairing_with_feat1 = pull(this_featINFO1_QCfil_fil_reduced, 1)[sorted_indices1])
      
      potential_featTable2_pairings_updt[which(pull(potential_featTable2_pairings_updt, "feat2") %in% pull(new_pairing_little_tab, "feat2")),] <- new_pairing_little_tab
      
      
    }
    
  }
  
  
  still_duplicated <- unique(potential_featTable2_pairings_updt$pairing_with_feat1[!is.na(potential_featTable2_pairings_updt$pairing_with_feat1) & duplicated(potential_featTable2_pairings_updt$pairing_with_feat1)])
  
  potential_featTable2_pairings_updt_bis <- potential_featTable2_pairings_updt
  
  
  for (b in still_duplicated) {
    duplicated_potential_paring_fil <- filter(potential_featTable2_pairings_updt, pairing_with_feat1 == b)
    
    this_featINFO1_QCfil_fil <- filter(featINFO1_QCfil, Alignment_ID %in% unique(duplicated_potential_paring_fil$pairing_with_feat1))
    
    this_featINFO2_QCfil_fil <- filter(featINFO2_QCfil, Alignment_ID %in% duplicated_potential_paring_fil$feat2)
    
    if (all(pull(this_featINFO2_QCfil_fil, "AnnoLevel") == pull(this_featINFO2_QCfil_fil, "AnnoLevel")[1])) {
      
      closest_index <- which.min(abs(pull(this_featINFO2_QCfil_fil, 2) - pull(this_featINFO1_QCfil_fil, 2)))
      
      potential_featTable2_pairings_updt_bis[which(pull(potential_featTable2_pairings_updt_bis, "feat2") %in% pull(this_featINFO2_QCfil_fil, 1)[-closest_index]), "pairing_with_feat1"] <- NA
      
    } else {
      
      highest_level_here <- min(from_AnnoLevel_to_ordered_number(pull(this_featINFO2_QCfil_fil, "AnnoLevel"))) %>% from_ordered_number_to_AnnoLevel()
      
      this_featINFO2_QCfil_more_fill <- filter(this_featINFO2_QCfil_fil, AnnoLevel == highest_level_here)
      this_featINFO2_QCfil_more_fill_discarded <- filter(this_featINFO2_QCfil_fil, AnnoLevel != highest_level_here)
      
      closest_index <- which.min(abs(pull(this_featINFO2_QCfil_more_fill, 2) - pull(this_featINFO1_QCfil_fil, 2)))
      
      potential_featTable2_pairings_updt_bis[which(pull(potential_featTable2_pairings_updt_bis, "feat2") %in% pull(this_featINFO2_QCfil_more_fill, 1)[-closest_index]  | pull(potential_featTable2_pairings_updt_bis, "feat2") %in% pull(this_featINFO2_QCfil_more_fill_discarded, 1)), "pairing_with_feat1"] <- NA
      
    }
    
  }
  
  
  still_still_duplicated <- unique(potential_featTable2_pairings_updt_bis$pairing_with_feat1[!is.na(potential_featTable2_pairings_updt_bis$pairing_with_feat1) & duplicated(potential_featTable2_pairings_updt_bis$pairing_with_feat1)])
  if(length(still_still_duplicated) != 0) {stop ( "something wrong as there are still some duplicates.... :( ")}
  
  
  
  
  
  
  ### now for the third table!! :O
  
  potential_featTable3_pairings <- tibble(feat3 = pull(featINFO3_QCfil, 1),
                                          pairing_with_feat1 = as.character(rep(NA, length(pull(featINFO3_QCfil, 1)))))
  
  
  for (i in 1:length(pull(featINFO3_QCfil, 1))) {
    
    this_feature_name <- pull(featINFO3_QCfil, 1)[i]
    this_INCHIKEY <- pull(featINFO3_QCfil, "INCHIKEY")[i]
    this_Formula <- pull(featINFO3_QCfil, "Formula")[i]
    this_AnnoLevel <- pull(featINFO3_QCfil, "AnnoLevel")[i]
    
    this_min_rt <- pull(featINFO3_QCfil, 2)[i] - (rt_window/2)
    this_max_rt <- pull(featINFO3_QCfil, 2)[i] + (rt_window/2)
    this_min_mz <- pull(featINFO3_QCfil, 3)[i] - (mz_widow/2)
    this_max_mz <- pull(featINFO3_QCfil, 3)[i] + (mz_widow/2)
    
    this_match_with_featINFO1_QCfil <- as.character(NA)
    
    this_featINFO1_QCfil_FIL <- featINFO1_QCfil[pull(featINFO1_QCfil,2) > this_min_rt & pull(featINFO1_QCfil,2) < this_max_rt & pull(featINFO1_QCfil,3) > this_min_mz & pull(featINFO1_QCfil,3) < this_max_mz,]
    
    if (length(pull(this_featINFO1_QCfil_FIL, 1)) != 0) {
      if (!is.na(this_INCHIKEY)) {
        this_featINFO1_QCfil_moreFIL <- this_featINFO1_QCfil_FIL[which(pull(this_featINFO1_QCfil_FIL, "INCHIKEY") == this_INCHIKEY),]
        if (length(pull(this_featINFO1_QCfil_moreFIL, 1)) == 1) {
          potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_moreFIL, 1)
        } else if (length(pull(this_featINFO1_QCfil_moreFIL, 1)) > 1) {
          closest_index <- which.min(abs(pull(this_featINFO1_QCfil_moreFIL, 2) - pull(featINFO3_QCfil, 2)[i]))
          potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_moreFIL, 1)[closest_index]
        } else {
          if (length(pull(this_featINFO1_QCfil_FIL, 1)) == 1) {
            potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)
          } else {
            closest_index <- which.min(abs(pull(this_featINFO1_QCfil_FIL, 2) - pull(featINFO3_QCfil, 2)[i]))
            potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)[closest_index]
          }
        }
      } else if (!is.na(this_Formula)) {
        this_featINFO1_QCfil_moreFIL <- this_featINFO1_QCfil_FIL[which(pull(this_featINFO1_QCfil_FIL, "Formula") == this_Formula),]
        if (length(pull(this_featINFO1_QCfil_moreFIL, 1)) == 1) {
          potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_moreFIL, 1)
        } else if (length(pull(this_featINFO1_QCfil_moreFIL, 1)) > 1) {
          closest_index <- which.min(abs(pull(this_featINFO1_QCfil_moreFIL, 2) - pull(featINFO3_QCfil, 2)[i]))
          potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_moreFIL, 1)[closest_index]
        } else {
          if (length(pull(this_featINFO1_QCfil_FIL, 1)) == 1) {
            potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)
          } else {
            closest_index <- which.min(abs(pull(this_featINFO1_QCfil_FIL, 2) - pull(featINFO3_QCfil, 2)[i]))
            potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)[closest_index]
          }
        }
      } else {
        if (length(pull(this_featINFO1_QCfil_FIL, 1)) == 1) {
          potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)
        } else {
          closest_index <- which.min(abs(pull(this_featINFO1_QCfil_FIL, 2) - pull(featINFO3_QCfil, 2)[i]))
          potential_featTable3_pairings[i, "pairing_with_feat1"] <- pull(this_featINFO1_QCfil_FIL, 1)[closest_index]
        }
      }
    }
  }
  
  duplicated_potential_paring <- unique(potential_featTable3_pairings$pairing_with_feat1[which(!is.na(potential_featTable3_pairings$pairing_with_feat1) & duplicated(potential_featTable3_pairings$pairing_with_feat1))])
  
  potential_featTable3_pairings_updt <- potential_featTable3_pairings
  
  for(a in duplicated_potential_paring) {
    duplicated_potential_paring_fil <- filter(potential_featTable3_pairings, pairing_with_feat1 == a)
    
    this_featINFO3_QCfil_fil <- filter(featINFO3_QCfil, Alignment_ID %in% duplicated_potential_paring_fil$feat3)
    
    this_min_rt <- min(pull(this_featINFO3_QCfil_fil, 2)) - (rt_window/2)
    this_max_rt <- max(pull(this_featINFO3_QCfil_fil, 2)) + (rt_window/2)
    this_min_mz <- min(pull(this_featINFO3_QCfil_fil, 3)) - (mz_widow/2)
    this_max_mz <- max(pull(this_featINFO3_QCfil_fil, 3)) + (mz_widow/2)
    
    this_featINFO1_QCfil_fil <- featINFO1_QCfil[pull(featINFO1_QCfil,2) > this_min_rt & pull(featINFO1_QCfil,2) < this_max_rt & pull(featINFO1_QCfil,3) > this_min_mz & pull(featINFO1_QCfil,3) < this_max_mz,]
    
    if (length(pull(this_featINFO1_QCfil_fil, 1)) == length(pull(this_featINFO3_QCfil_fil, 1))) {
      
      sorted_indices1 <- order(pull(this_featINFO1_QCfil_fil, 2))
      sorted_indices2 <- order(pull(this_featINFO3_QCfil_fil, 2))
      
      new_pairing_little_tab <- tibble(feat3 = pull(this_featINFO3_QCfil_fil, 1)[sorted_indices2],
                                       pairing_with_feat1 = pull(this_featINFO1_QCfil_fil, 1)[sorted_indices1])
      
      potential_featTable3_pairings_updt[which(pull(potential_featTable3_pairings_updt, "feat3") %in% pull(new_pairing_little_tab, "feat3")),] <- new_pairing_little_tab
      
    } else if (length(pull(this_featINFO1_QCfil_fil, 1)) < length(pull(this_featINFO3_QCfil_fil, 1))) {
      
      if (all(pull(this_featINFO3_QCfil_fil, "AnnoLevel") == pull(this_featINFO3_QCfil_fil, "AnnoLevel")[1])) {
        
        short <- pull(this_featINFO1_QCfil_fil, 2)
        long_original <- pull(this_featINFO3_QCfil_fil, 2)
        long_reduced <- long_original
        
        indices_to_remove_from_long_original <- numeric()
        
        repeat {
          indices_to_remove_from_long_original <- c(indices_to_remove_from_long_original, which(long_original==long_reduced[which.max(abs(long_reduced - mean(short)))]))
          
          long_reduced <- long_reduced[-which.max(abs(long_reduced - mean(short)))]
          
          if(length(long_reduced) == length(short)) {
            break
          }
        }
        
        potential_featTable3_pairings_updt[which(pull(potential_featTable3_pairings_updt, "feat3") %in% pull(this_featINFO3_QCfil_fil, 1)[indices_to_remove_from_long_original]),"pairing_with_feat1"] <- NA
        
        this_featINFO3_QCfil_fil_reduced <- this_featINFO3_QCfil_fil[-indices_to_remove_from_long_original,]
        
        
        sorted_indices1 <- order(pull(this_featINFO1_QCfil_fil, 2))
        sorted_indices2 <- order(pull(this_featINFO3_QCfil_fil_reduced, 2))
        
        new_pairing_little_tab <- tibble(feat3 = pull(this_featINFO3_QCfil_fil_reduced, 1)[sorted_indices2],
                                         pairing_with_feat1 = pull(this_featINFO1_QCfil_fil, 1)[sorted_indices1])
        
        potential_featTable3_pairings_updt[which(pull(potential_featTable3_pairings_updt, "feat3") %in% pull(new_pairing_little_tab, "feat3")),] <- new_pairing_little_tab
        
      } else {
        
        short <- pull(this_featINFO1_QCfil_fil, 2)
        long_original <- pull(this_featINFO3_QCfil_fil, 2)
        levels_original <- pull(this_featINFO3_QCfil_fil, "AnnoLevel")
        long_reduced <- long_original
        levels_reduced <- levels_original
        
        indices_to_remove_from_long_original <- numeric()
        
        repeat {
          
          lowest_level_here <- max(from_AnnoLevel_to_ordered_number(levels_reduced)) %>% from_ordered_number_to_AnnoLevel()
          
          this_long_reduced <- long_reduced[which(levels_reduced == lowest_level_here)]
          
          indices_to_remove_from_long_original <- c(indices_to_remove_from_long_original, which(long_original==this_long_reduced[which.max(abs(this_long_reduced - mean(short)))]))
          
          levels_reduced <- levels_reduced[-which(long_reduced==long_original[which(long_original==this_long_reduced[which.max(abs(this_long_reduced - mean(short)))])])]
          long_reduced <- long_reduced[-which(long_reduced==long_original[which(long_original==this_long_reduced[which.max(abs(this_long_reduced - mean(short)))])])]
          
          
          if(length(long_reduced) == length(short)) {
            break
          }
        }
        
        potential_featTable3_pairings_updt[which(pull(potential_featTable3_pairings_updt, "feat3") %in% pull(this_featINFO3_QCfil_fil, 1)[indices_to_remove_from_long_original]),"pairing_with_feat1"] <- NA
        
        this_featINFO3_QCfil_fil_reduced <- this_featINFO3_QCfil_fil[-indices_to_remove_from_long_original,]
        
        
        sorted_indices1 <- order(pull(this_featINFO1_QCfil_fil, 2))
        sorted_indices2 <- order(pull(this_featINFO3_QCfil_fil_reduced, 2))
        
        new_pairing_little_tab <- tibble(feat3 = pull(this_featINFO3_QCfil_fil_reduced, 1)[sorted_indices2],
                                         pairing_with_feat1 = pull(this_featINFO1_QCfil_fil, 1)[sorted_indices1])
        
        potential_featTable3_pairings_updt[which(pull(potential_featTable3_pairings_updt, "feat3") %in% pull(new_pairing_little_tab, "feat3")),] <- new_pairing_little_tab
        
      }
      
    } else if (length(pull(this_featINFO1_QCfil_fil, 1)) > length(pull(this_featINFO3_QCfil_fil, 1))) {
      
      short <- pull(this_featINFO3_QCfil_fil, 2)
      long_original <- pull(this_featINFO1_QCfil_fil, 2)
      long_reduced <- long_original
      
      indices_to_remove_from_long_original <- numeric()
      
      repeat {
        indices_to_remove_from_long_original <- c(indices_to_remove_from_long_original, which(long_original==long_reduced[which.max(abs(long_reduced - mean(short)))]))
        
        long_reduced <- long_reduced[-which.max(abs(long_reduced - mean(short)))]
        
        if(length(long_reduced) == length(short)) {
          break
        }
      }
      
      this_featINFO1_QCfil_fil_reduced <- this_featINFO1_QCfil_fil[-indices_to_remove_from_long_original,]
      
      sorted_indices1 <- order(pull(this_featINFO1_QCfil_fil_reduced, 2))
      sorted_indices2 <- order(pull(this_featINFO3_QCfil_fil, 2))
      
      new_pairing_little_tab <- tibble(feat3 = pull(this_featINFO3_QCfil_fil, 1)[sorted_indices2],
                                       pairing_with_feat1 = pull(this_featINFO1_QCfil_fil_reduced, 1)[sorted_indices1])
      
      potential_featTable3_pairings_updt[which(pull(potential_featTable3_pairings_updt, "feat3") %in% pull(new_pairing_little_tab, "feat3")),] <- new_pairing_little_tab
      
      
    }
    
  }
  
  
  still_duplicated <- unique(potential_featTable3_pairings_updt$pairing_with_feat1[!is.na(potential_featTable3_pairings_updt$pairing_with_feat1) & duplicated(potential_featTable3_pairings_updt$pairing_with_feat1)])
  
  potential_featTable3_pairings_updt_bis <- potential_featTable3_pairings_updt
  
  
  for (b in still_duplicated) {
    duplicated_potential_paring_fil <- filter(potential_featTable3_pairings_updt, pairing_with_feat1 == b)
    
    this_featINFO1_QCfil_fil <- filter(featINFO1_QCfil, Alignment_ID %in% unique(duplicated_potential_paring_fil$pairing_with_feat1))
    
    this_featINFO3_QCfil_fil <- filter(featINFO3_QCfil, Alignment_ID %in% duplicated_potential_paring_fil$feat3)
    
    if (all(pull(this_featINFO3_QCfil_fil, "AnnoLevel") == pull(this_featINFO3_QCfil_fil, "AnnoLevel")[1])) {
      
      closest_index <- which.min(abs(pull(this_featINFO3_QCfil_fil, 2) - pull(this_featINFO1_QCfil_fil, 2)))
      
      potential_featTable3_pairings_updt_bis[which(pull(potential_featTable3_pairings_updt_bis, "feat3") %in% pull(this_featINFO3_QCfil_fil, 1)[-closest_index]), "pairing_with_feat1"] <- NA
      
    } else {
      
      highest_level_here <- min(from_AnnoLevel_to_ordered_number(pull(this_featINFO3_QCfil_fil, "AnnoLevel"))) %>% from_ordered_number_to_AnnoLevel()
      
      this_featINFO3_QCfil_more_fill <- filter(this_featINFO3_QCfil_fil, AnnoLevel == highest_level_here)
      this_featINFO3_QCfil_more_fill_discarded <- filter(this_featINFO3_QCfil_fil, AnnoLevel != highest_level_here)
      
      closest_index <- which.min(abs(pull(this_featINFO3_QCfil_more_fill, 2) - pull(this_featINFO1_QCfil_fil, 2)))
      
      potential_featTable3_pairings_updt_bis[which(pull(potential_featTable3_pairings_updt_bis, "feat3") %in% pull(this_featINFO3_QCfil_more_fill, 1)[-closest_index]  | pull(potential_featTable3_pairings_updt_bis, "feat3") %in% pull(this_featINFO3_QCfil_more_fill_discarded, 1)), "pairing_with_feat1"] <- NA
      
    }
    
  }
  
  
  still_still_duplicated <- unique(potential_featTable3_pairings_updt_bis$pairing_with_feat1[!is.na(potential_featTable3_pairings_updt_bis$pairing_with_feat1) & duplicated(potential_featTable3_pairings_updt_bis$pairing_with_feat1)])
  if(length(still_still_duplicated) != 0) {stop ( "something wrong as there are still some duplicates.... :( ")}
  
  
  
  ##  potential pairings from table 3 to table 2:
  
  
  potential_featTable3toTable2_pairings <- tibble(feat3 = pull(featINFO3_QCfil, 1),
                                                  pairing_with_feat2 = as.character(rep(NA, length(pull(featINFO3_QCfil, 1)))))
  
  
  for (i in 1:length(pull(featINFO3_QCfil, 1))) {
    
    this_feature_name <- pull(featINFO3_QCfil, 1)[i]
    this_INCHIKEY <- pull(featINFO3_QCfil, "INCHIKEY")[i]
    this_Formula <- pull(featINFO3_QCfil, "Formula")[i]
    this_AnnoLevel <- pull(featINFO3_QCfil, "AnnoLevel")[i]
    
    this_min_rt <- pull(featINFO3_QCfil, 2)[i] - (rt_window/2)
    this_max_rt <- pull(featINFO3_QCfil, 2)[i] + (rt_window/2)
    this_min_mz <- pull(featINFO3_QCfil, 3)[i] - (mz_widow/2)
    this_max_mz <- pull(featINFO3_QCfil, 3)[i] + (mz_widow/2)
    
    this_match_with_featINFO2_QCfil <- as.character(NA)
    
    this_featINFO2_QCfil_FIL <- featINFO2_QCfil[pull(featINFO2_QCfil,2) > this_min_rt & pull(featINFO2_QCfil,2) < this_max_rt & pull(featINFO2_QCfil,3) > this_min_mz & pull(featINFO2_QCfil,3) < this_max_mz,]
    
    if (length(pull(this_featINFO2_QCfil_FIL, 1)) != 0) {
      if (!is.na(this_INCHIKEY)) {
        this_featINFO2_QCfil_moreFIL <- this_featINFO2_QCfil_FIL[which(pull(this_featINFO2_QCfil_FIL, "INCHIKEY") == this_INCHIKEY),]
        if (length(pull(this_featINFO2_QCfil_moreFIL, 1)) == 1) {
          potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_moreFIL, 1)
        } else if (length(pull(this_featINFO2_QCfil_moreFIL, 1)) > 1) {
          closest_index <- which.min(abs(pull(this_featINFO2_QCfil_moreFIL, 2) - pull(featINFO3_QCfil, 2)[i]))
          potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_moreFIL, 1)[closest_index]
        } else {
          if (length(pull(this_featINFO2_QCfil_FIL, 1)) == 1) {
            potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_FIL, 1)
          } else {
            closest_index <- which.min(abs(pull(this_featINFO2_QCfil_FIL, 2) - pull(featINFO3_QCfil, 2)[i]))
            potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_FIL, 1)[closest_index]
          }
        }
      } else if (!is.na(this_Formula)) {
        this_featINFO2_QCfil_moreFIL <- this_featINFO2_QCfil_FIL[which(pull(this_featINFO2_QCfil_FIL, "Formula") == this_Formula),]
        if (length(pull(this_featINFO2_QCfil_moreFIL, 1)) == 1) {
          potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_moreFIL, 1)
        } else if (length(pull(this_featINFO2_QCfil_moreFIL, 1)) > 1) {
          closest_index <- which.min(abs(pull(this_featINFO2_QCfil_moreFIL, 2) - pull(featINFO3_QCfil, 2)[i]))
          potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_moreFIL, 1)[closest_index]
        } else {
          if (length(pull(this_featINFO2_QCfil_FIL, 1)) == 1) {
            potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_FIL, 1)
          } else {
            closest_index <- which.min(abs(pull(this_featINFO2_QCfil_FIL, 2) - pull(featINFO3_QCfil, 2)[i]))
            potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_FIL, 1)[closest_index]
          }
        }
      } else {
        if (length(pull(this_featINFO2_QCfil_FIL, 1)) == 1) {
          potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_FIL, 1)
        } else {
          closest_index <- which.min(abs(pull(this_featINFO2_QCfil_FIL, 2) - pull(featINFO3_QCfil, 2)[i]))
          potential_featTable3toTable2_pairings[i, "pairing_with_feat2"] <- pull(this_featINFO2_QCfil_FIL, 1)[closest_index]
        }
      }
    }
  }
  
  duplicated_potential_paring <- unique(potential_featTable3toTable2_pairings$pairing_with_feat2[which(!is.na(potential_featTable3toTable2_pairings$pairing_with_feat2) & duplicated(potential_featTable3toTable2_pairings$pairing_with_feat2))])
  
  potential_featTable3toTable2_pairings_updt <- potential_featTable3toTable2_pairings
  
  for(a in duplicated_potential_paring) {
    duplicated_potential_paring_fil <- filter(potential_featTable3toTable2_pairings, pairing_with_feat2 == a)
    
    this_featINFO3_QCfil_fil <- filter(featINFO3_QCfil, Alignment_ID %in% duplicated_potential_paring_fil$feat3)
    
    this_min_rt <- min(pull(this_featINFO3_QCfil_fil, 2)) - (rt_window/2)
    this_max_rt <- max(pull(this_featINFO3_QCfil_fil, 2)) + (rt_window/2)
    this_min_mz <- min(pull(this_featINFO3_QCfil_fil, 3)) - (mz_widow/2)
    this_max_mz <- max(pull(this_featINFO3_QCfil_fil, 3)) + (mz_widow/2)
    
    this_featINFO2_QCfil_fil <- featINFO2_QCfil[pull(featINFO2_QCfil,2) > this_min_rt & pull(featINFO2_QCfil,2) < this_max_rt & pull(featINFO2_QCfil,3) > this_min_mz & pull(featINFO2_QCfil,3) < this_max_mz,]
    
    if (length(pull(this_featINFO2_QCfil_fil, 1)) == length(pull(this_featINFO3_QCfil_fil, 1))) {
      
      sorted_indices1 <- order(pull(this_featINFO2_QCfil_fil, 2))
      sorted_indices2 <- order(pull(this_featINFO3_QCfil_fil, 2))
      
      new_pairing_little_tab <- tibble(feat3 = pull(this_featINFO3_QCfil_fil, 1)[sorted_indices2],
                                       pairing_with_feat2 = pull(this_featINFO2_QCfil_fil, 1)[sorted_indices1])
      
      potential_featTable3toTable2_pairings_updt[which(pull(potential_featTable3toTable2_pairings_updt, "feat3") %in% pull(new_pairing_little_tab, "feat3")),] <- new_pairing_little_tab
      
    } else if (length(pull(this_featINFO2_QCfil_fil, 1)) < length(pull(this_featINFO3_QCfil_fil, 1))) {
      
      if (all(pull(this_featINFO3_QCfil_fil, "AnnoLevel") == pull(this_featINFO3_QCfil_fil, "AnnoLevel")[1])) {
        
        short <- pull(this_featINFO2_QCfil_fil, 2)
        long_original <- pull(this_featINFO3_QCfil_fil, 2)
        long_reduced <- long_original
        
        indices_to_remove_from_long_original <- numeric()
        
        repeat {
          indices_to_remove_from_long_original <- c(indices_to_remove_from_long_original, which(long_original==long_reduced[which.max(abs(long_reduced - mean(short)))]))
          
          long_reduced <- long_reduced[-which.max(abs(long_reduced - mean(short)))]
          
          if(length(long_reduced) == length(short)) {
            break
          }
        }
        
        potential_featTable3toTable2_pairings_updt[which(pull(potential_featTable3toTable2_pairings_updt, "feat3") %in% pull(this_featINFO3_QCfil_fil, 1)[indices_to_remove_from_long_original]),"pairing_with_feat2"] <- NA
        
        this_featINFO3_QCfil_fil_reduced <- this_featINFO3_QCfil_fil[-indices_to_remove_from_long_original,]
        
        
        sorted_indices1 <- order(pull(this_featINFO2_QCfil_fil, 2))
        sorted_indices2 <- order(pull(this_featINFO3_QCfil_fil_reduced, 2))
        
        new_pairing_little_tab <- tibble(feat3 = pull(this_featINFO3_QCfil_fil_reduced, 1)[sorted_indices2],
                                         pairing_with_feat2 = pull(this_featINFO2_QCfil_fil, 1)[sorted_indices1])
        
        potential_featTable3toTable2_pairings_updt[which(pull(potential_featTable3toTable2_pairings_updt, "feat3") %in% pull(new_pairing_little_tab, "feat3")),] <- new_pairing_little_tab
        
      } else {
        
        short <- pull(this_featINFO2_QCfil_fil, 2)
        long_original <- pull(this_featINFO3_QCfil_fil, 2)
        levels_original <- pull(this_featINFO3_QCfil_fil, "AnnoLevel")
        long_reduced <- long_original
        levels_reduced <- levels_original
        
        indices_to_remove_from_long_original <- numeric()
        
        repeat {
          
          lowest_level_here <- max(from_AnnoLevel_to_ordered_number(levels_reduced)) %>% from_ordered_number_to_AnnoLevel()
          
          this_long_reduced <- long_reduced[which(levels_reduced == lowest_level_here)]
          
          indices_to_remove_from_long_original <- c(indices_to_remove_from_long_original, which(long_original==this_long_reduced[which.max(abs(this_long_reduced - mean(short)))]))
          
          levels_reduced <- levels_reduced[-which(long_reduced==long_original[which(long_original==this_long_reduced[which.max(abs(this_long_reduced - mean(short)))])])]
          long_reduced <- long_reduced[-which(long_reduced==long_original[which(long_original==this_long_reduced[which.max(abs(this_long_reduced - mean(short)))])])]
          
          
          if(length(long_reduced) == length(short)) {
            break
          }
        }
        
        potential_featTable3toTable2_pairings_updt[which(pull(potential_featTable3toTable2_pairings_updt, "feat3") %in% pull(this_featINFO3_QCfil_fil, 1)[indices_to_remove_from_long_original]),"pairing_with_feat2"] <- NA
        
        this_featINFO3_QCfil_fil_reduced <- this_featINFO3_QCfil_fil[-indices_to_remove_from_long_original,]
        
        
        sorted_indices1 <- order(pull(this_featINFO2_QCfil_fil, 2))
        sorted_indices2 <- order(pull(this_featINFO3_QCfil_fil_reduced, 2))
        
        new_pairing_little_tab <- tibble(feat3 = pull(this_featINFO3_QCfil_fil_reduced, 1)[sorted_indices2],
                                         pairing_with_feat2 = pull(this_featINFO2_QCfil_fil, 1)[sorted_indices1])
        
        potential_featTable3toTable2_pairings_updt[which(pull(potential_featTable3toTable2_pairings_updt, "feat3") %in% pull(new_pairing_little_tab, "feat3")),] <- new_pairing_little_tab
        
      }
      
    } else if (length(pull(this_featINFO2_QCfil_fil, 1)) > length(pull(this_featINFO3_QCfil_fil, 1))) {
      
      short <- pull(this_featINFO3_QCfil_fil, 2)
      long_original <- pull(this_featINFO2_QCfil_fil, 2)
      long_reduced <- long_original
      
      indices_to_remove_from_long_original <- numeric()
      
      repeat {
        indices_to_remove_from_long_original <- c(indices_to_remove_from_long_original, which(long_original==long_reduced[which.max(abs(long_reduced - mean(short)))]))
        
        long_reduced <- long_reduced[-which.max(abs(long_reduced - mean(short)))]
        
        if(length(long_reduced) == length(short)) {
          break
        }
      }
      
      this_featINFO2_QCfil_fil_reduced <- this_featINFO2_QCfil_fil[-indices_to_remove_from_long_original,]
      
      sorted_indices1 <- order(pull(this_featINFO2_QCfil_fil_reduced, 2))
      sorted_indices2 <- order(pull(this_featINFO3_QCfil_fil, 2))
      
      new_pairing_little_tab <- tibble(feat3 = pull(this_featINFO3_QCfil_fil, 1)[sorted_indices2],
                                       pairing_with_feat2 = pull(this_featINFO2_QCfil_fil_reduced, 1)[sorted_indices1])
      
      potential_featTable3toTable2_pairings_updt[which(pull(potential_featTable3toTable2_pairings_updt, "feat3") %in% pull(new_pairing_little_tab, "feat3")),] <- new_pairing_little_tab
      
      
    }
    
  }
  
  
  still_duplicated <- unique(potential_featTable3toTable2_pairings_updt$pairing_with_feat2[!is.na(potential_featTable3toTable2_pairings_updt$pairing_with_feat2) & duplicated(potential_featTable3toTable2_pairings_updt$pairing_with_feat2)])
  
  potential_featTable3toTable2_pairings_updt_bis <- potential_featTable3toTable2_pairings_updt
  
  
  for (b in still_duplicated) {
    duplicated_potential_paring_fil <- filter(potential_featTable3toTable2_pairings_updt, pairing_with_feat2 == b)
    
    this_featINFO2_QCfil_fil <- filter(featINFO2_QCfil, Alignment_ID %in% unique(duplicated_potential_paring_fil$pairing_with_feat2))
    
    this_featINFO3_QCfil_fil <- filter(featINFO3_QCfil, Alignment_ID %in% duplicated_potential_paring_fil$feat3)
    
    if (all(pull(this_featINFO3_QCfil_fil, "AnnoLevel") == pull(this_featINFO3_QCfil_fil, "AnnoLevel")[1])) {
      
      closest_index <- which.min(abs(pull(this_featINFO3_QCfil_fil, 2) - pull(this_featINFO2_QCfil_fil, 2)))
      
      potential_featTable3toTable2_pairings_updt_bis[which(pull(potential_featTable3toTable2_pairings_updt_bis, "feat3") %in% pull(this_featINFO3_QCfil_fil, 1)[-closest_index]), "pairing_with_feat2"] <- NA
      
    } else {
      
      highest_level_here <- min(from_AnnoLevel_to_ordered_number(pull(this_featINFO3_QCfil_fil, "AnnoLevel"))) %>% from_ordered_number_to_AnnoLevel()
      
      this_featINFO3_QCfil_more_fill <- filter(this_featINFO3_QCfil_fil, AnnoLevel == highest_level_here)
      this_featINFO3_QCfil_more_fill_discarded <- filter(this_featINFO3_QCfil_fil, AnnoLevel != highest_level_here)
      
      closest_index <- which.min(abs(pull(this_featINFO3_QCfil_more_fill, 2) - pull(this_featINFO2_QCfil_fil, 2)))
      
      potential_featTable3toTable2_pairings_updt_bis[which(pull(potential_featTable3toTable2_pairings_updt_bis, "feat3") %in% pull(this_featINFO3_QCfil_more_fill, 1)[-closest_index]  | pull(potential_featTable3toTable2_pairings_updt_bis, "feat3") %in% pull(this_featINFO3_QCfil_more_fill_discarded, 1)), "pairing_with_feat2"] <- NA
      
    }
    
  }
  
  
  still_still_duplicated <- unique(potential_featTable3toTable2_pairings_updt_bis$pairing_with_feat2[!is.na(potential_featTable3toTable2_pairings_updt_bis$pairing_with_feat2) & duplicated(potential_featTable3toTable2_pairings_updt_bis$pairing_with_feat2)])
  if(length(still_still_duplicated) != 0) {stop ( "something wrong as there are still some duplicates.... :( ")}
  
  
  
  
  #### combining in a single "pairings" table:
  
  if (!all(sort(potential_featTable3_pairings_updt_bis$feat3)==sort(potential_featTable3toTable2_pairings_updt_bis$feat3))) {stop("something wrong!")}
  
  
  ## some checks:
  
  pair_tale1_table2 <- length(potential_featTable2_pairings_updt_bis$feat2[which(!is.na(potential_featTable2_pairings_updt_bis$pairing_with_feat1))])
  pair_tale1_table3 <- length(potential_featTable3_pairings_updt_bis$feat3[which(!is.na(potential_featTable3_pairings_updt_bis$pairing_with_feat1))])
  pair_tale2_table3 <- length(potential_featTable3toTable2_pairings_updt_bis$feat3[which(!is.na(potential_featTable3toTable2_pairings_updt_bis$pairing_with_feat2))])
  
  ordering_combining <- ifelse(pair_tale1_table2 > pair_tale1_table3 & pair_tale1_table3 > pair_tale2_table3, "table1, then 2, then 3",
                               ifelse(pair_tale1_table3 > pair_tale2_table3 & pair_tale2_table3 > pair_tale1_table2, "table1, then 3, then2",
                                      ifelse(pair_tale1_table2 > pair_tale2_table3 & pair_tale2_table3 > pair_tale1_table3, "table2, then 1, then3",
                                             ifelse(pair_tale2_table3 > pair_tale1_table2 & pair_tale1_table2 > pair_tale1_table3, "table2, then 3, then1",
                                                    ifelse(pair_tale1_table3 > pair_tale1_table2 & pair_tale1_table2 > pair_tale2_table3, "table3, then 1, then2",
                                                           ifelse(pair_tale2_table3 > pair_tale1_table3 & pair_tale1_table3 > pair_tale1_table2, "table3, then 2, then1", NA))))))
  
  
  pairings <- add_column(featINFO3_QCfil[,1], Alignment_ID_table2 = as.character(rep(NA, length(pull(featINFO3_QCfil, 1)))), Alignment_ID_table1 = as.character(rep(NA, length(pull(featINFO3_QCfil, 1)))))
  colnames(pairings) <- c(paste0(colnames(featINFO3_QCfil)[1], "_", name3),
                          paste0(colnames(featINFO3_QCfil)[1], "_", name2),
                          paste0(colnames(featINFO3_QCfil)[1], "_", name1))
  
  for (fn2 in pull(featINFO2_QCfil, 1)) {
    if (fn2 %in% pull(potential_featTable3toTable2_pairings_updt_bis, "pairing_with_feat2")) {
      pairings[which(pull(pairings, 1) == pull(potential_featTable3toTable2_pairings_updt_bis, "feat3")[which(pull(potential_featTable3toTable2_pairings_updt_bis, "pairing_with_feat2")==fn2)]), 2] <- fn2
      
    } else {
      this_new_row <- pairings[0,]
      this_new_row[1,1] <- NA
      this_new_row[1,2] <- fn2
      this_new_row[1,3] <- NA
      pairings <- rbind(pairings, this_new_row)
    }
  }
  
  for (fn1 in pull(featINFO1_QCfil, 1)) {
    if (fn1 %in% pull(potential_featTable3_pairings_updt_bis, "pairing_with_feat1")) {
      if (is.na(pull(pairings, 3)[which(pull(pairings, 1) == pull(potential_featTable3_pairings_updt_bis, "feat3")[which(pull(potential_featTable3_pairings_updt_bis, "pairing_with_feat1")==fn1)])])) {
        pairings[which(pull(pairings, 1) == pull(potential_featTable3_pairings_updt_bis, "feat3")[which(pull(potential_featTable3_pairings_updt_bis, "pairing_with_feat1")==fn1)]), 3] <- fn1
      } else {
        this_new_row <- pairings[0,]
        this_new_row[1,1] <- NA
        this_new_row[1,2] <- NA
        this_new_row[1,3] <- fn1
        pairings <- rbind(pairings, this_new_row)
      }
    } else if (fn1 %in% pull(potential_featTable2_pairings_updt_bis, "pairing_with_feat1")) {
      if(is.na(pull(pairings, 3)[which(pull(pairings, 2) == pull(potential_featTable2_pairings_updt_bis, "feat2")[which(pull(potential_featTable2_pairings_updt_bis, "pairing_with_feat1")==fn1)])])) {
        pairings[which(pull(pairings, 2) == pull(potential_featTable2_pairings_updt_bis, "feat2")[which(pull(potential_featTable2_pairings_updt_bis, "pairing_with_feat1")==fn1)]), 3] <- fn1
      } else {
        this_new_row <- pairings[0,]
        this_new_row[1,1] <- NA
        this_new_row[1,2] <- NA
        this_new_row[1,3] <- fn1
        pairings <- rbind(pairings, this_new_row)
      }
    } else {
      this_new_row <- pairings[0,]
      this_new_row[1,1] <- NA
      this_new_row[1,2] <- NA
      this_new_row[1,3] <- fn1
      pairings <- rbind(pairings, this_new_row)
    }
  }
  
  
  
  
  if(!all(length(pull(pairings, 3)[!is.na(pull(pairings, 3))]) == length(pull(featINFO1_QCfil, 1)),
          all(sort(pull(pairings, 3)[!is.na(pull(pairings, 3))]) == sort(pull(featINFO1_QCfil, 1))),
          length(pull(pairings, 2)[!is.na(pull(pairings, 2))]) == length(pull(featINFO2_QCfil, 1)),
          length(potential_featTable2_pairings_updt_bis$feat2) == length(pull(featINFO2_QCfil, 1)),
          all(sort(pull(pairings, 2)[!is.na(pull(pairings, 2))]) == sort(pull(featINFO2_QCfil, 1))),
          all(sort(potential_featTable2_pairings_updt_bis$feat2) == sort(pull(featINFO2_QCfil, 1))),
          length(pull(pairings, 1)[!is.na(pull(pairings, 1))]) == length(pull(featINFO3_QCfil, 1)),
          length(potential_featTable3_pairings_updt_bis$feat3) == length(pull(featINFO3_QCfil, 1)),
          all(sort(pull(pairings, 1)[!is.na(pull(pairings, 1))]) == sort(pull(featINFO3_QCfil, 1))),
          all(sort(potential_featTable3_pairings_updt_bis$feat3) == sort(pull(featINFO3_QCfil, 1))))) { stop("something wrong!")}
  
  
  
  
  
  
  
  combined_featTable <- cbind(pairings[0,], featTable1[0,-1], featTable2[0,-1], featTable3[0,-1])
  combined_featTable[1:length(pull(pairings,1)),1:3] <- pairings
  
  for(fn1 in pull(combined_featTable, 3)[which(!is.na(pull(combined_featTable, 3)))]) {
    combined_featTable[which(pull(combined_featTable, 3) == fn1), colnames(featTable1[0,-1])] <- featTable1[which(pull(featTable1, 1) == fn1), -1]
  }
  
  for(fn2 in pull(combined_featTable, 2)[which(!is.na(pull(combined_featTable, 2)))]) {
    combined_featTable[which(pull(combined_featTable, 2) == fn2), colnames(featTable2[0,-1])] <- featTable2[which(pull(featTable2, 1) == fn2), -1]
  }
  
  for(fn3 in pull(combined_featTable, 1)[which(!is.na(pull(combined_featTable, 1)))]) {
    combined_featTable[which(pull(combined_featTable, 1) == fn3), colnames(featTable3[0,-1])] <- featTable3[which(pull(featTable3, 1) == fn3), -1]
  }
  
  
  if(!all(
    all((sort(pull(combined_featTable, 1)[which(!is.na(pull(combined_featTable, 1)))]) == sort(pull(featINFO3_QCfil, 1)))),
    all((sort(pull(combined_featTable, 2)[which(!is.na(pull(combined_featTable, 2)))]) == sort(pull(featINFO2_QCfil, 1)))),
    all((sort(pull(combined_featTable, 3)[which(!is.na(pull(combined_featTable, 3)))]) == sort(pull(featINFO1_QCfil, 1))))
  )) { stop("something wrong!")}
  
  return(as_tibble(combined_featTable))
}


