
#### Function to process the QCs.

#' Filter a dataset of features considering pooled quality control samples.
#'
#' Given a dataframe with feature intensities from a non-targeted experiment and another dataframe containing information about sample types, it filters the dataframe of feature intensities  considering some defined cut-off in the quality control samples.
#'
#' @param featmatrix a dataframe of feature intensities. The first column should contain the feature name, each other columns should be related to an analysis of a sample and contains the feature intensities.
#' @param sampletype a dataframe containing two or three columns: the first one contains the name of samples reported in the featmatrix. The second column contains, for each sample, one of the following: "REMOVE", for samples to be discharged; "blank", for blank samples; "QC", for pooled quality control samples; "QC_half", for pooled quality control samples diluted by a half; "unknown", for other samples. The third column should contain the sample category that will be considered if sep_QC is TRUE.
#' @param sep_QC logical. If TRUE, separate QC filtration will be performed. For more details check: Frigerio et al. 2022 https://doi.org/10.3390/molecules27082580.
#' @param QC_to_merge charter vector. If sep_QC is TRUE, it should contain the names of all the groups of pooled QC samples to consider for the final merge of the filtered features
#' @param step1 logical. Whether to discharge features above a certain relative standard deviation percentage (RSD%) of QCs, defined by step1_cutoff
#' @param step1_cutoff numeric of length 1. If step1 is TRUE, features with a RSD% of QCs above this value will be considered not suitable.
#' @param step2 logical. Whether to discharge features below a certain QCs count%, defined by step2_cutoff.
#' @param step2_cutoff numeric of length 1. If step2 is TRUE, features with valid values (not NA) in less than a percentage of QCs specified in this value will be considered not suitable.
#' @param step3 logical. Whether to discharge features above a certain blank contribution%, defined by step3_cutoff.
#' @param step3_cutoff numeric of length 1. If step3 is TRUE, features with a blank contribution % (calculated as mean blank/mean QCs*100) higher that this value, will be considered not suitable.
#' @param step4 logical. Whether to discharge features not included in a certain range comparing QCs with the QC_half, defined by step4_cutoff.
#' @param step4_cutoff numeric of length 2. If step4 is TRUE,features that in the QC_half are not within this percentage range comapred to QCs will be considered not suitable.
#' @param rtrn_filtered_table logical. if TRUE returns the filtered table, if FALSE returns the non-filtered table (useful only if you want to check the parameters such as QC CV% or blank contribution in the features that would be filtered)
#' @param remove_results logical. If TRUE, returns the table of features, if FALSE, the last columns will report the results such as QC CV% and blank contribution
#' @param remove_QC_and_blanks logical. At the end of the processing, if TRUE it will remove columns containing QCs and blanks (so, if also remove_results is TRUE and rtrn_filtered_table is TRUE, at the end you will have a great table of feature intensities with only samples ready for the next processing)
#'
#'
#' @return a tibble with the feature intensities as result of the QC processing
#'
#' @export
QCs_process <- function(featmatrix, sampletype,
                        sep_QC = FALSE, QC_to_merge = NULL,
                        step1 = TRUE, step1_cutoff = 50,
                        step2 = TRUE, step2_cutoff = 50,
                        step3 = TRUE, step3_cutoff = 50,
                        step4 = FALSE, step4_cutoff = c(20, 80),
                        rtrn_filtered_table = TRUE, remove_results = FALSE, remove_QC_and_blanks = FALSE) {
  
  if (!is.data.frame(featmatrix)) {stop("featmatrix must be a data frame!")}
  if (!is.data.frame(sampletype)) {stop("sampletype must be a data frame!")}
  if (length(colnames(featmatrix)[-1]) != length(pull(sampletype, colnames(sampletype)[1]))) {stop("The first column of sampletype must contain the column names of the featmatrix (except for the first one)")}
  if (mean(colnames(featmatrix)[-1]  ==  pull(sampletype, colnames(sampletype)[1])) != 1) {stop("The first column of sampletype must contain the column names of the featmatrix (except for the first one)")}
  
  if (length(sep_QC)!=1) {stop("sep_QC must be exclusively TRUE or FALSE")}
  if (!is.logical(sep_QC)) {stop("sep_QC must be exclusively TRUE or FALSE")}
  if (is.na(sep_QC)) {stop("sep_QC must be exclusively TRUE or FALSE")}
  
  if (sep_QC) {
    if (!is.character(QC_to_merge)) {stop("QC_to_merge must contain a charter vector with the names of all the groups of QC to consider for the final merge of the filtered features")}
    if (any(is.na(QC_to_merge))) {stop("QC_to_merge must not contains NAs")}
    if (mean(QC_to_merge %in% pull(sampletype, colnames(sampletype)[3])) != 1) {stop("QC_to_merge must contain the charter vector with the names of all the groups of QC to consider for the final merge of the filtered features - apparently there is something not included in the third coloumn of sampletype")}
  }
  
  if (length(step1)!=1) {stop("step1 must be exclusively TRUE or FALSE")}
  if (!is.logical(step1)) {stop("step1 must be exclusively TRUE or FALSE")}
  if (is.na(step1)) {stop("step1 must be exclusively TRUE or FALSE")}
  if (step1) {
    if (length(step1_cutoff)!=1) {stop("step1_cutoff must be a numeric of length 1 containing the CV% cut-off above which features are discharged")}
    if (is.na(step1_cutoff)) {stop("step1_cutoff must be a numeric of length 1 containing the CV% cut-off above which features are discharged, must not be a missing value")}
    if (!is.numeric(step1_cutoff)) {stop("step1_cutoff must be a numeric of length 1 containing the CV% cut-off above which features are discharged")}
    if (step1_cutoff<0 | step1_cutoff>100) {stop("step1_cutoff must be between 0 and 100")}
  }
  
  if (length(step2)!=1) {stop("step2 must be exclusively TRUE or FALSE")}
  if (!is.logical(step2)) {stop("step2 must be exclusively TRUE or FALSE")}
  if (is.na(step2)) {stop("step2 must be exclusively TRUE or FALSE")}
  if (step2) {
    if (length(step2_cutoff)!=1) {stop("step2_cutoff must be a numeric of length 1 containing the cut-off of feature presence (in %) below which features are discharged")}
    if (is.na(step2_cutoff)) {stop("step2_cutoff must not be NA")}
    if (!is.numeric(step2_cutoff)) {stop("step2_cutoff must be a numeric of length 1 containing the cut-off of feature presence (in %) below which features are discharged")}
    if (step2_cutoff<0 | step2_cutoff>100) {stop("step2_cutoff must be between 0 and 100")}
  }
  
  if (length(step3)!=1) {stop("step3 must be exclusively TRUE or FALSE")}
  if (!is.logical(step3)) {stop("step3 must be exclusively TRUE or FALSE")}
  if (is.na(step3)) {stop("step3 must be exclusively TRUE or FALSE")}
  if (step3) {
    if (length(step3_cutoff)!=1) {stop("step3_cutoff must be a numeric of length 1 containing the cut-off of blank-contribution (in %) above which features are discharged")}
    if (is.na(step3_cutoff)) {stop("step3_cutoff must not be NA")}
    if (!is.numeric(step3_cutoff)) {stop("step3_cutoff must be a numeric of length 1 containing the cut-off of blank-contribution (in %) above which features are discharged")}
    if (step3_cutoff<0 | step3_cutoff>100) {stop("step3_cutoff must be between 0 and 100")}
  }
  
  if (length(step4)!=1) {stop("step4 must be exclusively TRUE or FALSE")}
  if (!is.logical(step4)) {stop("step4 must be exclusively TRUE or FALSE")}
  if (is.na(step4)) {stop("step4 must be exclusively TRUE or FALSE")}
  if (step4) {
    if (length(step4_cutoff)!=2) {stop("step4_cutoff must be a numeric vector of length 2 containing the %ratio accepted for the feature filtration by dilution")}
    if (any(is.na(step4_cutoff))) {stop("step4_cutoff values must not be NA")}
    if (!is.numeric(step4_cutoff)) {stop("step4_cutoff must be a numeric vector of length 2 containing the %ratio accepted for the feature filtration by dilution")}
    if (step4_cutoff[1]<0 | step4_cutoff[1]>100 | step4_cutoff[2]<0 | step4_cutoff[2]>100) {stop("step4_cutoff values must be between 0 and 100")}
  }
  
  if (length(rtrn_filtered_table)!=1) {stop("rtrn_filtered_table must be exclusively TRUE or FALSE")}
  if (!is.logical(rtrn_filtered_table)) {stop("rtrn_filtered_table must be exclusively TRUE or FALSE")}
  if (is.na(rtrn_filtered_table)) {stop("rtrn_filtered_table must be exclusively TRUE or FALSE")}
  
  if (length(remove_results)!=1) {stop("remove_results must be exclusively TRUE or FALSE")}
  if (!is.logical(remove_results)) {stop("remove_results must be exclusively TRUE or FALSE")}
  if (is.na(remove_results)) {stop("remove_results must be exclusively TRUE or FALSE")}
  
  if (length(remove_QC_and_blanks)!=1) {stop("remove_QC_and_blanks must be exclusively TRUE or FALSE")}
  if (!is.logical(remove_QC_and_blanks)) {stop("remove_QC_and_blanks must be exclusively TRUE or FALSE")}
  if (is.na(remove_QC_and_blanks)) {stop("remove_QC_and_blanks must be exclusively TRUE or FALSE")}
  
  
  sample_to_keep <- pull(sampletype, colnames(sampletype)[1])[pull(sampletype, colnames(sampletype)[2])!="REMOVE"]
  
  featmatrix_fil <- select(featmatrix, all_of(colnames(featmatrix)[1]), all_of(sample_to_keep))
  sampletype_fil <- sampletype[pull(sampletype, colnames(sampletype)[2])!="REMOVE",]
  
  if (mean(colnames(featmatrix_fil)[-1] == pull(sampletype_fil, colnames(sampletype_fil)[1])) != 1) {stop("Something wong after removing the samples to REMOVE!")}
  
  
  featmatrix_fil_QC <- featmatrix_fil
  
  
  for (i in 1:length(pull(sampletype_fil, colnames(sampletype_fil)[1]))) {
    if (pull(sampletype_fil, colnames(sampletype_fil)[2])[i] == "blank") {
      
      vect <- pull(featmatrix_fil, pull(sampletype_fil, colnames(sampletype_fil)[1])[i])
      vect[is.na(vect)] <- 0
      
      featmatrix_fil_QC[,pull(sampletype_fil, colnames(sampletype_fil)[1])[i]] <- vect
    }
  }
  
  blank_samples <- pull(sampletype_fil, colnames(sampletype_fil)[1])[pull(sampletype_fil, colnames(sampletype_fil)[2]) == "Blank" |  pull(sampletype_fil, colnames(sampletype_fil)[2]) == "blank"]
  featmatrix_fil_only_blank <- select(featmatrix_fil_QC, all_of(blank_samples))
  
  
  if (sep_QC == FALSE) {
    
    QC_samples <- pull(sampletype_fil, colnames(sampletype_fil)[1])[pull(sampletype_fil, colnames(sampletype_fil)[2]) == "QC" | pull(sampletype_fil, colnames(sampletype_fil)[2]) == "Qc"]
    QC_samples <- QC_samples[!is.na(QC_samples)]
    featmatrix_fil_only_QC <- select(featmatrix_fil_QC, all_of(QC_samples))
    
    
    if (step4 == TRUE) {QC_half_samples <- pull(sampletype_fil, colnames(sampletype_fil)[1])[pull(sampletype_fil, colnames(sampletype_fil)[2]) == "QC_half" |  pull(sampletype_fil, colnames(sampletype_fil)[2]) == "Qc_half"]}
    if (step4 == TRUE) {QC_half_samples <- QC_half_samples[!is.na(QC_half_samples)]}
    if (step4 == TRUE) {featmatrix_fil_only_QC_half <- select(featmatrix_fil_QC, all_of(QC_half_samples))}
    
    featmatrix_fil_QC <- mutate(featmatrix_fil_QC,
                                Mean_QCs = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))),
                                SD_QCs = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))),
                                RSD_QCs = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))),
                                COUNT_QCs = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))),
                                BLANK_CONTR = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))))
    
    if (step4 == TRUE) {
      if(length(QC_half_samples) != 0) {
        featmatrix_fil_QC <- mutate(featmatrix_fil_QC,
                                    DIL_EFF =  rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))))
      }
    }
    
    for (i in 1:length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))) {
      featmatrix_fil_QC$Mean_QCs[i] <- mean(as.double(featmatrix_fil_only_QC[i,]), na.rm=TRUE)
      featmatrix_fil_QC$SD_QCs[i] <- sd(as.double(featmatrix_fil_only_QC[i,]), na.rm=TRUE)
      featmatrix_fil_QC$RSD_QCs[i] <- featmatrix_fil_QC$SD_QCs[i]/featmatrix_fil_QC$Mean_QCs[i]*100
      featmatrix_fil_QC$COUNT_QCs[i] <- mean(!is.na(as.double(featmatrix_fil_only_QC[i,])))*100
      featmatrix_fil_QC$BLANK_CONTR[i] <- mean(as.double(featmatrix_fil_only_blank[i,]), na.rm= TRUE)/featmatrix_fil_QC$Mean_QCs[i]*100
      if (step4 == TRUE) {if(length(QC_half_samples) != 0) {featmatrix_fil_QC$DIL_EFF[i] <- mean(as.double(featmatrix_fil_only_QC_half[i,]), na.rm= TRUE)/featmatrix_fil_QC$Mean_QCs[i]*100}}
      
    }
    
    if (rtrn_filtered_table == FALSE) {
      
      final_tibble <- featmatrix_fil_QC
      
    } else if (rtrn_filtered_table == TRUE) {
      n_features_before <- length(pull(featmatrix_fil_QC, colnames(featmatrix_fil_QC[1])))
      
      featmatrix_fil_QC_processed <- featmatrix_fil_QC
      
      if (step1 == TRUE) {featmatrix_fil_QC_processed <- filter(featmatrix_fil_QC_processed, RSD_QCs < step1_cutoff)}
      if (step2 == TRUE) {featmatrix_fil_QC_processed <- filter(featmatrix_fil_QC_processed, COUNT_QCs > step2_cutoff)}
      if (step3 == TRUE) {featmatrix_fil_QC_processed <- filter(featmatrix_fil_QC_processed, BLANK_CONTR < step3_cutoff)}
      if (step4 == TRUE) {if (length(QC_half_samples) != 0) {featmatrix_fil_QC_processed <- filter(featmatrix_fil_QC_processed, DIL_EFF > step4_cutoff[1]) %>%
        filter(DIL_EFF < step4_cutoff[2])}}
      
      
      n_features_after <- length(pull(featmatrix_fil_QC_processed, colnames(featmatrix_fil_QC_processed[1])))
      
      cat(paste0("After the QC processing, the feature table was reduced from ", n_features_before, " to ", n_features_after, " features"))
      
      
      final_tibble <- featmatrix_fil_QC_processed
      
      if (remove_results) {
        final_tibble <- final_tibble %>%
          select(-Mean_QCs, -SD_QCs, -RSD_QCs, -COUNT_QCs, -BLANK_CONTR)
        if ("DIL_EFF" %in% colnames(final_tibble)) {final_tibble <- select(final_tibble, -DIL_EFF)}
      }
      
    }
    
  } else if (sep_QC == TRUE) {
    
    QC_group <- unique(pull(sampletype, colnames(sampletype)[3]))[!is.na(unique(pull(sampletype, colnames(sampletype)[3])))]
    
    featmatrixes <- vector("list", length = length(QC_group))
    text_to_print <- rep(NA, length(QC_group))
    
    for (u in 1:length(QC_group)) {
      
      QC_samples <- pull(sampletype_fil, colnames(sampletype_fil)[1])[(pull(sampletype_fil, colnames(sampletype_fil)[2]) == "QC" | pull(sampletype_fil, colnames(sampletype_fil)[2]) == "Qc") & pull(sampletype_fil, colnames(sampletype_fil)[3]) == QC_group[u]]
      QC_samples <- QC_samples[!is.na(QC_samples)]
      
      if (length(QC_samples) != 0) {
        featmatrix_fil_only_QC <- select(featmatrix_fil_QC, all_of(QC_samples))
        
        
        if (step4 == TRUE) {QC_half_samples <- pull(sampletype_fil, colnames(sampletype_fil)[1])[(pull(sampletype_fil, colnames(sampletype_fil)[2]) == "QC_half" |  pull(sampletype_fil, colnames(sampletype_fil)[2]) == "Qc_half") & pull(sampletype_fil, colnames(sampletype_fil)[3]) == QC_group[u]]}
        if (step4 == TRUE) {QC_half_samples <- QC_half_samples[!is.na(QC_half_samples)]}
        if (step4 == TRUE) {featmatrix_fil_only_QC_half <- select(featmatrix_fil_QC, all_of(QC_half_samples))}
        
        featmatrix_fil_QC <- mutate(featmatrix_fil_QC,
                                    Mean_QCs = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))),
                                    SD_QCs = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))),
                                    RSD_QCs = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))),
                                    COUNT_QCs = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))),
                                    BLANK_CONTR = rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))))
        if (step4 == TRUE) {if (length(QC_half_samples) != 0) {featmatrix_fil_QC <- mutate(featmatrix_fil_QC,
                                                                                           DIL_EFF =  rep(NA, length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))))}}
        
        for (i in 1:length(pull(featmatrix_fil_QC, names(featmatrix_fil_QC)[1]))) {
          featmatrix_fil_QC$Mean_QCs[i] <- mean(as.double(featmatrix_fil_only_QC[i,]), na.rm=TRUE)
          featmatrix_fil_QC$SD_QCs[i] <- sd(as.double(featmatrix_fil_only_QC[i,]), na.rm=TRUE)
          featmatrix_fil_QC$RSD_QCs[i] <- featmatrix_fil_QC$SD_QCs[i]/featmatrix_fil_QC$Mean_QCs[i]*100
          featmatrix_fil_QC$COUNT_QCs[i] <- mean(!is.na(as.double(featmatrix_fil_only_QC[i,])))*100
          featmatrix_fil_QC$BLANK_CONTR[i] <- mean(as.double(featmatrix_fil_only_blank[i,]), na.rm= TRUE)/featmatrix_fil_QC$Mean_QCs[i]*100
          if (step4 == TRUE) {if(length(QC_half_samples) != 0) {featmatrix_fil_QC$DIL_EFF[i] <- mean(as.double(featmatrix_fil_only_QC_half[i,]), na.rm= TRUE)/featmatrix_fil_QC$Mean_QCs[i]*100}}
        }
        
        if (rtrn_filtered_table == FALSE) {
          featmatrixes[[u]] <- featmatrix_fil_QC
        } else if (rtrn_filtered_table == TRUE) {
          n_features_before <- length(pull(featmatrix_fil_QC, colnames(featmatrix_fil_QC[1])))
          
          
          featmatrix_fil_QC_processed <- featmatrix_fil_QC
          
          if (step1 == TRUE) {featmatrix_fil_QC_processed <- filter(featmatrix_fil_QC_processed, RSD_QCs < step1_cutoff)}
          if (step2 == TRUE) {featmatrix_fil_QC_processed <- filter(featmatrix_fil_QC_processed, COUNT_QCs > step2_cutoff)}
          if (step3 == TRUE) {featmatrix_fil_QC_processed <- filter(featmatrix_fil_QC_processed, BLANK_CONTR < step3_cutoff)}
          if (step4 == TRUE) {if (length(QC_half_samples) != 0) {featmatrix_fil_QC_processed <- filter(featmatrix_fil_QC_processed, DIL_EFF > step4_cutoff[1]) %>%
            filter(DIL_EFF < step4_cutoff[2])}}
          
          n_features_after <- length(pull(featmatrix_fil_QC_processed, colnames(featmatrix_fil_QC_processed[1])))
          
          
          featmatrixes[[u]] <- featmatrix_fil_QC_processed
          
          text_to_print[u] <- paste0("After the QC processing, for ", QC_group[u], " the feature table was reduced from ", n_features_before, " to ", n_features_after, " fetures")
          
        }
      }
    }
    
    indexes_table_to_merge <- which(QC_group%in%QC_to_merge)
    
    merged_feat_matrix <-  tibble()
    
    for (w in indexes_table_to_merge) {
      merged_feat_matrix <- rbind(merged_feat_matrix, featmatrixes[[w]])
    }
    
    if (rtrn_filtered_table == FALSE) {
      final_tibble <- merged_feat_matrix
      
      if (remove_results) {
        final_tibble <- final_tibble %>%
          select(-Mean_QCs, -SD_QCs, -RSD_QCs, -COUNT_QCs, -BLANK_CONTR)
        if ("DIL_EFF" %in% colnames(final_tibble)) {final_tibble <- select(final_tibble, -DIL_EFF)}
      }
      
      
    } else if (rtrn_filtered_table == TRUE) {
      
      features_to_keep <- unique(pull(merged_feat_matrix, 1))
      
      final_tibble <- featmatrix[which(pull(featmatrix, 1) %in% features_to_keep),]
      
      
      text_to_print <- text_to_print[!is.na(text_to_print)]
      cat(paste0(text_to_print, collapse = "\n"))
      cat("\n")
      cat(paste0("Furthermore, while considering the feature matrix combined of ", paste(QC_to_merge, collapse = " "), " the final table contained ", length(pull(final_tibble, 1)), " features"))
      
      
      
    }
  }
  
  
  
  if (remove_QC_and_blanks) {
    sample_to_remove <- pull(sampletype, 1)[which(pull(sampletype, 2) %in% c("QC", "QC_half", "blank", "REMOVE"))]
    sample_to_keep <- colnames(final_tibble)[which(!colnames(final_tibble)%in%sample_to_remove)]
    
    final_tibble <- final_tibble[, sample_to_keep]
  }
  
  return(final_tibble)
}

