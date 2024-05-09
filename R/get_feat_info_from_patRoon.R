

#' Get a featINFO table from the patRoon outputs.
#'
#' Given the featureGroups, components, and MFsummary tables, it creates the featINFO table, i.e. the table with the full information for each feature.
#'
#' @param patRoon_featureGroups dataframe. Features groups and related intensities obtained after XCMS. This table should contain the feature group names in a column named "group", the retention times a column named "ret", the m/z ratio in a column named "mz", and the intensities in all other columns.
#' @param patRoon_MFsummary NULL or a dataframe obtained from converting as.data.table the output of the function generateCompounds from patRoon. At least the column named "group" and "score" must be present, and if multiple molecules are annotated for each feature group, the one with the top score as provided in the MFsummary_score_columns argument will be considered.
#' @param MFsummary_score_columns character of length 1 or more. The name of the column(s) of patRoon_MFsummary that will be used as prioritization score to assign the candidate. The first provided column will be used at first, if that score is tied, the second provided column will be considered, and so for. 
#' @param add_AnnoLevels logical. If TRUE, and if a MFsummary table was loaded with suitable individualMoNAScore, an additional column named "AnnoLevels" will be added, and the annotation levels will be calculated considering the cut-offs reported in https://doi.org/10.1007/s00216-022-04207-z.
#'
#' @return A tibble with the information for each feature.
#'
#' @export
get_feat_info_from_patRoon <- function(patRoon_featureGroups, patRoon_MFsummary = NULL, MFsummary_score_columns = c("individualMoNAScore", "score"), add_AnnoLevels = FALSE) {
  
  if (!is.data.frame(patRoon_featureGroups)) {stop("patRoon_featureGroups must be a data frame")}
  if (!all(c("group", "ret", "mz") %in% colnames(patRoon_featureGroups))) stop('patRoon_featureGroups must contain "group", "ret", "mz" as columns')
  if (any(duplicated(patRoon_featureGroups$group))) stop("the coulmn group of patRoon_featureGroups should not contain duplicated")
  
  feat_info_output <- select(patRoon_featureGroups, group, ret, mz)
  
  if (!is.null(patRoon_MFsummary)) {
    
    if (!is.data.frame(patRoon_MFsummary)) {stop("if not NULL, patRoon_MFsummary must be a data frame")}
    if (!"group"%in%colnames(patRoon_MFsummary)) {stop('patRoon_MFsummary must contain a column named "group"')}
    
    if (!(length(MFsummary_score_columns)>=1)) {stop("MFsummary_score_columns must contain at least one element")}
    if (!is.character(MFsummary_score_columns)) {stop("MFsummary_score_columns must be a character vector, with no missing values")}
    if (any(is.na(MFsummary_score_columns))) {stop("MFsummary_score_columns must not have missing values")}
    if (!all(MFsummary_score_columns %in% colnames(patRoon_MFsummary))) {stop('patRoon_MFsummary must contain the columns passed in MFsummary_score_columns')}
    if (!(all(map_lgl(select(patRoon_MFsummary, all_of(MFsummary_score_columns)), is.numeric)))) {stop('the columns of patRoon_MFsummary indicated with MFsummary_score_columns must contain numeric data')}
    
    patRoon_MFsummary_score_columns_arranged <- arrange_at(patRoon_MFsummary, MFsummary_score_columns, desc)
    
    patRoon_MFsummary_fil <- filter(patRoon_MFsummary_score_columns_arranged, !duplicated(group))
    
    feat_info_output <- left_join(x = feat_info_output, y = patRoon_MFsummary_fil, by = "group", suffix = c("_featGroup", "_MFsummary"))
    
    
    
    if (length(add_AnnoLevels)!=1) {stop("add_AnnoLevels must be exclusively TRUE or FALSE")}
    if (!is.logical(add_AnnoLevels)) {stop("add_AnnoLevels must be exclusively TRUE or FALSE")}
    if (is.na(add_AnnoLevels)) {stop("add_AnnoLevels must be exclusively TRUE or FALSE")}
    
    if (add_AnnoLevels) {
      
      if (!"individualMoNAScore"%in%colnames(patRoon_MFsummary)) {stop("Since add_AnnoLevels is TRUE, the patRoon_MFsummary must have a column named individualMoNAScore")}
      if (!is.numeric(patRoon_MFsummary$individualMoNAScore)) {stop("The column individualMoNAScore of patRoon_MFsummary must contain numeric data")}
      if (!"neutral_formula"%in%colnames(patRoon_MFsummary)) {stop("Since add_AnnoLevels is TRUE, the patRoon_MFsummary must have a column named neutral_formula")}
      if ("AnnoLevel"%in%colnames(feat_info_output)) {warning("A column named AnnoLevel was already present. Please, note that it has been completely replaced here")}
      
      feat_info_output <- add_column(feat_info_output, AnnoLevel = factor(as.character(rep(NA, length(pull(feat_info_output, 1)))), levels = c("2a", "3a", "3b", "4a", "5")), .after = 3)
      
      for (i in 1:length(feat_info_output$group)) {
        if (feat_info_output$group[i] %in% patRoon_MFsummary_score_columns_arranged$group) {
          MFsummary_PCL_fil <- filter(patRoon_MFsummary_score_columns_arranged, group == feat_info_output$group[i])
          
          
          feat_info_output[i, "AnnoLevel"] <- ifelse(feat_info_output$individualMoNAScore[i] >= 0.9, "2a",
                                                     ifelse(feat_info_output$individualMoNAScore[i] < 0.9 & feat_info_output$individualMoNAScore[i] >= 0.7, "3a",
                                                            ifelse(feat_info_output$individualMoNAScore[i] < 0.7 & feat_info_output$individualMoNAScore[i] >= 0.4, "3b",
                                                                   ifelse(all(MFsummary_PCL_fil$neutral_formula == MFsummary_PCL_fil$neutral_formula[1]), "4a", "5"))))
          
        } else {
          feat_info_output[i, "AnnoLevel"] <- "5"
        }
      }
    }
  }
  
  return(feat_info_output)
}


