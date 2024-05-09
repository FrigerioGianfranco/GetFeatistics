

#' Get a table of feature intensities from the patRoon output.
#'
#' Given the featureGroups table obtained from patRoon, it creates a table with only the name of features in the first column and the intensities of features in each other column.
#'
#' @param patRoon_featureGroups a dataframe. Features groups and related intensities obtained after XCMS. This table should contain the feature group names in a column named "group", the retention times a column named "ret", the m/z ratio in a column named "mz", and the intensities in all other columns.
#'
#'
#' @return A tibble with the feature intensities.
#'
#' @export
get_feat_table_from_patRoon <- function(patRoon_featureGroups) {
  
  if (!is.data.frame(patRoon_featureGroups)) {stop("patRoon_featureGroups must be a data frame")}
  if (!all(c("group", "ret", "mz") %in% colnames(patRoon_featureGroups))) stop('patRoon_featureGroups must contain "group", "ret", "mz" as columns')
  if (any(duplicated(patRoon_featureGroups$group))) stop("the coulmn group of patRoon_featureGroups should not contain duplicated")
  
  feat_table_output <- select(patRoon_featureGroups, -ret, -mz)
  
  return(feat_table_output)
}


