

#' Get a table of feature intensities from the patRoon output.
#'
#' Given the featureGroups table obtained from patRoon, it creates a table with only the name of features in the first column and the intensities of features in each other column.
#'
#' @param patRoon_featureGroups NULL or a dataframe. Features groups and related intensities obtained after XCMS. This table should contain the feature group names in a column named "group", the retention times a column named "ret", the m/z ratio in a column named "mz", and the intensities in all other columns.
#' @param patRoon_featureGroups_file_name NULL or a character vector of length 1. If patRoon_featureGroups is NULL, this can be used and directly import the table with the .csv or .txt file name passed.
#'
#' @return A tibble with the feature intensities.
#'
#' @export
get_feat_table_from_patRoon <- function(patRoon_featureGroups = NULL, patRoon_featureGroups_file_name = NULL) {
  
  if (!is.null(patRoon_featureGroups)) {
    if (!is.data.frame(patRoon_featureGroups)) {stop("patRoon_featureGroups must be a data frame")}
  } else if (!is.null(patRoon_featureGroups_file_name)) {
    if (length(patRoon_featureGroups_file_name) != 1) {stop("patRoon_featureGroups_file_name must be a character vector of length 1")}
    if (!is.character(patRoon_featureGroups_file_name)) {stop("patRoon_featureGroups_file_name must be a character vector of length 1")}
    if (is.na(patRoon_featureGroups_file_name)) {stop("patRoon_featureGroups_file_name must be a character vector of length 1, not NA")}
    
    if (grepl("\\.txt$", patRoon_featureGroups_file_name, ignore.case = TRUE)) {
      patRoon_featureGroups <- suppressMessages(read_tsv(patRoon_featureGroups_file_name,
                                                         show_col_types = FALSE,
                                                         guess_max = Inf))
    } else if (grepl("\\.csv$", patRoon_featureGroups_file_name, ignore.case = TRUE)) {
      patRoon_featureGroups <- suppressMessages(read_csv(patRoon_featureGroups_file_name,
                                                         show_col_types = FALSE,
                                                         guess_max = Inf))
    } else {
      stop('patRoon_featureGroups_file_name must ends with ".txt" or ".csv"')
    }
  } else {
    stop("either patRoon_featureGroups or patRoon_featureGroups_file_name must not be NULL")
  }
  
  if (!all(c("group", "ret", "mz") %in% colnames(patRoon_featureGroups))) stop('patRoon_featureGroups must contain "group", "ret", "mz" as columns')
  if (any(duplicated(patRoon_featureGroups$group))) stop("the coulmn group of patRoon_featureGroups should not contain duplicated")
  
  feat_table_output <- select(patRoon_featureGroups, -ret, -mz)
  
  if ("adduct" %in% colnames(feat_table_output)) {
    feat_table_output <- select(feat_table_output, -adduct)
  }
  if ("neutralMass" %in% colnames(feat_table_output)) {
    feat_table_output <- select(feat_table_output, -neutralMass)
  }
  
  
  return(feat_table_output)
}


