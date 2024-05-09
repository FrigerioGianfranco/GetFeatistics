

#' Get the annotation levels.
#'
#' Given a featINFO table obtained from MSDIAL with the function get_feat_info_from_MSDial, it adds a column with the annotation levels according to https://doi.org/10.1007/s00216-022-04207-z.
#'
#' @param feat_info_table_MSDial a dataframe. A featINFO table created with get_feat_info_from_MSDial.
#'
#'
#' @return A tibble, the featINFO table with an additional column with the annotation levels.
#'
#' @export
get_AnnoLevels_MSDial <- function(feat_info_table_MSDial) {
  
  if (!is.data.frame(feat_info_table_MSDial)) {stop("feat_info_table_MSDial must be a data frame")}
  if (!"Matched_peaks_count"%in%colnames(feat_info_table_MSDial)) {stop("feat_info_table_MSDial must contain a column named Matched_peaks_count")}
  if (!is.numeric(feat_info_table_MSDial$Matched_peaks_count)) {stop("The column Matched_peaks_count of feat_info_table_MSDial must contain numeric data")}
  if (!"Weighted_dot_product"%in%colnames(feat_info_table_MSDial)) {stop("feat_info_table_MSDial must contain a column named Weighted_dot_product")}
  if (!is.numeric(feat_info_table_MSDial$Weighted_dot_product)) {stop("The column Weighted_dot_product of feat_info_table_MSDial must contain numeric data")}
  if (!"Matched_peaks_percentage"%in%colnames(feat_info_table_MSDial)) {stop("feat_info_table_MSDial must contain a column named Matched_peaks_percentage")}
  if (!is.numeric(feat_info_table_MSDial$Matched_peaks_percentage)) {stop("The column Matched_peaks_percentage of feat_info_table_MSDial must contain numeric data")}
  if (!"INCHIKEY"%in%colnames(feat_info_table_MSDial)) {stop("feat_info_table_MSDial must contain a column named INCHIKEY")}
  if (!"Formula"%in%colnames(feat_info_table_MSDial)) {stop("feat_info_table_MSDial must contain a column named Formula")}
  if (!"MS_MS_assigned"%in%colnames(feat_info_table_MSDial)) {stop("feat_info_table_MSDial must contain a column named MS_MS_assigned")}
  if (!is.logical(feat_info_table_MSDial$MS_MS_assigned)) {stop("The column MS_MS_assigned of feat_info_table_MSDial must contain logical data (TRUE or FALSE)")}
  if ("AnnoLevel"%in%colnames(feat_info_table_MSDial)) {warning("feat_info_table_MSDial already contains a column named AnnoLevel. Please, note that it has been completely replaced here")}
  
  feat_info_table_MSDial_annolevels <- add_column(feat_info_table_MSDial, AnnoLevel = as.character(NA), .after = 3) %>%
    mutate(AnnoLevel = ifelse(MS_MS_assigned == TRUE & !is.na(Matched_peaks_count) & Matched_peaks_count >= 3 & !is.na(Weighted_dot_product) & Weighted_dot_product >= 0.7 & !is.na(Matched_peaks_percentage) & Matched_peaks_percentage >= 0.5 & !is.na(INCHIKEY), "2a",
                              ifelse(MS_MS_assigned == TRUE & !is.na(Matched_peaks_count) & Matched_peaks_count >= 3 & !is.na(Weighted_dot_product) & Weighted_dot_product >= 0.7 & !is.na(Matched_peaks_percentage) & Matched_peaks_percentage >= 0.5 & is.na(INCHIKEY), "2b",
                                     ifelse(MS_MS_assigned == TRUE & !is.na(Matched_peaks_count) & Matched_peaks_count >= 3 & !is.na(Weighted_dot_product) & Weighted_dot_product >= 0.5 & Weighted_dot_product < 0.7 & !is.na(Matched_peaks_percentage) & Matched_peaks_percentage >= 0.5, "3a",
                                            ifelse(MS_MS_assigned == TRUE & !is.na(Matched_peaks_count) & Matched_peaks_count < 3 & !is.na(Weighted_dot_product) & Weighted_dot_product >= 0.5 & !is.na(Matched_peaks_percentage) & Matched_peaks_percentage >= 0.5 & !is.na(INCHIKEY), "3b",
                                                   ifelse(MS_MS_assigned == TRUE & !is.na(Matched_peaks_count) & Matched_peaks_count < 3 & !is.na(Weighted_dot_product) & Weighted_dot_product >= 0.5 & !is.na(Matched_peaks_percentage) & Matched_peaks_percentage >= 0.5 & is.na(INCHIKEY), "3c",
                                                          ifelse(MS_MS_assigned == TRUE & !is.na(Weighted_dot_product) & Weighted_dot_product < 0.5 & !is.na(Matched_peaks_percentage) & Matched_peaks_percentage < 0.5, "4a",
                                                                 ifelse(MS_MS_assigned == FALSE & !is.na(Formula), "4a", "5"))))))))
  
  
  feat_info_table_MSDial_annolevels$AnnoLevel <- factor(feat_info_table_MSDial_annolevels$AnnoLevel, levels = c("1", "2a", "2b", "3a", "3b", "3c", "4a", "5"))
  
  return(feat_info_table_MSDial_annolevels)
}

