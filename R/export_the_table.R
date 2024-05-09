

#' Export the table
#'
#' It exports a data-frame or a matrix into the current working directory.
#' Do not use row names, as they will not be exported (but do use column names!).
#'
#' @param tab a dataframe or a matrix.
#' @param exprtname character of length 1. The desired name for the file to create (do not add here the file extension as it will be added automatically based on the next argument).
#' @param exprt_type one of the following: "txt", "csv", "xlsx". The desired type of file to create: a text file with the values tab_separated (txt), a text file with the values comma separated (csv), or an Excel file (xlsx).
#'
#' @return It creates a file in the current working director.
#'
#' @export
export_the_table <- function(tab, exprtname = "Table_name", exprt_type = "txt") {
  if (!(is.data.frame(tab) | is.matrix(tab))) {stop("tab must be a data-frame or a matrix")}
  
  if (length(exprtname) != 1) {stop('exprtname must be a character of length 1')}
  if (!is.character(exprtname)) {stop('exprtname must be a character of length 1, and not a missing value!')}
  if (is.na(exprtname)) {stop('exprtname must be a character of length 1, and not a missing value!')}
  
  if (!is.character(exprt_type)) {stop('exprt_type must be a character of length 1 containing one of the following: "txt", "csv", or "xlsx"')}
  if (length(exprt_type) != 1) {stop('exprt_type must be a character of length 1 containing one of the following: "txt", "csv", or "xlsx"')}
  if (!exprt_type%in%c("txt", "csv", "xlsx")) {stop('exprt_type must be a character of length 1 containing one of the following: "txt", "csv", or "xlsx"')}
  
  
  if (is.data.frame(tab)) {
    if (exprt_type == "txt") {
      write_tsv(tab, file = paste0(exprtname, ".txt"))
    } else if (exprt_type == "csv") {
      write_csv(tab, file = paste0(exprtname, ".csv"))
    } else if (exprt_type == "xlsx") {
      write_xlsx(tab, path = paste0(exprtname, ".xlsx"))
    }
  } else if (is.matrix(tab)) {
    if (exprt_type == "txt") {
      write.table(tab, file = paste0(exprtname, ".txt"), row.names=FALSE, col.names=TRUE, sep = "\t")
    } else if (exprt_type == "csv") {
      write.table(tab, file = paste0(exprtname, ".csv"), row.names=FALSE, col.names=TRUE, sep = ",")
    } else if (exprt_type == "xlsx") {
      write_xlsx(as_tibble(tab), path = paste0(exprtname, ".xlsx"))
    }
  }
}
