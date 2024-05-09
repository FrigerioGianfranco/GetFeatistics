

#' Create some simple boxplots.
#'
#' Given a table containing data, it create boxplots for each desired variable.
#'
#' @param df dataframe. Containing data to plot.
#' @param v character. The names of the columns of df containing numerical data to plot
#' @param f character of length 1. The names of the factor variable used to group the boxplots.
#'
#'
#'
#' @return A ggplot object (if v is only 1), or a list of ggplots (if v contains 2 or more elements).
#'
#' @export
getBoxplots <- function(df, v, f) {
  
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  if (!is.character(v)) {stop("v must be a character")}
  if (any(is.na(v))) {stop("v must not contain mising values")}
  if (!all(v %in% colnames(df))) {stop("the names you indicate in v must correspond to names of columns in df")}
  if (!all(map_lgl(df[,v], is.numeric))) {stop("in df, the columns chosed with v must contain numerical values")}
  
  if (length(f) != 1) {stop("f must contain one name")}
  if (is.na(f)) {stop("f must not contain mising value")}
  if (!is.character(f)) {stop("f must be a character")}
  if (!f %in% colnames(df)) {stop("the name you indicate in f must correspond to name of a column in df")}
  if (!is.factor(pull(df, f))) {stop("in df, the column chosen with f must contain a factor variable")}
  if (any(is.na(pull(df, f)))) {stop("in df, the column chosen with f must not contain missing values")}
  
  
  if (length(v) == 1) {
    
    the_plot <- ggplot(data = df, aes(y = !!sym(v), x = !!sym(f))) +
      geom_boxplot(colour = "black", fill = "#56B4E9") +
      scale_y_continuous(name = v) +
      theme_bw()
    
    return(the_plot)
    
  } else if (length(v) > 1) {
    
    
    boxplots_list <- vector(mode = "list", length = length(v))
    
    for (this_v in v) {
      
      this_plot <- ggplot(data = df, aes(y = !!sym(this_v), x = !!sym(f))) +
        geom_boxplot(colour = "black", fill = "#56B4E9") +
        scale_y_continuous(name = this_v) +
        theme_bw()
      
      boxplots_list[[which(v == this_v)]] <- this_plot
      
      
      
    }
    
    return(boxplots_list)
    
  }
}

