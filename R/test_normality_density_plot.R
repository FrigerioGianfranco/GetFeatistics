

#' Test normality by generating a density plot
#'
#' Given a dataframe and a set of numerical variables of that dataframe, it creates a density plot for each desired variable.
#'
#' @param df a dataframe.
#' @param v a character vector. Each element must correspond to a column name of the df, each of which must contain numeric values.
#'
#'
#' @return a list containing a number of element equal to the length of v. Each element is a density plot.
#'
#' @export
test_normality_density_plot <- function(df, v) {
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  if (mean(v %in% colnames(df)) != 1) {stop("v must be a vector containing the names of the coloumns of df that you want to apply the descriptive statistics")}
  if (mean(map_lgl(select(df, all_of(v)), is.numeric)) != 1) {stop("all coloumn passed in v must be numeric!")}
  
  
  if (any(check_if_fix_names_needed(v))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                         paste0("'", paste0(v[which(check_if_fix_names_needed(v))], collapse = "', '"), "'")))}
  
  
  density_plots_list <- vector(mode = "list", length = length(v))
  
  for (i in 1:length(v)) {
    
    This_density_plot <- ggdensity(pull(df,v[i]),
                                   main = paste0("Density plot of ", v[i])) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    density_plots_list[[i]] <- This_density_plot
    
    names(density_plots_list)[i] <- v[i]
    
    
  }
  
  return(density_plots_list)
}

