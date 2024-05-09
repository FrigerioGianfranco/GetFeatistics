

#' Generate a Volcano plot from the table of linear regression models
#'
#' Given a table generated with the function gentab_lm_long, it create a Volcano plot.
#'
#' @param tab a dataframe created with the function gentab_lm_long.
#' @param ind_main character of length 1. The independent variable to visualise in the Volcano plot. Important: for factor variables, it must me the name of the factor and the name of the level combined without space. e.g.: if the variable is "Sex" and the level is "Female", you must pass here "SexFemale".
#' @param x_values character of length 1. The name of the column in tab containing the value to plot as x.
#' @param y_values character of length 1. The name of the column in tab containing the value to plot as y.
#' @param dep_cat logical. Do you want to color the dependent variable based on a certain category?
#' @param category character of length 1. If dep_cat is TRUE, pass here the name of the column in tab that contains the categorisation.
#' @param cut_off_names numerical of length 1. Names of dependent variables will be shown only if above this cut-off.
#' @param line1 logical. Do you want to show a horizontal dotted line?
#' @param line1_position numerical of length 1. If line1 is TRUE, pass here the value of the dotted line.
#' @param line2 logical.  Do you want to show a second horizontal dotted line?
#' @param line2_position numerical of length 1. If line2 is TRUE, pass here the value of the second dotted line.
#'
#'
#' @return a ggplot object, with the Volcano plot.
#'
#' @export
Volcano_lm <- function(tab, ind_main, x_values = "variation_perc", y_values = "negative_log10fdr", dep_cat = FALSE, category = NULL, cut_off_names = -log10(0.0001), line1 = TRUE, line1_position = -log10(0.05), line2 = TRUE, line2_position = -log10(0.0001)) {
  
  if (!is.data.frame(tab)) {stop("tab must be a data frame created with the gentab_lm_long fucntion")}
  if (!"Dependent" %in% colnames(tab) |
      !"Independent" %in% colnames(tab) |
      !"N_observations" %in% colnames(tab)) {stop("tab must be a data frame created with the gentab_lm_long function, in particular it must contain Dependent, Independent, and N_observations coloumns")}
  
  if (length(ind_main)!=1) {stop("ind_main must be a character of lenght 1, and it should be the indipendent variable you want to show in the Volcano")}
  if (is.na(ind_main)) {stop("ind_main must be a character of lenght 1, indipendent variable you want to show in the Volcano")}
  if (!is.character(ind_main)) {stop("ind_main must be a character of lenght 1, indipendent variable you want to show in the Volcano.")}
  if (!ind_main%in%tab$Independent) {stop('ind_main must be the indipendent variable you want to show in the Volcano. If it is a factor variable, also reports the level; e.g.: if the variable is "Sex" and the level is "Female", you must pass here "SexFemale"')}
  
  if (any(check_if_fix_names_needed(ind_main))) {warning(paste0("Some coloumn names contain a special character or start with a number. Please, consider using the function fix_names before applying the current function. These are the names with issues: ",
                                                                paste0("'", paste0(ind_main[which(check_if_fix_names_needed(ind_main))], collapse = "', '"), "'")))}
  
  if (length(x_values)!=1) {stop("x_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as x-values")}
  if (is.na(x_values)) {stop("x_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as x-values")}
  if (!is.character(x_values)) {stop("x_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as x-values")}
  if (!x_values %in% colnames(tab)) {stop("x_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as x-values")}
  if (!is.numeric(pull(tab, x_values))) {stop("x_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as x-values")}
  
  if (length(y_values)!=1) {stop("y_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as y-values")}
  if (is.na(y_values))  {stop("y_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as y-values")}
  if (!is.character(y_values)) {stop("y_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as y-values")}
  if (!y_values %in% colnames(tab)) {stop("y_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as y-values")}
  if (!is.numeric(pull(tab, y_values))) {stop("y_values must be a character of lenght 1, and it should be a column in tab containing the values to pass as y-values")}
  
  if (length(dep_cat)!=1) {stop("dep_cat must be exclusively TRUE or FALSE")}
  if (is.na(dep_cat)) {stop("dep_cat must be exclusively TRUE or FALSE")}
  if (!is.logical(dep_cat)) {stop("dep_cat must be exclusively TRUE or FALSE")}
  
  if (dep_cat) {
    if (length(category)!=1) {stop("category must be a character of lenght 1, and it should be a column in tab containing the categories of the dependent variables")}
    if (is.na(category)) {stop("category must be a character of lenght 1, and it should be a column in tab containing the categories of the dependent variables")}
    if (!is.character(category)) {stop("category must be a character of lenght 1, and it should be a column in tab containing the categories of the dependent variables")}
    if (!category %in% colnames(tab)) {stop("category must be a character of lenght 1, and it should be a column in tab containing the categories of the dependent variables")}
  }
  
  if (length(cut_off_names)!=1) {stop("cut_off_names must be a single number")}
  if (is.na(cut_off_names)) {stop("cut_off_names must be a number")}
  if (!is.numeric(cut_off_names)) {stop("cut_off_names must be a number")}
  
  if (length(line1)!=1) {stop("line1 must be exclusively TRUE or FALSE")}
  if (is.na(line1)) {stop("line1 must be exclusively TRUE or FALSE")}
  if (!is.logical(line1)) {stop("line1 must be exclusively TRUE or FALSE")}
  
  if (line1) {
    if (length(line1_position)!=1) {stop("line1_position must be a single number")}
    if (is.na(line1_position)) {stop("line1_position must be a number")}
    if (!is.numeric(line1_position)) {stop("line1_position must be a number")}
  }
  
  if (length(line2)!=1) {stop("line2 must be exclusively TRUE or FALSE")}
  if (is.na(line2)) {stop("line2 must be exclusively TRUE or FALSE")}
  if (!is.logical(line2)) {stop("line2 must be exclusively TRUE or FALSE")}
  
  if (line2) {
    if (length(line2_position)!=1) {stop("line2_position must be a single number")}
    if (is.na(line2_position)) {stop("line2_position must be a number")}
    if (!is.numeric(line2_position)) {stop("line2_position must be a number")}
  }
  
  
  tab_fil <- filter(tab, Independent == ind_main)
  ind_covar <- unique(tab$Independent)[unique(tab$Independent) != "(Intercept)" & unique(tab$Independent) != ind_main]
  
  if (var(tab_fil$N_observations) != 0) {warning("Number of observations is not constant across dependent variables")}
  if (dep_cat == TRUE) {
    p <- ggplot(tab_fil) +
      geom_point(aes(x=!!sym(x_values), y=!!sym(y_values), colour=!!sym(category), fill= !!sym(category), shape=!!sym(category))) +
      theme_bw() +
      labs(x= ifelse(x_values=="variation_perc", "% Variation", ifelse(x_values=="beta", "beta_slopes", x_values)),
           y = ifelse(y_values == "negative_log10fdr", "\u2212log10(FDR P-value)", ifelse(y_values == "negative_log10p", "\u2212log10(P-value)", y_values)) ,
           title = paste0("Volcano plot - ", ind_main),
           subtitle = paste0("covariates: ", paste0(ind_covar, collapse = ", "), " \n Obs = ", ifelse(var(tab_fil$N_observations) == 0, mean(tab_fil$N_observations), paste0("from ", min(tab_fil$N_observations), " to ", max(tab_fil$N_observations))))) +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle=element_text(hjust=0.5, face="italic")) +
      geom_text_repel(data=tab_fil[which(pull(tab_fil,y_values) > cut_off_names),],
                      aes(x = !!sym(x_values), y = !!sym(y_values), label = Dependent))
    
    if (line1 == TRUE) {p <- p + geom_hline(yintercept = line1_position, linetype = "longdash")}
    if (line2 == TRUE) {p <- p + geom_hline(yintercept = line2_position, linetype = "dashed")}
    
  } else if (dep_cat == FALSE) {
    
    p <- ggplot(tab_fil) +
      geom_point(aes(x=!!sym(x_values), y=!!sym(y_values))) +
      theme_bw() +
      labs(x= ifelse(x_values=="variation_perc", "% Variation", ifelse(x_values=="beta", "beta_slopes", x_values)),
           y = ifelse(y_values == "negative_log10fdr", "\u2212log10(FDR P-value)", ifelse(y_values == "negative_log10p", "\u2212log10(P-value)", y_values)),
           title = paste0("Volcano plot - ", ind_main),
           subtitle = paste0("covariates: ", paste0(ind_covar, collapse = ", "), " \n Obs = ", ifelse(var(tab_fil$N_observations) == 0, mean(tab_fil$N_observations), paste0("from ", min(tab_fil$N_observations), " to ", max(tab_fil$N_observations))))) +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle=element_text(hjust=0.5, face="italic")) +
      geom_text_repel(data=tab_fil[which(pull(tab_fil,y_values) > cut_off_names),],
                      aes(x = !!sym(x_values), y = !!sym(y_values), label = Dependent))
    
    if (line1 == TRUE) {p <- p + geom_hline(yintercept = line1_position, linetype = "longdash")}
    if (line2 == TRUE) {p <- p + geom_hline(yintercept = line2_position, linetype = "dashed")}
  }
  
  return(p)
}

