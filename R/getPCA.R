

#' Get Principal components analysis.
#'
#' Given a table containing data, it performs the Principal components analysis.
#'
#' @param df dataframe. Containing data to plot.
#' @param v character. The names of the columns of df containing numerical data (ideally, they should be already centered and scaled!!).
#' @param s NULL or a character of length 1. The name of the column of df containing the sample names. Pass it only if you want sample names on the score plot.
#' @param f NULL or character of length 1. The name of the column of df containing the factor variable. Pass it only if you want colored points and ellipses on the score plot.
#' @param col_pal a character vector containing colors. If NULL, colors from the pals package will be used (see function build_long_vector_of_colors).
#' @param labels_on_loading logical. If TRUE, it will report the names of features the loading plot.
#' @param PC_to_plot character of length 2. The principal components to plots on the score and loading plots.
#'
#' @return A list with 4 objects:
#'
#' - `df_with_scores_table`: the df with additional columns containing the scores.
#' 
#' - `loadings_table`: the tibble containing the loadings.
#'
#' - `score_plot`: a ggplot object with the score plot.
#'
#' - `loading_plot`: a ggplot object with the loading plot.
#'
#'
#' @export
getPCA <- function(df, v, s = NULL, f = NULL, col_pal = NULL, labels_on_loading = TRUE, PC_to_plot = c("PC1", "PC2")) {
  
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  
  if (!is.character(v)) {stop("v must be a character")}
  if (any(is.na(v))) {stop("v must not contain mising values")}
  if (!all(v %in% colnames(df))) {stop("the names you indicate in v must correspond to names of columns in df")}
  if (!all(map_lgl(df[,v], is.numeric))) {stop("in df, the columns chosed with v must contain numerical values")}
  
  
  if (!is.null(s)) {
    if (length(s) != 1) {stop("s must contain one name")}
    if (is.na(s)) {stop("s must not contain mising value")}
    if (!is.character(s)) {stop("s must be a character")}
    if (!s %in% colnames(df)) {stop("the name you indicate in s must correspond to name of a column in df")}
    if (any(is.na(pull(df, s)))) {stop("in df, the column chosen with s must not contain missing values")}
  }
  
  if (!is.null(f)) {
    if (length(f) != 1) {stop("f must contain one name")}
    if (is.na(f)) {stop("f must not contain mising value")}
    if (!is.character(f)) {stop("f must be a character")}
    if (!f %in% colnames(df)) {stop("the name you indicate in f must correspond to name of a column in df")}
    if (!is.factor(pull(df, f))) {stop("in df, the column chosen with f must contain a factor variable")}
    if (any(is.na(pull(df, f)))) {stop("in df, the column chosen with f must not contain missing values")}
    
    if (!is.null(col_pal)) {
      if (!is.character(col_pal)) {stop("col_pal must be a character vector")}
      if (any(is.na(col_pal))) {stop("col_pal must not contain NAs")}
      
    } else {
      col_pal <- build_long_vector_of_colors()
    }
    
    if (length(col_pal)<length(levels(pull(df, f)))) {
      stop(paste0("There are ", length(levels(pull(df, f))), " groups, and you have specified only ", length(col_pal), " colors in col_pal"))
    } else {
      col_pal <- col_pal[1:length(levels(pull(df, f)))]
    }
    
    are.colors <- function (vect) {
      map_lgl(vect, ~tryCatch({
        is.matrix(col2rgb(.)) & ncol(col2rgb(.))>0
      }, error = function(e) {
        FALSE
      }))
    }
    if (!all(are.colors(col_pal))) {stop(paste0('col_pal must contain valid colors. In particular, these are not: "', paste(col_pal[!are.colors(col_pal)], collapse = '", "')), '"')}
    
    colors_of_groups <- col_pal
    names(colors_of_groups) <- levels(pull(df, f))
    
  }
  
  if (length(labels_on_loading)!=1) {stop("labels_on_loading must be exclusively TRUE or FALSE")}
  if (!is.logical(labels_on_loading)) {stop("labels_on_loading must be exclusively TRUE or FALSE")}
  if (is.na(labels_on_loading)) {stop("labels_on_loading must be exclusively TRUE or FALSE")}
  
  if (length(PC_to_plot) != 2) {stop("PC_to_plot must contain exactly 2 elements")}
  if (!is.character(PC_to_plot)) {stop("PC_to_plot must be a character vector")}
  if (any(is.na(PC_to_plot))) {stop("PC_to_plot must not contain NAs")}
  if (PC_to_plot[1] %in% colnames(df)) {stop(paste0("df already contains a column named ", PC_to_plot[1]))}
  if (PC_to_plot[2] %in% colnames(df)) {stop(paste0("df already contains a column named ", PC_to_plot[2]))}
  
  
  
  df_fil <- df[, v]
  
  pca_res <- prcomp(df_fil, retx = TRUE, center = FALSE, scale. = FALSE)
  
  df_with_scores_table <- bind_cols(df, as_tibble(pca_res$x))
  
  loadings_table <- bind_cols(tibble(variables = row.names(pca_res$rotation)),
                              as_tibble(pca_res$rotation))
  
  if (!PC_to_plot[1] %in% colnames(df_with_scores_table)) {stop(paste0(PC_to_plot[1], " is not contained in the scores tables"))}
  if (!PC_to_plot[2] %in% colnames(df_with_scores_table)) {stop(paste0(PC_to_plot[2], " is not contained in the scores tables"))}
  if (!PC_to_plot[1] %in% colnames(loadings_table)) {stop(paste0(PC_to_plot[1], " is not contained in the loadings tables"))}
  if (!PC_to_plot[2] %in% colnames(loadings_table)) {stop(paste0(PC_to_plot[2], " is not contained in the loadings tables"))}
  
  
  score_plot <- NULL
  
  if (is.null(s) & is.null(f)) {
    score_plot <- ggplot(df_with_scores_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
      geom_point() + 
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " (", round(summary(pca_res)$importance[2,1] * 100, 1), "%)")) +
      ylab(paste0(PC_to_plot[2], " (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  } else if (is.null(s) & !is.null(f)) {
    score_plot <- ggplot(df_with_scores_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]), color = !!sym(f), fill = !!sym(f))) +
      geom_point() + 
      stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2) + 
      scale_fill_manual(values = colors_of_groups) +
      scale_color_manual(values = colors_of_groups) + 
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " (", round(summary(pca_res)$importance[2,1] * 100, 1), "%)")) +
      ylab(paste0(PC_to_plot[2], " (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  } else if (!is.null(s) & is.null(f)) {
    score_plot <- ggplot(df_with_scores_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
      geom_point() + 
      geom_text_repel(aes(label = !!sym(s))) +
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " (", round(summary(pca_res)$importance[2,1] * 100, 1), "%)")) +
      ylab(paste0(PC_to_plot[2], " (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  } else if (!is.null(s) & !is.null(f)) {
    score_plot <- ggplot(df_with_scores_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]), color = !!sym(f), fill = !!sym(f))) +
      geom_point() + 
      stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2) + 
      geom_text_repel(aes(label = !!sym(s))) +
      scale_fill_manual(values = colors_of_groups) +
      scale_color_manual(values = colors_of_groups) + 
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " (", round(summary(pca_res)$importance[2,1] * 100, 1), "%)")) +
      ylab(paste0(PC_to_plot[2], " (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }
  
  
  loading_plot <- NULL
  
  if (labels_on_loading) {
    loading_plot <- ggplot(loadings_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
      geom_point() +
      geom_text_repel(aes(label = variables)) +
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " Loadings")) +
      ylab(paste0(PC_to_plot[2], " Loadings")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  } else {
    loading_plot <- ggplot(loadings_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
      geom_point() +
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " Loadings")) +
      ylab(paste0(PC_to_plot[2], " Loadings")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }
  
  
  
  final_list <- list(df_with_scores_table = df_with_scores_table,
                     loadings_table = loadings_table,
                     score_plot = score_plot,
                     loading_plot = loading_plot)
  
  return(final_list)
}

