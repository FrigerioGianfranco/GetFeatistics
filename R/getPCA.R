

#' Get Principal components analysis.
#'
#' Given a table containing data, it performs the Principal components analysis.
#'
#' @param df dataframe. Containing data to plot.
#' @param v character. The names of the columns of df containing numerical data (ideally, they should be already centered and scaled!!).
#' @param s NULL or a character of length 1. The name of the column of df containing the sample names. Pass it only if you want sample names on the score plot.
#' @param f NULL or character of length 1. The name of the column of df containing the factor variable. Pass it only if you want colored points and ellipses on the score plot.
#' @param dfv NULL or a dataframe. The first column must contain v. To this dataframe the lodgings table will be added.
#' @param sv NULL or character. The name of the column of dfv containing the names you want to put on the loading plot. Pass it only if you want those names on the loading plot.
#' @param fv NULL or character. The name of the column of dfv containing the factor variable. Pass it only if you want colored points and ellipses on the loading plot.
#' @param labels_on_loading logical. Even if dfv and/or sv are NULL, if this argument is set to TRUE, the loading plot will report v as labels.
#' @param center logical. Whether the variables should be shifted to be zero centered (as in the prcomp function).
#' @param scale. logical. whether the variables should be scaled to have unit variance before the analysis takes place (as in prcomp function).
#' @param col_pal a character vector containing colors for f. If NULL, colors from the pals package will be used (see function build_long_vector_of_colors).
#' @param col_pal_fv a character vector containing colors for fv. If NULL, colors from the pals package will be used (see function build_long_vector_of_colors).
#' @param PC_to_plot character of length 2. The principal components to plots on the score and loading plots.
#' @param ellipses_on_score logical. If you specified f and this is TRUE, ellipses will be added to the score plot.
#' @param ellipses_on_loading logical. If you specified fv and this is TRUE, ellipses will be added to the score plot.
#'
#' @return A list with 4 objects:
#'
#' - `df_with_scores_table`: the df with additional columns containing the scores.
#' 
#' - `dfv_with_loadings_table`: the dfv with additional columns containing containing the loadings.
#'
#' - `score_plot`: a ggplot object with the score plot.
#'
#' - `loading_plot`: a ggplot object with the loading plot.
#'
#'
#' @export
getPCA <- function(df, v, s = NULL, f = NULL, dfv = NULL, sv = NULL, fv = NULL, labels_on_loading = TRUE,
                   center = FALSE, scale. = FALSE,
                   col_pal = NULL, col_pal_fv = NULL , PC_to_plot = c("PC1", "PC2"),
                   ellipses_on_score = TRUE, ellipses_on_loading = FALSE) {
  
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
  
  if (!is.null(dfv)) {
    if (!is.data.frame(dfv)) {stop("if not NULL, dfv must be a data frame!")}
    if (nrow(dfv) < length(v)) {stop("if not NULL, dfv must be a data frame with a number of rows not less than the length of v")}
    if (!all(v %in% pull(dfv, 1))) {stop(paste0('if not NULL, the first column of dfv must contain v. The following element of v are not present: "',
                                                paste0(v[which(!v%in%pull(dfv, 1))], collapse = '", "'), '"'))}
    
    dfv_fil <- dfv[which(pull(dfv, 1) %in% v),]
    
    if (any(duplicated(pull(dfv_fil, 1)))) {stop(paste0('the first column of dfv must not contain duplicated in the elements of v. The following are dupicated: "',
                                                    paste0(pull(dfv_fil, 1)[which(duplicated(pull(dfv_fil, 1)))], collapse = '", "'), '"'))}
    
    if (!is.null(sv)) {
      if (length(sv) != 1) {stop("sv must contain one name")}
      if (is.na(sv)) {stop("sv must not be a missing value")}
      if (!is.character(sv)) {stop("sv must be a character")}
      if (!sv %in% colnames(dfv)) {stop("the name you indicate in sv must correspond to name of a column in dfv")}
    }
    
    if (!is.null(fv)) {
      if (length(fv) != 1) {stop("fv must contain one name")}
      if (is.na(fv)) {stop("fv must not be a missing value")}
      if (!is.character(fv)) {stop("if not NULL, fv must be a character")}
      if (!all(fv %in% colnames(dfv))) {stop("if not NULL, the names you indicate in fv must correspond to names of columns in dfv")}
      if (!is.factor(pull(dfv, fv))) {stop("in dfv, the column chosen with fv must contain a factor variable")}
    }
  }
  
  if (length(labels_on_loading)!=1) {stop("labels_on_loading must be exclusively TRUE or FALSE")}
  if (!is.logical(labels_on_loading)) {stop("labels_on_loading must be exclusively TRUE or FALSE")}
  if (is.na(labels_on_loading)) {stop("labels_on_loading must be exclusively TRUE or FALSE")}
  
  if (length(center)!=1) {stop("center must be exclusively TRUE or FALSE")}
  if (!is.logical(center)) {stop("center must be exclusively TRUE or FALSE")}
  if (is.na(center)) {stop("center must be exclusively TRUE or FALSE")}
  
  if (length(scale.)!=1) {stop("scale. must be exclusively TRUE or FALSE")}
  if (!is.logical(scale.)) {stop("scale. must be exclusively TRUE or FALSE")}
  if (is.na(scale.)) {stop("scale. must be exclusively TRUE or FALSE")}
  
  
  used_the_long_vector_of_colors <- FALSE
  
  are.colors <- function (vect) {
    map_lgl(vect, ~tryCatch({
      is.matrix(col2rgb(.)) & ncol(col2rgb(.))>0
    }, error = function(e) {
      FALSE
    }))
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
      used_the_long_vector_of_colors <- TRUE
    }
    
    if (length(col_pal)<length(levels(pull(df, f)))) {
      stop(paste0("There are ", length(levels(pull(df, f))), " groups, and you have specified only ", length(col_pal), " colors in col_pal"))
    } else {
      col_pal <- col_pal[1:length(levels(pull(df, f)))]
    }
    
    if (!all(are.colors(col_pal))) {stop(paste0('col_pal must contain valid colors. In particular, these are not: "', paste(col_pal[!are.colors(col_pal)], collapse = '", "')), '"')}
    
    colors_of_groups <- col_pal
    if (is.null(names(colors_of_groups))) {
      names(colors_of_groups) <- levels(pull(df, f))
    } else {
      if (any(duplicated(names(colors_of_groups)))) {stop("col_pal has some duplicated in the names")}
      if (!all(names(colors_of_groups) %in% levels(pull(df, f)) & levels(pull(df, f)) %in% names(colors_of_groups))) {stop("the names of col_pal don't correspond to the levels of f")}
    }
  }
  
  if (length(PC_to_plot) != 2) {stop("PC_to_plot must contain exactly 2 elements")}
  if (!is.character(PC_to_plot)) {stop("PC_to_plot must be a character vector")}
  if (any(is.na(PC_to_plot))) {stop("PC_to_plot must not contain NAs")}
  
  
  if (!is.null(dfv) & !is.null(fv)) {
    if (!is.null(col_pal_fv)) {
      if (!is.character(col_pal_fv)) {stop("col_pal_fv must be a character vector")}
      if (any(is.na(col_pal_fv))) {stop("col_pal_fv must not contain NAs")}
      
    } else {
      if (used_the_long_vector_of_colors) {
        col_pal_fv <- build_long_vector_of_colors()[(length(col_pal)+1):length(build_long_vector_of_colors())]
      } else {
        col_pal_fv <- build_long_vector_of_colors()
      }
    }
    
    if (length(col_pal_fv)<length(levels(pull(dfv, fv)))) {
      stop(paste0("There are ", length(levels(pull(dfv, fv))), " groups in f of dfv, and only ", length(col_pal_fv), " colors have been specified in col_pal_fv"))
    } else {
      col_pal_fv <- col_pal_fv[1:length(levels(pull(dfv, fv)))]
    }
    
    if (!all(are.colors(col_pal_fv))) {stop(paste0('col_pal_fv must contain valid colors. In particular, these are not: "', paste(col_pal_fv[!are.colors(col_pal_fv)], collapse = '", "')), '"')}
    
    colors_of_groups_fv <- col_pal_fv
    if (is.null(names(colors_of_groups_fv))) {
      names(colors_of_groups_fv) <- levels(pull(dfv, fv))
    } else {
      if (any(duplicated(names(colors_of_groups_fv)))) {"col_pal_fv has some duplicated in the names"}
      if (!all(names(colors_of_groups_fv) %in% levels(pull(dfv, fv)) & levels(pull(dfv, fv)) %in% names(colors_of_groups_fv))) {stop("the names of col_pal_fv don't correspond to the levels of fv")}
    }
  }
  
  if (length(ellipses_on_score)!=1) {stop("ellipses_on_score must be exclusively TRUE or FALSE")}
  if (!is.logical(ellipses_on_score)) {stop("ellipses_on_score must be exclusively TRUE or FALSE")}
  if (is.na(ellipses_on_score)) {stop("ellipses_on_score must be exclusively TRUE or FALSE")}
  
  if (length(ellipses_on_loading)!=1) {stop("ellipses_on_loading must be exclusively TRUE or FALSE")}
  if (!is.logical(ellipses_on_loading)) {stop("ellipses_on_loading must be exclusively TRUE or FALSE")}
  if (is.na(ellipses_on_loading)) {stop("ellipses_on_loading must be exclusively TRUE or FALSE")}
  
  
  
  
  
  
  df_fil <- df[, v]
  
  pca_res <- prcomp(df_fil, retx = TRUE, center = center, scale. = scale.)
  
  if (any(colnames(as_tibble(pca_res$x)) %in% colnames(df))) {
    warning(paste0("The following column were already present in df, and they are replaced with this function!\n",
                   paste0(colnames(as_tibble(pca_res$x))[which(colnames(as_tibble(pca_res$x)) %in% colnames(df))], collapse = " ")))
    
    df <- df[, colnames(df)[which(!colnames(df)%in%colnames(as_tibble(pca_res$x)))]]
  }
  
  df_with_scores_table <- bind_cols(df, as_tibble(pca_res$x))
  
  
  loadings_table <- bind_cols(tibble(variables = row.names(pca_res$rotation)),
                              as_tibble(pca_res$rotation))
  
  if (length(v) != nrow(pca_res$rotation)) {stop("rows on loading don't correspond to v. That's wired, ask to Gianfranco to check what happened!!")}
  if (!all(row.names(pca_res$rotation) == v)) {stop("rows on loading don't correspond to v. That's wired, ask to Gianfranco to check what happened!!")}
  
  if (is.null(dfv)) {
    dfv_with_loadings_table <- loadings_table
    dfv_with_loadings_table_fil <- dfv_with_loadings_table
  } else {
    colnames(loadings_table)[1] <- colnames(dfv)[1]
    
    if (any(colnames(loadings_table)[-1] %in% colnames(dfv))) {
      warning(paste0("The following column were already present in dfv, and they are replaced with this function!\n",
                     paste0(colnames(loadings_table)[-1][which(colnames(loadings_table)[-1] %in% colnames(dfv))], collapse = " ")))
      
      dfv <- dfv[, colnames(dfv)[which(!colnames(dfv)%in%colnames(loadings_table)[-1])]]
    }
    
    dfv_with_loadings_table <- left_join(dfv, loadings_table, by = colnames(dfv)[1])
    dfv_with_loadings_table_fil <- dfv_with_loadings_table[which(pull(dfv, 1) %in% v),]
  }
  
  if (!PC_to_plot[1] %in% colnames(df_with_scores_table)) {stop(paste0(PC_to_plot[1], " is not contained in the scores tables"))}
  if (!PC_to_plot[2] %in% colnames(df_with_scores_table)) {stop(paste0(PC_to_plot[2], " is not contained in the scores tables"))}
  if (!PC_to_plot[1] %in% colnames(dfv_with_loadings_table)) {stop(paste0(PC_to_plot[1], " is not contained in the loadings tables"))}
  if (!PC_to_plot[2] %in% colnames(dfv_with_loadings_table)) {stop(paste0(PC_to_plot[2], " is not contained in the loadings tables"))}
  
  
  score_plot <- NULL
  
  if (is.null(s) & is.null(f)) {
    score_plot <- ggplot(df_with_scores_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
      geom_point() + 
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " (", round(summary(pca_res)$importance[2,PC_to_plot[1]] * 100, 1), "%)")) +
      ylab(paste0(PC_to_plot[2], " (", round(summary(pca_res)$importance[2,PC_to_plot[2]] * 100, 1), "%)")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  } else if (is.null(s) & !is.null(f)) {
    score_plot <- ggplot(df_with_scores_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]), color = !!sym(f), fill = !!sym(f))) +
      geom_point() + scale_fill_manual(values = colors_of_groups) +
      scale_color_manual(values = colors_of_groups) + 
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " (", round(summary(pca_res)$importance[2,PC_to_plot[1]] * 100, 1), "%)")) +
      ylab(paste0(PC_to_plot[2], " (", round(summary(pca_res)$importance[2,PC_to_plot[2]] * 100, 1), "%)")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
    
    if (ellipses_on_score) {
      score_plot <- score_plot +
        stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2)
    }
  } else if (!is.null(s) & is.null(f)) {
    score_plot <- ggplot(df_with_scores_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
      geom_point() + 
      geom_text_repel(aes(label = !!sym(s))) +
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " (", round(summary(pca_res)$importance[2,PC_to_plot[1]] * 100, 1), "%)")) +
      ylab(paste0(PC_to_plot[2], " (", round(summary(pca_res)$importance[2,PC_to_plot[2]] * 100, 1), "%)")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  } else if (!is.null(s) & !is.null(f)) {
    score_plot <- ggplot(df_with_scores_table, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]), color = !!sym(f), fill = !!sym(f))) +
      geom_point() + 
      geom_text_repel(aes(label = !!sym(s))) +
      scale_fill_manual(values = colors_of_groups) +
      scale_color_manual(values = colors_of_groups) + 
      theme_bw() +
      ggtitle("PCA Score Plot") +
      xlab(paste0(PC_to_plot[1], " (", round(summary(pca_res)$importance[2,PC_to_plot[1]] * 100, 1), "%)")) +
      ylab(paste0(PC_to_plot[2], " (", round(summary(pca_res)$importance[2,PC_to_plot[2]] * 100, 1), "%)")) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
    
    if (ellipses_on_score) {
      score_plot <- score_plot +
        stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2)
    }
  }
  
  
  loading_plot <- NULL
  
  if (!is.null(dfv)) {
    if (!is.null(fv)) {
      if (!is.null(sv)) {
        loading_plot <- ggplot(dfv_with_loadings_table_fil, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]), color = !!sym(fv), fill = !!sym(fv))) +
          geom_point() +
          geom_text_repel(aes(label = !!sym(sv))) +
          scale_fill_manual(values = colors_of_groups_fv) +
          scale_color_manual(values = colors_of_groups_fv) + 
          theme_bw() +
          ggtitle("PCA Score Plot") +
          xlab(paste0(PC_to_plot[1], " Loadings")) +
          ylab(paste0(PC_to_plot[2], " Loadings")) + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))
      } else if (labels_on_loading) {
        loading_plot <- ggplot(dfv_with_loadings_table_fil, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]), color = !!sym(fv), fill = !!sym(fv))) +
          geom_point() +
          geom_text_repel(aes(label = !!sym(colnames(dfv_with_loadings_table_fil)[1]))) +
          scale_fill_manual(values = colors_of_groups_fv) +
          scale_color_manual(values = colors_of_groups_fv) + 
          theme_bw() +
          ggtitle("PCA Score Plot") +
          xlab(paste0(PC_to_plot[1], " Loadings")) +
          ylab(paste0(PC_to_plot[2], " Loadings")) + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))
      } else {
        loading_plot <- ggplot(dfv_with_loadings_table_fil, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]), color = !!sym(fv), fill = !!sym(fv))) +
          geom_point() +
          scale_fill_manual(values = colors_of_groups_fv) +
          scale_color_manual(values = colors_of_groups_fv) + 
          theme_bw() +
          ggtitle("PCA Score Plot") +
          xlab(paste0(PC_to_plot[1], " Loadings")) +
          ylab(paste0(PC_to_plot[2], " Loadings")) + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))
      }
      
      if (ellipses_on_loading) {
        loading_plot <- loading_plot +
          stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2)
      }
      
    } else {
      if (!is.null(sv)) {
        loading_plot <- ggplot(dfv_with_loadings_table_fil, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
          geom_point() +
          geom_text_repel(aes(label = !!sym(sv))) +
          theme_bw() +
          ggtitle("PCA Score Plot") +
          xlab(paste0(PC_to_plot[1], " Loadings")) +
          ylab(paste0(PC_to_plot[2], " Loadings")) + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))
      } else if (labels_on_loading) {
        loading_plot <- ggplot(dfv_with_loadings_table_fil, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
          geom_point() +
          geom_text_repel(aes(label = !!sym(colnames(dfv_with_loadings_table_fil)[1]))) +
          theme_bw() +
          ggtitle("PCA Score Plot") +
          xlab(paste0(PC_to_plot[1], " Loadings")) +
          ylab(paste0(PC_to_plot[2], " Loadings")) + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))
      } else {
        loading_plot <- ggplot(dfv_with_loadings_table_fil, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
          geom_point() +
          theme_bw() +
          ggtitle("PCA Score Plot") +
          xlab(paste0(PC_to_plot[1], " Loadings")) +
          ylab(paste0(PC_to_plot[2], " Loadings")) + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))
      }
    }
  } else {
    if (labels_on_loading) {
      loading_plot <- ggplot(dfv_with_loadings_table_fil, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
        geom_point() +
        geom_text_repel(aes(label = !!sym(colnames(dfv_with_loadings_table_fil)[1]))) +
        theme_bw() +
        ggtitle("PCA Score Plot") +
        xlab(paste0(PC_to_plot[1], " Loadings")) +
        ylab(paste0(PC_to_plot[2], " Loadings")) + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))
    } else {
      loading_plot <- ggplot(dfv_with_loadings_table_fil, aes(x = !!sym(PC_to_plot[1]), y = !!sym(PC_to_plot[2]))) +
        geom_point() +
        theme_bw() +
        ggtitle("PCA Score Plot") +
        xlab(paste0(PC_to_plot[1], " Loadings")) +
        ylab(paste0(PC_to_plot[2], " Loadings")) + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))
    }
  }
  
  
  final_list <- list(df_with_scores_table = df_with_scores_table,
                     dfv_with_loadings_table = dfv_with_loadings_table,
                     score_plot = score_plot,
                     loading_plot = loading_plot)
  
  return(final_list)
}

