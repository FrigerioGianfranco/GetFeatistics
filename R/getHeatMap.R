#' Get an Heat Map.
#'
#' Given a table containing data, it creates an heat map graph.
#'
#' @param df dataframe. Containing data to plot.
#' @param v character. The names of the columns of df containing numerical data (ideally, they should be already centered and scaled!!).
#' @param s NULL or a character of length 1. The name of the column of df containing the sample names. You need to pass it only if you want sample names on the heat map.
#' @param f NULL or character. The name(s) of the column(s) of df containing the factor variable. Pass it only if you want the additional rectangles for those groups on the heat map.
#' @param dfv NULL or a dataframe. The first column must contain v. Additional columns can be used for the ordering or for grouping related to v.
#' @param sv NULL or a character of length 1. The name of the column of dfv containing the names. You need to pass it only if you want names on the heat map and if you want to replace existing column names.
#' @param fv NULL or character. The name(s) of the column(s) of dfv containing the factor variable. Pass it only if you want the additional rectangles for those groups on the heat map.
#' @param order_df_by NULL or character. The name(s) of the column(s) of df that you want to use to order the values.
#' @param order_dfv_by NULL or character. The name(s) of the column(s) of dfv that you want to use to order the values.
#' @param trnsp logical. If TRUE, the matrix of created from the columns v of df will be transposed before being used for the heat map.
#' @param cluster_rows logical. If TRUE, a dendrogram built from the hierarchical clustering of distances of row values will be added to the left side of the heat map.
#' @param cluster_columns logical. If TRUE, a dendrogram built from the hierarchical clustering of distances of column values will be added above the heat map.
#' @param name_rows logical. If TRUE, names will be added to rows, on the right of the heat map.
#' @param name_columns logical. if TRUE, names will be added to column, below the heat map.
#' @param rotate_name_columns logical. if TRUE, column names will be rotated vertically (this argument is meaningless if name_columns is FALSE).
#' @param three_heat_colors character of length 3, each specifying a color. These 3 colors will be used as color scale for values of the heat map.
#' @param set_heat_colors_limits logical. If TRUE, the absolute of the minimum or the absolute of the maximum value (which is higher) will be set in positive as the upper limit and in negative as the lower limit for the color gradients of values of the heat map (this will also set the middle color exactly to zero). 
#' @param heat_colors_limits NULL or a numeric of length 2. If set_heat_colors_limits is FALSE, you can specify here the limits for the color gradients of values of the heat map (if NULL, the maximum and the minimum values will be used).
#' @param col_pal_list NULL of a list containing elements named as f and fv; each element has to be a character vector containing colors. Those colors will be used for the rectangles of the group classifications. For each element, if NULL, colors will be taken from the pals package (see the function build_long_vector_of_colors).
#'  
#' @return a ggplot object.
#'
#' @export
getHeatMap <- function(df, v, s = NULL, f = NULL, dfv = NULL, sv = NULL, fv = NULL, order_df_by = NULL, order_dfv_by = NULL,
                       trnsp = TRUE, cluster_rows = FALSE, cluster_columns = FALSE, name_rows = FALSE, name_columns = FALSE, rotate_name_columns = TRUE, 
                       three_heat_colors = c("red", "white", "blue"), set_heat_colors_limits = FALSE, heat_colors_limits = NULL, col_pal_list = NULL) {
  
  if (!is.data.frame(df)) {stop("df must be a data frame!")}
  
  if (!is.character(v)) {stop("v must be a character")}
  if (any(is.na(v))) {stop("v must not contain mising values")}
  if (!all(v %in% colnames(df))) {stop("the names you indicate in v must correspond to names of columns in df")}
  if (!all(map_lgl(df[,v], is.numeric))) {stop("in df, the columns chosed with v must contain numerical values")}
  
  if (!is.null(s)) {
    if (length(s) != 1) {stop("if not NULL, s must be a character of length 1")}
    if (is.na(s)) {stop("if not NULL, s must be a character of length 1, not a missing value")}
    if (!is.character(s)) {stop("if not NULL, s must be a character")}
    if (!s %in% colnames(df)) {stop("if not NULL, s must be a column of df")}
  }
  
  if (!is.null(f)) {
    if (!is.character(f)) {stop("if not NULL, f must be a character")}
    if (any(is.na(f))) {stop("if not NULL, f must not contain mising values")}
    if (!all(f %in% colnames(df))) {stop("if not NULL, the names you indicate in f must correspond to names of columns in df")}
    if (any(c("rownames_in_use", "rownames_wanted", "colnames_in_use", "colnames_wanted", "rowid", "variable")%in%f)) {stop('f must not contain any of the following: "rownames_in_use", "rownames_wanted", "colnames_in_use", "colnames_wanted", "rowid", "variable". \n Please change column names, thank you!')}
  }
  
  if (!is.null(dfv)) {
    if (!is.data.frame(dfv)) {stop("if not NULL, dfv must be a data frame!")}
    if (nrow(dfv) < length(v)) {stop("if not NULL, dfv must be a data frame with a number of rows not less than the length of v")}
    if (!all(v %in% pull(dfv, 1))) {stop(paste0('if not NULL, the first column of dfv must contain v. The following element of v are not present: "',
                                                paste0(v[which(!v%in%pull(dfv, 1))], collapse = '", "'), '"'))}
    
    dfv_fil_test <- dfv[which(pull(dfv, 1) %in% v),]
    
    if (any(duplicated(pull(dfv_fil_test, 1)))) {stop(paste0('the first column of dfv must not contain duplicated in the elements of v. The following are dupicated: "',
                                                        paste0(pull(dfv_fil_test, 1)[which(duplicated(pull(dfv_fil_test, 1)))], collapse = '", "'), '"'))}
    
    dfv_fil <- dfv[0,]
    
    for (vname in v) {
      this_new_row <- dfv[which(pull(dfv, 1) == vname),]
      
      dfv_fil <- rbind(dfv_fil, this_new_row)
    }
    
    if (!is.null(sv)) {
      if (length(sv) != 1) {stop("sv must contain one name")}
      if (is.na(sv)) {stop("sv must not be a missing value")}
      if (!is.character(sv)) {stop("sv must be a character")}
      if (!sv %in% colnames(dfv)) {stop("the name you indicate in sv must correspond to name of a column in dfv")}
    }
    
    if (!is.null(fv)) {
      if (!is.character(fv)) {stop("if not NULL, fv must be a character")}
      if (any(is.na(fv))) {stop("if not NULL, fv must not contain mising values")}
      if (!all(fv %in% colnames(dfv))) {stop("if not NULL, the names you indicate in fv must correspond to names of columns in dfv")}
      if (any(c("rownames_in_use", "rownames_wanted", "colnames_in_use", "colnames_wanted", "rowid", "variable")%in%fv)) {stop('fv must not contain any of the following: "rownames_in_use", "rownames_wanted", "colnames_in_use", "colnames_wanted", "rowid", "variable". \n Please change column names, thank you!')}
      if (!is.null(f)) {
        if (any(f %in% fv) | any(fv %in% f)) {"f and fv must not have same names. \n Please change column names, thank you!"}
      }
    }
    
    if (!is.null(order_dfv_by)) {
      if (!is.character(order_dfv_by)) {stop("if not NULL, order_dfv_by must be a character")}
      if (any(is.na(order_dfv_by))) {stop("if not NULL, order_dfv_by must not contain mising values")}
      if (!all(order_dfv_by %in% colnames(dfv))) {stop("if not NULL, the names you indicate in order_dfv_by must correspond to names of columns in dfv")}
    }
  }
  
  if (!is.null(order_df_by)) {
    if (!is.character(order_df_by)) {stop("if not NULL, order_df_by must be a character")}
    if (any(is.na(order_df_by))) {stop("if not NULL, order_df_by must not contain mising values")}
    if (!all(order_df_by %in% colnames(df))) {stop("if not NULL, the names you indicate in order_df_by must correspond to names of columns in df")}
  }
  
  if (length(trnsp)!=1) {stop("trnsp must be exclusively TRUE or FALSE")}
  if (!is.logical(trnsp)) {stop("trnsp must be exclusively TRUE or FALSE")}
  if (is.na(trnsp)) {stop("trnsp must be exclusively TRUE or FALSE")}
  
  if (length(cluster_rows)!=1) {stop("cluster_rows must be exclusively TRUE or FALSE")}
  if (!is.logical(cluster_rows)) {stop("cluster_rows must be exclusively TRUE or FALSE")}
  if (is.na(cluster_rows)) {stop("cluster_rows must be exclusively TRUE or FALSE")}
  
  if (length(cluster_columns)!=1) {stop("cluster_columns must be exclusively TRUE or FALSE")}
  if (!is.logical(cluster_columns)) {stop("cluster_columns must be exclusively TRUE or FALSE")}
  if (is.na(cluster_columns)) {stop("cluster_columns must be exclusively TRUE or FALSE")}
  
  if (length(name_rows)!=1) {stop("name_rows must be exclusively TRUE or FALSE")}
  if (!is.logical(name_rows)) {stop("name_rows must be exclusively TRUE or FALSE")}
  if (is.na(name_rows)) {stop("name_rows must be exclusively TRUE or FALSE")}
  
  if (length(name_columns)!=1) {stop("name_columns must be exclusively TRUE or FALSE")}
  if (!is.logical(name_columns)) {stop("name_columns must be exclusively TRUE or FALSE")}
  if (is.na(name_columns)) {stop("name_columns must be exclusively TRUE or FALSE")}
  
  if (name_columns) {
    if (length(rotate_name_columns)!=1) {stop("rotate_name_columns must be exclusively TRUE or FALSE")}
    if (!is.logical(rotate_name_columns)) {stop("rotate_name_columns must be exclusively TRUE or FALSE")}
    if (is.na(rotate_name_columns)) {stop("rotate_name_columns must be exclusively TRUE or FALSE")}
  }
  
  are.colors <- function (vect) {
    map_lgl(vect, ~tryCatch({
      is.matrix(col2rgb(.)) & ncol(col2rgb(.))>0
    }, error = function(e) {
      FALSE
    }))
  }
  
  if (!is.character(three_heat_colors)) {stop("three_heat_colors must be a character vector containing three colors")}
  if (length(three_heat_colors) != 3) {stop("three_heat_colors must be a character vector containing three colors")}
  if (any(is.na(three_heat_colors))) {stop("three_heat_colors must be a character vector containing three colors, with no missing values")}
  if (!all(are.colors(three_heat_colors))) {stop(paste0('three_heat_colors must be a character vector containing three colors. The following are not: "',
                                                        paste0(three_heat_colors[which(!are.colors(three_heat_colors))], collapse = '", "'), '"'))}
  
  if (length(set_heat_colors_limits)!=1) {stop("set_heat_colors_limits must be exclusively TRUE or FALSE")}
  if (!is.logical(set_heat_colors_limits)) {stop("set_heat_colors_limits must be exclusively TRUE or FALSE")}
  if (is.na(set_heat_colors_limits)) {stop("set_heat_colors_limits must be exclusively TRUE or FALSE")}
  
  if (set_heat_colors_limits==FALSE) {
    if (!is.null(heat_colors_limits)) {
      if (length(heat_colors_limits) != 2) {stop("if not NULL, heat_colors_limits must be a numeric of length 2")}
      if (!is.numeric(heat_colors_limits)) {stop("if not NULL, heat_colors_limits must be a numeric of length 2")}
    }
  }
  
  if (!is.null(col_pal_list)) {
    if (!is.list(col_pal_list)) {stop("if not NULL, col_pal_list must be a list")}
  }
  
  
  
  
  df_fil_matrix <- as.matrix(df[, v])
  
  if (!is.numeric(df_fil_matrix)) {stop("v must be column names of numeric variables in df")}
  
  if (!is.null(s)) {
    rownames(df_fil_matrix) <- paste0("r", 1:length(pull(df, s)))
    
    rownames_df <- tibble(rownames_in_use = rownames(df_fil_matrix),
                          rownames_wanted = pull(df, s))
    
  } else {
    rownames(df_fil_matrix) <- paste0("r", 1:nrow(df_fil_matrix))
    
    rownames_df <- tibble(rownames_in_use = rownames(df_fil_matrix),
                          rownames_wanted = as.character(1:nrow(df_fil_matrix)))
  }
  
  if (!is.null(f)) {
    for (ff in f) {
      rownames_df[, ff] <- pull(df, ff)
    }
  }
  
  
  if (!is.null(dfv) & !is.null(sv)) {
    colnames(df_fil_matrix) <- paste0("c", 1:length(pull(dfv_fil, sv)))
    
    colnames_df <- tibble(colnames_in_use = colnames(df_fil_matrix),
                          colnames_wanted = pull(dfv_fil, sv))
  } else {
    colnames_df <- tibble(colnames_in_use = colnames(df_fil_matrix),
                          colnames_wanted = colnames(df_fil_matrix))
  }
  
  if (!is.null(dfv) & !is.null(fv)) {
    for (ffv in fv) {
      colnames_df[, ffv] <- pull(dfv_fil, ffv)
    }
  }
  
  
  if (!is.null(order_df_by)) {
    
    for (odf in rev(order_df_by)) {
      
      df_fil_matrix <- df_fil_matrix[order(pull(df, odf)),]
      
    }
  }
    
  if (!is.null(order_dfv_by) & !is.null(dfv)) {
    
    for (odfv in rev(order_dfv_by)) {
      
      df_fil_matrix <- df_fil_matrix[,order(pull(dfv_fil, odfv))]
      
    }
  }  
    
  
  
  if (trnsp) {
    df_fil_matrix <- t(df_fil_matrix)
    
    rownames_df_t <- colnames_df
    colnames(rownames_df_t)[which(colnames(rownames_df_t)=="colnames_in_use")] <- "rownames_in_use"
    colnames(rownames_df_t)[which(colnames(rownames_df_t)=="colnames_wanted")] <- "rownames_wanted"
    colnames_df_t <- rownames_df
    colnames(colnames_df_t)[which(colnames(colnames_df_t)=="rownames_in_use")] <- "colnames_in_use"
    colnames(colnames_df_t)[which(colnames(colnames_df_t)=="rownames_wanted")] <- "colnames_wanted"
    
    rownames_df <- rownames_df_t
    colnames_df <- colnames_df_t
  }
  
  
  
  if (set_heat_colors_limits) {
    
    if (abs(max(df_fil_matrix, na.rm = TRUE)) >= abs(min(df_fil_matrix, na.rm = TRUE))) {
      limit_abs_value <- abs(max(df_fil_matrix, na.rm = TRUE))
    } else {
      limit_abs_value <- abs(min(df_fil_matrix, na.rm = TRUE))
    }
    
    heat_colors_limits <- c((-1)*limit_abs_value, limit_abs_value)
  }
  
  
  if (cluster_rows & cluster_columns) {
    
    rowclus <- hclust(dist(df_fil_matrix))
    colclus <- hclust(dist(t(df_fil_matrix)))
    
    hm <- hmReady(as.data.frame(df_fil_matrix), rowclus = rowclus, colclus = colclus)
    
  } else if (cluster_rows) {
    
    rowclus <- hclust(dist(df_fil_matrix))
    
    hm <- hmReady(as.data.frame(df_fil_matrix), rowclus = rowclus, colclus = NULL)
    
  } else if (cluster_columns) {
    
    colclus <- hclust(dist(t(df_fil_matrix)))
    
    hm <- hmReady(as.data.frame(df_fil_matrix), rowclus = NULL, colclus = colclus)
    
  } else {
    
    hm <- hmReady(as.data.frame(df_fil_matrix), rowclus = NULL, colclus = NULL)
    
  }
  
  
  colnames(rownames_df)[1] <- "rowid"
  colnames(colnames_df)[1] <- "variable"
  
  hm <- left_join(hm, rownames_df, by = "rowid")
  hm <- left_join(hm, colnames_df, by = "variable")
  
  
  
  bar_col_names <- NULL
  bar_row_names <- NULL
  
  if (!is.null(f)) {
    if (trnsp) {
      bar_col_names <- f
    } else {
      bar_row_names <- f
    }
  }
  
  if (!is.null(dfv) & !is.null(fv)) {
    
    if (trnsp) {
      bar_row_names <- fv
    } else {
      bar_col_names <- fv
    }
  }
  
  
  the_heatmap_plot <- ggplot() + 
    geom_tile(data=hm, aes(x=x, y=y, fill=value)) +
    scale_fill_gradientn(colors=hmGradient(highcol = three_heat_colors[1],
                                           midcol = three_heat_colors[2],
                                           lowcol = three_heat_colors[3]),
                         limits = heat_colors_limits) +
    theme_hm() + 
    theme(axis.title=element_blank())
  
  if (name_rows) {
    the_heatmap_plot <- the_heatmap_plot +
      scale_y_continuous(expand=c(0,0), breaks=hm[which(!duplicated(hm$y)),]$y, labels=hm[which(!duplicated(hm$y)),]$rownames_wanted, position = "right") +
      theme(axis.text.y=element_text(angle = 0, vjust = 0.5, hjust = 0.5))
  } else {
    the_heatmap_plot <- the_heatmap_plot +
      theme(axis.text.y=element_blank())
  }
  
  if (name_columns) {
    if (rotate_name_columns) {
      the_heatmap_plot <- the_heatmap_plot +
        scale_x_continuous(expand=c(0,0), breaks=hm[which(!duplicated(hm$x)),]$x, labels=hm[which(!duplicated(hm$x)),]$colnames_wanted) +
        theme(axis.text.x=element_text(angle = 270, vjust = 0.5, hjust = 0.5))
    } else {
      the_heatmap_plot <- the_heatmap_plot +
        scale_x_continuous(expand=c(0,0), breaks=hm[which(!duplicated(hm$x)),]$x, labels=hm[which(!duplicated(hm$x)),]$colnames_wanted) +
        theme(axis.text.x=element_text(angle = 0, vjust = 0.5, hjust = 0.5))
    }
  } else {
    the_heatmap_plot <- the_heatmap_plot +
      theme(axis.text.x=element_blank())
  }
  
  
  
  long_vector_of_colors <- build_long_vector_of_colors()
  colors_count <- 0
  
  
  the_x_unit <- max(hm$x, na.rm = TRUE)*0.1
  x1_cluster_position <- 0.5 - (the_x_unit*0.05) 
  x2_cluster_position <- x1_cluster_position - (the_x_unit*1.75)
  
  
  if (!is.null(bar_row_names)) {
    for(brn in bar_row_names) {
      
      df_this_brn <- tibble(y = unique(hm$y))
      df_this_brn <- left_join(df_this_brn, hm[!duplicated(hm$y),c("y", brn)], by = "y")
      
      df_this_brn_rect <- tibble(xmin = rep(x1_cluster_position - (the_x_unit*0.1), nrow(df_this_brn)),
                                 xmax = rep((x1_cluster_position - (the_x_unit*0.1)) - (the_x_unit*0.25), nrow(df_this_brn)),
                                 ymin = df_this_brn$y - 0.5,
                                 ymax = df_this_brn$y + 0.5)
      if (is.factor(pull(df_this_brn, brn))) {
        df_this_brn_rect[,brn] <- pull(df_this_brn, brn)
      } else {
        df_this_brn_rect[,brn] <- as.factor(pull(df_this_brn, brn))
      }
      
      if (is.null(col_pal_list[[brn]])) {
        col_pal_list[[brn]] <- long_vector_of_colors[(colors_count+1):(colors_count+length(levels(pull(df_this_brn_rect, brn))))]
        colors_count <- colors_count+length(levels(pull(df_this_brn_rect, brn)))
        names(col_pal_list[[brn]]) <- levels(pull(df_this_brn_rect, brn))
      } else {
        if (!is.character(col_pal_list[[brn]])) {stop(paste0('col_pal_list$', brn, ' must be a character vector'))}
        if (any(is.na(col_pal_list[[brn]]))) {stop(paste0('col_pal_list$', brn, ' must not contain missing values'))}
        if (length(col_pal_list[[brn]])<length(levels(pull(df_this_brn_rect, brn)))) {
          stop(paste0("There are ", length(levels(pull(df_this_brn_rect, brn))), " levels in ",brn ,", and you have specified only ", length(col_pal_list[[brn]]), " colors in col_pal_list$", brn))
        } else {
          col_pal_list[[brn]] <- col_pal_list[[brn]][1:length(levels(pull(df_this_brn_rect, brn)))]
        }
        
        
        if (!all(are.colors(col_pal_list[[brn]]))) {stop(paste0('col_pal_list$', brn, ' must contain valid colors. In particular, these are not: "', paste(col_pal_list[[brn]][!are.colors(col_pal_list[[brn]])], collapse = '", "')), '"')}
        
        if (is.null(names(col_pal_list[[brn]]))) {
          names(col_pal_list[[brn]]) <- levels(pull(df_this_brn_rect, brn))
        } else {
          if (!(all(names(col_pal_list[[brn]])%in%levels(pull(df_this_brn_rect, brn))) & all(levels(pull(df_this_brn_rect, brn)) %in% names(col_pal_list[[brn]])))) {
            names(col_pal_list[[brn]]) <- levels(pull(df_this_brn_rect, brn))
          }
        }
      }
      
      
      the_heatmap_plot <- the_heatmap_plot +
        new_scale_fill() +
        geom_rect(data = df_this_brn_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = !!sym(brn))) +
        scale_fill_manual(values = col_pal_list[[brn]])
      
      
      x1_cluster_position <- df_this_brn_rect$xmax[1] - (the_x_unit*0.05) 
      x2_cluster_position <- x1_cluster_position - (the_x_unit*1.75)
      
    }
  }
  
  if (cluster_rows) {
    
    the_heatmap_plot <- the_heatmap_plot +
      geom_dendro(rowclus, xlim=c(x1_cluster_position, x2_cluster_position), pointing="side", axis.labels = FALSE)
  }
  
  
  
  
  the_y_unit <- max(hm$y, na.rm = TRUE)*0.1
  y1_cluster_position <- (the_y_unit*10)+0.5 + (the_y_unit*0.05)  
  y2_cluster_position <- y1_cluster_position + (the_y_unit*1.75)
  
  if (!is.null(bar_col_names)) {
    for(bcn in bar_col_names) {
      
      df_this_bcn <- tibble(x = unique(hm$x))
      df_this_bcn <- left_join(df_this_bcn, hm[!duplicated(hm$x),c("x", bcn)], by = "x")
      
      df_this_bcn_rect <- tibble(xmin = df_this_bcn$x - 0.5,
                                 xmax = df_this_bcn$x + 0.5,
                                 ymin = rep(y1_cluster_position + (the_y_unit*0.1), nrow(df_this_bcn)),
                                 ymax = rep((y1_cluster_position + (the_y_unit*0.1)) + (the_y_unit*0.25), nrow(df_this_bcn)))
      if (is.factor(pull(df_this_bcn, bcn))) {
        df_this_bcn_rect[,bcn] <- pull(df_this_bcn, bcn)
      } else {
        df_this_bcn_rect[,bcn] <- as.factor(pull(df_this_bcn, bcn))
      }
      
      if (is.null(col_pal_list[[bcn]])) {
        col_pal_list[[bcn]] <- long_vector_of_colors[(colors_count+1):(colors_count+length(levels(pull(df_this_bcn_rect, bcn))))]
        colors_count <- colors_count+length(levels(pull(df_this_bcn_rect, bcn)))
        names(col_pal_list[[bcn]]) <- levels(pull(df_this_bcn_rect, bcn))
      } else {
        if (!is.character(col_pal_list[[bcn]])) {stop(paste0('col_pal_list$', bcn, ' must be a character vector'))}
        if (any(is.na(col_pal_list[[bcn]]))) {stop(paste0('col_pal_list$', bcn, ' must not contain missing values'))}
        if (length(col_pal_list[[bcn]])<length(levels(pull(df_this_bcn_rect, bcn)))) {
          stop(paste0("There are ", length(levels(pull(df_this_bcn_rect, bcn))), " levels in ",bcn ,", and you have specified only ", length(col_pal_list[[bcn]]), " colors in col_pal_list$", bcn))
        } else {
          col_pal_list[[bcn]] <- col_pal_list[[bcn]][1:length(levels(pull(df_this_bcn_rect, bcn)))]
        }
        
        
        if (!all(are.colors(col_pal_list[[bcn]]))) {stop(paste0('col_pal_list$', bcn, ' must contain valid colors. In particular, these are not: "', paste(col_pal_list[[bcn]][!are.colors(col_pal_list[[bcn]])], collapse = '", "')), '"')}
        
        if (is.null(names(col_pal_list[[bcn]]))) {
          names(col_pal_list[[bcn]]) <- levels(pull(df_this_bcn_rect, bcn))
        } else {
          if (!(all(names(col_pal_list[[bcn]])%in%levels(pull(df_this_bcn_rect, bcn))) & all(levels(pull(df_this_bcn_rect, bcn)) %in% names(col_pal_list[[bcn]])))) {
            names(col_pal_list[[bcn]]) <- levels(pull(df_this_bcn_rect, bcn))
          }
        }
      }
      
      
      the_heatmap_plot <- the_heatmap_plot +
        new_scale_fill() +
        geom_rect(data = df_this_bcn_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = !!sym(bcn))) +
        scale_fill_manual(values = col_pal_list[[bcn]])
      
      
      y1_cluster_position <- df_this_bcn_rect$ymax[1] + (the_y_unit*0.05) 
      y2_cluster_position <- y1_cluster_position + (the_y_unit*1.75)
      
    }
  }
  
  
  if (cluster_columns) {
    
    the_heatmap_plot <- the_heatmap_plot +
      geom_dendro(colclus, ylim=c(y1_cluster_position, y2_cluster_position), pointing="updown", axis.labels = FALSE)
  }
  
  
  return(the_heatmap_plot)
}

