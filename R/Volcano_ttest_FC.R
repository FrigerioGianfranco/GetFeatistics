

#' Generate a Volcano plot.
#'
#' Given a t-test analysis performed with the function gentab_P.t.test, and a Fold Change analyses performed with the function gentab_FC, it gives a Volcano plot.
#'
#' @param ttest_results data frame obtained from the function gentab_P.t.test.
#' @param FC_results data frame obtained from the function gentab_FC. The first column must contain the same elements of the first column of ttest_results.
#' @param FDR logical. If TRUE, it will plot on the Y-axis the value from the PvaluesFDR column of the ttest_results; if FALSE, the Pvalues.
#' @param log_base numeric of length 1. The base of the logarithm used in the FC analysis. Only for visualisation purpose as it will be shown in the x-axis label.
#' @param cut_off_names NULL or numerical of length 1. The names will be shown only if above this cut-off in the y-axis. Pass 0 if you want to see all names. Pass NULL if you don't want to see any name.
#' @param line1_position NULL or numerical of length 1. The y-value of the horizontal dotted line. Pass NULL if you don't want it.
#' @param line2_position NULL or numerical of length 1. The y-value of the second horizontal dotted line. Pass NULL if you don't want it.
#' @param names_to_plot NULL or character of length 1. The name of the column containing the names to report on the plot. If this column name is absent in ttest_results, it will be considered from FC_results. If NULL is passed to this arguments, the names will be taken from the first column of ttest_results (which elements must match with the first column of FC_results).
#' @param category NULL or character of length 1. The name of the column containing the groups to color the points. If this column name is absent in ttest_results, it will be considered from FC_results. If NULL is passed to this arguments, all the points will be black.
#' @param col_pal NULL or a character vector containing colors. If NULL, colors from the pals package will be used (see function build_long_vector_of_colors).
#' 
#'
#' @return A ggplot object with the Volcano plot.
#'
#' @export
Volcano_ttest_FC <- function(ttest_results, FC_results, FDR = FALSE, log_base = 2,
                             cut_off_names = -log10(0.05), line1_position = -log10(0.05), line2_position = NULL,
                             names_to_plot = NULL, category = NULL, col_pal = NULL) {
  
  if (!is.data.frame(ttest_results)) {stop("ttest_results must be a data frame")}
  if (any(duplicated(pull(ttest_results, 1)))) {stop("ttest_results must not contain dupicated in the first column")}
  
  if (!is.data.frame(FC_results)) {stop("FC_results must be a data frame")}
  if (any(duplicated(pull(FC_results, 1)))) {stop("FC_results must not contain dupicated in the first column")}
  if (!"logFC" %in% colnames(FC_results)) {stop('FC_results must contain the column called "logFC"')}
  if (!is.numeric(FC_results$logFC)) {stop("The column logFC of FC_results must contain numerical data!")}
  
  if (length(pull(ttest_results, 1)) != length(pull(FC_results, 1))) {stop("The elments of the first column of ttest_results and of the first column of FC_results must correspond")}
  if (!(all(pull(ttest_results, 1) %in% pull(FC_results, 1)) & all(pull(FC_results, 1) %in% pull(ttest_results, 1)))) {stop("The elments of the first column of ttest_results and of the first column of FC_results must correspond")}
  
  if (length(FDR)!=1) {stop("FDR must be exclusively TRUE or FALSE")}
  if (!is.logical(FDR)) {stop("FDR must be exclusively TRUE or FALSE")}
  if (is.na(FDR)) {stop("FDR must be exclusively TRUE or FALSE")}
  
  if (FDR) {
    if (!"PvaluesFDR" %in% colnames(ttest_results)) {stop('ttest_results must contain the column called "PvaluesFDR"')}
    if (!is.numeric(ttest_results$PvaluesFDR)) {stop("The column PvaluesFDR of ttest_results must contain numerical data!")}
    if (!all(ttest_results$PvaluesFDR <= 1 & ttest_results$PvaluesFDR >= 0, na.rm = TRUE)){stop("The column PvaluesFDR of ttest_results must contain numerical data between 0 and 1")}
  } else {
    if (!"Pvalues" %in% colnames(ttest_results)) {stop('ttest_results must contain the column called "Pvalues"')}
    if (!is.numeric(ttest_results$Pvalues)) {stop("The column Pvalues of ttest_results must contain numerical data!")}
    if (!all(ttest_results$Pvalues <= 1 & ttest_results$Pvalues >= 0, na.rm = TRUE)){stop("The column Pvalues of ttest_results must contain numerical data between 0 and 1")}
  }
  
  if (length(log_base)!=1) {stop("log_base must be a single number")}
  if (is.na(log_base)) {stop("log_base must be a number")}
  if (!is.numeric(log_base)) {stop("log_base must be a number")}
  
  if (!is.null(cut_off_names)) {
    if (length(cut_off_names)!=1) {stop("cut_off_names must be NULL or a single number")}
    if (is.na(cut_off_names)) {stop("cut_off_names must be NULL or a number")}
    if (!is.numeric(cut_off_names)) {stop("cut_off_names must be NULL or a number")}
  }
  
  if (!is.null(line1_position)) {
    if (length(line1_position)!=1) {stop("line1_position must be NULL or a single number")}
    if (is.na(line1_position)) {stop("line1_position must be NULL or a number")}
    if (!is.numeric(line1_position)) {stop("line1_position must be NULL or a number")}
  }
  
  if (!is.null(line2_position)) {
    if (length(line2_position)!=1) {stop("line2_position must be NULL or a single number")}
    if (is.na(line2_position)) {stop("line2_position must be NULL or a number")}
    if (!is.numeric(line2_position)) {stop("line2_position must be NULL or a number")}
  }
  
  names_to_plot_from_ttest <- FALSE
  names_to_plot_from_FC <- FALSE
  
  if (!is.null(names_to_plot)) {
    if (length(names_to_plot)!=1) {stop("if not NULL, names_to_plot must be a character of lenght 1")}
    if (!is.character(names_to_plot)) {stop("if not NULL, names_to_plot must be a character")}
    if (is.na(names_to_plot)) {stop("if not NULL, names_to_plot must be a character of lenght 1, not a missing value")}
    if (names_to_plot %in% colnames(ttest_results)) {
      names_to_plot_from_ttest <- TRUE
    } else if (names_to_plot %in% colnames(FC_results)) {
      names_to_plot_from_FC <- TRUE
    } else {
      stop("if not NULL, names_to_plot must be a column name in either ttest_results or FC_results")
    }
  } else {
    names_to_plot <- colnames(ttest_results)[1]
  }
  
  category_from_ttest <- FALSE
  category_from_FC <- FALSE
  
  if (!is.null(category)) {
    if (length(category)!=1) {stop("if not NULL, category must be a character of lenght 1")}
    if (!is.character(category)) {stop("if not NULL, category must be a character")}
    if (is.na(category)) {stop("if not NULL, category must be a character of lenght 1, not a missing value")}
    if (category %in% colnames(ttest_results)) {
      if (!is.factor(pull(ttest_results, category))) {
        ttest_results[,category] <- as.factor(pull(ttest_results, category))
      }
      category_from_ttest <- TRUE
    } else if (category %in% colnames(FC_results)) {
      if (!is.factor(pull(FC_results, category))) {
        FC_results[,category] <- as.factor(pull(FC_results, category))
      }
      category_from_FC <- TRUE
    } else {
      stop("if not NULL, category must be a column name in either ttest_results or FC_results")
    }
  }
  
  if (!is.null(col_pal)) {
    if (!is.character(col_pal)) {stop("col_pal must be a character vector")}
    if (any(is.na(col_pal))) {stop("col_pal must not contain NAs")}
  } else {
    col_pal <- build_long_vector_of_colors()
  }
  
  are.colors <- function (vect) {
    map_lgl(vect, ~tryCatch({
      is.matrix(col2rgb(.)) & ncol(col2rgb(.))>0
    }, error = function(e) {
      FALSE
    }))
  }
  
  
  
  
  colnames(FC_results)[1] <- colnames(ttest_results)[1]
  
  if (FDR) {
    useful_columns_from_ttest <- c(colnames(ttest_results)[1], "PvaluesFDR")
    
    if (names_to_plot_from_ttest) {
      useful_columns_from_ttest <- c(useful_columns_from_ttest, names_to_plot)
    }
    
    if (category_from_ttest) {
      useful_columns_from_ttest <- c(useful_columns_from_ttest, category)
    }
    
    useful_columns_from_ttest <- unique(useful_columns_from_ttest)
    
    
    ttest_results_fil <- ttest_results %>%
      select(all_of(useful_columns_from_ttest)) %>%
      mutate(minus_Log_P = -log10(PvaluesFDR))
    
  } else {
    useful_columns_from_ttest <- c(colnames(ttest_results)[1], "Pvalues")
    
    if (names_to_plot_from_ttest) {
      useful_columns_from_ttest <- c(useful_columns_from_ttest, names_to_plot)
    }
    
    if (category_from_ttest) {
      useful_columns_from_ttest <- c(useful_columns_from_ttest, category)
    }
    
    useful_columns_from_ttest <- unique(useful_columns_from_ttest)
    
    
    ttest_results_fil <-  ttest_results %>%
      select(all_of(useful_columns_from_ttest)) %>%
      mutate(minus_Log_P = -log10(Pvalues))
  }
  
  
  useful_columns_from_FC <- c(colnames(FC_results)[1], "logFC")
  
  if (names_to_plot_from_FC) {
    useful_columns_from_FC <- c(useful_columns_from_FC, names_to_plot)
  }
  
  if (category_from_FC) {
    useful_columns_from_FC <- c(useful_columns_from_FC, category)
  }
  
  useful_columns_from_FC <- unique(useful_columns_from_FC)
  
  FC_results_fil <- FC_results %>%
    select(all_of(useful_columns_from_FC))
  
  
  
  
  ttest_FC_results_fil <- left_join(x = ttest_results_fil, y = FC_results_fil, by = colnames(ttest_results)[1])
  
  
  
  
  
  
  if (is.null(category)) {
    The_volcano_plot <- ggplot(data = ttest_FC_results_fil, mapping = aes(x = logFC, y = minus_Log_P)) + 
      geom_point() + 
      theme_bw() +
      ggtitle("Volcano plot") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  } else {
    
    if (length(col_pal)<length(levels(pull(ttest_FC_results_fil, category)))) {
      stop(paste0("There are ", length(levels(pull(ttest_FC_results_fil, category))), " groups, and you have specified only ", length(col_pal), " colors in col_pal"))
    } else {
      col_pal <- col_pal[1:length(levels(pull(ttest_FC_results_fil, category)))]
    }
    
    
    if (!all(are.colors(col_pal))) {stop(paste0('col_pal must contain valid colors. In particular, these are not: "', paste(col_pal[!are.colors(col_pal)], collapse = '", "')), '"')}
    
    colors_of_groups <- col_pal
    if (is.null(names(colors_of_groups))) {
      names(colors_of_groups) <- levels(pull(ttest_FC_results_fil, category))
    } else {
      if (any(duplicated(names(colors_of_groups)))) {"col_pal has some duplicated in the names"}
      if (!all(names(colors_of_groups) %in% levels(pull(ttest_FC_results_fil, category)) & levels(pull(ttest_FC_results_fil, category)) %in% names(colors_of_groups))) {stop("the names of col_pal don't correspond to the levels of category")}
    }
    
    
    The_volcano_plot <- ggplot(data = ttest_FC_results_fil, mapping = aes(x = logFC, y = minus_Log_P, colour=!!sym(category), fill= !!sym(category), shape=!!sym(category))) + 
      geom_point() + 
      scale_fill_manual(values = colors_of_groups) +
      scale_colour_manual(values = colors_of_groups) +
      theme_bw() +
      ggtitle("Volcano plot") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }
    
  
  
  if (FDR) {
    The_volcano_plot <- The_volcano_plot +
      ylab("-log10(t-test FDR p-values)")
  } else {
    The_volcano_plot <- The_volcano_plot +
      ylab("-log10(t-test p-values)")
  }
  
  if (log_base == exp(1)) {
    The_volcano_plot <- The_volcano_plot +
      xlab("ln(Fold Change)")
  } else {
    The_volcano_plot <- The_volcano_plot +
      xlab(paste0("log", log_base, "(Fold Change)"))
  }
  
  
  if (!is.null(cut_off_names)) {
    The_volcano_plot <- The_volcano_plot +
      geom_text_repel(data=ttest_FC_results_fil[which(pull(ttest_FC_results_fil, minus_Log_P) > cut_off_names),],
                      aes(x = logFC, y = minus_Log_P, label = !!sym(names_to_plot)))
  }
  
  if (!is.null(line1_position)) {
    The_volcano_plot <- The_volcano_plot +
      geom_hline(yintercept = line1_position, linetype = "longdash")
  }
  
  if (!is.null(line2_position)) {
    The_volcano_plot <- The_volcano_plot +
      geom_hline(yintercept = line2_position, linetype = "dashed")
  }
  
  
  
  return(The_volcano_plot)
}

