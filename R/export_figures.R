
#' Export plots
#'
#' It exports a plot or a list of plots created with ggplot or ggplot-like functions into the current working directory.
#'
#' @param plots a plot or a list of plots of the types "gg" and "ggplot".
#' @param exprtname_figures NULL or a character of length 1. The desired name for the file to create (do not add here the file extension as it will be added automatically based on the next argument). If NULL, the name of the object passed to the argument plots will be used.
#' @param exprt_fig_type one of the following: "png" or "pdf". The desired type of file to create: if "png", it will use ggsave considering the plot_sizes and plot_unit arguments, and it will create a different picture for each plot in plots; if "pdf", it will create a single file with each plot in a different page.
#' @param plot_sizes a numeric vector of length 2. It should contains the width and height for saving the figures in png format. If not specified, it will use the size of current graphics device (as in the ggsave function)
#' @param plot_unit one of the following: "in", "cm", "mm", or "px". It is referred to the with and height of the plot_sizes argument (as in the ggsave function)
#'
#' @return It creates file(s) in the current working director.
#'
#' @export
export_figures <- function(plots, exprtname_figures = NULL , exprt_fig_type = "png", plot_sizes = c(NA, NA), plot_unit = "in") {
  if (!is.list(plots)) {"plots must be a ggplot or a list of ggplots"}
  if (inherits(plots, c("gg", "ggplot"))) {
    characteristics <- "single_plot"
  } else if (all(map_lgl(plots, ~ inherits(., c("gg", "ggplot"))))) {
    characteristics <- "list_of_plots"
  } else {
    stop("plots must be a ggplot or a list of ggplots")
  }
  
  if (is.null(exprtname_figures)) {
    exprtname_figures <- deparse(substitute(plots))
  } else {
    if (length(exprtname_figures) != 1) {stop('exprtname_figures must be a character of length 1')}
    if (is.na(exprtname_figures)) {stop('exprtname_figures must be a character of length 1, and not a missing value!')}
    if (!is.character(exprtname_figures)) {stop('exprtname_figures must be a character of length 1')}
  }
  
  
  if (length(exprt_fig_type) != 1) {stop('exprt_fig_type must be a character of length 1 containing one of the following: "png" or "pdf"')}
  if (!is.character(exprt_fig_type)) {stop('exprt_fig_type must be a character of length 1 containing one of the following: "png" or "pdf"')}
  if (!exprt_fig_type%in%c("png", "pdf")) {stop('exprt_fig_type must be a character of length 1 containing one of the following: "png" or "pdf"')}
  
  if (length(plot_sizes) != 2) {stop("plot_sizes should be a vector of length 2 containing the width and height for saving the figures in png format, or either of those can be NA")}
  if (mean(is.na(plot_sizes)) != 1) {
    if (!is.numeric(plot_sizes)) {
      stop("plot size should be a vector of length 2 containing the width and height for saving the figures in png format, or either of those can be NA")
    }
  }
  
  if (length(plot_unit) != 1) {stop('plot_unit must be "in", "cm", "mm", or "px"')}
  if (!is.character(plot_unit)) {stop('plot_unit must be "in", "cm", "mm", or "px"')}
  if (!plot_unit%in%c("in", "cm", "mm", "px")) {stop('plot_unit must be "in", "cm", "mm", or "px"')}
  
  
  if (characteristics == "list_of_plots") {
    if (exprt_fig_type == "png") {
      for (i in 1:length(plots)) {
        
        this_numbername <- zero_prefixing(i, highest = length(plots))
        
        if (is.null(names(plots)[i])) {
          this_name <- this_numbername
        } else {
          this_name <- paste0(this_numbername, "_", names(plots)[i])
        }
        
        ggsave(filename = paste0(exprtname_figures, "_", this_name, ".png"),
               plot = plots[[i]],
               width = plot_sizes[1],
               height = plot_sizes[2],
               units = plot_unit)
      }
    } else if (exprt_fig_type == "pdf") {
      
      pdf(file = paste0(exprtname_figures, ".pdf"))
      print(plots)
      dev.off()
    }
  } else if (characteristics == "single_plot") {
    if (exprt_fig_type == "png") {
      ggsave(filename = paste0(exprtname_figures, ".png"),
             plot = plots,
             width = plot_sizes[1],
             height = plot_sizes[2],
             units = plot_unit)
    } else if (exprt_fig_type == "pdf") {
      pdf(file = paste0(exprtname_figures, ".pdf"))
      print(plots)
      dev.off()
    }
  }
}

