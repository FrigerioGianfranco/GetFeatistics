

#' Plotting calibration curves of a targeted elaboration
#'
#' It creates plots of the calibration curves from a targeted elaboration.
#'
#' @param targeted_elaboration list, an output of the function get_targeted_elaboration.
#'
#' @return a list with ggplots, one for each linear model computed. You can print it to the console to see the plots, or export them using the function export_figures.
#'
#' @export
plot_calibration_curves <- function(targeted_elaboration) {
  if (!is.list(targeted_elaboration)) {stop("targeted_elaboration must be a list obtained with the get_targeted_elaboration function")}
  if (any(duplicated(names(targeted_elaboration)))) {stop("targeted_elaboration must be a list obtained with the get_targeted_elaboration function")}
  if (!"regression_models"%in%names(targeted_elaboration)) {stop("targeted_elaboration must be a list obtained with the get_targeted_elaboration function")}
  if (!is.list(targeted_elaboration$regression_models)) {stop("targeted_elaboration must be a list obtained with the get_targeted_elaboration function")}
  if (!"summary_regression_models"%in%names(targeted_elaboration)) {stop("targeted_elaboration must be a list obtained with the get_targeted_elaboration function")}
  if (!all(c("slope", "intercept", "r.squared", "adj.r.squared") %in% pull(targeted_elaboration$summary_regression_models,1))) {stop("targeted_elaboration must be a list obtained with the get_targeted_elaboration function")}
  
  
  all_curve_plots <- vector(mode = "list", length = length(names(targeted_elaboration$regression_models)))
  names(all_curve_plots) <- names(targeted_elaboration$regression_models)
  
  for (a in names(targeted_elaboration$regression_models)) {
    if (!is.list(targeted_elaboration$regression_models[[a]])) {stop("targeted_elaboration must be a list obtained with the get_targeted_elaboration function")}
    if (!is.data.frame(targeted_elaboration$regression_models[[a]]$model)) stop("targeted_elaboration must be a list obtained with the get_targeted_elaboration function")
    if (!(all(c("X", "Y")%in%colnames(targeted_elaboration$regression_models[[a]]$model)))) stop("targeted_elaboration must be a list obtained with the get_targeted_elaboration function")
    
    this_unit_measure <- as.character(NA)
    if (!is.null(targeted_elaboration$compound_legend)) {
      if (length(colnames(targeted_elaboration$compound_legend))>=4) {
        if (a %in% (pull(targeted_elaboration$compound_legend, 1))) {
          this_unit_measure <- pull(targeted_elaboration$compound_legend, 4)[which(pull(targeted_elaboration$compound_legend, 1) == a)]
        }
      }
    } 
    
    
    if (!"weights"%in%names(TARGETED_ELABORATED$regression_models[[a]])) {
      this_curve_plot <- ggplot(data = targeted_elaboration$regression_models[[a]]$model, aes(x = X, y = Y)) +
        geom_point() +
        geom_smooth(method = "lm", se=FALSE) +
        labs(x = ifelse(is.na(this_unit_measure), paste0(a, "\n(concentrations)"), paste0(a, "\n(", this_unit_measure, ")")),
             y = ifelse(is.null(targeted_elaboration$compound_legend), paste0(a, "\n(intensities)"), ifelse(is.na(pull(targeted_elaboration$compound_legend, 2)[which(pull(targeted_elaboration$compound_legend, 1)==a)]), paste0(a, "\n(intensities)"), paste0(a, " / ", pull(targeted_elaboration$compound_legend, 2)[which(pull(targeted_elaboration$compound_legend, 1)==a)], "\n(ratio intensities)")))) +
        geom_text(data = tibble(textX = min(targeted_elaboration$regression_models[[a]]$model$X),
                                textY = max(targeted_elaboration$regression_models[[a]]$model$Y),
                                the_text = paste0("slope = ", format(pull(targeted_elaboration$summary_regression_models, a)[which(pull(targeted_elaboration$summary_regression_models, 1)=="slope")], digits = 5), "\n",
                                                  "intercept = ", format(pull(targeted_elaboration$summary_regression_models, a)[which(pull(targeted_elaboration$summary_regression_models, 1)=="intercept")], digits = 5), "\n",
                                                  "r.squared = ", format(pull(targeted_elaboration$summary_regression_models, a)[which(pull(targeted_elaboration$summary_regression_models, 1)=="r.squared")], digits = 5), "\n",
                                                  "adj.r.squared = ", format(pull(targeted_elaboration$summary_regression_models, a)[which(pull(targeted_elaboration$summary_regression_models, 1)=="adj.r.squared")], digits = 5), "\n",
                                                  "weighting = none")),
                  aes(x = textX, y = textY, label = the_text), vjust = "inward", hjust = "inward") + 
        theme_bw()
      
    } else {
      
      colnames(targeted_elaboration$regression_models[[a]]$model)[which(colnames(targeted_elaboration$regression_models[[a]]$model) == "(weights)")] <- "weights"
      
      this_curve_plot <- ggplot(data = targeted_elaboration$regression_models[[a]]$model, aes(x = X, y = Y)) +
        geom_point() +
        geom_smooth(method = "lm", mapping = aes(weight = weights), se=FALSE, show.legend = FALSE) +
        labs(x = ifelse(is.na(this_unit_measure), paste0(a, "\n(concentrations)"), paste0(a, "\n(", this_unit_measure, ")")),
             y = ifelse(is.null(targeted_elaboration$compound_legend), paste0(a, "\n(intensities)"), ifelse(is.na(pull(targeted_elaboration$compound_legend, 2)[which(pull(targeted_elaboration$compound_legend, 1)==a)]), paste0(a, "\n(intensities)"), paste0(a, " / ", pull(targeted_elaboration$compound_legend, 2)[which(pull(targeted_elaboration$compound_legend, 1)==a)], "\n(ratio intensities)")))) +
        geom_text(data = tibble(textX = min(targeted_elaboration$regression_models[[a]]$model$X),
                                textY = max(targeted_elaboration$regression_models[[a]]$model$Y),
                                the_text = paste0("slope = ", format(pull(targeted_elaboration$summary_regression_models, a)[which(pull(targeted_elaboration$summary_regression_models, 1)=="slope")], digits = 5), "\n",
                                                  "intercept = ", format(pull(targeted_elaboration$summary_regression_models, a)[which(pull(targeted_elaboration$summary_regression_models, 1)=="intercept")], digits = 5), "\n",
                                                  "r.squared = ", format(pull(targeted_elaboration$summary_regression_models, a)[which(pull(targeted_elaboration$summary_regression_models, 1)=="r.squared")], digits = 5), "\n",
                                                  "adj.r.squared = ", format(pull(targeted_elaboration$summary_regression_models, a)[which(pull(targeted_elaboration$summary_regression_models, 1)=="adj.r.squared")], digits = 5), "\n",
                                                  "weighting = ", pull(targeted_elaboration$compound_legend, 3)[which(pull(targeted_elaboration$compound_legend, 1) == a)])),
                  aes(x = textX, y = textY, label = the_text), vjust = "inward", hjust = "inward") + 
        theme_bw()
    }
    
    
    all_curve_plots[[a]] <- this_curve_plot
    
  }
  
  return(all_curve_plots)
}

