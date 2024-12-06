

## Packages required: FELLA igraph


#' Do FELLA enrichment_analysis.
#'
#' Given KEGG codes and a defined organism, it performs the enrichment_analysis with the FELLA package.
#'
#' @param organism_code character of length 1. The KEGG organism code. For example, for homo sapiens (human) is 'hsa'. All codes are here: https://www.genome.jp/kegg/tables/br08606.html.
#' @param KEGG_codes Character. KEGG codes of compounds.
#' @param path_databases Character of length 1. Path of local organism databases.
#' @param output_prefix Character of length 1. What to add at the beginning of generated file names and object names.
#' @param output_suffix Character of length 1. What to add at the end of generated file names and object names.
#'
#' @return Export csv table and png file, along with creating object in the global environment.
#'
#' @export
do_FELLA_enrichment_analysis <- function(organism_code, KEGG_codes, path_databases = "C:/databases/FELLA/", output_prefix = "", output_suffix = "") {
  
  if (!is.character(organism_code)) {stop("organism_code must be a character of length 1")}
  if (length(organism_code)!=1) {stop("organism_code must be a character of length 1")}
  if (is.na(organism_code)) {stop("organism_code must be a character of length 1, not a missing value")}
  
  if (!is.character(KEGG_codes)) {stop("KEGG_codes must be a character vector")}
  if (any(is.na(KEGG_codes))) {stop("KEGG_codes must be a character vector, with no missing values")}
  
  
  
  
  if (!is.character(path_databases)) {stop("path_databases must be a character of length 1")}
  if (length(path_databases)!=1) {stop("path_databases must be a character of length 1")}
  if (is.na(path_databases)) {stop("path_databases must be a character of length 1, not a missing value")}
  
  if (!is.character(output_prefix)) {stop("output_prefix must be a character of length 1")}
  if (length(output_prefix)!=1) {stop("output_prefix must be a character of length 1")}
  if (is.na(output_prefix)) {stop("output_prefix must be a character of length 1, not a missing value")}
  
  if (!is.character(output_suffix)) {stop("output_suffix must be a character of length 1")}
  if (length(output_suffix)!=1) {stop("output_suffix must be a character of length 1")}
  if (is.na(output_suffix)) {stop("output_suffix must be a character of length 1, not a missing value")}
  
  
  
  if (!file.exists(paste0(path_databases, organism_code))) {
    tot_this_organism <- buildGraphFromKEGGREST(organism = "lam", filter.path = NULL)
    
    buildDataFromGraph(keggdata.graph = tot_this_organism,
                       databaseDir = paste0(path_databases, organism_code),
                       internalDir = FALSE,
                       matrices = "diffusion",
                       normality = "diffusion")
  }
  
  
  fella.data_this_organism <- loadKEGGdata(databaseDir = paste0(path_databases, organism_code),
                                           internalDir = FALSE,
                                           loadMatrix = "diffusion")
  
  
  enrich_analysis_this_organism <- defineCompounds(compounds = KEGG_codes,
                                                   data = fella.data_this_organism)
  
  assign(paste0(output_prefix, "enrich_analysis_", organism_code, output_suffix), enrich_analysis_this_organism, envir = .GlobalEnv)
  
  cat(paste0(length(getInput(enrich_analysis_sign_molecules_hsa)), " codes used; ",
             length(getExcluded(enrich_analysis_sign_molecules_hsa)), " codes excluded; ",
             length(Significative_molecules_KEGG_codes), " total codes\n"))
  
  
  enrich_analysis_run_this_organism <- runDiffusion(object = enrich_analysis_this_organism,
                                                    data = fella.data_this_organism,
                                                    approx = "normality")
  assign(paste0(output_prefix, "enrich_analysis_run_", organism_code, output_suffix), enrich_analysis_run_this_organism, envir = .GlobalEnv)
  
  
  enrich_analysis_run_resultsGraph_this_organism <- generateResultsGraph(object = enrich_analysis_run_this_organism,
                                                                         method = "diffusion",
                                                                         data = fella.data_this_organism,
                                                                         nlimit = 250)
  
  assign(paste0(output_prefix, "enrich_analysis_run_resultsGraph_", organism_code, output_suffix), enrich_analysis_run_resultsGraph_this_organism, envir = .GlobalEnv)
  
  
  
  
  exportResults(format = "csv", file = paste0(output_prefix, "enrich_analysis_run_resultsGraph_", organism_code, output_suffix, ".csv"),
                method = "diffusion", object = enrich_analysis_run_this_organism, data = fella.data_this_organism)
  
  ## potentially add geneOntology (not done here..)
  
  enrich_analysis_generated_table <- generateResultsTable(method = "diffusion",
                                                          nlimit = 1000, 
                                                          #threshold = 0.05, plimit = 15, LabelLengthAtPlot = 45,
                                                          #capPscores = 1e-06,
                                                          object = enrich_analysis_run_this_organism, data = fella.data_this_organism)
  
  assign(paste0(output_prefix, "enrich_analysis_generated_table_", organism_code, output_suffix), enrich_analysis_generated_table, envir = .GlobalEnv)
  
  
  plotGraph(graph = enrich_analysis_run_resultsGraph_this_organism,
            #layout = FALSE, graph.layout = NULL,
            #plotLegend = TRUE, plot.fun = "plot.igraph", NamesAsLabels = TRUE,
            vertex.label.cex = 0.5)
  
  
  write_graph(enrich_analysis_run_resultsGraph_this_organism, paste0(output_prefix, "enrich_analysis_graph_to_export_", organism_code, output_suffix, ".png"))
  
  png(paste0(output_prefix, "enrich_analysis_run_resultsGraph_", organism_code, output_suffix, ".png"), width = 1000, height = 1000)
  plotGraph(graph = enrich_analysis_run_resultsGraph_this_organism,
            #layout = FALSE, graph.layout = NULL,
            #plotLegend = TRUE, plot.fun = "plot.igraph", NamesAsLabels = TRUE,
            vertex.label.cex = 1)
  dev.off()
  
}



