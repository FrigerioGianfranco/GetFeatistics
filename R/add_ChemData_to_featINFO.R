#' Add chemical data to a featINFO table.
#'
#' Given a featINFO table, with at least a column with a known identifier, it first retrieves the PubChem CID (unless that was the passed identifier), then it gets from PubChem the desired proprieties and other identifiers, and can also classify the compounds based on ClassiFire classification, and adds it to the featINFO table. This function is a wrapper of the function getChemData.
#'
#' @param featINFO data frame. The featINFO table.  
#' @param name_column_id character of length 1. Name of the column of the featINFO table containing the identifiers.
#' @param idtype character of length 1. One of the following: "CID", "SMILES", "InChI", "InChIKey". The type of the identifier containied in the column name_column_id of featINFO.
#' @param properties NULL or a character. If "all" is passed, all the PubChem properties will be fetched. The most wanted properties you might want to get are: "Title", "SMILES", "InChI", "InChIKey", "IUPACName", "MolecularWeight", "ExactMass", "MonoisotopicMass". To know all the PubChem properties run all_PubChem_properties(), and you can also read the full description here: https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Compound-Property-Tables.
#' @param otheridentifiers NULL or a character. If "all" is passed, all the other identifiers available in PubChem will be fetched. The most common other identifiers you might want to pass here are: "CAS", "HMDB", "KEGG", "ChEBI", "ChEMBL", "DrugBank", "DSSTox".
#' @param synonyms NULL or an integer. If Inf is passed, all the synonyms available in PubChem will be fetched (with a maximum of 999). Otherwise, pass the number of top synonyms reported in PubChem to retrieve.
#' @param ClassiFire NULL or a character. If "all" is passed, all the levels of the ClassiFire classification will be retrieved; otherwise you can pass here only the desired ones. The levels are: "kingdom", "superclass", "class", "subclass", "level 5", "level 6", "level 7", "level 8".
#'
#' @return the featINFO data frame with the additional columns containing the retrieved chemical data.
#'
#' @export
add_ChemData_to_featINFO <- function(featINFO, name_column_id, idtype, properties = NULL, otheridentifiers = NULL, synonyms = NULL, ClassiFire = NULL) {
  
  if (!is.data.frame(featINFO)) {stop("featINFO must be a data frame")}
  
  if (length(name_column_id) != 1) {stop("name_column_id must be a character of length 1")}
  if (!is.character(name_column_id)) {stop("name_column_id must be a character of length 1")}
  if (is.na(name_column_id)) {stop("name_column_id must be a character of length 1, not a missing value")}
  if (length(which(colnames(featINFO) == name_column_id)) != 1) {stop("name_column_id must be the name of a unique column of featINFO")}
  
  if (length(idtype) != 1) {stop("idtype must be a character of length 1")}
  if (!is.character(idtype)) {stop("idtype must be a character of length 1")}
  if (is.na(idtype)) {stop("idtype must be a character of length 1, not a missing value")}
  
  
  TABLE_CHEMDATA <- getChemData(id = pull(featINFO, name_column_id),
                                idtype = idtype,
                                properties = properties,
                                otheridentifiers = otheridentifiers,
                                synonyms = synonyms,
                                ClassiFire = ClassiFire)
  
  if (TABLE_CHEMDATA$idtype[1] == "CID") {
    TABLE_CHEMDATA <- TABLE_CHEMDATA[, -c(1,2,3)]
  } else {
    TABLE_CHEMDATA <- TABLE_CHEMDATA[, -c(1,2)]
  }
  
  if (which(colnames(featINFO) == name_column_id) == length(colnames(featINFO))) {
    featINFO_output <- bind_cols(featINFO, TABLE_CHEMDATA)
  } else {
    featINFO_before <- featINFO[, 1:which(colnames(featINFO) == name_column_id)]
    featINFO_after <- featINFO[, (which(colnames(featINFO) == name_column_id)+1):length(colnames(featINFO))]
    
    featINFO_output <- bind_cols(featINFO_before, TABLE_CHEMDATA)
    featINFO_output <- bind_cols(featINFO_output, featINFO_after)
  }
  
  return(featINFO_output)
}
