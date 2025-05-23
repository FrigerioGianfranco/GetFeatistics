% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_ChemData_to_featINFO.R
\name{add_ChemData_to_featINFO}
\alias{add_ChemData_to_featINFO}
\title{Add chemical data to a featINFO table.}
\usage{
add_ChemData_to_featINFO(
  featINFO,
  name_column_id,
  idtype,
  properties = NULL,
  otheridentifiers = NULL,
  synonyms = NULL,
  ClassiFire = NULL
)
}
\arguments{
\item{featINFO}{data frame. The featINFO table.}

\item{name_column_id}{character of length 1. Name of the column of the featINFO table containing the identifiers.}

\item{idtype}{character of length 1. One of the following: "CID", "SMILES", "InChI", "InChIKey". The type of the identifier containied in the column name_column_id of featINFO.}

\item{properties}{NULL or a character. If "all" is passed, all the PubChem properties will be fetched. The most wanted properties you might want to get are: "Title", "SMILES", "InChI", "InChIKey", "IUPACName", "MolecularWeight", "ExactMass", "MonoisotopicMass". To know all the PubChem properties run all_PubChem_properties(), and you can also read the full description here: https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Compound-Property-Tables.}

\item{otheridentifiers}{NULL or a character. If "all" is passed, all the other identifiers available in PubChem will be fetched. The most common other identifiers you might want to pass here are: "CAS", "HMDB", "KEGG", "ChEBI", "ChEMBL", "DrugBank", "DSSTox".}

\item{synonyms}{NULL or an integer. If Inf is passed, all the synonyms available in PubChem will be fetched (with a maximum of 999). Otherwise, pass the number of top synonyms reported in PubChem to retrieve.}

\item{ClassiFire}{NULL or a character. If "all" is passed, all the levels of the ClassiFire classification will be retrieved; otherwise you can pass here only the desired ones. The levels are: "kingdom", "superclass", "class", "subclass", "level 5", "level 6", "level 7", "level 8".}
}
\value{
the featINFO data frame with the additional columns containing the retrieved chemical data.
}
\description{
Given a featINFO table, with at least a column with a known identifier, it first retrieves the PubChem CID (unless that was the passed identifier), then it gets from PubChem the desired proprieties and other identifiers, and can also classify the compounds based on ClassiFire classification, and adds it to the featINFO table. This function is a wrapper of the function getChemData.
}
