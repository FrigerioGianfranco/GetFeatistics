# Get Chemical data from PubChem.

Given a set of molecules with at least a known identifier code for each
one, it first retrieve the PubChem CID (unless that was the passed
identifier), then it gets from PubChem the desired proprierties and
other identifier, and can also classify the compounds based on
ClassiFire classification. This function needs a stable internet
connection and might take several time (depending on the number of
compounds and information to retrieve).

## Usage

``` r
getChemData(
  id,
  idtype,
  properties = NULL,
  otheridentifiers = NULL,
  synonyms = NULL,
  ClassiFire = NULL
)
```

## Arguments

- id:

  vector with identifier codes.

- idtype:

  character of length 1 or of the same length as id. The elements of
  this vector must be one of the following: "CID", "SMILES", "InChI",
  "InChIKey". If only one is provided, it is assumed that all the id are
  of that type. Otherwise, the type of each element of id can be
  specified here.

- properties:

  NULL or a character. If "all" is passed, all the PubChem properties
  will be fetched. The most wanted properties you might want to get are:
  "Title", "SMILES", "InChI", "InChIKey", "IUPACName",
  "MolecularWeight", "ExactMass", "MonoisotopicMass". To know all the
  PubChem properties run all_PubChem_properties(), and you can also read
  the full description here:
  https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Compound-Property-Tables.

- otheridentifiers:

  NULL or a character. If "all" is passed, all the other identifiers
  available in PubChem will be fetched. The most common other
  identifiers you might want to pass here are: "CAS", "HMDB", "KEGG",
  "ChEBI", "ChEMBL", "DrugBank", "DSSTox".

- synonyms:

  NULL or an integer. If Inf is passed, all the synonyms available in
  PubChem will be fetched (with a maximum of 999). Otherwise, pass the
  number of top synonyms reported in PubChem to retrieve.

- ClassiFire:

  NULL or a character. If "all" is passed, all the levels of the
  ClassiFire classification will be retrieved; otherwise you can pass
  here only the desired ones. The levels are: "kingdom", "superclass",
  "class", "subclass", "level 5", "level 6", "level 7", "level 8".

## Value

A data frame (tibble) in which each row is a compound as provided in id.
The first column is the id provided, the second column is the PubChem
CID, all other columns contain the desired information retrieved. On the
console it will be also printed the status of the retrival.
