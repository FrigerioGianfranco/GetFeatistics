# Perform FELLA Enrichment Analysis.

Given KEGG codes and a defined organism, it performs the enrichment
analysis with the FELLA package.

## Usage

``` r
do_FELLA_enrichment_analysis(
  organism_code,
  KEGG_codes,
  path_databases = "C:/databases/FELLA/",
  output_prefix = "",
  output_suffix = ""
)
```

## Arguments

- organism_code:

  character of length 1. The KEGG organism code. For example, for homo
  sapiens (human) is 'hsa'. All codes are here:
  https://www.genome.jp/kegg/tables/br08606.html.

- KEGG_codes:

  Character. KEGG codes of compounds.

- path_databases:

  Character of length 1. Path of local organism databases.

- output_prefix:

  Character of length 1. What to add at the beginning of generated file
  names and object names.

- output_suffix:

  Character of length 1. What to add at the end of generated file names
  and object names.

## Value

Export csv table and png file, along with creating object in the global
environment.
