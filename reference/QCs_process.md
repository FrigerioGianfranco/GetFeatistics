# Filter a dataset of features considering pooled quality control samples.

Given a dataframe with feature intensities from a non-targeted
experiment and another dataframe containing information about sample
types, it filters the dataframe of feature intensities considering some
defined cut-off in the quality control samples.

## Usage

``` r
QCs_process(
  featmatrix,
  sampletype,
  sep_QC = FALSE,
  QC_to_merge = NULL,
  step1 = TRUE,
  step1_cutoff = 50,
  step2 = TRUE,
  step2_cutoff = 50,
  step3 = TRUE,
  step3_cutoff = 50,
  step4 = FALSE,
  step4_cutoff = c(20, 80),
  rtrn_filtered_table = TRUE,
  remove_results = FALSE,
  remove_QC_and_blanks = FALSE
)
```

## Arguments

- featmatrix:

  a dataframe of feature intensities. The first column should contain
  the feature name, each other columns should be related to an analysis
  of a sample and contains the feature intensities.

- sampletype:

  a dataframe containing two or three columns: the first one contains
  the name of samples reported in the featmatrix. The second column
  contains, for each sample, one of the following: "REMOVE", for samples
  to be discharged; "blank", for blank samples; "QC", for pooled quality
  control samples; "QC_half", for pooled quality control samples diluted
  by a half; "unknown", for other samples. The third column should
  contain the sample category that will be considered if sep_QC is TRUE.

- sep_QC:

  logical. If TRUE, separate QC filtration will be performed. For more
  details check: Frigerio et al. 2022
  https://doi.org/10.3390/molecules27082580.

- QC_to_merge:

  charter vector. If sep_QC is TRUE, it should contain the names of all
  the groups of pooled QC samples to consider for the final merge of the
  filtered features

- step1:

  logical. Whether to discharge features above a certain relative
  standard deviation percentage (RSD%) of QCs, defined by step1_cutoff

- step1_cutoff:

  numeric of length 1. If step1 is TRUE, features with a RSD% of QCs
  above this value will be considered not suitable.

- step2:

  logical. Whether to discharge features below a certain QCs count%,
  defined by step2_cutoff.

- step2_cutoff:

  numeric of length 1. If step2 is TRUE, features with valid values (not
  NA) in less than a percentage of QCs specified in this value will be
  considered not suitable.

- step3:

  logical. Whether to discharge features above a certain blank
  contribution%, defined by step3_cutoff.

- step3_cutoff:

  numeric of length 1. If step3 is TRUE, features with a blank
  contribution % (calculated as mean blank/mean QCs\*100) higher that
  this value, will be considered not suitable.

- step4:

  logical. Whether to discharge features not included in a certain range
  comparing QCs with the QC_half, defined by step4_cutoff.

- step4_cutoff:

  numeric of length 2. If step4 is TRUE,features that in the QC_half are
  not within this percentage range comapred to QCs will be considered
  not suitable.

- rtrn_filtered_table:

  logical. if TRUE returns the filtered table, if FALSE returns the
  non-filtered table (useful only if you want to check the parameters
  such as QC CV% or blank contribution in the features that would be
  filtered)

- remove_results:

  logical. If TRUE, returns the table of features, if FALSE, the last
  columns will report the results such as QC CV% and blank contribution

- remove_QC_and_blanks:

  logical. At the end of the processing, if TRUE it will remove columns
  containing QCs and blanks (so, if also remove_results is TRUE and
  rtrn_filtered_table is TRUE, at the end you will have a great table of
  feature intensities with only samples ready for the next processing)

## Value

a tibble with the feature intensities as result of the QC processing
