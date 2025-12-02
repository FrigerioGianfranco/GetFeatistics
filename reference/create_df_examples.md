# Create example datasets

It create mock datasets that can be used to test other functions of this
package.

## Usage

``` r
create_df_examples(
  datasets = c("all", "df_example_generic", "df_example_targeted",
    "df_example_targeted_legend", "df_example_targeted_compounds_legend",
    "df_example_sample_data", "df_example_feat_intensities", "df_example_feat_info",
    "df_example_qc_sampletype", "df_example_melecules_to_search", "df_example_gradient")
)
```

## Arguments

- datasets:

  one of the following: "all", "df_example_generic",
  "df_example_targeted", "df_example_targeted_legend",
  "df_example_targeted_compounds_legend", "df_example_sample_data",
  "df_example_feat_intensities", "df_example_feat_info",
  "df_example_qc_sampletype", "df_example_melecules_to_search",
  "df_example_gradient".

  - If "all", all of the following dataframes (tibble) will be created
    as different objects.

  - df_example_generic: generic dataframe useful for testing some
    statistical functions.

  - df_example_targeted: example for the first argument of
    get_targeted_elaboration.

  - df_example_targeted_legend: example for the second argument of
    get_targeted_elaboration.

  - df_example_targeted_compounds_legend: example for the third argument
    of get_targeted_elaboration.

  - df_example_sample_data: example of the metadata (additional
    variables besides molecular data, in real life they could be age,
    sex, BMI) that can be used to test statistical functions.

  - df_example_feat_intensities: example of featTable, with feature
    intensities.

  - df_example_feat_info: example of featINFO, with retention times and
    m/z.

  - df_example_qc_sampletype: example of a table to pass in the
    sampletype argument of the QCs_process function.

  - df_example_melecules_to_search; example of known standards to pass
    to the third argument of checkmolecules_in_feat_table.

  - df_example_gradient: example for get_phase_amount;
    features_data_example, example of a feature data table.

## Value

it directly creates the object(s) in the current environment.
