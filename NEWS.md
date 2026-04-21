# GetFeatistics v1.1.1
- **Fix:** `gentab_FC_more_than2levels()` now correctly report the paired comparison order in the column names (before it just showed the opposite order of the two groups if second_to_first_ratio was TRUE)

# GetFeatistics v1.1
- Created the new function `calculate_neutral_mass()`, that calculates the neutral mass for each given observed m/z and estimated adduct.
- Added the argument `add_neutral_mass_calc` to the function `get_feat_info_from_MSDial()`. If TRUE is passed to it, the function `calculate_neutral_mass()` is internally applied and those calculated neutral masses are added in a new column called 'neutral_mass_calc'.

# GetFeatistics v1.0.2
- the requirement of installing git before installing this package has been added in the README and in the vignettes.
- minor update in `gentab_P.t.test()`, `gentab_P.1wayANOVA_posthocTukeyHSD()`, `gentab_P.2wayANOVA_posthocTukeyHSD()`: if groupdiff is TRUE and FDR is TRUE, the differences between groups are now showed both considering p-values and FDR corrected p-values (before, if FDR was TRUE, they were showed considering exclusively FDR corrected p-values).

# GetFeatistics v1.0.1
- **Fix:** `gentab_P.t.test()` now performs paired t-tests correctly by using the vector interface of `stats::t.test()` (rather than the formula method), preventing errors when `paired = TRUE`.

# GetFeatistics v1.0
- released along with the paper

  Frigerio G (2025). “Streamlining feature elaboration and statistics analysis in metabolomics: the GetFeatistics R-package.” _Journal of Integrative Bioinformatics_.
  <https://doi.org/10.1515/jib-2025-0047>.


