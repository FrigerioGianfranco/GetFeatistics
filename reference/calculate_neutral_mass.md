# Calculate neutral mass considering the observed m/z and the assigned adducts.

Given mass-to-charge ratio measurement(s) and given adduct
estimation(s), it calculates the related neutral mass.

## Usage

``` r
calculate_neutral_mass(observed_mz, adduct)
```

## Arguments

- observed_mz:

  numeric. Observed mass-to-charge ratios.

- adduct:

  character. Possible adduct estimated. Examples: .

## Value

A numeric vector, with the calculated neutral masses. NA will be
computed in case of adducts with formula elements not recognised.

## Examples

``` r
if (FALSE) { # \dontrun{


## Examples of adducts include:

# "[M+H]+"
# "[M-H]-"
# "[M+NH4]+"
# "[M+Na]+"
# "[M+H-H2O]+"
# "[M+2H]2+"
# "[M-H2O-H]-"
# "[M+Cl]-"
# "[M+FA-H]-"
# "[M+HCOO]-"


calculate_neutral_mass(observed_mz = c(174.14748, 240.84195, 460.3591, 245.22432),
                       adduct = c("[M+H]+", "[M+H]+", "[M+Na]+", "[M+H-H2O]+"))


} # }
```
