# Plot Regional Share

Plot Regional Share

## Usage

``` r
plot_reg_share(
  year = 2024,
  region = "The Americas",
  pop_type = "REF",
  label_font_size = 15
)
```

## Examples

``` r
plot_reg_share(
    year = 2024,
    region = "The Americas",
    label_font_size = 15
)
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `unhcr_region = countrycode::countrycode(coa_iso, "iso3c",
#>   "unhcr.region")`.
#> Caused by warning:
#> ! Some values were not matched unambiguously: UNK
```
