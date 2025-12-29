# Plot Regional Population Type Absolute

Plot Regional Population Type Absolute

## Usage

``` r
plot_reg_population_type_abs(
  year = 2024,
  region = "The Americas",
  top_n_countries = 5,
  pop_type = "REF",
  show_diff_label = TRUE,
  label_font_size = 6,
  category_font_size = 10
)
```

## Examples

``` r
plot_reg_population_type_abs(
    year = 2024,
    region = "The Americas",
    label_font_size = 6,
    category_font_size = 10
)
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `unhcr_region = countrycode::countrycode(coa_iso, "iso3c",
#>   "unhcr.region")`.
#> Caused by warning:
#> ! Some values were not matched unambiguously: UNK
```
