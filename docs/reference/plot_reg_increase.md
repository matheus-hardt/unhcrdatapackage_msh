# Plot Regional Increase

Plot Regional Increase

## Usage

``` r
plot_reg_increase(
  year = 2024,
  lag = 5,
  topn = 5,
  region = "The Americas",
  pop_type = c("REF", "ASY", "OIP"),
  category_font_size = 10
)
```

## Examples

``` r
plot_reg_increase(
    year = 2024,
    region = "The Americas",
    category_font_size = 10
)
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `unhcr_region = countrycode::countrycode(coa_iso, "iso3c",
#>   "unhcr.region")`.
#> Caused by warning:
#> ! Some values were not matched unambiguously: UNK
```
