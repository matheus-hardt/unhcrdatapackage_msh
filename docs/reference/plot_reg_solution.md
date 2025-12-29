# Plot Regional Solution

Plot Regional Solution

## Usage

``` r
plot_reg_solution(
  year = 2024,
  region = "The Americas",
  lag = 10,
  label_font_size = 4,
  category_font_size = 10
)
```

## Examples

``` r
plot_reg_solution(
    year = 2024,
    region = "The Americas",
    label_font_size = 4,
    category_font_size = 10
)
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `unhcr_region = countrycode::countrycode(coa_iso, "iso3c",
#>   "unhcr.region")`.
#> Caused by warning:
#> ! Some values were not matched unambiguously: UNK
```
