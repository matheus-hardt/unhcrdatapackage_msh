# Plot Regional Map

Plot Regional Map

## Usage

``` r
plot_reg_map(
  year = 2024,
  region = "The Americas",
  topn = 5,
  pop_type = c("REF", "ASY", "OIP"),
  projection = "Mercator",
  maxSymbolsize = 0.25
)
```

## Examples

``` r
plot_reg_map(
    year = 2024,
    region = "The Americas"
)
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `unhcr_region = countrycode::countrycode(coa_iso, "iso3c",
#>   "unhcr.region")`.
#> Caused by warning:
#> ! Some values were not matched unambiguously: UNK
#> Warning: There was 1 warning in `stopifnot()`.
#> ℹ In argument: `unhcr_region = countrycode::countrycode(iso_a3, "iso3c",
#>   "unhcr.region")`.
#> Caused by warning:
#> ! Some values were not matched unambiguously: -99, ALA, ASM, ATF, BLM, FLK, GGY, HMD, IMN, IOT, JEY, NFK, PCN, SHN, TWN, VIR, WLF
```
