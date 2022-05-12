
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qbr

<!-- badges: start -->
<!-- badges: end -->

The goal of qbr is to make it easier to interact with Quickbase’s API.

## Installation

You can install the development version of qbr like so:

``` r
 library(devtools)
 install_github("Author/qbr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(qbr)

# Get data from a Quickbase report as a tibble
my_tibble <- qb_run(subdomain = "bhi",
    user_token = keyring::key_get("qb_example"),
    table_id = "bn9d8iesz",
    report_id = "1")
#> [1] "Calling Quickbase API"
#> Response [https://api.quickbase.com/v1/reports/1/run?tableId=bn9d8iesz]
#>   Date: 2022-05-12 13:58
#>   Status: 200
#>   Content-Type: application/json; charset=UTF-8
#>   Size: 3.69 kB
#> 
#> [1] "Cleaning extracted data"
#> [1] "Process complete"
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
