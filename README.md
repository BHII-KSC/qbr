
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qbr

<!-- badges: start -->
<!-- badges: end -->

The goal of qbr is to make it easy to interact with Quickbase’s JSON
API.

## Installation

You can install the development version of qbr like so:

``` r
 library(devtools)
 install_github("BHII-KSC/qbr")
```

## Usage

It is often cumbersome to manually download data from Quickbase to work
on it in R. The qb_run function makes it easy to extract report data via
the Quickbase JSON API:

``` r
library(qbr)

# Get data from a Quickbase report as a tibble
qb_run(subdomain = "bhi",
       token = keyring::key_get("qb_example"),
       table_id = "bn9d8iesz",
       report_id = "7")
#> # A tibble: 7 × 5
#>   `Date assessed` Accessible            Intuitive  `Record ID#` `Respondent ty…`
#>   <chr>           <chr>                 <chr>             <int> <chr>           
#> 1 2018-12-19      4 - Somewhat agree    5 - Stron…            1 Data analyst    
#> 2 2018-12-19      4 - Somewhat agree    4 - Somew…            2 Data analyst    
#> 3 2018-12-19      2 - Somewhat disagree 1 - Stron…            3 Evaluator       
#> 4 2018-12-19      4 - Somewhat agree    3 - Neutr…            4 Evaluator       
#> 5 2019-12-04      3 - Neutral           2 - Somew…           20 Data analyst    
#> 6 2021-03-30      4 - Somewhat agree    2 - Somew…           22 Data analyst    
#> 7 2019-11-27      4 - Somewhat agree    2 - Somew…            5 Data analyst
```

Notice that this function returns a tibble even though the payload from
Quickbase is non-tabular JSON. This function extracts data from the
Quickbase report (recursively if needed to handle the API’s
auto-pagination) and then makes the data tidy using ‘tidyverse’
principles.

## Complex data types

This packages returns some field types slightly differently to a
Quickbase report:

| Field type        | Returned data type                  |
|:------------------|:------------------------------------|
| Multi-select text | Semicolon-separated text            |
| User              | User’s email address                |
| List-user         | Semicolon-separated email addresses |

## Limitations

Some extended ASCII characters (codes 128-255) cannot be properly parsed
when converting from JSON at present, such as ’ (&#146). Reports
containing text and rich-text fields are the most likely to suffer an
error resulting from the use of these extended characters.

This package makes no attempt to convert data types. Number fields in
Quickbase may well be interpreted in the resulting tibble as characters.
The qb_run function does provide a type_suffix argument to allow you to
discern the intended datatype more easily.
