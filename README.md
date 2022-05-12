
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
       user_token = keyring::key_get("qb_example"),
       table_id = "bn9d8iesz",
       report_id = "1")
#> [1] "Calling Quickbase API"
#> Response [https://api.quickbase.com/v1/reports/1/run?tableId=bn9d8iesz]
#>   Date: 2022-05-12 17:51
#>   Status: 200
#>   Content-Type: application/json; charset=UTF-8
#>   Size: 3.69 kB
#> # A tibble: 7 × 13
#>   `User-friendly`    Accessible Intuitive `Easy to start` Powerful `Good syntax`
#>   <chr>              <chr>      <chr>     <chr>           <chr>    <chr>        
#> 1 3 - Neutral        4 - Somew… 5 - Stro… 3 - Neutral     5 - Str… 2 - Somewhat…
#> 2 5 - Strongly agree 4 - Somew… 4 - Some… 4 - Somewhat a… 5 - Str… 3 - Neutral  
#> 3 1 - Strongly disa… 2 - Somew… 1 - Stro… 1 - Strongly d… 5 - Str… 4 - Somewhat…
#> 4 3 - Neutral        4 - Somew… 3 - Neut… 4 - Somewhat a… 4 - Som… 4 - Somewhat…
#> 5 3 - Neutral        3 - Neutr… 2 - Some… 4 - Somewhat a… 5 - Str… 2 - Somewhat…
#> 6 2 - Somewhat disa… 4 - Somew… 2 - Some… 5 - Strongly a… 1 - Str… 3 - Neutral  
#> 7 2 - Somewhat disa… 4 - Somew… 2 - Some… 2 - Somewhat d… 3 - Neu… 2 - Somewhat…
#> # … with 7 more variables: Efficient <chr>, Flexible <chr>, `Record ID#` <int>,
#> #   `Respondent ID` <chr>, `Date of assessment` <chr>,
#> #   `Assessment period` <chr>, `Respondent type` <chr>
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

This package makes no attempt to convert datatypes. Number fields in
Quickbase may well be interpreted in the resulting tibble as characters.
The qb_run function does provide a type_suffix argument to allow you to
discern the intended datatype more easily.
