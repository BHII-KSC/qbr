
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qbr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/qbr)](https://cran.r-project.org/package=qbr)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/qbr)](https://cran.r-project.org/package=qbr)
<!-- badges: end -->

The goal of qbr is to make it easy to interact with Quickbase’s JSON
API.

## Installation

You can install the development version of qbr like so:

``` r
 library(devtools)
 install_github("BHII-KSC/qbr")
```

## Functions

| API page                                                                    | Function           | Description                                                  |
|:----------------------------------------------------------------------------|:-------------------|:-------------------------------------------------------------|
| [Users](https://developer.quickbase.com/operation/getUsers)                 | `get_users`        | Returns a tibble with details of each user in an account     |
| [User token](https://developer.quickbase.com/operation/cloneUserToken)      | `clone_token`      | Copy a usertoken                                             |
| [User token](https://developer.quickbase.com/operation/deactivateUserToken) | `deactivate_token` | Deactivate a usertoken                                       |
| [User token](https://developer.quickbase.com/operation/deleteUserToken)     | `delete_token`     | Delete a usertoken                                           |
| [Apps](https://developer.quickbase.com/operation/getApp)                    | `get_app`          | Get metadata for an app                                      |
| [Apps](https://developer.quickbase.com/operation/copyApp)                   | `copy_app`         | Copy an app                                                  |
| [Apps](https://developer.quickbase.com/operation/deleteApp)                 | `delete_app`       | Delete an app                                                |
| [Apps](https://developer.quickbase.com/operation/getAppEvents)              | `get_app_events`   | Returns a tibble of triggerable events                       |
| [Tables](https://developer.quickbase.com/operation/getAppTables)            | `get_tables`       | Get metadata for all tables in an app                        |
| [Fields](https://developer.quickbase.com/operation/getFields)               | `get_fields`       | Get metadata for all fields in a table                       |
| [Reports](https://developer.quickbase.com/operation/getReport)              | `get_report`       | Returns a named list of metadata for the specified report    |
| [Reports](https://developer.quickbase.com/operation/getTableReports)        | `get_reports`      | Returns a tibble of metadata for each report in a table      |
| [Reports](https://developer.quickbase.com/operation/runReport)              | `run_report`       | Returns a tibble containing all data in the specified report |
| N/A                                                                         | `summarize_app`    | Get metadata for an app and its users, tables, and fields    |

## Usage

It is often cumbersome to manually download data from Quickbase to work
on it in R. `run_report` makes it easy to extract report data via the
Quickbase JSON API:

``` r
library(qbr)

# Get data from a Quickbase report as a tibble
run_report(subdomain = "bhi",
       auth = keyring::key_get("qb_example"),
       table_id = "bn9d8iesz",
       report_id = "7")
#> # A tibble: 7 × 5
#>   `Date assessed` Accessible            Intuitive  `Record ID#` `Respondent ty…`
#>   <chr>           <chr>                 <chr>             <int> <chr>           
#> 1 2018-12-19      4 - Somewhat agree    4 - Somew…            2 Data analyst    
#> 2 2018-12-19      4 - Somewhat agree    5 - Stron…            1 Data analyst    
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

If you don’t know the report ID of the report you want to retrieve data
from, you can use `get_reports` to retrieve metadata about all reports
in a table:

``` r
library(qbr)

get_reports(subdomain = "bhi",
            auth = keyring::key_get("qb_example"),
            table_id = "bn9d8iesz")
#> # A tibble: 5 × 13
#>   description id    name  type  usedCount usedLast properties.disp… query.fields
#>   <chr>       <chr> <chr> <chr>     <int> <chr>    <lgl>            <list>      
#> 1 ""          6     Aspi… table        28 2022-06… FALSE            <int [25]>  
#> 2 ""          5     Find… table        60 2021-11… FALSE            <int [4]>   
#> 3 ""          1     List… table       107 2022-11… FALSE            <int [13]>  
#> 4 "Sorted by… 2     List… table         0 <NA>     TRUE             <int [0]>   
#> 5 ""          7     qbr … table        51 2022-11… FALSE            <int [5]>   
#> # … with 5 more variables: query.filter <chr>, query.formulaFields <list>,
#> #   query.groupBy <list>, query.sortBy <list>, query.tableId <chr>
```

It’s sometimes helpful to manage user tokens programmatically:

``` r
library(qbr)

# Clone a user token. The 'clone_name' must be unique. 
token <- clone_token(subdomain = "bhi", 
                     auth = keyring::key_get("qb_example"),
                     clone_name = "My new token",
                     clone_desc = "A token cloned by an R script")

# The token passed to 'auth' is deleted. Token supplied must be active.
delete_token(subdomain = "bhi", auth = token)
#> Token deleted
```

You can manage apps using the app functions:

``` r
library(qbr)

# Copy an app and print the new app's ID
app <- copy_app(subdomain = "bhi",
                auth = keyring::key_get("qb_example"),
                app_id = "bn9d8f78g",
                app_name = "R Testing copy",
                app_desc = "Used to test copy_app() from qbr package",
                keep_data = TRUE)

print(app$id)
#> NULL

# Delete the newly created app
delete_app(subdomain = "bhi",
           auth = keyring::key_get("qb_example"),
           app_id = app$id,
           app_name = app$name)
#> $message
#> [1] "NOT_FOUND"
#> 
#> $description
#> [1] "Error APIKIT:NOT_FOUND"

# Get the triggerable events of an app
get_app_events(subdomain = "bhi",
               auth = keyring::key_get("qb_example"),
               app_id = "bn9d8f78g")
#> # A tibble: 2 × 8
#>   isActive name     tableId type  owner.email owner.id owner.name owner.userName
#>   <lgl>    <chr>    <chr>   <chr> <chr>       <chr>    <chr>      <chr>         
#> 1 TRUE     Push on… bp5gg5… webh… john.erdma… 5962446… John Erdm… jerdmann      
#> 2 TRUE     GET Goo… bp84km… webh… john.erdma… 5962446… John Erdm… jerdmann
```

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
The run_report function does provide a type_suffix argument to allow you
to discern the intended datatype more easily.
