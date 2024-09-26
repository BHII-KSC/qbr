# qbr (development version)

# qbr 1.2.4
- Added a function to run a user-defined query against a Quickbase table.
- Belatedly heralding a function to insert & update records into a Quickbase table that was released in v1.2.3 but went unmentioned in news & release notes.
- Fixed an issue with run_report which failed when a report included some field types (e.g., User fields).
- Replaced run_report dependency 'httr' with the more modern 'httr2' equivalent.
- Added validation for key arguments in all functions
- get_fields and get_tables now place the most important columns at the left side of the tibble returned.

# qbr 1.2.3
- Added a function to delete records.
- Revised run_report function to construct tibble with columns in the same order
as the queried report.
- Fixed an issue with get_users where returned users were not limited to the "app_ids" supplied.
- Made an internal function to validate common API inputs and correct where possible.
- Fixed a [package documentation issue](https://github.com/r-lib/roxygen2/issues/1491) that
resulted from an unnoticed change to roxygen.
- Began transition from httr to httr2 for API calls.

# qbr 1.2.2
- Fixed an issue with parsing table size with decimal values.

# qbr 1.2.1
- Fixed an error thrown by get_fields when include_perms was set to TRUE but no custom permissions existed for fields in the table queried. 
- Fixed an error thrown by get_app when include_vars was set to TRUE but no variables existed for an app.
- Fixed an error thrown by summarize_app caused by the bugs in get_fields and get_app.

# qbr 1.2.0
Added functions to do the following:

- get metadata for an app
- get metadata for all tables in an app
- get metadata for all fields in a table
- summarize metadata for an app and its users, tables, and fields

# qbr 1.1.0
Added functions to do the following:  
  
- copy an app  
- delete an app  
- get app events  
- get users  
- get metadata about one or more reports  
- clone a user token  
- deactivate a user token  
- delete a user token  

# qbr 1.0.0
Published to CRAN after removing 'print' statements

# qbr 0.0.0.9000
Initial release of qbr on GitHub
