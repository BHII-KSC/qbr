## Submission 2025-01-10
* There were no errors, warnings, or notes from check() on my windows device
* devtools::check_mac_release and devtools::check_win_devel() ran without error, warning or notes.

## Submission 2023-09-24
* There were no errors, warnings, or notes from check() on my windows device
* devtools::check_mac_release and devtools::check_win_devel() ran without error, warning or notes.
* A reverse dependency check using revdepcheck::revdep_check found no reverse dependencies.

## Submission 2023-08-23
* There were no errors, warnings, or notes when running check() on my windows device
* Ran devtools::check_mac_release and rhub::check on a Debian OS without error, 
  warning, or notes.
* This version of qbr resolves an absence of a package help file by replacing qbr.R
  with qbr-package.R and thereby qbr-package.Rd. This problem was introduced when
  a breaking change in roxygen2 7.0.0 (2019-11-12) went unnoticed.

## Submission 2022-11-11
* There were no ERRORs, WARNINGs, or NOTEs on local Windows device using check()
* Ran devtools::check_mac_release  and rhub::check without error, 
  warning, or notes.

## Resubmission 2022-05-17
In this resubmitted version I:
* Removed a third instance of the print() function (qb_run)

## Resubmission 2022-05-16
In this resubmitted version I:
* Reworded the package description in DESCRIPTION to avoid the redundant opener "Functions to"
* Added a web reference for the 'Quickbase' JSON API in the description of DESCRIPTION
* Removed both instances of the print() function (qb_run)
* Renamed qb_run parameter 'user_token' as 'token' and added line 61 to allow a 
  second type of token be used for authentication. This change was not specifically 
  requested by the CRAN reviewer.

## R CMD check results
* There were no ERRORs, WARNINGs, or NOTEs on local Windows device:
* Also ran devtools::check_mac_release  and rhub::check without error, 
  warning, or notes.

## Downstream dependencies
There are currently no downstream dependencies for this package

## Comments
Examples provided for most function are in dontrun{} because I use keyring::keyget()
to avoid exposing real Quickbase credentials/usertokens
