## Resubmission
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
Examples provided for qb_run() are in dontrun{} because I use keyring::keyget()
to avoid exposing real Quickbase credentials/usertokens
