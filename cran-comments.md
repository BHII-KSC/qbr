## R CMD check results
* There were no ERRORs, WARNINGs, or NOTEs on local Windows device:
* Also ran devtools::check_mac_release  and rhub::check without error, 
  warning, or notes.

## Downstream dependencies
There are currently no downstream dependencies for this package

## Comments
Examples provided for qb_run() are in dontrun{} because I use keyring::keyget()
to avoid exposing real Quickbase credentials/usertokens
