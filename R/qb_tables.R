#' Get all tables
#'
#' \code{get_tables} Get metadata for all tables in an app.
#'
#' @template subdomain
#' @template auth
#' @template app_id
#' @template agent
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'    get_tables(subdomain = "abc",
#'               auth = keyring::key_get("qb_example"),
#'               app_id = "bsf5hphe5")
#' }
get_tables <- function(subdomain, auth, app_id, agent = NULL){

  if(!stringr::str_detect(auth, "^QB-USER-TOKEN ") &
     !stringr::str_detect(auth, "^QB-TEMP-TOKEN ")){
    auth <- stringr::str_c("QB-USER-TOKEN ", auth)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  qb_url <- paste0("https://api.quickbase.com/v1/tables?appId=", app_id)

  req <- httr::GET(url = qb_url,
                   encode = "json",
                   httr::accept_json(),
                   httr::add_headers("QB-Realm-Hostname" = subdomain,
                                     "User-Agent" = agent,
                                     "Authorization" = auth))

  tryCatch(
    resp <- jsonlite::fromJSON(httr::content(req, as = "text"), flatten = TRUE),
    error = function(e)
      return(req))

  return(tibble::as_tibble(resp))
}
