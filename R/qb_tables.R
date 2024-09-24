#' Get all tables
#'
#' Get metadata for all tables in an app.
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

  stopifnot(val_subdomain(subdomain), is.character(app_id), length(app_id) == 1)

  auth <- val_token(auth)

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

  resp <- resp %>%
    tibble::as_tibble() %>%
    dplyr::relocate(c(id, name), .before = 1)

  return(resp)
}
