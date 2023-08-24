#' Delete records
#'
#' Delete one or more records from a table.
#'
#' @template subdomain
#' @template auth
#' @param from Character vector. Identifier of the target table.
#' @param where Character vector. Condition(s) which target one or more records for
#' deletion. Use the Quickbase query language to construct your condition(s).
#' @template agent
#'
#' @return Named integer vector
#' @export
#'
#' @examples
#' \dontrun{
#'   delete_records(subdomain = "abc",
#'              auth = keyring::key_get("qb_example"),
#'              from = "bn9d8iesz",
#'              where = "{6.EX.'105'}")
#' }
delete_records <- function(subdomain, auth, from, where, agent = NULL){
  stopifnot(val_subdomain(subdomain), is.character(from), length(from) == 1,
            is.character(where), length(where) == 1)
  auth <- val_token(auth)

  req <- httr2::request("https://api.quickbase.com/v1/records") %>%
    httr2::req_headers("QB-Realm-Hostname" = subdomain,
                "User-Agent" = agent,
                "Authorization" = auth) %>%
    httr2::req_body_json(list(from = from, where = where)) %>%
    httr2::req_method("DELETE")

  resp <- httr2::req_perform(req) %>%
    httr2::resp_body_json() %>%
    unlist()

  return(resp)
}
