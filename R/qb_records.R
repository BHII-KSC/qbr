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


#' Insert/Update records
#'
#' Insert and/or update record(s) in a table. Update can use the key field on
#' the table, or any other supported unique field. Refer to the
#' \href{https://developer.quickbase.com/fieldInfo}{Field types page} for more
#' information about how each field type should be formatted. This operation
#' allows for incremental processing of successful records, even when some of
#' the records fail. This endpoint supports a maximum payload size of 10MB.
#'
#' @template subdomain
#' @template auth
#' @param to Character vector. Identifier of the target table.
#' @param records Tibble containing the data you want to insert/update in the
#'   target table, where column names correspond to field identifiers.
#'   Alternatively, a list containing record data as json if your data contain
#'   User or List-user data types.
#' @param mergeFieldId Character vector. Field identifier of the key field or
#'   unique field to merge upon. Defaults to 3, representing Record ID#.
#' @param fieldsToReturn Character vector of field identifiers. Returns data for
#'   field 3 (Record ID#) in addition to any field identifiers supplied.
#' @template agent
#'
#' @return List of JSON containing return data requested via the fieldsToReturn
#'   argument and metadata regarding created/updated records, referenced but
#'   unchanged records, and records having any errors while being processed.
#' @export
#'
#' @examples
#' \dontrun{
#'   new_data <- dplyr::tibble(`3` = c(5, 7), `6` = c("A", "B"))
#'   update_records(subdomain = "bhi",
#'                  auth = keyring::key_get("qb_example"),
#'                  to = "bn9d8iesz",
#'                  records = new_data)
#' }
update_records <- function(subdomain, auth, to, records, mergeFieldId = 3,
                           fieldsToReturn = list(3), agent = NULL){

  stopifnot(val_subdomain(subdomain), is.character(to), length(to) == 1)
  auth <- val_token(auth)

  # Account for all allowable record data types
  if(tibble::is_tibble(records)){
    json_body <- records %>%
      purrr::transpose() %>%
      purrr::map_depth(2, \(y) list(value = y))
  } else {
    json_body <- records
  }

  # Multiple components get stuffed into the body for this API endpoint
  json_body <- json_body %>%
    list(to = to,
         data = .,
         mergeFieldId = mergeFieldId,
         fieldsToReturn = fieldsToReturn) %>%
    jsonlite::toJSON(auto_unbox = T)


  req <- httr2::request("https://api.quickbase.com/v1/records") %>%
    httr2::req_headers("QB-Realm-Hostname" = subdomain,
                       "User-Agent" = agent,
                       "Authorization" = auth) %>%
    httr2::req_body_raw(json_body) %>%
    httr2::req_method("POST")


  resp <- httr2::req_perform(req) %>%
    httr2::resp_body_json()

  return(resp)
}
