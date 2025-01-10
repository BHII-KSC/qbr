#' Get all fields in a table
#'
#' Get metadata for all fields in a table.
#'
#' @template subdomain
#' @template auth
#' @template table_id
#' @template agent
#' @param include_props Logical. Includes field properties if true.
#' @param include_perms Logical. Includes custom field permissions if true. Only
#'   returns data if custom permissions exist for at least 1 field in the table.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'    get_fields(subdomain = "abc",
#'               auth = keyring::key_get("qb_example"),
#'               table_id = "bsf5hphe5")
#' }
get_fields <- function(subdomain, auth, table_id, agent = NULL, include_props = T, include_perms = F){

  stopifnot(val_subdomain(subdomain), is.character(table_id), length(table_id) == 1)

  auth <- val_token(auth)

  qb_url <- paste0("https://api.quickbase.com/v1/fields?tableId=", table_id,
                   "&includeFieldPerms=", tolower(include_perms))

  req <- httr::GET(url = qb_url,
                   encode = "json",
                   httr::accept_json(),
                   httr::add_headers("QB-Realm-Hostname" = subdomain,
                                     "User-Agent" = agent,
                                     "Authorization" = auth))

  tryCatch(
    resp <- jsonlite::fromJSON(httr::content(req, as = "text"), flatten = F),
    error = function(e)
      return(req))

  field_data <- resp[names(resp) %in% c("properties", "permissions") == F] %>%
    tibble::as_tibble()

  if(include_props & "properties" %in% names(resp)){
        props <- tibble::as_tibble(resp[["properties"]])
        props <- props %>%
          dplyr::rename_with(~ paste0("prop_", names(props)))
        field_data <- field_data %>% dplyr::bind_cols(props)
  }

  if(include_perms & "permissions" %in% names(resp)){
    perms <- dplyr::bind_rows(resp[["permissions"]], .id = "id") %>%
      dplyr::mutate(role = paste("perm", role, roleId, sep = "_")) %>%
      dplyr::select(-roleId) %>%
      tidyr::pivot_wider(names_from = role, values_from = permissionType)

    field_data <- field_data %>% dplyr::bind_cols(perms %>% dplyr::select(-id))
  }

  field_data <- field_data %>% dplyr::relocate(c(id, label, fieldType), .before = 1)

  return(field_data)
}


#' Delete field(s) in a table
#'
#' Delete a list of one or more fields in a table.
#'
#' @template subdomain
#' @template auth
#' @template table_id
#' @param field_ids Character or numeric vector. Field identifier for fields to delete.
#' @template agent
#'
#' @return A tibble with the status of the fields passed to field_ids.
#' @export
#'
#' @examples
#' \dontrun{
#'    delete_fields(subdomain = "abc",
#'               auth = keyring::key_get("qb_example"),
#'               table_id = "bsf5hphe5",
#'               field_ids = c(40:43, 45))
#' }
delete_fields <- function(subdomain, auth, table_id, field_ids, agent = NULL){

  stopifnot(val_subdomain(subdomain), is.character(table_id), length(table_id) == 1,
            (is.character(field_ids) | is.numeric(field_ids)))

  auth <- val_token(auth)
  qb_url <- paste0("https://api.quickbase.com/v1/fields?tableId=", table_id)
  field_ids <- list(fieldIds = field_ids)


  req <- httr2::request(qb_url) %>%
    httr2::req_headers("QB-Realm-Hostname" = subdomain,
                       "User-Agent" = agent,
                       "Authorization" = auth) %>%
    httr2::req_body_json(field_ids) %>%
    httr2::req_method("DELETE")

  resp <- httr2::req_perform(req)

  # Extract JSON payload from HTTP response
  tryCatch(
    payload <- httr2::resp_body_json(resp),
    error = function(e)
      stop("The report data could not be parsed.
           This is likely due to special characters in text or rich-text fields.
           Try removing fields containing extended ASCII characters from your
           Quickbase report, such as &#146;"))


  # Detect deletions & errors, then tidy and bind
  dels <- payload %>% purrr::pluck("deletedFieldIds") %>% data.frame()
  errs <- payload %>% purrr::pluck("errors") %>% data.frame()

  if(nrow(dels) > 0) {
    dels <- dels %>%
      tidyr::pivot_longer(tidyr::everything(), values_to = "field_id") %>%
      dplyr::mutate(status = "deleted",
                    field_id = as.character(field_id),
                    message = "") %>%
      dplyr::select(-name)
  }

  if(nrow(errs) > 0){
    errs <- errs %>%
      tidyr::pivot_longer(tidyr::everything(), values_to = "field_id") %>%
      dplyr::mutate(status = "error",
             message = field_id,
             field_id = stringr::str_remove_all(field_id, "[[^0-9]]")) %>%
      dplyr::select(-name)
  }

  resp_tidy <- dplyr::bind_rows(dels, errs)

  return(resp_tidy)
}
