#' Get all fields in a table
#'
#' Get metadata for all fields in a table.
#'
#' @template subdomain
#' @template auth
#' @template table_id
#' @template agent
#' @param include_props Logical. Includes field properties if true.
#' @param include_perms Logical. Includes field permissions if true.
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

  if(!stringr::str_detect(auth, "^QB-USER-TOKEN ") &
     !stringr::str_detect(auth, "^QB-TEMP-TOKEN ")){
    auth <- stringr::str_c("QB-USER-TOKEN ", auth)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  qb_url <- paste0("https://api.quickbase.com/v1/fields?tableId=", table_id,
                   "&includeFieldPerms=", include_perms)

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

  if(include_props){
    props <- tibble::as_tibble(resp[["properties"]])
    props <- props %>%
      dplyr::rename_with(~ paste0("prop_", names(props)))
    field_data <- field_data %>% dplyr::bind_cols(props)
  }

  if(include_perms){
    perms <- dplyr::bind_rows(resp[["permissions"]], .id = "id") %>%
      dplyr::mutate(role = paste("perm", role, roleId, sep = "_")) %>%
      dplyr::select(-roleId) %>%
      tidyr::pivot_wider(names_from = role, values_from = permissionType)

    field_data <- field_data %>% dplyr::bind_cols(perms %>% dplyr::select(-id))
  }

  return(field_data)
}
