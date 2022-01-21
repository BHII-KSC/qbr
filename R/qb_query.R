#' Query quickbase tables
#'
#' @importFrom magrittr %>%
#'
#' @param user_token A character vector with one element. Created in 'My Preferences' under 'Manage user tokens' link.
#' @param subdomain A character vector with one element. Found at the beginning of the Quickbase URL. Realm specific.
#' @param table_id A character vector of table ids. Found in the 'Reports & Charts' page in Quickbase and in the report URL.
#' @param query Optional. A character vector with one element. A query contructed using Quickbase's query language.
#' @param field_ids Optional. A vector of integers representing the field ids you want returned.
#' @param agent Optional. A character vector with one element.
#'
#' @return A list of tibbles with queried data
#' @export
#'
#' @examples
qb_query <- function(subdomain, user_token, table_id, query = NULL, field_ids = NULL, agent = NULL) {

  # Check that user parameter input and fix where broken
  if(!stringr::str_detect(user_token, "^QB-USER-TOKEN ")){
    user_token <- stringr::str_c("QB-USER-TOKEN ", user_token)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  # Build body of http request
  json_body <- jsonlite::toJSON(
    list(from = table_id, select = field_ids, where = query),
    auto_unbox = TRUE)

  # Deliver API call to QB via an HTTP request and store QB's JSON payload
  data_raw <- httr::POST("https://api.quickbase.com/v1/records/query",
                         httr::accept_json(),
                         httr::add_headers("QB-Realm-Hostname" = subdomain,
                                           "User-Agent" = agent,
                                           "Authorization" = user_token),
                         body = json_body)

  # Stop the script if we receive an error code in response to our request
  httr::stop_for_status(data_raw)

  # Tidy JSON payload in a tibble
  data_as_text <- jsonlite::fromJSON(httr::content(data_raw, as = "text"), flatten = T)
  data_fields <- tibble::as_tibble(data_as_text[[2]]) %>%
    dplyr::mutate(id = as.character(id))
  data_clean <- tibble::as_tibble(data_as_text[[1]]) %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::rename_with(~ stringr::str_replace(., "\\.value", "")) %>%
    tidyr::pivot_longer(-row_id, names_to = "id", values_to = "vals") %>%
    dplyr::left_join(data_fields %>% dplyr::select(-type), by = "id") %>%
    dplyr::select(-id) %>%
    tidyr::pivot_wider(names_from = label, values_from = vals) %>%
    dplyr::select(-row_id)

  return(data_clean)
}
