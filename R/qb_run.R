#' Run a Quickbase report
#'
#' @importFrom magrittr %>%
#'
#' @param table_id A character vector with one element. Found in the URL of a Quickbase table between /db/ and ?
#' @param report_id A character vector with one element. Found in the 'Reports & Charts' page in Quickbase and in the report URL.
#' @param user_token A character vector with one element. Created in 'My Preferences' under 'Manage user tokens' link.
#' @param subdomain A character vector with one element. Found at the beginning of the Quickbase URL. Realm specific.
#' @param skip Optional. An integer. The number of rows to skip from the top of a record set.
#' @param top Optional. An integer. The limit on the number of records to pull starting top of a record set.
#' @param agent Optional. A character vector with one element.
#'
#' @return A tibble with the report's data.
#' @export
#'
#' @examples
#' # Minimum required info
#' my_tibble <- qb_run(table_id = "bn9d8iesz",
#'     report_id = "1",
#'     user_token = "b25vav_itkb_wux8ipccd4345dsaq7dvczqvddz",
#'     subdomain = "bhi.quickbase.com")
#'
#' # Full argument set
#' my_tibble <- qb_run(table_id = "bn9d8iesz",
#'     report_id = "1",
#'     user_token = "b25vav_itkb_wux8ipccd4345dsaq7dvczqvddz",
#'     subdomain = "bhi",
#'     skip = 2,
#'     top = 10,
#'     agent = "FileService_Integration_V2.1")
qb_run <- function(table_id, report_id, user_token, subdomain, skip = NULL, top = NULL, agent = NULL) {

  # Check that user parameter input and fix where broken
  if(!stringr::str_detect(user_token, "^QB-USER-TOKEN ")){
    user_token <- stringr::str_c("QB-USER-TOKEN ", user_token)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  if(!is.null(skip)){
    skip = stringr::str_c("&skip=", as.character(skip))
  }

  if(!is.null(top)){
    top = stringr::str_c("&top=", as.character(top))
  }

  # Build the API call using the function parameters supplied by user
  qb_url <- stringr::str_c("https://api.quickbase.com/v1/reports/",
                  report_id,
                  "/run?tableId=",
                  table_id,
                  skip,
                  top)

  # Deliver API call to QB via an HTTP request and store QB's JSON payload
  data_raw <- httr::POST(qb_url,
                         httr::accept_json(),
                         httr::add_headers("QB-Realm-Hostname" = subdomain,
                               "User-Agent" = agent,
                               "Authorization" = user_token))

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
