#' Query quickbase records
#'
#' \code{qb_query} submits a user-defined query to the Quickbase API and returns
#' the result.
#'
#' @importFrom magrittr %>%
#'
#' @param subdomain Character vector with one element. Found at the beginning of
#'   the Quickbase URL. Realm specific.
#' @param user_token Character vector with one element. Created in 'My
#'   Preferences' under 'Manage user tokens' link.
#' @param table_id Character vector with one element. Found in the URL of a
#'   Quickbase table between /db/ and ?
#' @param agent Optional. Character vector with one element. Describes
#'   user/agent making API call.
#' @param query Optional. A character vector with one element. A query
#'   constructed using Quickbase's query language. Omitting this argument will
#'   return all records.
#' @param field_ids Optional. Integer. Represents the field ids you want
#'   returned. Omitting this argument will return the \emph{default columns}.
#' @param json_out Optional. Logical. Set TRUE if you want JSON returned instead
#'   of a tibble
#' @param flat Optional. Logical. Set TRUE to reduce nesting in the payload.
#' @param type_suffix Optional. Logical. Set TRUE to append each field label
#'   with its Quickbase data type.
#'
#' @return A tibble or JSON object.
#'
#' @section Warning: Extracting nested fields (e.g., user-lists) will result a
#'   nested tibble.
#'
#' @references \href{https://developer.quickbase.com/}{Quickbase API
#'   documentation}
#'
#' @export
#'
#' @examples
#' # Get all records with \emph{default columns} for a table
#' qb_query(subdomain = "bhi",
#'     user_token = keyring::key_get("qb_example"),
#'     table_id = "bn9d8iesz")
#'
#' # Get records where field_id 6 != 105 for specified columns.
#' qb_query(subdomain = "bhi.quickbase.com",
#'     user_token = keyring::key_get("qb_example"),
#'     table_id = "bn9d8iesz",
#'     agent = "je_testing",
#'     query = "{6.XEX.105}",
#'     field_ids = c(1, 2, 3, 6, 10, 11),
#'     json_out = FALSE,
#'     flat = TRUE)


qb_query <- function(subdomain, user_token, table_id, agent = NULL, query = NULL, field_ids = NULL, json_out = FALSE, flat = TRUE, type_suffix = FALSE) {

  # Check that user parameter input and fix where broken
  if(!stringr::str_detect(user_token, "^QB-USER-TOKEN ")){
    user_token <- stringr::str_c("QB-USER-TOKEN ", user_token)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  # Build body of http request
  body_list <- list(from = table_id, where = query, select = field_ids)
  body_list <- body_list[!sapply(body_list, is.null)]

  json_body <- jsonlite::toJSON(
    body_list,
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

  # Extract JSON payload from HTTP response and flatten
  data_text <- jsonlite::fromJSON(httr::content(data_raw, as = "text"), flatten = flat)


  # Extract JSON payload from HTTP response and flatten
  data_text <- jsonlite::fromJSON(httr::content(data_raw, as = "text"), flatten = flat)

  if(json_out){
    data_clean <- jsonlite::toJSON(data_text)
  } else {

    # Prepare field labels for renaming values object
    data_fields <- tibble::as_tibble(data_text[[2]]) %>%
      dplyr::mutate(id = as.character(id),
                    label_type = stringr::str_c(label, type, sep =".")) %>%
      dplyr::arrange(id)

    # Multi-dimensional fields to drop by suffix
    drop_me <- c(".version", ".id", ".name", ".userName")

    tryCatch(
      data_clean <- tibble::as_tibble(data_text[[1]]) %>%
        dplyr::select(-dplyr::contains(drop_me)) %>%
        dplyr::rename_with( ~ data_fields$label_type) %>%
        dplyr::mutate(dplyr::across(dplyr::matches("[.]multitext"), ~ lapply(., paste, collapse = "; "))) %>%
        dplyr::mutate(dplyr::across(dplyr::matches("[.]multiuser"), ~ purrr::map(., "email"))) %>%
        dplyr::mutate(dplyr::across(dplyr::matches("[.]multiuser"), ~ lapply(., paste, collapse = "; "))) %>%
        dplyr::mutate(dplyr::across(dplyr::matches("[.]multiuser"), ~ unlist(.))),
      error = function(e)
        stop("The report data could not be parsed.
             Consider trying again setting json_out = TRUE and flat = FALSE."))

    if(type_suffix == FALSE){
      data_clean <- data_clean %>%
        dplyr::rename_with( ~ stringr::str_remove(., "[.].*"))
    }
  }

  return(data_clean)
}
