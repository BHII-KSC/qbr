#' Run a Quickbase report
#'
#' \code{qb_run} asks the Quickbase API to run a report and returns its data.
#'
#' @importFrom magrittr %>%
#'
#' @param subdomain Character vector with one element. Found at the beginning of
#'   the Quickbase URL. Realm specific.
#' @param user_token Character vector with one element. Created in 'My
#'   Preferences' under 'Manage user tokens' link.
#' @param table_id Character vector with one element. Found in the URL of a
#'   Quickbase table between /db/ and ?
#' @param report_id Character vector with one element. Found in the 'Reports &
#'   Charts' page in Quickbase and in the report URL.
#' @param agent Optional. Character vector with one element. Describes
#'   user/agent making API call.
#' @param skip Optional. Integer. The number of rows to skip from the top of a
#'   record set.
#' @param top Optional. Integer. The limit on the number of records to pull
#'   starting at the top of a record set.
#' @param type_suffix Optional. Logical. Set TRUE to append each field label
#'   with its Quickbase data type.
#' @param paginate Optional. Logical. Set TRUE to recursively call the API until
#'   all report pages are collected
#'
#' @return A tibble.
#'
#'
#' @references \href{https://developer.quickbase.com/}{Quickbase API
#'   documentation}
#'
#' @export
#'
#' @examples
#' # Get all data in a report
#' my_tibble <- qb_run(subdomain = "bhi",
#'     user_token = keyring::key_get("qb_example"),
#'     table_id = "bn9d8iesz",
#'     report_id = "1")
#'
#' # Get rows 3 to 6 from a report
#' my_tibble <- qb_run(subdomain = "bhi.quickbase.com",
#'     user_token = keyring::key_get("qb_example"),
#'     table_id = "bn9d8iesz",
#'     report_id = "1",
#'     skip = 2,
#'     top = 3)
qb_run <- function(subdomain, user_token, table_id, report_id, agent = NULL,
                   skip = 0, top = 0, type_suffix = FALSE, paginate = TRUE) {

  # Validate arguments and fix where possible
  # stopifnot()

  if(!stringr::str_detect(user_token, "^QB-USER-TOKEN ")){
    user_token <- stringr::str_c("QB-USER-TOKEN ", user_token)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  # Call API
  print("Calling Quickbase API")
  data_text <- run_report(subdomain, user_token, table_id, report_id, agent, skip, top, paginate)

  # Prepare field labels for renaming values object
  data_fields <- data_text[[2]] %>%
    dplyr::mutate(id = as.character(id),
                  label_type = stringr::str_c(label, type, sep =".")) %>%
    dplyr::arrange(id)

  # Multi-dimensional fields to drop by suffix
  drop_me <- c(".version", ".id", ".name", ".userName")

  tryCatch(
    data_clean <- data_text[[1]] %>%
      dplyr::select(-dplyr::contains(drop_me)) %>%
      dplyr::rename_with( ~ data_fields$label_type) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("[.]multitext"), ~ lapply(., paste, collapse = "; "))) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("[.]multiuser"), ~ purrr::map(., "email"))) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("[.]multiuser"), ~ lapply(., paste, collapse = "; "))) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("[.]multiuser"), ~ unlist(.))),
    error = function(e)
      stop("The report data could not be parsed.
           Try removing fields with complex data types."))

  # Reduce # records to 'top' (only applies to paginated returns)
  if(nrow(data_clean) > top & top > 0){
    data_clean <- data_clean[1:top, ]
  }

  if(type_suffix == FALSE){
    data_clean <- data_clean %>%
      dplyr::rename_with( ~ stringr::str_remove(., "[.].*"))
  }

  return(data_clean)
}

#' Calls QB API 'run report' function
#' @noRd
run_report <- function(subdomain, user_token, table_id, report_id, agent,
                       skip, top, paginate, pages = NULL, page_skip = 0){

  # Needed when paginating
  total_skip <- ifelse(page_skip == 0, skip, page_skip)

  # Build the API call
  qb_url <- paste0("https://api.quickbase.com/v1/reports/", report_id,
                           "/run?tableId=", table_id,
                           if(total_skip > 0){paste0("&skip=", total_skip)},
                           if(top > 0){paste0("&top=", top)})

  # Deliver API call to QB via an HTTP request, store response
  data_raw <- httr::POST(qb_url,
                         httr::accept_json(),
                         httr::add_headers("QB-Realm-Hostname" = subdomain,
                                           "User-Agent" = agent,
                                           "Authorization" = user_token))

  # Stop the script if HTTP request fails
  httr::stop_for_status(data_raw)

  print(data_raw)

  # Extract JSON payload from HTTP response and flatten
  tryCatch(
    data_text <- jsonlite::fromJSON(httr::content(data_raw, as = "text"), flatten = TRUE),
    error = function(e)
      stop("The report data could not be parsed.
           This is likely due to special characters in text or rich-text fields.
           Try removing fields containing extended ASCII characters from your
           Quickbase report, such as &#146;"))

  new_page <- tibble::as_tibble(data_text[[1]])
  meta <- data_text[[3]]

  # Stack new page onto extracted pages
  if(!is.null(pages)){
    pages <- rbind(pages, new_page)
  } else {
    pages <- new_page
  }

  # If not last page, recur
  if(meta$totalRecords - skip > nrow(pages) & (nrow(pages) < top | top == 0) & paginate){

    print("Dataset too large for single call, paginating...")

    return(run_report(subdomain, user_token, table_id, report_id, agent,
                      skip, top, paginate, pages, skip + nrow(pages)))

  } else {
    return(list("data" = pages, "fields" = data_text[[2]]))
  }

}
