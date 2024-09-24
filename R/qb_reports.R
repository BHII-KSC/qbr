#' Run a report
#'
#' Run a report and get its data.
#'
#' @importFrom magrittr %>%
#'
#' @template subdomain
#' @template auth
#' @template table_id
#' @template report_id
#' @template agent
#' @template skip
#' @template top
#' @template type_suffix
#' @param paginate Optional. Logical. Set TRUE to recursively call the API until
#'   all report pages are collected.
#'
#' @return A tibble.
#'
#'
#' @references \href{https://developer.quickbase.com}{Quickbase API
#'   documentation}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'    # Get all data in a report
#'    my_tibble <- run_report(subdomain = "abc",
#'        auth = keyring::key_get("qb_example"),
#'        table_id = "bn9d8iesz",
#'        report_id = "1")
#'
#'    # Get rows 3 to 6 from a report
#'    my_tibble <- run_report(subdomain = "abc.quickbase.com",
#'        auth = keyring::key_get("qb_example"),
#'        table_id = "bn9d8iesz",
#'        report_id = "1",
#'        skip = 2,
#'        top = 3)
#' }
run_report <- function(subdomain, auth, table_id, report_id, agent = NULL,
                   skip = 0, top = 0, type_suffix = FALSE, paginate = TRUE) {

  # Validate arguments and fix where possible
  stopifnot(val_subdomain(subdomain), is.character(table_id),
            is.character(report_id), is.numeric(skip), is.numeric(top),
            is.logical(type_suffix), is.logical(paginate), length(table_id) == 1,
            length(report_id) == 1)

  auth <- val_token(auth)

  # Call API and store payload as list of data (1) and fields (2)
  payload <- qb_run_report(subdomain, auth, table_id, report_id, agent, skip, top, paginate)


  tryCatch(
    data_clean <- tabularize(payload[[1]], payload[[2]]),
    error = function(e)
      stop("The report data could not be parsed.
           Try removing fields with complex data types."))

  # Reduce # records to 'top' (only applies to paginated returns)
  if(nrow(data_clean) > top & top > 0){
    data_clean <- data_clean[1:top, ]
  }

  if(type_suffix == FALSE){
    data_clean <- data_clean %>%
      dplyr::rename_with( ~ stringr::str_remove(., "[.][^.]+?$"))
  }

  return(data_clean)
}



#' Run a Quickbase report
#'
#' Run a report and get its data.
#'
#' @importFrom magrittr %>%
#'
#' @param subdomain Character vector with one element. Found at the beginning of
#'   the Quickbase URL. Realm specific.
#' @param token Character vector with one element. Created in 'My Preferences'
#'   under 'Manage user tokens' link.
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
#' @references \href{https://developer.quickbase.com}{Quickbase API
#'   documentation}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'     # Get all data in a report
#'     my_tibble <- qb_run(subdomain = "abc",
#'                         token = keyring::key_get("qb_example"),
#'                         table_id = "bn9d8iesz",
#'                         report_id = "1")
#'
#'     # Get rows 3 to 6 from a report
#'     my_tibble <- qb_run(subdomain = "abc.quickbase.com",
#'                         token = keyring::key_get("qb_example"),
#'                         table_id = "bn9d8iesz",
#'                         report_id = "1",
#'                         skip = 2,
#'                         top = 3)
#' }
qb_run <- function(subdomain, token, table_id, report_id, agent = NULL,
                   skip = 0, top = 0, type_suffix = FALSE, paginate = TRUE){
  .Deprecated("run_report")
  run_report(subdomain, token, table_id, report_id, agent = NULL,
             skip = 0, top = 0, type_suffix = FALSE, paginate = TRUE)
}

qb_run_report <- function(subdomain, auth, table_id, report_id, agent,
                       skip, top, paginate, pages = NULL, page_skip = 0){

  # Needed when paginating
  total_skip <- ifelse(page_skip == 0, skip, page_skip)

  # Build the API call
  qb_url <- paste0("https://api.quickbase.com/v1/reports/", report_id,
                           "/run?tableId=", table_id,
                           if(total_skip > 0){paste0("&skip=", total_skip)},
                           if(top > 0){paste0("&top=", top)})

  # Deliver API call to QB via an HTTP request, store response
  req <- httr2::request(qb_url) %>%
    httr2::req_headers("QB-Realm-Hostname" = subdomain,
                "User-Agent" = agent,
                "Authorization" = auth) %>%
    httr2::req_method("POST")

  resp <- httr2::req_perform(req)


  # Extract JSON payload from HTTP response
  tryCatch(
    payload <- httr2::resp_body_json(resp),
    error = function(e)
      stop("The report data could not be parsed.
           This is likely due to special characters in text or rich-text fields.
           Try removing fields containing extended ASCII characters from your
           Quickbase report, such as &#146;"))

  new_page <- payload[[1]] %>%
    lapply(., tibble::as_tibble) %>%
    dplyr::bind_rows()

  meta <- payload[[3]] %>% tibble::as_tibble()

  # Stack new page onto extracted pages
  if(!is.null(pages)){
    pages <- rbind(pages, new_page)
  } else {
    pages <- new_page
  }

  # If not last page, recur
  if(meta$totalRecords - skip > nrow(pages) & (nrow(pages) < top | top == 0) & paginate){

    return(qb_run_report(subdomain, auth, table_id, report_id, agent,
                      skip, top, paginate, pages, skip + nrow(pages)))

  } else {
    return(list("data" = pages, "fields" = payload[[2]]))
  }

}


#' Get a report
#'
#' Get metadata about a report.
#'
#' @template subdomain
#' @template auth
#' @template table_id
#' @template report_id
#' @template agent
#'
#' @return A named list.
#' @export
#'
#' @examples
#' \dontrun{
#'    get_report(subdomain = "abc",
#'               auth = keyring::key_get("qb_example"),
#'               table_id = "bn9d8iesz",
#'               report_id = "7")
#' }
get_report <- function(subdomain, auth, table_id, report_id, agent = NULL){

  # Validate arguments and fix where possible
  stopifnot(is.character(report_id), length(report_id) == 1)

  qb_get_report(subdomain, auth, table_id, report_id, agent)
}


#' Get all reports for a table
#'
#' \code{get_reports} retrieves metadata for each report in a table.
#'
#' @template subdomain
#' @template auth
#' @template table_id
#' @template agent
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'    get_reports(subdomain = "abc",
#'               auth = keyring::key_get("qb_example"),
#'               table_id = "bn9d8iesz")
#' }
get_reports <- function(subdomain, auth, table_id, agent = NULL){
  reports <- qb_get_report(subdomain, auth, table_id, agent = agent)
  tibble::as_tibble(reports)
}


qb_get_report <- function(subdomain, auth, table_id, report_id = NULL, agent){

  # Validate arguments and fix where possible
  stopifnot(val_subdomain(subdomain), is.character(table_id), length(table_id) == 1)

  auth <- val_token(auth)

  qb_url <- ifelse(is.null(report_id),
                   paste0("https://api.quickbase.com/v1/reports", "?tableId=", table_id),
                   paste0("https://api.quickbase.com/v1/reports/", report_id, "?tableId=", table_id))

  # Deliver API call to QB via an HTTP request, store response
  data_raw <- httr::GET(qb_url,
                        httr::accept_json(),
                        httr::add_headers("QB-Realm-Hostname" = subdomain,
                                          "User-Agent" = agent,
                                          "Authorization" = auth))

  # Stop if HTTP request fails
  httr::stop_for_status(data_raw)

  # Extract JSON payload from HTTP response and flatten
  tryCatch(
    data_text <- jsonlite::fromJSON(httr::content(data_raw, as = "text"), flatten = TRUE),
    error = function(e)
      stop("The response could not be parsed."))
}
