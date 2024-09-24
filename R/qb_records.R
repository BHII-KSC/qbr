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


#' Query for data
#'
#' Get tabular data from a Quickbase table using a query.
#'
#' @template subdomain
#' @template auth
#' @param from Character vector with one element. Table identifier.
#' @param select Optional. Numeric vector containing field identifiers for
#'   columns to return. If omitted, the default columns for the table will be
#'   returned.
#' @param where Optional. Character vector with one element. Use the Quickbase
#'   query language to set criteria for records to returned.
#' @param group_by Optional. Nested named list with 'fieldId' and 'grouping'
#'   pairs for each field to group by.
#' @param sort_by Optional. Nested named list with 'fieldId' and sort 'order'
#'   pairs for each field to sort by. See
#'   \href{https://developer.quickbase.com/operation/runQuery}{Quickbase JSON
#'   API documentation} for details on sort order configuration.
#' @template agent
#' @template skip
#' @template top
#' @param local_time Logical. When true, date time fields are returned using
#'   app's local time. When false, date time fields are returned using UTC time.
#' @template type_suffix
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'    # Get all data matching query specification
#'    my_tibble <- query_records(subdomain = "abc",
#'        auth = keyring::key_get("qb_example"),
#'        from = "bn9d8iesz",
#'        select = c(3, 6:9),
#'        where = "{8.EX.'6-month'}")
#'
#'    # Query data, group, then sort it
#'    my_tibble <- query_records(subdomain = "bhi",
#'        auth = keyring::key_get("qb_example"),
#'        from = "bn9d8iesz",
#'        select = c(3, 8:12),
#'        sort_by = list(list(fieldId = 12, order = "ASC"),
#'                       list(fieldId = 3, order = "DESC")),
#'        group_by = list(list(fieldId = 4, grouping = "equal-values"),
#'                        list(fieldId = 9, grouping = "equal-values")))
#' }
query_records <- function(subdomain, auth, from, select = as.numeric(),
                          where = NULL, group_by = NULL, sort_by = NULL,
                          agent = NULL, skip = 0, top = 0, local_time = FALSE,
                          type_suffix = FALSE){

  stopifnot(val_subdomain(subdomain), is.character(from), length(from) == 1,
            is.numeric(select), is.numeric(skip), is.numeric(top))
  auth <- val_token(auth)


  params <- list(from = from, select = select, where = where, sortBy = sort_by,
                 groupBy = group_by,
                 options = list(skip = skip, top = top,
                                compareWithAppLocalTime = local_time))


  # Call API and store payload as list of data (1) and fields (2)
  payload <- qb_query_records(subdomain, auth, params, skip, agent, paginate = TRUE)


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


qb_query_records <- function(subdomain, auth, params, skip, agent, paginate,
                      pages = NULL){

  req <- httr2::request("https://api.quickbase.com/v1/records/query") %>%
    httr2::req_headers("QB-Realm-Hostname" = subdomain,
                       "User-Agent" = agent,
                       "Authorization" = auth) %>%
    httr2::req_body_json(params) %>%
    httr2::req_method("POST")

  resp <- httr2::req_perform(req)

  # Extract JSON payload from HTTP response and flatten
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
  if(meta$totalRecords - skip > nrow(pages) &
     (nrow(pages) < params$options$top | params$options$top == 0) & paginate){

    params$options$skip <- params$options$skip + nrow(pages)
    return(qb_query_records(subdomain, auth, params, skip, agent, paginate, pages))

  } else {
    return(list("data" = pages, "fields" = payload[[2]]))
  }

}
