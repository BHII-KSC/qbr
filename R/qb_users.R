#' Get users
#'
#' Get all users in an account. Provides options to limit
#' values to a set of users and/or apps.
#'
#' @template subdomain
#' @template auth
#' @template agent
#' @param account_id Optional. Positive integer. The account ID being used to
#'   get users. If no value is specified, the first account associated with the
#'   requesting user token is chosen.
#' @param user_emails Optional. List of characters. Limit returned users to those
#'   specified in this list.
#' @param app_ids Optional. List of characters. Limit returned users to those
#'   assigned to these app ID's. The provided app ID's should belong to the same
#'   account.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'    get_users(subdomain = "abc",
#'              auth = keyring::key_get("qb_example"))
#' }
get_users <- function(subdomain, auth, agent = NULL, account_id = NULL,
                      user_emails = NULL, app_ids = NULL){

  if(!stringr::str_detect(auth, "^QB-USER-TOKEN ") &
     !stringr::str_detect(auth, "^QB-TEMP-TOKEN ")){
    auth <- stringr::str_c("QB-USER-TOKEN ", auth)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  qb_url <- ifelse(is.null(account_id),
                   "https://api.quickbase.com/v1/users",
                   paste0("https://api.quickbase.com/v1/users?accountId=", account_id))

  qb_get_users(subdomain, auth, agent, qb_url, user_emails, app_ids)

}

qb_get_users <- function(subdomain, auth, agent, qb_url, user_emails, app_ids,
                         page_token = NULL, user_data = NULL){

  req_body <- list(emails = user_emails,
                   appIds = app_ids,
                   nextPageToken = page_token)

  req <- httr::POST(url = qb_url,
                    body = req_body,
                    encode = "json",
                    httr::accept_json(),
                    httr::add_headers("QB-Realm-Hostname" = subdomain,
                                      "User-Agent" = agent,
                                      "Authorization" = auth))

  tryCatch(
    resp <- jsonlite::fromJSON(httr::content(req, as = "text"), flatten = TRUE),
    error = function(e)
      return(req))

  user_page <- tibble::as_tibble(resp[["users"]])

  if(is.null(user_data)){
    user_data <- user_page
  } else {
    user_data <- rbind(user_data, user_page)
  }

  if(resp[["metadata"]][["nextPageToken"]] != ""){
    return(qb_get_users(subdomain, auth, agent, qb_url, user_emails, app_ids,
                        page_token = resp[["metadata"]][["nextPageToken"]],
                        user_data = user_data))
  } else {
    return(user_data)
  }

}
