#' Clone a user token
#'
#' \code{clone_token} Makes a copy of the supplied token and returns its value.
#'
#' @param subdomain Character vector with one element. Found at the beginning of
#'   the Quickbase URL. Realm specific.
#' @param auth Character vector with one element. The Quickbase authentication
#'   scheme you are using to authenticate the request.
#' @param agent Optional. Character vector with one element. Describes
#'   user/agent making API call.
#' @param clone_name Optional. Character vector with one element. Name the token
#'   clone.
#' @param clone_desc Optional. Character vector with one element. Provide a
#'   description for the token clone.
#'
#' @return A character vector with one element containing the token clone.
#'
#' @references \href{https://developer.quickbase.com}{Quickbase API
#'   documentation}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'    x <- clone_token(subdomain = "abc",
#'                     auth = keyring::key_get("qb_example"),
#'                     clone_name = "My new token",
#'                     clone_desc = "This clone was created using R")
#' }
clone_token <- function(subdomain, auth, agent = NULL, clone_name = NULL, clone_desc = NULL){
  req_body <- list(name = clone_name, description = clone_desc)
  token <- manage_token(subdomain, auth, "clone", agent, req_body)
}

#' Deactivate a user token
#'
#' \code{deactivate_token} Makes an active user token inactive.
#'
#' @param subdomain Character vector with one element. Found at the beginning of
#'   the Quickbase URL. Realm specific.
#' @param auth Character vector with one element. The Quickbase authentication
#'   scheme you are using to authenticate the request.
#' @param agent Optional. Character vector with one element. Describes
#'   user/agent making API call.
#'
#' @return A message confirming deactivation was successful.
#' @export
#'
#' @examples
#' \dontrun{
#'    x <- deactivate_token(subdomain = "abc",
#'                     auth = keyring::key_get("qb_example"))
#' }
deactivate_token <- function(subdomain, auth, agent = NULL){
  manage_token(subdomain, auth, "deactivate", agent)
  message("Token deactivated")
}

#' Delete a user token
#'
#' @param subdomain Character vector with one element. Found at the beginning of
#'   the Quickbase URL. Realm specific.
#' @param auth Character vector with one element. The Quickbase authentication
#'   scheme you are using to authenticate the request.
#' @param agent Optional. Character vector with one element. Describes
#'   user/agent making API call.
#'
#' @return A message confirming deactivation was successful.
#' @export
#'
#' @examples
#' \dontrun{
#'    x <- delete_token(subdomain = "abc",
#'                     auth = keyring::key_get("qb_example"))
#' }
delete_token <- function(subdomain, auth, agent = NULL){
  manage_token(subdomain, auth, "delete", agent)
  message("Token deleted")
}

manage_token <- function(subdomain, auth, action, agent, req_body){

  # Validate arguments and fix where possible
  stopifnot(is.character(subdomain), is.character(auth),
            length(subdomain) == 1, length(auth) == 1)

  if(!stringr::str_detect(auth, "^QB-USER-TOKEN ") &
     !stringr::str_detect(auth, "^QB-TEMP-TOKEN ")){
    auth <- stringr::str_c("QB-USER-TOKEN ", auth)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  # Build the API call
  qb_url <- paste0("https://api.quickbase.com/v1/usertoken/", action)

  # Deliver API call to QB via an HTTP request, store response
  if(action == "clone"){
    data_raw <- httr::POST(qb_url,
                         body = req_body,
                         encode = "json",
                         httr::accept_json(),
                         httr::add_headers("QB-Realm-Hostname" = subdomain,
                                           "User-Agent" = agent,
                                           "Authorization" = auth))
  } else if(action == "deactivate"){
    data_raw <- httr::POST(qb_url,
                           httr::accept_json(),
                           httr::add_headers("QB-Realm-Hostname" = subdomain,
                                             "User-Agent" = agent,
                                             "Authorization" = auth))
  } else if(action == "delete"){
    data_raw <- httr::DELETE("https://api.quickbase.com/v1/usertoken",
                           httr::accept_json(),
                           httr::add_headers("QB-Realm-Hostname" = subdomain,
                                             "User-Agent" = agent,
                                             "Authorization" = auth))
  }

  # Stop if HTTP request fails
  httr::stop_for_status(data_raw)

  # Extract JSON payload from HTTP response and flatten
  tryCatch(
    data_text <- jsonlite::fromJSON(httr::content(data_raw, as = "text"), flatten = TRUE),
    error = function(e)
      stop("The JSON response could not be parsed."))

  # Return just the token value
  token <- data_text$token
}
