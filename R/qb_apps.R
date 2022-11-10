#' Copy an app
#'
#' \code{copy_app} Copy an app. Provides options to copy data and users.
#'
#' @template subdomain
#' @template auth
#' @template app_id
#' @param app_name Character vector with one element. Name the app's copy.
#' @param app_desc Optional. Character vector with one element. Describe the
#'   app's copy.
#' @template agent
#' @param users_and_roles Logical. If true, users will be copied along with
#'   their assigned roles. If false, users and roles will be copied but roles
#'   will not be assigned.
#' @param keep_data Logical. Whether to copy the app's data along with the
#'   schema.
#' @param exclude_files Logical. If keep_data is true, whether to copy the file
#'   attachments as well. If keep_data is false, this parameter is ignored.
#' @param assign_user_token Logical. Whether to add the user token used to make
#'   this request to the new app.
#'
#' @return A list.
#' @export
#'
#' @examples
#' \dontrun{
#'    copy_app(subdomain = "abc",
#'             auth = keyring::key_get("qb_example"),
#'             app_id = "bn9d8f78g",
#'             app_name = "Copy of my app",
#'             keep_data = TRUE)
#' }
copy_app <- function(subdomain, auth, app_id, app_name, app_desc = NULL,
                     agent = NULL, users_and_roles = FALSE, keep_data = FALSE,
                     exclude_files = TRUE, assign_user_token = TRUE){

  # Validate arguments and fix where possible
  stopifnot(is.character(subdomain), is.character(auth), is.character(app_id),
            is.logical(users_and_roles), is.logical(keep_data), is.logical(exclude_files),
            is.logical(assign_user_token), length(subdomain) == 1, length(auth) == 1,
            length(app_id) == 1)

  if(!stringr::str_detect(auth, "^QB-USER-TOKEN ") &
     !stringr::str_detect(auth, "^QB-TEMP-TOKEN ")){
    auth <- stringr::str_c("QB-USER-TOKEN ", auth)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  qb_url <- paste("https://api.quickbase.com/v1/apps", app_id, "copy", sep = "/")

  req_body <- list(name = app_name,
                   description = app_desc,
                   properties = list(
                     keepData = keep_data,
                     excludeFiles = exclude_files,
                     usersAndRoles = users_and_roles,
                     assignUserToken = assign_user_token))

  req <- httr::POST(url = qb_url,
                    body = req_body,
                    encode = "json",
                    httr::accept_json(),
                    httr::add_headers("QB-Realm-Hostname" = subdomain,
                                      "User-Agent" = agent,
                                      "Authorization" = auth))

  tryCatch(
    return(httr::content(req)),
    error = function(e)
      return(req))

}


#' Delete an app
#'
#' \code{delete_app} Delete an entire app, including all of the tables and data.
#'
#' @template subdomain
#' @template auth
#' @template app_id
#' @param app_name Character vector with one element. The name of the app to be
#'   delete. Confirms you want to delete the app.
#' @template agent
#'
#' @return A list.
#' @export
#'
#' @examples
#' \dontrun{
#'    delete_app(subdomain = "abc",
#'               auth = keyring::key_get("qb_example"),
#'               app_id = "bsf5hphe5",
#'               app_name = "R Testing copy")
#' }
delete_app <- function(subdomain, auth, app_id, app_name, agent = NULL){

  if(!stringr::str_detect(auth, "^QB-USER-TOKEN ") &
     !stringr::str_detect(auth, "^QB-TEMP-TOKEN ")){
    auth <- stringr::str_c("QB-USER-TOKEN ", auth)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  qb_url <- paste0("https://api.quickbase.com/v1/apps/", app_id)

  req <- httr::DELETE(url = qb_url,
                      body = list(name = app_name),
                      encode = "json",
                      httr::accept_json(),
                      httr::add_headers("QB-Realm-Hostname" = subdomain,
                                        "User-Agent" = agent,
                                        "Authorization" = auth))

  tryCatch(
    return(httr::content(req)),
    error = function(e)
      return(req))
}


#' Get app events
#'
#' \code{get_app_events} Get a tibble of events that can be triggered based on
#' data or user actions in this application, includes: Email notification,
#' Reminders, Subscriptions, QB Actions, Webhooks, record change triggered
#' Automations (does not include scheduled).
#'
#' @template subdomain
#' @template auth
#' @template app_id
#' @template agent
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'    get_app_events(subdomain = "abc",
#'                   auth = keyring::key_get("qb_example"),
#'                   app_id = "bn9d8f78g")
#' }
get_app_events <- function(subdomain, auth, app_id, agent = NULL){

  if(!stringr::str_detect(auth, "^QB-USER-TOKEN ") &
     !stringr::str_detect(auth, "^QB-TEMP-TOKEN ")){
    auth <- stringr::str_c("QB-USER-TOKEN ", auth)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  qb_url <- paste("https://api.quickbase.com/v1/apps", app_id, "events", sep = "/")

  req <- httr::GET(url = qb_url,
                   httr::accept_json(),
                   httr::add_headers("QB-Realm-Hostname" = subdomain,
                                     "User-Agent" = agent,
                                     "Authorization" = auth))

  tryCatch(
    events <- jsonlite::fromJSON(httr::content(req, as = "text"), flatten = TRUE),
    error = function(e)
      return(req))

  return(tibble::as_tibble(events))
}

#' Get an app
#'
#' \code{get_app} Get metadata for an app.
#'
#' @template subdomain
#' @template auth
#' @template app_id
#' @template agent
#' @param inc_sec Logical. Includes security properties if true.
#' @param inc_var Logical. Includes app variables if true.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'    get_app(subdomain = "abc",
#'               auth = keyring::key_get("qb_example"),
#'               app_id = "bsf5hphe5")
#' }
get_app <- function(subdomain, auth, app_id, agent = NULL, inc_sec = T, inc_var = T){

  if(!stringr::str_detect(auth, "^QB-USER-TOKEN ") &
     !stringr::str_detect(auth, "^QB-TEMP-TOKEN ")){
    auth <- stringr::str_c("QB-USER-TOKEN ", auth)
  }

  if(!stringr::str_detect(subdomain, "\\.+")){
    subdomain <- stringr::str_c(subdomain, ".quickbase.com")
  }

  qb_url <- paste0("https://api.quickbase.com/v1/apps/", app_id)

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

  app_data <- resp[names(resp) %in% c("securityProperties", "variables") == F] %>%
    tibble::as_tibble()


  if(inc_sec){
    sec <- tibble::as_tibble(resp[["securityProperties"]])
    sec <- sec %>%
      dplyr::rename_with(~ paste0("sec_", names(sec)))
    app_data <- app_data %>% dplyr::bind_cols(sec)
  }

  if(inc_var){
    var <- tibble::as_tibble(resp[["variables"]]) %>%
      dplyr::mutate(name = paste0("var_", name)) %>%
      tidyr::pivot_wider(names_from = name, values_from = value)
    app_data <- app_data %>% dplyr::bind_cols(var)
  }

  return(app_data)
}
