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
