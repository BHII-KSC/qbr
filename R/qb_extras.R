#' Summarize an app
#'
#' Get metadata for an app, its tables, and its fields.
#'
#' @template subdomain
#' @template auth
#' @template app_id
#' @template agent
#'
#' @return A list of tibbles.
#' @export
#'
#' @examples
#' \dontrun{
#'    summarize_app(subdomain = "abc",
#'                  auth = keyring::key_get("qb_example"),
#'                  app_id = "bsf5hphe5")
#' }
summarize_app <- function(subdomain, auth, app_id, agent = NULL){

  stopifnot(val_subdomain(subdomain), is.character(app_id), length(app_id) == 1)

  auth <- val_token(auth)

  app <- qbr::get_app(subdomain, auth, app_id, agent, T, T)
  users <- qbr::get_users(subdomain, auth, agent, app_ids = list(app_id))
  tables <- qbr::get_tables(subdomain, auth, app_id, agent) %>%
    dplyr::mutate(spaceUsedInt = as.numeric(gsub("[[:alpha:]]|\\s", "", spaceUsed)),
                  spaceUsedMB = ifelse(grepl(" KB", spaceUsed), spaceUsedInt / 1000,
                                ifelse(grepl(" MB", spaceUsed), spaceUsedInt,
                                ifelse(grepl(" GB", spaceUsed), spaceUsedInt * 1000,
                                ifelse(grepl(" TB", spaceUsed), spaceUsedInt * 1000000,
                                       spaceUsedInt)))))
  fields <- list()

  for (i in 1:nrow(tables)){
    fields[[i]] <- qbr::get_fields(subdomain, auth, tables[[i, "id"]], agent, T, T)
  }

  names(fields) <- tables$name
  fields <- dplyr::bind_rows(fields, .id = "name")

  overview <- app %>%
    dplyr::select(name, id, description) %>%
    dplyr::mutate(num_users = nrow(users),
                  num_tables = nrow(tables),
                  table_names = paste0(tables$name, collapse = ";"),
                  table_data_size_MB = sum(tables$spaceUsedMB),
                  num_fields = nrow(fields))

  sum_data <- list(Overview = overview, App = app, Users = users, Tables = tables, Fields = fields)

  return(sum_data)
}
