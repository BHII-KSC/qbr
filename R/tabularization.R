#' Flattens nested tibble from JSON payload
#' @noRd
tabularize <- function(data, fields){

  fields <- fields %>%
    lapply(., tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id = as.character(id),
                  label_type = paste(label, type, sep ="."))

  data <- data %>%
    dplyr::select(fields$id) %>%
    dplyr::rename_with( ~ fields$label_type) %>%
    dplyr::mutate(dplyr::across(tidyselect::matches("[.]multitext"),
                                ~ lapply(., paste, collapse = "; ")),
                  dplyr::across(tidyselect::matches("[.]multiuser"),
                                ~ purrr::map(purrr::map_depth(., 2, "email"), paste, collapse = "; ")),
                  dplyr::across(tidyselect::matches("[.]user"),
                                ~ purrr::map(., purrr::pluck, "email")),
                  dplyr::across(tidyselect::matches("[.]file"),
                                ~ purrr::map(., ~ purrr::pluck(.x[["versions"]], length(.x[["versions"]]), "fileName"))),
                  dplyr::across(tidyselect::everything(),
                                ~ purrr::map_if(., is.null, ~ NA)),
                  dplyr::across(tidyselect::everything(),
                                ~ unname(unlist(.))))

  return(data)
}
