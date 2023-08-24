#' Validates subdomain argument
#' @noRd
val_subdomain <- function(subdomain){
  if (!is.character(subdomain)){
    stop("subdomain must be of type character.")
  } else if (length(subdomain) != 1){
    stop("subdomain must be of length 1.")
  } else {
    return(TRUE)
  }
}

#' Validates token argument
#' @noRd
val_token <- function(token){
  if(!is.character(token)){
    stop("token must be of type character.")
  } else if(length(token) != 1){
    stop("token must be of length 1.")
  } else if (!stringr::str_detect(token, "^QB-USER-TOKEN ") &
     !stringr::str_detect(token, "^QB-TEMP-TOKEN ")){
    token <- stringr::str_c("QB-USER-TOKEN ", token)
  }
  return(token)
}
