#' Query builder
#'
#' @description Construct query for request
#' @param service Service being used
#' @param ... Any acceptable WebAPI parameters
#' @param token App ID
#'
build_query <- function(service, ..., token = NULL) {
  arg_params <- names(list(...))
  arg_values <- unlist(list(...), use.names = FALSE)
  paths <- arg_params |>
    sapply(build_path, USE.NAMES = FALSE) |>
    sprintf(arg_values) |>
    paste(collapse = "/")
  query <- request(base_uri()) |>
    req_url_path_append(service) |>
    req_url_path_append(paths)
  if (!is.null(token)) {
    query <- add_token(query, token)
  }
  return(query)
}

#' Path builder
#'
#' @description Construct path for query
#' @param param Query parameter name
#' @param value Query parameter value. Used if replace = FALSE
#' @param replace Replacable value, default to TRUE
#'
build_path <- function(param, value = NULL, replace = TRUE) {
  if (replace) value = "%s" else value
  return(paste(param, value, sep = "/"))
}

#' Dataframe builder
#'
#' @description Construct tibble dataframe from list of JSON
#' @param listdata List data from JSON
#' @importFrom dplyr as_tibble
#'
build_dataframe <- function(listdata) {
  rows <- lapply(listdata, function(row) {
    row |>
      unlist() |>
      t() |>
      dplyr::as_tibble()
  })
  return(do.call(rbind, rows))
}

#' Add token
#'
#' @description Add token to the end of query
#' @param query Full query without token
#' @param token App ID
#'
add_token <- function(query, token) {
  req_url_path_append(query, build_path("key", token, replace = FALSE))
}

#' Get token
#'
#' @description Handle missing token and get from system environment
get_token <- function() {
  token <- Sys.getenv("BPS_APP_ID")
  if (token == "") stop(message_token()) else token
}
