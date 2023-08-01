#' Query builder
#'
#' @description Construct query for request
#' @param service Service being used
#' @param token App ID
#'
build_query <- function(service, token, ...) {
  arg_params <- names(list(...))
  arg_values <- unlist(list(...), use.names = FALSE)
  paths <- arg_params |>
    sapply(build_path, USE.NAMES = FALSE) |>
    sprintf(arg_values) |>
    paste(collapse = "/")
  query <- request(base_uri()) |>
    req_url_path_append(service) |>
    req_url_path_append(paths) |>
    req_url_path_append(token_uri(token))
  return(query)
}

#' Path builder
#'
#' Construct path for query
#'
build_path <- function(arg) {
  paste(arg, "%s", sep = "/")
}

#' Dataframe builder
#'
#' @description Construct tibble dataframe from list of JSON
#' @param listdata List data from JSON
#'
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
