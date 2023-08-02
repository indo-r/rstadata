#' Base URI
#'
#' RestAPI service base URI
#'
base_uri <- function() {
  return("https://webapi.bps.go.id/v1/api/")
}

#' Message for asking token
#'
#' Asking to BPS WebAPI token and registration message
#'
message_token <- function() {
  url_regist = "https://webapi.bps.go.id/developer/register"
  return(sprintf("Register to %s and use `bps_set_token()`", url_regist))
}

#' Message to refuse domain pattern
#'
#' Asking to input acceptable value for domain
#'
message_domain <- function() {
  return("Not acceptable `domain_id`")
}

#' Message to request failure
#'
#' Return information about request failure
#' @param status Response status
#'
message_resp_error <- function(status) {
  return(sprintf("Response status %s", status))
}

#' Snippet message for list result
#'
#' Return information about list search result
#' @param params string to key-value pair of arguments
#' @param meta metadata from response
#'
message_snippet_tbl <- function(params, meta) {
  message_tbl <- "Arguments: %s\nPage %i of %i. Use argument `page` to access."
  message(sprintf(message_tbl, params, meta$page, meta$pages))
}
