#' Base URI
#'
#' RestAPI service base URI
#'
base_uri <- function() {
  return("https://webapi.bps.go.id/v1/api/")
}

#' Token parameter
#'
#' RestAPI service token endpoint
#' @param token App ID
#'
token_uri <- function(token) {
  return(paste("key", token, sep = "/"))
}

#' Message for asking token
#'
#' Asking to BPS WebAPI token and registration message
#'
message_token <- function() {
  url_regist = "https://webapi.bps.go.id/developer/register"
  return(sprintf("Register to get token %s", url_regist))
}
