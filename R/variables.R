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
  return(sprintf("Register to get token %s", url_regist))
}

#' Message to refuse domain pattern
#'
#' Asking to input acceptable value for domain
#'
message_domain <- function() {
  return("Not acceptable `domain_id`")
}
