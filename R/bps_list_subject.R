#' Get list of subject data
#'
#' @description Retrieve list of available subject data
#' @param domain_id Domain id for single BPS website
#' @param ... Any acceptable WebAPI parameters for subject data: page, subcat, lang
#' @param token App ID
#'
#' @examples
#' \dontrun{
#' bps_list_subject(domain_id = 1100)
#' }
#'
#' @import httr2
#' @export
#'
bps_list_subject <- function(domain_id = "0000", ..., token) {
  if (missing(token)) token <- get_token()
  if (!grepl("^\\d{4}$", domain_id)) stop(message_domain())
  query <- build_query(
    service = "list",
    model = "subject",
    domain = domain_id,
    ...
  )
  query_ready <- add_token(query, token)
  resp <- req_perform(query_ready)
  if (resp_status(resp) != 200) {
    stop(message_resp_error(resp_status(resp)))
  } else {
    body <- resp_body_json(resp)
    if (body$status != "OK") {
      stop(body$message)
    } else {
      data <- build_dataframe(body$data[[2]])
      meta <- body$data[[1]]
      if (as.integer(meta$pages) == 1L) {
        return(data)
      } else {
        data_more <- get_more_pages(query, meta$pages, token)
        return(rbind(data, data_more))
      }
    }
  }
}
