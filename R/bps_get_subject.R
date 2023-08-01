#' Get list of subject data
#'
#' @description Retrieve list of available subject data
#' @param domain_id Domain id for single BPS website
#' @param ... Any acceptable WebAPI parameters for subject data
#' @param token App ID
#'
#' @examples
#' \dontrun{
#' bps_get_subject(token = "your_secret_token")
#' }
#'
#' @import httr2
#' @export
#'
bps_get_subject <- function(domain_id = "0000", ..., token) {
  # lang, subcat
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
    stop(sprintf("Response status %s", resp_status(resp)))
  } else {
    body <- resp_body_json(resp)
    if (body$status != "OK") {
      stop(body$message)
    } else {
      data <- build_dataframe(body$data[[2]])
      meta <- body$data[[1]]
      if (as.integer(meta$pages) == 1) {
        return(data)
      } else {
        data_more <- list()
        for (i in 2:as.integer(meta$pages)) {
          path <- build_path("page", as.character(i), replace = FALSE)
          next_query <- query |>
            req_url_path_append(path)
          next_body <- next_query |>
            add_token(token) |>
            req_perform() |>
            resp_body_json()
          data_more[[i]] <- build_dataframe(next_body$data[[2]])
        }
        data_more <- do.call(rbind, data_more)
        return(rbind(data, data_more))
      }
    }
  }
}
