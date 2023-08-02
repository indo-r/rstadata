#' Get list of domain
#'
#' @description Retrieve list of nation wide BPS domain.
#' @param type Type to show domain
#' @param ... Any acceptable WebAPI parameters: prov
#' @param token App ID
#'
#' @examples
#' \dontrun{
#' bps_list_domain() # return all domains
#' bps_list_domain(type = "prov")
#' bps_list_domain(type = "kabbyprov", prov = 3200)
#' }
#'
#' @import httr2
#' @importFrom dplyr mutate case_when
#' @export
#'
bps_list_domain <- function(type = "all", ..., token) {
  if (missing(token)) token <- get_token()
  allowed_type <- c("all", "prov", "kab", "kabbyprov")
  match.arg(type, allowed_type)
  resp <- build_query(service = "domain", token = token, type = type, ...) |>
    req_perform()
  if (resp_status(resp) != 200) {
    stop(sprintf("Response status %s", resp_status(resp)))
  } else {
    body <- resp_body_json(resp)
    if (body$status != "OK") {
      stop(body$message)
    } else {
      data <- body$data[[2]] |>
        build_dataframe() |>
        mutate(level = "kota/kabupaten") |>
        mutate(
          level = case_when(
            domain_id == "0000" ~ "nasional",
            grepl("^.*00$", domain_id) ~ "provinsi",
            TRUE ~ "kota/kabupaten"
          )
        )
      return(data)
    }
  }
}
