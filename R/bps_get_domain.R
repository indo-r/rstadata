#' Get list of domain
#'
#' @description Retrieve list of national wide BPS domain.
#' @param token App ID.
#'
#' @examples
#' \dontrun{
#' bps_get_domain(token = "your_secret_token")
#' }
#'
#' @import httr2
#' @importFrom dplyr mutate case_when
#' @export
#'
bps_get_domain <- function(type = "all", ..., token) {
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
