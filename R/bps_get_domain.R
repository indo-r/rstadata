#' Get list of domain
#'
#' @description Retrieve list of national wide BPS domain.
#' @param token App ID. Please register to https://webapi.bps.go.id/developer/register
#'
#' @examples
#' \dontrun{
#' bps_get_domain(token = Sys.getenv("BPS_APP_ID"))
#' }
#'
#' @import httr2
#' @importFrom dplyr mutate case_when
#' @export
#'
bps_get_domain <- function(token, type = "all", ...) {
  if (missing(token)) {
    stop(message_token())
  }
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
