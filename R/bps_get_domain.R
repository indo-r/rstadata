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
bps_get_domain <- function(token) {
  base_url <- "https://webapi.bps.go.id/v1/"
  resp <- request(base_url) |>
    req_url_path_append("api/domain/type/all/key/") |>
    req_url_path_append(token) |>
    req_perform()
  if (resp_status(resp) != 200) {
    message(sprintf("Response status %s", resp_status(resp)))
    return(NULL)
  } else {
    body <- resp_body_json(resp)
    if (body$status != "OK") {
      message(sprintf("Error %s", body$message))
      return(NULL)
    } else {
      data <- body$data[[2]] |>
        build_df() |>
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