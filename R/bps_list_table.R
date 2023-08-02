#' Get list of table
#'
#' @description Retrieve list of table based on model
#' @param keyword Search parameter
#' @param model Table model can be static or dynamic
#' @param var Variable ID selected to display dynamic table data
#' @param domain_id Domain id for single BPS website
#' @param ... Any acceptable WebAPI for table: page, year, month, lang
#' @param token App ID
#'
#' @examples
#' \dontrun{
#' bps_list_table(keyword = "penduduk")
#' bps_list_table("penduduk", domain_id = "1100")
#' }
#'
#' @import httr2
#' @export
#'
bps_list_table <- function(
    keyword,
    model = "static",
    var,
    domain_id = "0000",
    ...,
    token
  ) {
  if (missing(token)) token <- get_token()
  allowed_model <- c("static", "dynamic")
  match.arg(model, allowed_model)
  if (identical(model, "dynamic") & missing(var)) {
    stop("Missing `var` argument for model dynamic")
  }
  if (identical(model, "static") & missing(keyword)) {
    stop("Missing `keyword` argument for model static")
  }
  if (identical(model, "dynamic")) {
    params <- list(model = model, var = var, domain = domain_id, ...)
  }
  if (identical(model, "static")) {
    params <- list(model = model, keyword = keyword, domain = domain_id, ...)
  }
  arguments <- paste(
    paste(
      names(params),
      sapply(params, c, USE.NAMES = FALSE),
      sep = " = "
    ),
    collapse = ", "
  )
  params$model <- paste0(model, "table")
  params$service <- "list"
  query <- do.call(build_query, params)
  query_ready <- add_token(query, token)
  resp <- req_perform(query_ready)
  if (resp_status(resp) != 200) {
    stop(message_resp_error(resp_status(resp)))
  } else {
    body <- resp_body_json(resp)
    if (body$status != "OK") {
      stop(body$message)
    } else {
      if (body$`data-availability` != "available") {
        stop(
          sprintf("Not available data for: %s", arguments)
        )
      } else {
        tryCatch(
          {
            data <- build_dataframe(body$data[[2]])
            meta <- body$data[[1]]
            message_snippet_tbl(arguments, meta)
            return(data)
          }, error = function(e) {
            stop(e$message)
          }
        )
      }
    }
  }
}
