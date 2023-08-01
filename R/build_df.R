#' Dataframe builder
#'
#' @description Construct tibble dataframe from list of JSON
#' @param listdata List data from JSON
#'
#' @importFrom dplyr as_tibble
#'

build_df <- function(listdata) {
  rows <- lapply(listdata, function(row) {
    row |>
      unlist() |>
      t() |> 
      dplyr::as_tibble()
  })
  return(do.call(rbind, rows))
}