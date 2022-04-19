#' View html table of IPUMS extract variable descriptions
#' @importFrom DT datatable
#' @importFrom dplyr filter tibble .data
#' @export
view_var_descs <- function(dd, include_geo = FALSE) {
  check_ipums_data(x, "x")

  descs <- sapply(dd, function(x) attr(x, "var_desc"))
  descs <- dplyr::tibble(
    name = names(descs),
    desc = unname(descs)
  )
  if (!include_geo) {
    descs <- dplyr::filter(descs, !grepl("^geo_", .data$name))
  }

  DT::datatable(descs, options = list(paging = FALSE))
}
