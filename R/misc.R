#' View html table of IPUMS extract variable descriptions
#' @param x TODO
#' @param include_geo TODO
#' @importFrom DT datatable
#' @importFrom dplyr filter tibble .data
#' @export
view_var_descs <- function(x, include_geo = FALSE) {
  check_ipums_data(x, "x")

  descs <- sapply(x, function(x) attr(x, "var_desc"))
  lbls <- sapply(x, function(x) attr(x, "label"))
  descs <- dplyr::tibble(
    name = names(descs),
    label = unname(lbls),
    desc = unname(descs)
  )
  if (!include_geo) {
    descs <- dplyr::filter(descs, !grepl("^geo_", .data$name))
    descs <- dplyr::filter(descs, !grepl("^geoalt_", .data$name))
  }

  dt <- DT::datatable(descs, width = "100%", options = list(paging = FALSE))

  tags <- htmltools::tags

  res <- htmltools::tagList(
    tags$head(tags$style("
      body {
        margin: 0;
        padding: 30px;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif
      }
    ")),
    dt
  )

  class(res) <- c("idhs_browsable", class(res))

  res
}

get_tkn <- function() {
  if (Sys.getenv("IDHS_DEV_MODE") == "true") {
    Sys.getenv("MAPBOX_API_KEY")
  } else {
    safer::decrypt_string("XLNDMUY6HdUjQ34+47BLmKf42KyqrWRmvL0QwGolHdOUAXEQqtp+SGd3/z1/4GweGP5vk0P7TLkJdI/e3iqekM5cgDq2riRySW1Vo1Tca3AtngJ0rVildwtkjwi9GMl75iOmrLDkGSI=", "idhs")
  }
}

dev_mode <- function(bool = FALSE) {
  if (bool) {
    Sys.setenv(IDHS_DEV_MODE = "true")
  } else {
    Sys.setenv(IDHS_DEV_MODE = "")
  }
}


# ‘DescTools’ ‘forcats’ ‘htmltools’ ‘jsonlite’ ‘plotly’ ‘rmapshaper’
#     ‘sf’ ‘tidyr’ ‘viridisLite’
