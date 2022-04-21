#' Create linked scatter and geo visualization
#' @param rates TODO
#' @param title TODO
#' @param geo_dir TODO
#' @param bins TODO
#' @importFrom htmltools browsable
#' @export
geo_scatter_vis <- function(rates, title, geo_dir, bins = NULL) {
  p1 <- scatter_vis_all(rates)

  p2 <- geo_vis_all(
    rates = rates,
    title = title,
    geo_dir = geo_dir,
    bins = bins,
    width = "40vw"
  )

  tags <- htmltools::tags
  res <- htmltools::tagList(
    tags$head(tags$style("
      .window-container {
        display: flex;
        flex-direction: row;
      }
    ")),
    tags$div(class = "window-container",
      tags$div(style = "width: 60vw; height: 100vh;", p1),
      p2
    )
  )

  htmltools::browsable(res)
}
