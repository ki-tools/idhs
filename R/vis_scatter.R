#' @importFrom DescTools BinomCI
get_ci <- function(x, n) {
  # default 0.95 Wilson
  dplyr::as_tibble(DescTools::BinomCI(x, n)) %>%
    dplyr::mutate_all(function(x) x * 100) %>%
    dplyr::rename(
      percent = .data$est, lower = .data$lwr.ci, upper = .data$upr.ci)
}

#' Create scatterplot visualization for all geographies
#' @param rates TODO
#' @importFrom plotly plot_ly add_trace layout subplot
#' @importFrom forcats fct_reorder
#' @importFrom tidyr nest
#' @export
scatter_vis_all <- function(rates) {
  alldat <- dplyr::bind_rows(
    lapply(rates, function(x) x$yrregstats)) %>%
    dplyr::mutate(
      country = forcats::fct_reorder(.data$country, .data$pct, mean)
    )
  alldat <- dplyr::bind_cols(alldat, get_ci(alldat$nsti, alldat$n))

  ctrydat <- dplyr::bind_rows(
    lapply(rates, function(x) x$yrstats)) %>%
    dplyr::mutate(
      country = factor(.data$country, levels = levels(alldat$country)),
      region = "")
  ctrydat <- dplyr::bind_cols(ctrydat, get_ci(ctrydat$nsti, ctrydat$n))

  pdat <- dplyr::left_join(
    tidyr::nest(alldat, rdat = -c("country")),
    tidyr::nest(ctrydat, cdat = -c("country")),
    by = "country"
  ) %>%
    dplyr::arrange(.data$country)

  maxrng <- max(alldat$pct)
  yrrng <- range(alldat$year)

  figs <- lapply(seq_len(nrow(pdat)), function(ii) {
    curr <- tidyr::nest(pdat$rdat[[ii]], data = -c("region"))
    curc <- pdat$cdat[[ii]]
    cur_cntry <- as.character(pdat$country[ii])

    fig <- plotly::plot_ly()
    for (jj in seq_len(nrow(curr))) {
      tmp <- curr$data[[jj]]
      cur_reg <- as.character(curr$region[jj])
      fig <- fig %>%
        # plotly::add_ribbons(
        #   x = tmp$year,
        #   ymin = tmp$lower,
        #   ymax = tmp$upper,
        #   color = I("darkgray"),
        #   line = list(color = "transparent"),
        #   alpha = 0.4,
        #   hoverinfo = "text",
        #   showlegend = FALSE,
        #   visible = "legendonly",
        #   name = paste(cur_cntry, cur_reg, "CI", sep = "___")
        # ) %>%
        plotly::add_trace(
          x = tmp$year,
          y = tmp$pct,
          color = I("darkgray"),
          line = list(width = 3),
          size = 4,
          alpha = 0.6,
          name = paste(cur_cntry, cur_reg, sep = "___"),
          type = "scatter",
          mode = "lines+markers",
          hoverinfo = "text",
          text = paste0(round(tmp$pct, 1), "%<br>Region: ", cur_reg),
          showlegend = FALSE,
          name = paste(cur_cntry, cur_reg, sep = "___")
        ) %>%
        plotly::layout(
          yaxis = list(
            range = list(0, maxrng),
            zeroline = FALSE
          ),
          # TODO: determine fixed tickvals dynamically
          xaxis = list(
            range = list(yrrng[1] - 1, yrrng[2] + 1),
            tickmode = "array",
            ticktext = c(
              "'00",
              "'05",
              "'10",
              "'15"
            ),
            tickvals = c(
              2000,
              2005,
              2010,
              2015
            ),
            categoryorder = "array",
            categoryarray = c(
              "'00",
              "'05",
              "'10",
              "'15"
            ),
            tickangle = 0,
            zeroline = FALSE
          )
        )
    }
    fig
  })

  res <- plotly::subplot(figs, nrows = 2, shareY = TRUE, shareX = TRUE,
    margin = 0.004)

  res$elementId <- "scatterplot"

  res$sizingPolicy$defaultHeight <- "100%"

  res
}
