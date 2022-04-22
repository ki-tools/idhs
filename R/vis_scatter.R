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

  nrows <- 3
  ncols <- ceiling(nrow(pdat) / nrows)
  xpand <- function(rng, mult = 0.07) {
    dff <- diff(rng)
    c(rng[1] - mult * dff, rng[2] + mult * dff)
  }

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
          size = 3,
          alpha = 0.6,
          name = paste(cur_cntry, cur_reg, sep = "___"),
          type = "scatter",
          mode = "lines+markers",
          hoverinfo = "text",
          text = paste0(round(tmp$pct, 1),
            "% (", round(tmp$lower, 1), " \u2013 ", round(tmp$upper, 1),
            ")<br>Country: ",cur_cntry, "<br>Region: ", cur_reg),
          showlegend = FALSE,
          name = paste(cur_cntry, cur_reg, sep = "___")
        ) %>%
        plotly::layout(
          yaxis = list(
            range = xpand(c(0, maxrng)),
            zeroline = FALSE
          ),
          # TODO: determine fixed tickvals dynamically
          xaxis = list(
            range = xpand(c(yrrng[1], yrrng[2])),
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

  annotations <- lapply(seq_len(nrow(pdat)), function(ii) {
    cur_col <- (ii - 1) %% ncols + 1
    cur_row <- ceiling(ii / ncols)
    list(
      x = (1 / ncols) / 2 + (cur_col - 1) * (1 / ncols),
      y = 1 - (cur_row - 1) * (1 / nrows),
      text = as.character(pdat$country[ii]),
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = ifelse(cur_row == 1, "bottom", "top"),
      showarrow = FALSE
    )
  })

  annotations <- c(annotations, list(
    list(
      text = "Survey Year",
      x = 0.5,
      y = 0,
      showarrow = FALSE,
      ax = 0,
      ay = 0,
      font = list(
        size = 14.6118721461187
      ),
      xref = "paper",
      yref = "paper",
      textangle = 0,
      xanchor = "center",
      yanchor = "top",
      annotationType = "axis",
      yshift = -19
    ),
    list(
      text = "STI Rate",
      x = 0,
      y = 0.5,
      showarrow = FALSE,
      ax = 0,
      ay = 0,
      font = list(
        size = 14.6118721461187
      ),
      xref = "paper",
      yref = "paper",
      textangle = -90,
      xanchor = "right",
      yanchor = "center",
      annotationType = "axis",
      xshift = -24
    )
  ))

  res <- plotly::subplot(figs, nrows = nrows, shareY = TRUE, shareX = TRUE,
    margin = c(0.004, 0.004, 0.02, 0.02)) %>%
    layout(annotations = annotations)

  res$elementId <- "scatterplot"

  res$sizingPolicy$defaultHeight <- "100%"

  res
}
