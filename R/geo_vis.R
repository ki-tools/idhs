
#' @importFrom dplyr bind_rows recode
#' @importFrom mapboxer as_mapbox_source mapboxer add_fill_layer
#' add_line_layer as_mapbox_source add_tooltips basemaps
geo_vis_all <- function(
  rates, geo_dir, bins = NULL, width = NULL, height = NULL
) {
  world <- ipumsr::read_ipums_sf(
    file.path(geo_dir, "IPUMSI_world_release2020.zip"),
    verbose = FALSE
  ) %>%
    dplyr::rename_all(tolower) %>%
    # DRC is the only one coded differently here vs. in IPUMS data
    # setdiff(sort(unique(x$country2)), world$cntry_name)
    dplyr::mutate(country = dplyr::recode(.data$cntry_name,
      "Congo, DRC" = "Congo Democratic Republic"
    ))

  world2 <- dplyr::filter(world, .data$country %in% names(rates))

  alldat <- dplyr::bind_rows(lapply(rates, function(x) x$yrregstats)) %>%
    dplyr::group_by(.data$country, .data$region, .data$dhscode) %>%
    dplyr::filter(year == max(.data$year)
  )

  allgeo <- lapply(rates, function(x) x$geodf) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(country = "cntry_name")

  if (is.null(bins)) {
    bins <- pretty(alldat$pct, n = 8)
    # classInt::classIntervals(alldat$pct, n = 10)
  }
  cols <- viridisLite::viridis(length(bins) - 1) %>%
    substr(1, 7)

  leglbl <- paste0(bins[-length(bins)], "-", bins[-1], "%")

  geodat <- ipumsr::ipums_shape_inner_join(alldat, allgeo,
    by = c("country", "dhscode")) %>%
    dplyr::mutate(
      fill_color = cols[cut(.data$pct, bins, labels = FALSE)],
      pct = round(.data$pct, 1)
    )

  # setdiff(names(sti_rates), geodat$country)

  # legend: https://docs.mapbox.com/help/tutorials/choropleth-studio-gl-pt-2/
  mapboxer::as_mapbox_source(geodat) %>%
    mapboxer::mapboxer(
      style = mapboxer::basemaps$Mapbox$light_v10,
      bounds = sf::st_bbox(geodat),
      fitBoundsOptions = list(padding = 20),
      width = width,
      height = height
    ) %>%
    mapboxer::add_fill_layer(
      id = "map",
      fill_color = c("get", "fill_color"),
      fill_opacity = 0.6
    ) %>%
    mapboxer::add_line_layer(
      source =  mapboxer::as_mapbox_source(world2),
      line_color = "black",
      line_width = 1
    ) %>%
    mapboxer::add_tooltips(
      "map", paste0(
        "Country: {{country}}<br>",
        "Region: {{region}}<br>",
        "Year: {{year}}<br>",
        "Pct STI: {{pct}}%"))
}