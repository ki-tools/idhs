#' Create Mapbox visualization of rate data
#' @param rates TODO
#' @param geo_dir TODO
#' @param title TODO
#' @param bins TODO
#' @param width TODO
#' @param height TODO
#' @importFrom dplyr bind_rows recode
#' @importFrom mapboxer as_mapbox_source mapboxer add_fill_layer
#' add_line_layer add_tooltips
#' @importFrom jsonlite toJSON
#' @importFrom rmapshaper ms_simplify
#' @importFrom sf st_bbox
#' @importFrom viridisLite viridis
geo_vis_all <- function(
  rates, geo_dir, title = "", bins = NULL,
  width = "100vw", height = "100vh"
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
    dplyr::filter(.data$year == max(.data$year)
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
      pct = round(.data$pct, 1),
      country_region = paste(.data$country, .data$region, sep = "___")
    )

  # setdiff(names(sti_rates), geodat$country)
  geodat2 <- rmapshaper::ms_simplify(geodat, keep = 0.01, keep_shapes = FALSE)
  world3 <- rmapshaper::ms_simplify(world2, keep = 0.01, keep_shapes = FALSE)

  bboxes <- lapply(
      c(lapply(world3$geometry, identity), list(world3$geometry)
    ), function(x) {
    tmp <- as.list(sf::st_bbox(x))
    list(
      bottomleft = c(tmp$xmin, tmp$ymin),
      topright = c(tmp$xmax, tmp$ymax)
    )
  })
  names(bboxes) <- c(world3$country, "all")
  bboxstr <- paste0("var bboxes = ",
    jsonlite::toJSON(bboxes, auto_unbox = TRUE, pretty = TRUE))

  # legend: https://docs.mapbox.com/help/tutorials/choropleth-studio-gl-pt-2/
  p <- mapboxer::as_mapbox_source(geodat2) %>%
    mapboxer::mapboxer(
      style = mapboxer::basemaps$Mapbox$light_v10,
      bounds = sf::st_bbox(geodat),
      fitBoundsOptions = list(padding = 20),
      width = "100%",
      height = "100%",
      token = get_tkn()
    ) %>%
    mapboxer::add_fill_layer(
      id = "map",
      fill_color = c("get", "fill_color"),
      fill_opacity = 0.6
    ) %>%
    mapboxer::add_line_layer(
      id = "map-lines",
      line_color = "black",
      line_width = 1,
      line_opacity = 0.1
    ) %>%
    mapboxer::add_line_layer(
      id = "countries",
      source =  mapboxer::as_mapbox_source(world3),
      line_color = "black",
      line_width = 1
    ) %>%
    mapboxer::add_tooltips(
      "map", paste0(
        "Country: {{country}}<br>",
        "Region: {{region}}<br>",
        "Year: {{year}}<br>",
        "Pct STI: {{pct}}%"))

  build_map(p, bins, leglbl, cols, title, bboxstr, height, width)
}

#' @importFrom htmltools tags tagList HTML browsable
build_map <- function(p, bins, leglbl, cols, title, bboxstr, height, width) {
  tags <- htmltools::tags

  res <- htmltools::tagList(
    tags$script(htmltools::HTML(bboxstr)),
    tags$script(htmltools::HTML("
      function throttle(callback, interval) {
        let enableCall = true;

        return function(...args) {
          if (!enableCall) return;

          enableCall = false;
          callback.apply(this, args);
          setTimeout(() => enableCall = true, interval);
        }
      }

      var hoveredId = null;

      function resize() {
        console.log('resize...')
        var keys = Object.keys(window.mapboxer._widget);
        keys.forEach(function(key) {
          var curmap = window.mapboxer._widget[key].map;
          curmap.resize();
          curmap.fitBounds(window.mapboxer_bounds[key]);
        });
      };

      var doresize;
      window.onresize = function() {
        clearTimeout(doresize);
        doresize = setTimeout(resize, 100);
      };

      window.onload = function() {
        var scatter = document.getElementById('scatterplot');
        var bnds = {};
        var keys = Object.keys(window.mapboxer._widget);
        keys.forEach(function(key) {
          var curmap = window.mapboxer._widget[key].map;
          bnds[key] = curmap.getBounds();
        });
        window.mapboxer_bounds = bnds;

        var curmap = window.mapboxer._widget[keys[0]].map

        let onHover = (e) => {
          if (
            e.features.length > 0 &&
            e.features[0].properties.country_region !== hoveredId
          ) {
            // Plotly.restyle(scatter, {'line': {'color': 'darkgray'}}, [])
            hoveredId = e.features[0].properties.country_region;
            curmap.setPaintProperty(
              'map-lines',
              'line-opacity',
              ['match', ['get', 'country_region'], hoveredId, 1, 0.1]
            );

            curmap.setPaintProperty(
              'map-lines',
              'line-width',
              ['match', ['get', 'country_region'], hoveredId, 3, 1]
            );
            // var traceidx = scatter.data.findIndex(obj => {
            //   return obj.name === hoveredId;
            // });
            // if (traceidx > -1) {
            //   console.log('restyling hover...');
            //   Plotly.addTraces(scatterplot, [{x: [2010], y: [20], mode: 'markers', marker: {symbol: 6, size: 12}, xaxis: 'x', yaxis: 'y'}])
            //   Plotly.restyle(scatter, {'line': {'color': 'blue'}}, [traceidx])
            // }
          }
        };

        let onUnhover = (e) => {
          curmap.setPaintProperty(
            'map-lines',
            'line-opacity',
            0.1
          );
          curmap.setPaintProperty(
            'map-lines',
            'line-width',
            1
          );

          // console.log('restyling unhover...');
          // Plotly.restyle(scatter, {'line': {'color': 'darkgray'}}, [])
          hoveredId = null;
        };

        // curmap.on('mousemove', 'map', throttle(onHover, 50));
        // curmap.on('mouseleave', 'map', throttle(onUnhover, 50));
        curmap.on('mousemove', 'map', onHover);
        curmap.on('mouseleave', 'map', onUnhover);

        scatterplot.on('plotly_hover', function(d) {
          // console.log('restyling plotly hover...')
          // Plotly.restyle(scatterplot, {'line': {'color': 'blue'}}, [d.points[0].curveNumber])
          var hoveredId = d.points[0].data.name;
          var country = hoveredId.split('___')[0];
          if (bboxes && bboxes[country]) {
            curmap.fitBounds([bboxes[country].bottomleft, bboxes[country].topright]);
          }
          curmap.setPaintProperty(
            'map-lines',
            'line-opacity',
            ['match', ['get', 'country_region'], hoveredId, 1, 0.1]
          );
          curmap.setPaintProperty(
            'map-lines',
            'line-width',
            ['match', ['get', 'country_region'], hoveredId, 4, 1]
          );
        });

        scatterplot.on('plotly_unhover', function(d) {
          if (bboxes && bboxes.all) {
            curmap.fitBounds([bboxes.all.bottomleft, bboxes.all.topright]);
          }
          curmap.setPaintProperty(
            'map-lines',
            'line-opacity',
            0.1
          );
          curmap.setPaintProperty(
            'map-lines',
            'line-width',
            1
          );
          // console.log('restyling plotly unhover...')
          // Plotly.restyle(scatterplot, {'line': {'color': 'darkgray'}}, [d.points[0].curveNumber]);
        });
      }
    ")),
    tags$head(tags$style("
      body {
        margin: 0px;
      }
      .geo-container {
        position: relative;
        width: fit-content;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif
      }
      .geo-title {
        position: absolute;
        font-size: 16px;
        top: 0;
        right: 10px;
        font-style: italic;
        padding: 4px;
        color: #565656;
        font-weight: 300;
        background: rgba(255, 255, 255, 0.7);
      }
      .legend-container {
        position: absolute;
        display: flex;
        flex-direction: column;
        top: 26px;
        right: 10px;
        background: rgba(255, 255, 255, 0.7);
      }
      .legend-entry {
        display: flex;
        flex-direction: row;
        margin-bottom: 2px;
      }
      .legend-color-box {
        width: 17px;
        height: 17px;
        margin-right: 10px;
      }
      .legend-item-text {
        font-size: 14px;
      }
    ")),
    tags$div(
      class = "geo-container",
      style = paste0("width: ", width, "; height: ", height, ";"),
      p,
      tags$div(class = "geo-title", title),
      tags$div(class = "legend-container",
        lapply(seq_along(leglbl), function(ii) {
          tags$div(class = "legend-entry",
            tags$div(
              class = "legend-color-box",
              style = paste0("background: ", cols[ii], ";")
            ),
            tags$div(
              class = "legend-item-text",
              leglbl[ii]
            )
          )
        })
      )
    )
  )

  class(res) <- c("idhs_browsable", class(res))

  res
}

#' @export
print.idhs_browsable <- function(x, ...) {
  class(x) <- setdiff(class(x), "idhs_browsable")
  print(htmltools::browsable(x))
}
