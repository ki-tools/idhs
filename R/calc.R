#' Calculate rates from ipums data and associated geography
#' @examples
#' \dontrun{
#' calc_rates(dd, geo_dir = "ext-data/idhs/geo")
#' }
#' @importFrom %>% dplyr group_by ungroup rename_all filter summarise n
#' select all_of mutate
#' @importFrom ipumsr read_ipums_sf as_factor lbl_clean zap_labels
#' @export
calc_rates <- function(x, geo_dir, num_var, denom_var, num_cond, denom_cond) {
  check_ipums_data(x, "x")
  x$country2 <- ipumsr::as_factor(ipumsr::lbl_clean(x$country))

  if (!dir.exists(geo_dir))
    stop("directory '", geo_dir, "' doesn't exist")

  ff <- list.files(geo_dir, pattern = "geo_", full.names = TRUE)
  if (length(ff) == 0) {
    stop("no files starting with 'geo_' were found in the directory '",
      geo_dir, "'")
  }

  ftbl <- dplyr::tibble(
    f = ff,
    country = substr(basename(ff), 5, 6),
    start_yr = as.integer(substr(basename(ff), 7, 10)),
    end_yr = as.integer(substr(basename(ff), 12, 15))
  )

  # there are duplicate shapefiles for many countries
  # for now, choose those that have the latest data
  # and also go back the farthest
  # (could add an option later to do latest start and end date)
  ftbl <- ftbl %>%
    dplyr::group_by(.data$country) %>%
    dplyr::filter(end_yr == max(end_yr)) %>%
    dplyr::filter(start_yr == max(start_yr)) %>%
    dplyr::ungroup()
  ff <- ftbl$f

  xnms <- names(x)

  res <- list()
  for (f in ff) {
    geodf <- ipumsr::read_ipums_sf(f, verbose = FALSE) %>%
      dplyr::rename_all(tolower)
    names(geodf)[3] <- "dhscode"
    geodf$dhscode <- as.integer(geodf$dhscode)
    cur_var <- gsub(".*(geo_.*).zip", "\\1", f)
    cur_cntry <- geodf$cntry_name[1]
    message(cur_cntry, ": ", cur_var)

    if (!cur_var %in% xnms) {
      message("  not found...")
      next
    }

    vars <- c("year", "perweight", num_var, denom_var)
    # first get just the country
    tmp <- x %>%
      dplyr::filter(.data$country2 == cur_cntry)
    # then get rid of years where n == nna for numerator variable
    tmp2 <- tmp %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(
        n = dplyr::n(),
        nna = length(which(is.na(.data[[num_var]])))
      ) %>%
      dplyr::filter(.data$n == nna)
    tmp <- tmp %>%
      dplyr::filter(!.data$year %in% tmp2$year, ) %>%
      dplyr::select(all_of(c(cur_var, vars))) %>%
      dplyr::rename_all(function(x)
        ifelse(x %in% vars, x, "dhscode")) %>%
      dplyr::mutate(
        region = ipumsr::as_factor(ipumsr::lbl_clean(.data$dhscode)),
        dhscode = ipumsr::zap_labels(.data$dhscode)
      )

    if (nrow(tmp) == 0) {
      message("  no data for '", num_var, "'...")
      next
    }
    if (all(is.na(tmp[[denom_var]]))) {
      message("  denominator variable '", denom_var, "' is all NA...")
      next
    }

    if (
      length(which(is.na(tmp$year))) != 0 ||
      # length(which(is.na(tmp$sexactiv4wk))) != 0 ||
      length(which(is.na(tmp[[num_var]]))) != 0
    ) {
      # browser()
      stop("something isn't right...")
    }

    yrregstats <- tmp %>%
      dplyr::filter(.data[[denom_var]] %in% denom_cond) %>%
      dplyr::group_by(.data$year, .data$dhscode, .data$region) %>%
      dplyr::summarise(
        n = sum(.data$perweight),
        n2 = dplyr::n(), # length(which(.data[[num_var]] %in% num_cond)),
        nsti = sum((.data[[num_var]] == 1) * .data$perweight),
        nsti2 = length(which(.data[[num_var]] == 1)),
        pct = 100 * .data$nsti / .data$n,
        .groups = "drop") %>%
      dplyr::mutate(country = cur_cntry, geo_var = cur_var)

    regstats <- tmp %>%
      dplyr::filter(.data[[denom_var]] %in% denom_cond) %>%
      dplyr::group_by(.data$dhscode, .data$region) %>%
      dplyr::summarise(
        n = sum(.data$perweight),
        n2 = dplyr::n(), # length(which(.data[[num_var]] %in% num_cond)),
        nsti = sum((.data[[num_var]] == 1) * .data$perweight),
        nsti2 = length(which(.data[[num_var]] == 1)),
        pct = 100 * .data$nsti / .data$n,
        .groups = "drop") %>%
      dplyr::mutate(country = cur_cntry, geo_var = cur_var)

    yrstats <- tmp %>%
      dplyr::filter(.data[[denom_var]] %in% denom_cond) %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(
        n = sum(.data$perweight),
        n2 = dplyr::n(), # length(which(.data[[num_var]] %in% num_cond)),
        nsti = sum((.data[[num_var]] == 1) * .data$perweight),
        nsti2 = length(which(.data[[num_var]] == 1)),
        pct = 100 * .data$nsti / .data$n) %>%
      dplyr::mutate(country = cur_cntry, geo_var = cur_var)

    allstats <- tmp %>%
      dplyr::filter(.data[[denom_var]] %in% denom_cond) %>%
      dplyr::summarise(
        n = sum(.data$perweight),
        n2 = length(which(.data[[num_var]] %in% num_cond)),
        nsti = sum((.data[[num_var]] == 1) * .data$perweight),
        nsti2 = length(which(.data[[num_var]] == 1)),
        pct = 100 * .data$nsti / .data$n) %>%
      dplyr::mutate(country = cur_cntry, geo_var = cur_var)

    res[[cur_cntry]] <- list(
      country = cur_cntry,
      geo_var = cur_var,
      allstats = allstats,
      yrstats = yrstats,
      regstats = regstats,
      yrregstats = yrregstats,
      geodf = geodf
    )
  }
  class(res) <- c("list", "ipums-rates")
  res
}
