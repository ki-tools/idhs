#' Preprocess IPUMS extract
#' @importFrom ipumsr read_ipums_ddi read_ipums_micro as_factor lbl_clean
#' @importFrom dplyr rename_all
#' @examples
#' \dontrun{
#' preprocess_ipums(
#'   input_xml = "ext-data/idhs/extract2/idhs_00002.xml",
#'   output_file = "ext-data/idhs/extract2/dd.rds"
#' )
#' }
#' @export
preprocess_ipums <- function(input_xml, output_file) {
  message("reading xml")
  ddi <- ipumsr::read_ipums_ddi(input_xml)
  message("reading microdata")
  dd <- ipumsr::read_ipums_micro(ddi)
  dd <- dplyr::rename_all(dd, tolower)
  attr(dd, "ipums_ddi") <- ddi

  message("saving output")
  saveRDS(dd, file = output_file)

  invisible(dd)
}

# https://www.idhsdata.org/idhs/gis.shtml

#' Preprocess (download) shapfiles associated with an IPUMS extract
#' @examples
#' \dontrun{
#' dd <- preprocess_ipums(
#'   input_xml = "ext-data/idhs/extract2/idhs_00002.xml",
#'   output_file = "ext-data/idhs/extract2/dd.rds"
#' )
#' preprocess_geo(dd, output_dir = "ext-data/idhs/geo")
#' }
#' @export
#' @importFrom utils download.file
preprocess_geo <- function(x, output_dir) {
  check_ipums_data(x, "x")

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  prefix <- "https://www.idhsdata.org/idhs/resources/gis/"

  nms <- names(x)
  geonms <- sort(nms[grepl("geo_.*_.*", nms)])

  if (length(geonms) == 0) {
    message("no shapefiles found associated with this data")
    return(invisible(FALSE))
  }

  for (nm in geonms) {
    message("downloading shapefiles for ", nm)
    tryres <- try(utils::download.file(
      paste0(prefix, nm, ".zip"),
      file.path(output_dir, paste0(nm, ".zip")),
      quiet = TRUE
    ), silent = TRUE)
    if (inherits(tryres, "try-error")) {
      message("  could not find shapefiles associated with this country")
    }
  }

  message("downloading shapefiles for world countries")
  tryres <- try(utils::download.file(
    "https://international.ipums.org/international/resources/gis/IPUMSI_world_release2020.zip",
    file.path(output_dir, "IPUMSI_world_release2020.zip"),
    quiet = TRUE
  ))
  if (inherits(tryres, "try-error")) {
    message("  could not find world shapefiles")
  }

  invisible(TRUE)
}

# https://spatialdata.dhsprogram.com/population-estimates/
# https://dhs-sites.s3.amazonaws.com/SDR/boundaries/production/data/Population_Estimates_ALL.zip
