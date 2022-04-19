
check_ipums_data <- function(x, nm = "x") {
  if (!"ipums_ddi" %in% names(attributes(x))) {
    stop("Input '", nm, "' must come from preprocess_ipums()")
  }
}
