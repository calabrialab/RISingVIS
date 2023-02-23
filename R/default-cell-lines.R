
default_cell_lines <- function(name=NULL) {
  defaults <- list()

  # CEM37 -----------------------------------------------------
  known_cem_is <- tibble::tribble(
    ~chr,	~integration_locus,	~strand,
    "8",	8866486,	"+",
    "11",	64537168,	"-",
    "17",	47732339,	"-",
    "2",	73762398,	"-",
    "2",	24546571,	"-",
    "17",	2032352,	"-",
    "16",	28497498,	"-",
  )
  cem37 <- ControlLine$new(
    name = "CEM37",
    known_iss = known_cem_is
  )
  defaults[[cem37$name]] <- cem37

  # Returns ---------------------------------------------------
  if (is.null(name)) {
    return(defaults)
  }
  found <- defaults[names(defaults) %in% name]
  return(found)
}
