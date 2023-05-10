.onAttach <- function(libname, pkgname) {
  options(
    RISingVIS.parallel_processing = TRUE,
    RISingVIS.cell_line_db = ControlLinesDb$new()
  )
}
