
WorkFlow <- R6::R6Class(
  classname = "WorkFlow",
  private = list(
    .name = NA,
    .date_created = NA,
    .isa_options = NA,
    .metadata = NA,
    .data = NA,
    .recalibrated = NA,
    .filter_threshold = NA,
    .seq_ratio = NA,
    .repl_ratio = NA,
    .plots = NA
  ),
  public = list(
    initialize = function(name = "Workflow-1") {
      private$.name <- name
      private$.date_created <- Sys.time()
    },
    set_ISA_options = function(opt) {
      private$.isa_options <- opt
    },
    get_ISA_options = function() {
      return(private$.isa_options)
    },
    set_metadata = function(df, checks) {
      private$.metadata <- list(df = df, checks = checks)
    },
    get_metadata = function(checks = FALSE) {
      if (!checks) {
        return(private$.metadata$df)
      } else {
        return(private$.metadata)
      }
    },
    set_data = function(df, checks) {
      private$.data <- list(df = df, checks = checks)
    },
    get_data = function(checks = FALSE) {
      if (!checks) {
        return(private$.data$df)
      } else {
        return(private$.data)
      }
    },
    set_recalibr = function(rec_data) {
      private$.recalibrated <- rec_data
    },
    get_recalibr = function() {
      return(private$.recalibrated)
    }
  )
)
