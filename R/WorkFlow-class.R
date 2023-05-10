WorkFlow <- R6::R6Class(
  classname = "WorkFlow",
  private = list(
    # A string with the name of the workflow
    .name = NULL,
    # The date the workflow was created (automatic)
    .date_created = NULL,
    # A named list with available control cell lines
    .available_cl = NULL,
    # A list with 2 components, `json_source` and `options`.
    # `json_source` is either NULL or a string with the path to the JSON file
    # `options` is a named list with the actual lookup tables
    .isa_options = NULL,
    # Path were ISAnalytics reports are optionally saved
    .isa_report_path = NULL,
    # A list with the following components:
    # - `root`: path for file system alignment used
    # - `file_path`: path to the tabular file used
    # - `project`: name of the project
    # - `separator`: character used to separate fields
    # - `date_format`: format of the date fields (general)
    # - `ind_sample_id`: character vector of fields that identify an independent
    # sample
    .metadata = NULL,
    .data = NULL,
    .recalibrated = NULL,
    .filter_threshold = NULL,
    .seq_ratio = NULL,
    .repl_ratio = NULL,
    .plots = NULL
  ),
  public = list(
    initialize = function(name = "Workflow-1") {
      private$.name <- name
      private$.date_created <- Sys.time()
      private$.isa_options <- list(
        json_source = NULL,
        options = .get_isa_is_vars()
      )
      private$.isa_report_path <- ISAnalytics::default_report_path()
      private$.available_cl <- default_cell_lines()
    },
    get_name = function() {
      return(private$.name)
    },
    change_name = function(new_name, flag = NULL) {
      old <- private$.name
      private$.name <- new_name
      if (not_null(flag)) {
        gargoyle::trigger(flag)
      }
      return(invisible(old))
    },
    get_date = function() {
      return(private$.date_created)
    },
    get_available_cl = function(name = NULL) {
      if (not_null(name)) {
        return(private$.available_cl[[name]])
      }
      return(private$.available_cl)
    },
    add_control_cell_line = function(cl, flag = NULL) {
      private$.available_cl[[cl$name]] <- cl
      if (not_null(flag)) {
        gargoyle::trigger(flag)
      }
    },
    set_ISA_options = function(opt, flag = NULL) {
      private$.isa_options <- opt
      if (not_null(flag)) {
        gargoyle::trigger(flag)
      }
    },
    get_ISA_options = function() {
      return(private$.isa_options)
    },
    set_ISA_report_path = function(path, flag = NULL) {
      private$.isa_report_path <- path
      if (not_null(flag)) {
        gargoyle::trigger(flag)
      }
    },
    get_ISA_report_path = function() {
      return(private$.isa_report_path)
    },
    # x: new object value
    # element: name of the element to be changed in the list.
    # If set to `metadata` x must be the complete list (for debugging)
    set_metadata = function(x, element, flag = NULL) {
      if (element == "metadata") {
        private$.metadata <- x
      } else {
        private$.metadata[[element]] <- x
      }
      if (not_null(flag)) {
        gargoyle::trigger(flag)
      }
    },
    get_metadata = function(element = "metadata") {
      if (element == "metadata") {
        return(private$.metadata)
      } else {
        return(private$.metadata[[element]])
      }
    },
    set_data = function(df, checks) {
      private$.data <- list(df = df, checks = checks)
    },
    get_data = function(checks = FALSE) {
      if (!checks) {
        if (!all(is.na(private$.data)) & !is.null(private$.data)) {
          return(private$.data$df)
        } else {
          return(NULL)
        }
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
