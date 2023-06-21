#' data_import_metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_metadata_ui <- function(id) {
  ns <- NS(id)
  tab_1_btn_id <- ns(ids()$data_import$metadata_section$inputs$tab_1_btn)
  tab_1_content_id <- ns(
    ids()$data_import$metadata_section$inputs$tab_1_content
  )
  tab_2_btn_id <- ns(ids()$data_import$metadata_section$inputs$tab_2_btn)
  tab_2_content_id <- ns(
    ids()$data_import$metadata_section$inputs$tab_2_content
  )
  div(
    id = id,
    style = "width: 80%;",
    class = "card mb-2",
    div(
      class = "card-header",
      tags$ul(
        class = "nav nav-pills card-header-pills",
        role = "tablist",
        tags$li(
          class = "nav-item",
          role = "presentation",
          tags$button(
            id = tab_1_btn_id,
            class = "nav-link active",
            `data-bs-toggle` = "pill",
            `data-bs-target` = paste0("#", tab_1_content_id),
            type = "button",
            role = "tab",
            `aria-controls` = tab_1_content_id,
            `aria-selected` = "true",
            "Import params"
          )
        ),
        tags$li(
          class = "nav-item",
          role = "presentation",
          shinyjs::disabled(
            tags$button(
              id = tab_2_btn_id,
              class = "nav-link",
              `data-bs-toggle` = "pill",
              `data-bs-target` = paste0("#", tab_2_content_id),
              type = "button",
              role = "tab",
              `aria-controls` = tab_2_content_id,
              `aria-selected` = "false",
              "Import summary"
            )
          )
        )
      )
    ),
    div(
      class = "card-body tab-content",
      div(
        id = tab_1_content_id,
        class = "tab-pane fade show active",
        role = "tabpanel",
        `aria-labelledby` = tab_1_btn_id,
        .section_name_with_tooltip(
          section_name = "Metadata",
          type = h2,
          tooltip_content_builder = .metadata_tooltip_builder
        ),
        .file_input(
          button_id = ns(ids()$data_import$metadata_section$inputs$file_input),
          display_id = ns(ids()$data_import$metadata_section$outputs[[
            "file_input_display"
          ]]),
          label = "Choose metadata file (*.tsv, *.csv or *.xlsx):",
          multiple = FALSE
        ),
        .folder_input(
          button_id = ns(ids()$data_import$metadata_section$inputs$root_dir),
          display_id = ns(ids()$data_import$metadata_section$outputs[[
            "root_display"
          ]]),
          "Root folder for file system alignment:"
        ),
        h5("Project info"),
        div(
          class = "container text-start",
          div(
            class = "row",
            div(
              class = "col-6",
              textInput(
                inputId = ns(
                  ids()$data_import$metadata_section$inputs$project
                ),
                label = .section_name_with_tooltip(
                  section_name = "Project name",
                  type = div,
                  tooltip_content_builder = .project_name_tooltip_builder
                ),
                width = "100%"
              )
            ),
            div(
              class = "col-6",
              selectizeInput(
                inputId = ns(
                  ids()$data_import$metadata_section$inputs$separator
                ),
                label = "File separator",
                choices = c(
                  "tab" = "\t", "comma" = ",",
                  "semilcolon" = ";", "space" = " "
                ),
                width = "100%"
              )
            )
          ),
          div(
            class = "row",
            div(
              class = "col-6",
              selectizeInput(
                inputId = ns(
                  ids()$data_import$metadata_section$inputs$dates_format
                ),
                label = .section_name_with_tooltip(
                  section_name = "General dates format",
                  type = div,
                  tooltip_content_builder = .dates_format_tooltip_builder
                ),
                choices = ISAnalytics::date_formats(),
                width = "100%"
              )
            ),
            div(
              class = "col-6",
              div(
                selectizeInput(
                  inputId = ns(
                    ids()$data_import$metadata_section$inputs$sample_id
                  ),
                  label = .section_name_with_tooltip(
                    section_name = "Independent sample identifier (ISid)",
                    type = div,
                    tooltip_content_builder = .sample_id_tooltip_builder
                  ),
                  choices = NULL,
                  multiple = TRUE,
                  width = "100%"
                )
              )
            )
          ),
          div(
            class = "row",
            div(
              class = "col-6",
              shinyWidgets::pickerInput(
                inputId = ns(
                  ids()$data_import$metadata_section$inputs$control_line
                ),
                label = .section_name_with_tooltip(
                  section_name = "Control cell line",
                  type = div,
                  tooltip_content_builder = .control_line_tooltip_builder
                ),
                choices = list(),
                multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE
                ),
                width = "100%"
              )
            ),
            div(
              class = "col-6",
              style = "align-self: center; padding-top: 1.5rem;",
              checkboxInput(
                inputId = ns(
                  ids()$data_import$metadata_section$inputs$save_report
                ),
                label = "Save ISAnalytics report",
                value = TRUE
              ),
            )
          )
        ),
        div(
          class = "w-100 d-flex justify-content-center",
          shinyjs::disabled(
            .with_spinner_btn_ui(
              ns = ns,
              btn_id = ids()$data_import$metadata_section$inputs$import_btn,
              position = "before",
              icon = icon("upload"),
              label = "Load",
              btn_class = "btn-secondary btn-lg"
            )
          )
        )
      ),
      div(
        id = tab_2_content_id,
        class = "tab-pane fade",
        role = "tabpanel",
        `aria-labelledby` = tab_2_btn_id,
        h2("Import summary report"),
        uiOutput(
          outputId = ns(
            ids()$data_import$metadata_section$outputs[["meta_report"]]
          )
        )
      )
    )
  )
}

#' data_import_metadata Server Functions
#'
#' @noRd
mod_data_import_metadata_server <- function(id, workflow, cldb) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    proj_name_id <- ids()$data_import$metadata_section$inputs$project
    sep_id <- ids()$data_import$metadata_section$inputs$separator
    dates_format_id <- ids()$data_import$metadata_section$inputs$dates_format
    sample_id_id <- ids()$data_import$metadata_section$inputs$sample_id
    cell_lines_id <- ids()$data_import$metadata_section$inputs$control_line
    load_btn_id <- ids()$data_import$metadata_section$inputs[["import_btn"]]
    save_report <- ids()$data_import$metadata_section$inputs$save_report
    # Field validation --------------------------------------------------------
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule(
      inputId = proj_name_id,
      rule = shinyvalidate::sv_required(message = "Project name is required")
    )
    iv$add_rule(
      inputId = sample_id_id,
      rule = shinyvalidate::sv_required(
        message = "Sample identifier is required"
      )
    )
    iv$add_rule(
      inputId = cell_lines_id,
      rule = shinyvalidate::sv_required(
        message = ""
      )
    )
    iv$add_rule(
      inputId = sep_id,
      rule = shinyvalidate::sv_required(message = "Separator is required")
    )
    iv$add_rule(
      inputId = dates_format_id,
      rule = shinyvalidate::sv_required(message = "Date format is required")
    )
    iv$enable()
    # File input --------------------------------------------------------------
    volumes <- c(
      Home = fs::path_home(), "R Installation" = R.home(),
      shinyFiles::getVolumes()()
    )
    file_input_id <- ids()$data_import$metadata_section$inputs$file_input
    file_input_display_id <- ids()$data_import$metadata_section$outputs[[
      "file_input_display"
    ]]
    metadata_file <- reactiveVal(NULL)
    shinyFiles::shinyFileChoose(
      input = input,
      session = session,
      id = file_input_id,
      roots = volumes,
      filetypes = c("csv", "tsv", "txt", "xlsx", "xls")
    )
    .file_input_handler_react(
      file_btn_id = file_input_id,
      file_display_id = file_input_display_id,
      volumes = volumes,
      input = input,
      output = output,
      file_reactive = metadata_file
    )
    meta_file_validator <- .file_input_validator(
      input = input,
      output = output,
      file_btn_id = file_input_id,
      file_display_id = file_input_display_id,
      message = "Metadata file is required"
    )
    # Root folder -------------------------------------------------------------
    metadata_root <- reactiveVal(NULL)
    shinyFiles::shinyDirChoose(
      input,
      ids()$data_import$metadata_section$inputs$root_dir,
      roots = volumes,
      session = session
    )
    .folder_input_handler_react(
      dir_btn_id = ids()$data_import$metadata_section$inputs$root_dir,
      dir_display_id = ids()$data_import$metadata_section$outputs$root_display,
      volumes = volumes,
      input = input,
      output = output,
      folder_reactive = metadata_root
    )
    # Sample identifier -------------------------------------------------------
    ## Update choices when ISAnalytics options change
    observe({
      req(workflow())
      gargoyle::watch("isa-opt")
      af_file_cols <- workflow()$get_ISA_options()$options[[
        "af_specs"
      ]]
      updateSelectizeInput(
        session = session,
        inputId = sample_id_id,
        choices = af_file_cols$names,
        server = TRUE
      )
    })
    # Control lines -----------------------------------------------------------
    ## Update choices when control lines db gets updated
    observe({
      gargoyle::watch("cl-db-change")
      default_lines <- names(cldb$default_lines)
      user_defined_lines <- names(cldb$user_defined_lines)
      if (not_null(user_defined_lines) &&
        length(user_defined_lines) > 0) {
        choices <- list(
          "Default lines" = as.list(default_lines),
          "User-defined" = as.list(user_defined_lines)
        )
        choices_opt <- list(
          content = list(
            "Default lines" = .render_cl_choices(
              default_lines,
              type = "default"
            ),
            "User-defined" = .render_cl_choices(
              user_defined_lines,
              type = "user-defined"
            )
          )
        )
      } else {
        choices <- list("Default lines" = as.list(default_lines))
        choices_opt <- list(
          content = list(
            "Default lines" = .render_cl_choices(
              default_lines,
              type = "default"
            )
          )
        )
      }
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = cell_lines_id,
        choices = choices,
        choicesOpt = choices_opt
      )
    })
    # Load button -------------------------------------------------------------
    .activate_load_observer(
      input_validator = iv,
      custom_validators = list(meta_file_validator),
      input = input
    )
    ## Init flags and reactive values ------------------------------------------
    gargoyle::init(
      "begin-import-meta", "end-import-meta",
      "finalize-import-meta", "cl-issues-proceed",
      "update-import-manual-panel"
    )
    prog <- reactiveVal(NULL)
    cl_choices_react <- reactiveValues(
      cl_summary_tbl = NULL,
      cl_selection_tbl = NULL,
      exclude_data = NULL,
      exclude_cl = NULL
    )
    add_checks <- reactiveVal(NULL)
    internal_checks_env <- reactiveVal(NULL)
    imported_meta <- reactiveVal(NULL)
    res_results <- reactiveVal(NULL)
    report_path <- reactive({
      if (input[[save_report]]) {
        workflow()$get_ISA_report_path()
      } else {
        NULL
      }
    })
    import_params <- reactive(
      list(
        path = metadata_file()$datapath,
        root = metadata_root(),
        dates_format = input[[dates_format_id]],
        project_name = input[[proj_name_id]],
        project_col = workflow()$proj_col,
        sep = input[[sep_id]],
        report_path = report_path(),
        checks_env = internal_checks_env,
        react_val = imported_meta
      )
    )
    ## Modal handling ----------------------------------------------------------
    cl_issues_modal <- .cl_issues_modal_ui(session = session)
    .handle_cl_issues_modal(
      input = input,
      output = output,
      session = session,
      cl_issues_choices = cl_choices_react,
      add_checks = add_checks
    )
    ## Additional checks handling ----------------------------------------------
    observeEvent(imported_meta(), {
      req(imported_meta())
      add_checks(NULL)
      prog()$inc(1, detail = "Performing additional checks...")
      .meta_additional_checks(
        imported_meta = imported_meta(),
        control_cell_line = input[[cell_lines_id]],
        indep_sample_id = input[[sample_id_id]],
        isa_opt = workflow()$get_ISA_options()$options,
        add_checks_react = add_checks,
        cldb = cldb
      )
    })
    observeEvent(add_checks(), {
      req(add_checks())
      .handle_add_checks(
        checks = add_checks(),
        input = input,
        output = output,
        session = session,
        cl_issues_modal = cl_issues_modal,
        cl_choices_react = cl_choices_react
      )
    })
    ## Flags observers ---------------------------------------------------------
    gargoyle::on("begin-import-meta",
      expr = {
        prog(Progress$new(
          session = session,
          min = 0,
          max = 3
        ))
        .activate_spinner(load_btn_id)
        prog()$set(
          message = "Importing metadata...",
          detail = "Reading file...",
          value = 1
        )
        imported_meta(NULL)
        rlang::exec(
          .meta_import,
          !!!import_params()
        )
      }
    )
    gargoyle::on("end-import-meta", expr = {
      .deactivate_spinner(load_btn_id)
      if (not_null(prog())) {
        prog()$close()
        prog(NULL)
      }
    })
    gargoyle::on("cl-issues-proceed", {
      all_checks <- list(
        cl_choices = cl_choices_react,
        internal_checks = internal_checks_env()
      )
      res_results(list(
        meta = imported_meta(),
        checks = all_checks
      ))
      gargoyle::trigger("finalize-import-meta")
    })
    gargoyle::on("finalize-import-meta", {
      .finalize_import(
        import_results = res_results(),
        was_aligned = not_null(metadata_root()),
        file_path = metadata_file()$datapath,
        project = input[[proj_name_id]],
        separator = input[[sep_id]],
        date_format = input[[dates_format_id]],
        ind_sample_id = input[[sample_id_id]],
        workflow = workflow(),
        progress_obj = prog()
      )
      report_content <- workflow()$render_report(
        type = "metadata",
        mode = "interactive",
        output = output
      )
      output[[
        ids()$data_import$metadata_section$outputs[[
          "meta_report"
        ]]
      ]] <- renderUI(report_content)
      shinyjs::enable(
        id = ids()$data_import$metadata_section[[
          "inputs"
        ]][["tab_2_btn"]]
      )
      gargoyle::trigger("end-import-meta")
      gargoyle::trigger("update-import-manual-panel")
    })

    .load_metadata_btn_observer(
      input,
      folder_reactive = metadata_root
    )
  })
}
