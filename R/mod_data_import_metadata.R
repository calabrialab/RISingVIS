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
  div(
    id = id,
    style = "width: 80%;",
    class = "card mb-2",
    div(
      class = "card-body",
      .section_name_with_tooltip(
        section_name = "Metadata",
        type = h2,
        tooltip_content_builder = .metadata_tooltip_builder
      ),
      fileInput(
        inputId = ns(ids()$data_import$metadata_section$inputs$file_input),
        label = "Choose metadata file (*.tsv, *.csv or *.xlsx)",
        accept = c(".csv", ".tsv", ".xls", ".xlsx"),
        width = "100%"
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
            class = "col",
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
            class = "col",
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
            class = "col",
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
            class = "col",
            div(
              selectizeInput(
                inputId = ns(
                  ids()$data_import$metadata_section$inputs$sample_id
                ),
                label = .section_name_with_tooltip(
                  section_name = "Independent sample identifier",
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
            class = "col",
            .cell_line_selectize(ns)
          ),
          div(
            class = "col",
            style = "align-self: center;",
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
          tagAppendAttributes(
            actionButton(
              inputId = ns(
                ids()$data_import$metadata_section$inputs$import_btn
              ),
              icon = icon("upload"),
              label = span(
                span("Load"),
                shinyjs::hidden(
                  span(
                    id = ns(
                      ids()$data_import$metadata_section$inputs$import_spinner
                    ),
                    class = "spinner-border spinner-border-sm"
                  )
                )
              )
            ),
            class = "btn-secondary text-nowrap btn-lg"
          )
        )
      )
    )
  )
}

#' data_import_metadata Server Functions
#'
#' @noRd
mod_data_import_metadata_server <- function(id, workflow) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Field validation --------------------------------------------------------
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule(
      inputId = ids()$data_import$metadata_section$inputs$project,
      rule = shinyvalidate::sv_required(message = "Project name is required")
    )
    iv$add_rule(
      inputId = ids()$data_import$metadata_section$inputs$sample_id,
      rule = shinyvalidate::sv_required(
        message = "Sample identifier is required"
      )
    )
    iv$add_rule(
      inputId = ids()$data_import$metadata_section$inputs$file_input,
      rule = shinyvalidate::sv_required(message = "Metadata file is required")
    )
    iv$add_rule(
      inputId = ids()$data_import$metadata_section$inputs$control_line,
      rule = shinyvalidate::sv_required(
        message = ""
      )
    )
    iv$add_rule(
      inputId = ids()$data_import$metadata_section$inputs$separator,
      rule = shinyvalidate::sv_required(message = "Separator is required")
    )
    iv$add_rule(
      inputId = ids()$data_import$metadata_section$inputs$dates_format,
      rule = shinyvalidate::sv_required(message = "Date format is required")
    )
    iv$enable()
    # File input --------------------------------------------------------------
    # observeEvent(input[[
    #   ids()$data_import$metadata_section$inputs$file_input
    # ]], {
    #   print(input[[
    #     ids()$data_import$metadata_section$inputs$file_input
    #   ]])
    #   # req(workflow())
    #   # workflow()$set_metadata(
    #   #   element = "file_path",
    #   #   value = input[[
    #   #     ids()$data_import$metadata_section$inputs$file_input
    #   #   ]]
    #   # )
    # })
    # Root folder -------------------------------------------------------------
    metadata_root <- reactiveVal(NULL)
    volumes <- c(
      Home = fs::path_home(), "R Installation" = R.home(),
      shinyFiles::getVolumes()()
    )
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
    # .folder_input_handler(
    #   dir_btn_id = ids()$data_import$metadata_section$inputs$root_dir,
    #   dir_display_id = ids()$data_import$metadata_section$outputs$root_display,
    #   volumes = volumes,
    #   input = input,
    #   output = output,
    #   flag = "meta-root-dir",
    #   workflow = workflow,
    #   getter = "get_metadata",
    #   setter = "set_metadata",
    #   setter_args = list(element = "root"),
    #   getter_args = list(element = "root")
    # )
    # Project input -----------------------------------------------------------
    # observeEvent(input[[
    #   ids()$data_import$metadata_section$inputs$project
    # ]], {
    #   req(workflow())
    #   workflow()$set_metadata(
    #     element = "project",
    #     value = input[[
    #       ids()$data_import$metadata_section$inputs$project
    #     ]]
    #   )
    # })
    # File separator ----------------------------------------------------------
    # observeEvent(input[[
    #   ids()$data_import$metadata_section$inputs$separator
    # ]], {
    #   req(workflow())
    #   workflow()$set_metadata(
    #     element = "separator",
    #     value = input[[
    #       ids()$data_import$metadata_section$inputs$separator
    #     ]]
    #   )
    # })
    # Date format -------------------------------------------------------------
    # observeEvent(input[[
    #   ids()$data_import$metadata_section$inputs$dates_format
    # ]], {
    #   req(workflow())
    #   workflow()$set_metadata(
    #     element = "date_format",
    #     value = input[[
    #       ids()$data_import$metadata_section$inputs$dates_format
    #     ]]
    #   )
    # })
    # Sample identifier -------------------------------------------------------
    ## Update choices when ISAnalytics options change
    observe({
      req(workflow())
      gargoyle::watch("isa-opt")
      af_file_cols <- workflow()$get_ISA_options()$options[[
        "association_file_columns"
      ]]$names
      updateSelectizeInput(
        session = session,
        inputId = ids()$data_import$metadata_section$inputs$sample_id,
        choices = af_file_cols
      )
    })
    ## Update the value in workflow
    # observeEvent(input[[
    #   ids()$data_import$metadata_section$inputs$sample_id
    # ]], {
    #   req(workflow())
    #   workflow()$set_metadata(
    #     element = "ind_sample_id",
    #     x = input[[
    #       ids()$data_import$metadata_section$inputs$sample_id
    #     ]]
    #   )
    # })
    # Control lines -----------------------------------------------------------
    ## Update choices when workflow gets updated
    gargoyle::init("cl-list")
    observe({
      req(workflow())
      gargoyle::watch("cl-list")
      current_cl_list <- workflow()$get_available_cl()
      updateSelectizeInput(
        session = session,
        inputId = ids()$data_import$metadata_section$inputs$control_line,
        choices = names(current_cl_list)
      )
    })
    ## Button when clicked shows modal for import
    .add_cl_btn_observer(ns, input, workflow)
    # Load button -------------------------------------------------------------
    .activate_load_observer(input_validator = iv, input = input)
    .load_metadata_btn_observer(input, output, workflow)
  })
}
