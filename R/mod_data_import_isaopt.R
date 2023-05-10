#' data_import_isaopt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_isaopt_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "width: 80%;",
    class = "card mb-2",
    id = id,
    div(
      class = "card-body",
      .section_name_with_tooltip(
        section_name = "ISAnalytics options configuration",
        type = h2, tooltip_content_builder = .isa_opt_tooltip_builder
      ),
      shinyWidgets::radioGroupButtons(
        inputId = ns(ids()$data_import$isa_opt_section$inputs$toggle1),
        choices = c("Use defaults", "Import configuration"),
        size = "sm",
        status = "secondary"
      ),
      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'Import configuration'",
          ns(ids()$data_import$isa_opt_section$inputs$toggle1)
        ),
        div(
          class = "input-row-container",
          div(
            style = "padding-right: 10px;",
            tagAppendAttributes(
              fileInput(
                inputId = ns(
                  ids()$data_import$isa_opt_section$inputs[[
                    "json_file_input"
                  ]]
                ),
                label = "Choose configuration file (*.json)",
                accept = ".json", width = "100%"
              ),
              style = "margin-bottom: 0px;"
            )
          ),
          div(
            style = "align-self: flex-end;",
            shinyjs::disabled(actionButton(
              inputId = ns(
                ids()$data_import$isa_opt_section$inputs[[
                  "upload_config_btn"
                ]]
              ),
              label = "Load"
            ))
          )
        ),
        div(
          id = ns(
            ids()$data_import$isa_opt_section$outputs$status_container
          ),
          style = "padding-top: 10px;",
          uiOutput(
            ns(ids()$data_import$isa_opt_section$outputs$import_status)
          )
        )
      ),
      div(
        uiOutput(ns(
          ids()$data_import$isa_opt_section$outputs$confirm_choice
        ))
      ),
      .folder_input(
        button_id = ns(ids()$data_import$isa_opt_section$inputs$report_folder),
        display_id = ns(ids()$data_import$isa_opt_section$outputs[[
          "report_folder_display"
        ]]),
        "ISAnalytics report save folder:"
      )
    )
  )
}

#' data_import_isaopt Server Functions
#'
#' @noRd
mod_data_import_isaopt_server <- function(id, workflow) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    gargoyle::init("isa-opt")
    # Options choice banner ---------------------------------------------------
    ## Initialize choice banner
    output[[
      ids()$data_import$isa_opt_section$outputs$confirm_choice
    ]] <- renderUI({
      .generate_status_banner(
        type = "primary",
        content = list(div(
          strong(
            "Using",
            span("default",
              style = "text-decoration: underline;"
            ),
            "ISAnalytics settings"
          )
        ))
      )
    })
    ## Register observer
    output[[
      ids()$data_import$isa_opt_section$outputs$confirm_choice
    ]] <- renderUI({
      gargoyle::watch("isa-opt")
      .isa_status_banner_info(workflow = workflow())
    })
    # Changing options ---------------------------------------------------
    upload_status_banner <- reactiveVal(NULL)
    observeEvent(upload_status_banner(), {
      output[[
        ids()$data_import$isa_opt_section$outputs$import_status
      ]] <- renderUI({
        upload_status_banner()
      })
    })
    ## Toggle observer
    .isa_toggle_observer(input, workflow, upload_status_banner)
    ## File input observer
    .isa_file_input_observer(input)
    ## Upload button observer
    .isa_upload_btn_observer(input, upload_status_banner, workflow)
    # Report folder ---------------------------------------------------
    volumes <- c(
      Home = fs::path_home(), "R Installation" = R.home(),
      shinyFiles::getVolumes()()
    )
    shinyFiles::shinyDirChoose(
      input,
      ids()$data_import$isa_opt_section$inputs$report_folder,
      roots = volumes,
      session = session
    )
    .folder_input_handler(
      dir_btn_id = ids()$data_import$isa_opt_section$inputs$report_folder,
      dir_display_id = ids()$data_import$isa_opt_section$outputs[[
        "report_folder_display"
      ]],
      volumes = volumes,
      input = input,
      output = output,
      flag = "isa-report-folder",
      workflow = workflow,
      getter = "get_ISA_report_path",
      setter = "set_ISA_report_path"
    )
  })
}
