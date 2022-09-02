options(shiny.maxRequestSize = 50 * 1024^2)

custom_css <- function() {
  system.file("css", "custom_rules.scss", package = "RISingVIS")
}

#' @importFrom bslib bs_add_variables bs_theme bs_add_rules
#' @importFrom sass sass_file
#' @importFrom magrittr `%>%`
app_theme <- function() {
  bslib::bs_add_variables(
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
    "white" = "#ffffff",
    "light" = "#e1f2e9",
    "dark" = "#080942",
    "primary" = "#ffc812",
    "secondary" = "#d90889",
    "info" = "#609fe6",
    "accent1" = "#78cdd7",
    "accent2" = "#a273ef",
    "accent3" = "#a8a1df",
    "success" = "#198754",
    "warning" = "#ffc107",
    "danger" = "#dc3545",
    "alert-bg-scale" = "-80%",
    "alert-border-scale" = "-70%",
    "alert-color-scale" = "40%"
  ) %>%
    bslib::bs_add_rules(sass::sass_file(custom_css()))
}

#' @importFrom shinyWidgets materialSwitch radioGroupButtons
#' @importFrom shinyjs useShinyjs
#' @importFrom bslib page_navbar nav
#' @importFrom shinyFiles shinyDirButton
ui <- tagList(
  shinyjs::useShinyjs(),
  tags$head(
    tags$script(src = "node_clicked.js"),
    tags$script(src = "style_fileinput.js"),
    tags$script(src = "js_functions.js"),
  ),
  bslib::page_navbar(
    title = "RISingVIS",
    theme = app_theme(),
    window_title = "RISingVIS",
    bg = "#ffc812",
    position = "fixed-top",
    # --- Main workflow section
    bslib::nav(
      "Workflow",
      fluidPage(
        # ## --- Workflow home
        # fluidRow(
        #   style = "display: none;",
        #   column(
        #     width = 12,
        #     id = "first-page",
        #     div("New workflow",
        #         class = "display-2",
        #         align = "center"
        #     ),
        #     uiOutput("svg_full"),
        #   )
        # ),
        ## --- Container for all workflow pages
        fluidRow(
          ### --- Left side mini-graph
          column(
            width = 2,
            style = "padding-top: 2%;",
            align = "center",
            uiOutput(id_list()$side_graph)
          ),
          ### --- Pages content
          column(
            width = 10,
            class = "pages-container",
            dataImportUI(id_list()$data_import$section_id),
            RecUI(id_list()$recalibration$section_id)
          )
        )
      )
    )
  )
)


# ui <- tagList(
#   shinyjs::useShinyjs(),
#   tags$head(
#     tags$script(src = "node_clicked.js"),
#     tags$script(src = "style_fileinput.js"),
#     tags$script(src = "js_functions.js"),
#   ),
#   bslib::page_navbar(
#     title = "RISingVIS",
#     theme = app_theme(),
#     window_title = "RISingVIS",
#     bg = "#ffc812",
#     position = "fixed-top",
#     # --- Main workflow section
#     bslib::nav(
#       "Workflow",
#       fluidPage(
#         ## --- Workflow home
#         fluidRow(
#           style = "display: none;",
#           column(
#             width = 12,
#             id = "first-page",
#             div("New workflow",
#                 class = "display-2",
#                 align = "center"
#             ),
#             uiOutput("svg_full"),
#           )
#         ),
#         ## --- Container for all workflow pages
#         fluidRow(
#           ### --- Left side mini-graph
#           column(
#             width = 2,
#             style = "padding-top: 2%;",
#             align = "center",
#             uiOutput("svg_mini")
#           ),
#           ### --- Pages content
#           column(
#             width = 10,
#             class = "pages-container",
#             div(
#               id = "data-import-section",
#               #style = "display: none;",
#               width = "100%",
#               div("Data import", class = "display-2"),
#               div(
#                 id = "metadata-sub-section",
#                 h2("ISAnalytics options configuration"),
#                 shinyWidgets::radioGroupButtons(
#                   inputId = "isaoptions",
#                   choices = c("Use defaults", "Import configuration"),
#                   size = "sm",
#                   status = "secondary"
#                 ),
#                 conditionalPanel(
#                   condition = "input.isaoptions == 'Import configuration'",
#                   div(
#                     class = "input-row-container",
#                     div(
#                       style = "padding-right: 10px;",
#                       tagAppendAttributes(
#                         fileInput(
#                           inputId = "isa_config",
#                           label = "Choose configuration file (*.json)",
#                           accept = ".json", width = "100%"
#                         ), style = "margin-bottom: 0px;"
#                       )
#                     ),
#                     div(
#                       style = "align-self: flex-end;",
#                       shinyjs::disabled(actionButton(
#                         inputId = "load_isa_opt_btn",
#                         label = "Load"
#                       ))
#                     )
#                   ),
#                   div(
#                     id = "isa_opt_import_status_container",
#                     style = "width: 80%; padding-top: 10px;",
#                     uiOutput("isa_opt_import_status")
#                   )
#                 ),
#                 div(
#                   style = "width: 80%;",
#                   uiOutput("confirm_isa_opt_choice")
#                 ),
#                 h2("Metadata"),
#                 fileInput(
#                   inputId = "metadata_file",
#                   label = "Choose metadata file (*.tsv, *.csv or *.xlsx)",
#                   accept = c(".csv", ".tsv", ".xls", ".xlsx"),
#                   width = "80%"
#                 ),
#                 div(
#                   align = "left",
#                   shinyWidgets::materialSwitch(
#                     inputId = "align_fs_option",
#                     label = "Align file system (VISPA2)",
#                     value = TRUE,
#                     status = "primary"
#                   )
#                 ),
#                 div(
#                   align = "left",
#                   id = "root_dir_container",
#                   class = "input-mock-container",
#                   shinyFiles::shinyDirButton(
#                     id = "root_dir",
#                     title = "Root directory",
#                     label = "Choose",
#                     icon = icon("folder"),
#                     buttonType = "primary",
#                     class = "choose-dir-btn"
#                   ),
#                   tagAppendAttributes(
#                     textOutput(
#                       outputId = "root_dir_display"
#                     ), class = "text-out-mock-enabled"
#                   )
#                 ),
#                 div(
#                   class = "input-row-container",
#                   style = "margin-top: 2em;",
#                   div(
#                     style = "padding-right: 10px;",
#                     textInput(inputId = "proj_name",
#                               label = "Project name",
#                               width = "100%")
#                   ),
#                   div(
#                     style = "padding-right: 10px;",
#                     selectizeInput(
#                       inputId = "meta_separator",
#                       label = "File separator",
#                       choices = c("tab", ",", ";", "space"),
#                       width = "100%"
#                     )
#                   ),
#                   div(
#                     selectizeInput(
#                       inputId = "meta_dates_format",
#                       label = "General dates format",
#                       choices = ISAnalytics::date_formats(),
#                       width = "100%"
#                     )
#                   )
#                 ),
#                 div(
#                   class = "input-row-container",
#                   div(
#                     style = "padding-right: 10px;",
#                     selectizeInput(
#                       inputId = "indep_sample_id",
#                       label = "Independent sample identifier",
#                       choices = NULL,
#                       multiple = TRUE,
#                       width = "100%"
#                     )
#                   ),
#                   div(
#                     style = "padding-right: 10px;",
#                     selectizeInput(
#                       inputId = "control_cell_line",
#                       label = "Control cell line",
#                       choices = c("CEM37"),
#                       multiple = FALSE,
#                       width = "100%"
#                     )
#                   )
#                 ),
#                 div(
#                   class = "input-row-container",
#                   actionButton(
#                     inputId = "meta_import_btn",
#                     label = tags$div(
#                       tags$span(
#                         id = "import_meta_spinner",
#                         class = "spinner-border spinner-border-sm",
#                         style = "display: none;"
#                       ),
#                       tags$span(
#                         "Import"
#                       )
#                     )
#                   ),
#                   div(
#                     id = "metadata_import_status",
#                     style = "margin-left: 10px;",
#                     uiOutput("meta_import_status")
#                   )
#                 ),
#                 tags$button(
#                   class = "btn",
#                   id = "meta_detail_btn",
#                   `data-bs-toggle` = "collapse",
#                   `data-bs-target` = "#metaImportDetails",
#                   `aria-expanded` = "false",
#                   `aria-controls` = "metaImportDetails",
#                   style = paste("margin-top:10px; margin-bottom: 5px;",
#                                 "visibility: hidden;"),
#                   icon("info"),
#                   "Details"
#                 ),
#                 div(
#                   class = "collapse",
#                   id = "metaImportDetails",
#                   uiOutput("metaImportDetailsContent")
#                 ),
#               ),
#               div(
#                 id = "data-sub-section",
#                 style = "display: none;",
#                 h2("Data"),
#                 shinyWidgets::radioGroupButtons(
#                   inputId = "auto_manual_import_switch",
#                   choices = c("Automatic import", "Manual import"),
#                   size = "sm",
#                   status = "secondary"
#                 ),
#                 shinyWidgets::awesomeCheckbox(
#                   inputId = "matrix_annotated",
#                   label = "Matrices are annotated",
#                   value = TRUE,
#                   status = "success"
#                 ),
#                 div(
#                   selectizeInput(
#                     inputId = "data_separator",
#                     label = "File separator",
#                     choices = c("tab", ",", ";", "space"),
#                     width = "50%"
#                   )
#                 ),
#                 conditionalPanel(
#                   condition = paste0("input.auto_manual_import_switch",
#                                      " == 'Automatic import'"),
#                   p("Import matrices automatically using metadata"),
#                   textInput(# CHANGE THIS TO NUMERIC
#                     inputId = "max_par_workers",
#                     label = "Maximum parallel workers",
#                     value = 2),
#                   tags$button(
#                     class = "btn btn-light",
#                     style = "margin-bottom: 10px;",
#                     id = "adv_data_imp_opts_btn",
#                     `data-bs-toggle` = "collapse",
#                     `data-bs-target` = "#advDataImpOpts",
#                     `aria-expanded` = "false",
#                     `aria-controls` = "advDataImpOpts",
#                     "Advanced options"
#                   ),
#                   div(
#                     class = "collapse",
#                     style = "margin-bottom: 10px;",
#                     id = "advDataImpOpts",
#                     div(
#                       class = "card",
#                       div(
#                         class = "card-body",
#                         selectizeInput(
#                           inputId = "data_file_patterns",
#                           options = list(create = TRUE),
#                           multiple = TRUE,
#                           choices = c(),
#                           label = "File patterns (regular expressions)"
#                         ),
#                         selectizeInput(
#                           inputId = "data_file_matching_opt",
#                           multiple = FALSE,
#                           choices = c("ANY", "ALL", "OPTIONAL"),
#                           label = "Matching option",
#                           selected = "ANY"
#                         )
#                       )
#                     )
#                   )
#                 ),
#                 conditionalPanel(
#                   condition = paste0("input.auto_manual_import_switch",
#                                      " == 'Manual import'"),
#                   div(
#                     align = "left",
#                     style = "margin-top: 5px;",
#                     shinyWidgets::materialSwitch(
#                       inputId = "files_tidy",
#                       label = "My files are in tidy format",
#                       value = FALSE,
#                       status = "primary"
#                     )
#                   ),
#                   fileInput(
#                     inputId = "data_files",
#                     label = "Choose file(s) (*.tsv, *.csv or *.xlsx)",
#                     accept = c(".csv", ".tsv", ".xls", ".xlsx"),
#                     width = "80%", multiple = TRUE
#                   ),
#                 ),
#                 div(
#                   class = "input-row-container",
#                   actionButton(
#                     inputId = "data_import_btn",
#                     label = tags$div(
#                       tags$span(
#                         id = "import_data_spinner",
#                         class = "spinner-border spinner-border-sm",
#                         style = "display: none;"
#                       ),
#                       tags$span(
#                         "Import"
#                       )
#                     )
#                   ),
#                   div(
#                     id = "data_import_status",
#                     style = "margin-left: 10px;",
#                     uiOutput("data_import_status")
#                   )
#                 ),
#                 tags$button(
#                   class = "btn",
#                   id = "data_detail_btn",
#                   `data-bs-toggle` = "collapse",
#                   `data-bs-target` = "#dataImportDetails",
#                   `aria-expanded` = "false",
#                   `aria-controls` = "dataImportDetails",
#                   style = paste("margin-top:10px; margin-bottom: 5px;",
#                                 "visibility: hidden;"),
#                   icon("info"),
#                   "Details"
#                 ),
#                 div(
#                   class = "collapse",
#                   id = "dataImportDetails",
#                   uiOutput("dataImportDetailsContent")
#                 )
#               ),
#               div(
#                 align = "center",
#                 style = "width: 80%;",
#                 tagAppendAttributes(
#                   actionButton(
#                     inputId = "to_rec_page",
#                     label = "NEXT"
#                   ),
#                   class = "btn btn-primary btn-lg",
#                   style = paste("margin-top: 10px;",
#                                 "display: none;"
#                                 )
#                 )
#               )
#             ),
#             div(
#               id = "recalibration-section",
#               style = "display: none;",
#               width = "100%",
#               div("Recalibration",
#                   class = "display-2"),
#               div(
#                 class = "alert alert-info",
#                 style = "width: 80%",
#                 icon(name = "circle-info"),
#                 "Warning: the recalibration step might require several",
#                 "minutes or hours for bigger datasets"
#               ),
#               h3("Recalibration parameters"),
#               div(
#                 class = "input-row-container",
#                 tagAppendAttributes(
#                   numericInput(
#                     inputId = "rec-threshold",
#                     label = "Threshold",
#                     value = 4,
#                     step = 1,
#                     width = "20%"
#                   ),
#                   style = "margin-right: 10px;"
#                 ),
#                 selectizeInput(# MUST UPDATE WHEN UPDATING ISAOPTIONS
#                   inputId = "rec-is-tags",
#                   label = "IS identity tags",
#                   choices = ISAnalytics::mandatory_IS_vars(TRUE)$tag[
#                     ISAnalytics::mandatory_IS_vars(TRUE)$tag != "locus"
#                   ],
#                   selected = ISAnalytics::mandatory_IS_vars(TRUE)$tag[
#                     ISAnalytics::mandatory_IS_vars(TRUE)$tag != "locus"
#                   ],
#                   multiple = TRUE,
#                   width = "50%"
#                 )
#               ),
#               div(
#                 class = "input-row-container",
#                 selectizeInput(
#                   inputId = "rec-criteria",
#                   label = "Keep criteria",
#                   choices = c("max_value", "keep_first"),
#                   selected = "max_value",
#                   multiple = FALSE,
#                   width = "25%"
#                 ),
#                 tagAppendAttributes(
#                   numericInput(
#                     inputId = "rec-workers",
#                     label = "Parallel workers",
#                     value = 4,
#                     step = 1,
#                     width = "20%"
#                   ),
#                   style = "margin-left: 10px;"
#                 ),
#               ),
#               div(
#                 align = "left",
#                 shinyWidgets::materialSwitch(
#                   inputId = "rec-map",
#                   label = "Save recalibration map",
#                   value = FALSE,
#                   status = "primary"
#                 ),
#                 div(
#                   align = "left",
#                   id = "rec-map-path-container",
#                   class = "input-mock-container",
#                   shinyFiles::shinyDirButton(
#                     id = "rec-map-path",
#                     title = "Save map to folder",
#                     label = "Choose",
#                     icon = icon("folder"),
#                     buttonType = "primary",
#                     class = "choose-dir-btn"
#                   ),
#                   tagAppendAttributes(
#                     textOutput(
#                       outputId = "map-rec-path-display"
#                     ), class = "text-out-mock-enabled",
#                     style = "width: 50%;"
#                   )
#                 )
#               ),
#               div(
#                 class = "input-row-container",
#                 style = paste("justify-content: center;",
#                               "margin-top: 3rem;"),
#                 tagAppendAttributes(
#                   actionButton(inputId = "rec-btn",
#                                label = "Recalibrate",
#                                width = "20%"),
#                   class = "btn btn-secondary",
#                   style = "margin-right: 1rem;"
#                 ),
#                 tagAppendAttributes(
#                   actionButton(inputId = "rec-skip-btn", label = "Skip",
#                                width = "20%"),
#                   class = "btn btn-grey"
#                 )
#               )
#             )
#           )
#         )
#       )
#     ),
#     # --- FAQs section
#     bslib::nav("FAQs", {
#       h1("Faqs here")
#     }),
#     # --- About section
#     bslib::nav(title = "About")
#   )
# )
