################################################################################
# Utility internal functions for the control lines db module                   #
################################################################################

# Main table display utilities -------------------------------------------------

#' Converts the control cell line db to a table format for display
#' @param cldb The control cell line db object
#' @return A tibble with the control cell line db in a table format
#' @noRd
.convert_db_to_table <- function(cldb) {
    # Converter
    cl_to_row <- function(cl, default) {
        type <- if (default) {
            "Default"
        } else {
            "User defined"
        }
        description <- cl$description
        if (is.null(description)) {
            description <- ""
        }
        tibble::tibble_row(
            Name = cl$name,
            known_iss = list(cl$known_iss),
            Description = description,
            Type = type,
            editable = cl$is_editable()
        )
    }
    # Defaults
    default_lines <- cldb$default_lines
    db_tbl_defaults <- purrr::map(default_lines, ~ cl_to_row(.x, TRUE)) |>
        purrr::list_rbind()
    # User-defined
    user_defined_lines <- cldb$user_defined_lines
    db_tbl_user_defined <- purrr::map(
        user_defined_lines,
        ~ cl_to_row(.x, FALSE)
    ) |>
        purrr::list_rbind()
    return(dplyr::bind_rows(db_tbl_defaults, db_tbl_user_defined))
}

#' Builds the actual reactable for the control lines db
#' @param cldb_df The control cell line db in a table format
#' @param ns The namespace of the module
#' @return A reactable object
#' @noRd
.build_cl_db_table <- function(cldb_df, ns) {
    tbl_id <- "cl-db-reactable"
    # Functions for rendering
    ## Rendering of known_iss sub table
    row_details <- function(index) {
        div(
            class = "card w-100",
            div(
                class = "card-body",
                h5("Known integration sites", class = "card-title"),
                reactable::reactable(
                    data = cldb_df$known_iss[[index]],
                    outlined = TRUE,
                    bordered = TRUE,
                    striped = TRUE,
                    defaultColDef = reactable::colDef(
                        align = "center"
                    )
                )
            )
        )
    }
    ## Rendering of edit column
    edit_column_def <- reactable::colDef(
        name = "",
        sortable = FALSE,
        searchable = FALSE,
        filterable = FALSE,
        minWidth = 50,
        maxWidth = 50,
        cell = function(value, index) {
            btn <- tags$button(
                id = paste0("cl-tbl-row-", index),
                class = paste(
                    "btn btn-icon-only",
                    "shiny-bound-input action-button"
                ),
                icon("pen-to-square")
            )
            if (value) {
                btn
            } else {
                shinyjs::disabled(btn)
            }
        }
    )
    del_column_def <- reactable::colDef(
        name = "",
        sortable = FALSE,
        searchable = FALSE,
        filterable = FALSE,
        cell = function(value, index) {
            btn <- tags$button(
                id = paste0("cl-tbl-row-", index),
                class = paste(
                    "btn btn-icon-only",
                    "shiny-bound-input action-button"
                ),
                icon("trash")
            )
            if (value) {
                btn
            } else {
                shinyjs::disabled(btn)
            }
        }
    )
    show_edit_modal_id <- ns(ids()$control_lines_db$inputs$show_edit_modal)
    edit_modal_name_id <- ns(ids()$control_lines_db$inputs$edit_modal_name)
    show_delete_modal_id <- ns(ids()$control_lines_db$inputs$show_delete_modal)
    delete_modal_name_id <- ns(ids()$control_lines_db$inputs$delete_modal_name)
    edit_js_fn <- reactable::JS(
        sprintf(
            "function(rowInfo, column) {
                if (column.id === 'editable') {
                    if (window.Shiny && rowInfo.values.editable) {
                        Shiny.setInputValue('%s', true, { priority: 'event' });
                        Shiny.setInputValue('%s',
                        rowInfo.values.Name, { priority: 'event' });
                    }
                    return;
                }
                if (column.id === 'delete') {
                    if (window.Shiny && rowInfo.values.editable) {
                        Shiny.setInputValue('%s',
                        rowInfo.values.Name, { priority: 'event' });
                        Shiny.setInputValue('%s', true, { priority: 'event' });
                    }
                    return;
                }
                return;
            }",
            show_edit_modal_id,
            edit_modal_name_id,
            delete_modal_name_id,
            show_delete_modal_id
        )
    )
    ## Rendering of type column
    type_column_def <- reactable::colDef(
        name = "Type",
        sortable = TRUE,
        searchable = TRUE,
        filterable = TRUE,
        filterInput = .filter_input(tbl_id),
        filterMethod = .filter_strictly_equal(),
        cell = function(value) {
            if (value == "Default") {
                div(
                    class = "badge rounded-pill text-bg-accent1",
                    value
                )
            } else {
                div(
                    class = "badge rounded-pill text-bg-accent2",
                    value
                )
            }
        }
    )

    reactable::reactable(
        elementId = tbl_id,
        data = cldb_df |>
            dplyr::select(-"known_iss") |>
            dplyr::mutate(delete = .data$editable),
        details = row_details,
        filterable = TRUE,
        sortable = TRUE,
        searchable = TRUE,
        showSortable = TRUE,
        onClick = edit_js_fn,
        columns = list(
            Description = reactable::colDef(
                show = TRUE
            ),
            editable = edit_column_def,
            delete = del_column_def,
            Type = type_column_def
        )
    )
}

# Modal utilities --------------------------------------------------------------

## Known iss sub-panel ---------------------------------------------------------

#' Builds the ui for the portion of the modals resposible for adding/editing
#' known integration sites.
#' Note: sub components of the ui are rendered server-side through uiOutput
#' @param type The type of modal to build, either "edit" or "add"
#' @param ns The namespace of the module
#' @return The ui portion of just the known iss section
#' @noRd
.known_iss_edit_panel_ui <- function(type = c("edit", "add"), ns) {
    # Retrieve components ids -----------------------------------------------
    known_iss_tbl_id <- ns(
        ids()$control_lines_db$outputs[[
            paste0("modal_known_iss_tbl_", type)
        ]]
    )
    source_selectize_id <- ns(
        ids()$control_lines_db$inputs[[
            paste0("modal_source_selectize_", type)
        ]]
    )
    source_ui_id <- ns(
        ids()$control_lines_db$outputs[[
            paste0("modal_source_ui_", type)
        ]]
    )
    modal_add_row_ui_id <- ns(
        ids()$control_lines_db$outputs[[
            paste0("modal_add_row_ui_", type)
        ]]
    )
    modal_add_row_btn_id <- ns(
        ids()$control_lines_db$inputs[[
            paste0("modal_add_row_btn_", type)
        ]]
    )
    modal_edit_row_ui_id <- ns(
        ids()$control_lines_db$outputs[[
            paste0("modal_edit_row_ui_", type)
        ]]
    )
    modal_confirm_row_btn_id <- ns(
        ids()$control_lines_db$inputs[[
            paste0("modal_confirm_row_btn_", type)
        ]]
    )
    modal_delete_row_btn_id <- ns(
        ids()$control_lines_db$inputs[[
            paste0("modal_delete_row_btn_", type)
        ]]
    )
    # Define sub components -------------------------------------------------
    ## Edit content panel ---------------------------------------------------
    card_add_row <- div(
        class = "card",
        style = "margin-bottom: 1rem; margin-top: 0.5rem;",
        div(
            class = "card-body",
            h6(
                class = "card-title",
                "Add new row"
            ),
            uiOutput(
                outputId = modal_add_row_ui_id
            ),
            div(
                class = "d-flex w-100 justify-content-end",
                actionButton(
                    inputId = modal_add_row_btn_id,
                    label = "Add",
                    icon = icon("plus")
                )
            )
        )
    )
    card_edit_row <- div(
        class = "card",
        div(
            class = "card-body",
            h6(
                class = "card-title",
                "Edit row"
            ),
            uiOutput(
                outputId = modal_edit_row_ui_id
            ),
            div(
                class = "d-flex w-100 justify-content-end",
                tagAppendAttributes(
                    actionButton(
                        inputId = modal_confirm_row_btn_id,
                        label = "Confirm",
                    ),
                    style = "margin-right: 0.5rem;"
                ),
                actionButton(
                    inputId = modal_delete_row_btn_id,
                    label = "Delete",
                    icon = icon("trash")
                )
            )
        )
    )
    manual_edit_section_id <- ns(paste0("manual_edit_section_", type))
    manual_edit_section <- if (type == "edit") {
        div(
            id = manual_edit_section_id,
            h5("Manual edit"),
            div(
                class = "text-muted",
                "To edit existing rows select the row in the table first"
            ),
            card_add_row,
            card_edit_row
        )
    } else {
        shinyjs::hidden(
            div(
                id = manual_edit_section_id,
                h5("Manual edit"),
                div(
                    class = "text-muted",
                    "To edit existing rows select the row in the table first"
                ),
                card_add_row,
                card_edit_row
            )
        )
    }
    edit_content_panel <- bslib::nav(
        title = "Edit content",
        h5("Choose new source"),
        tagAppendAttributes(
            selectizeInput(
                inputId = source_selectize_id,
                label = NULL,
                choices = c(
                    "Import from tabular file",
                    "Copy from existing cell line"
                )
            ),
            style = "margin-bottom: 0;"
        ),
        uiOutput(
            outputId = source_ui_id
        ),
        manual_edit_section
    )
    ## Edit header panel ----------------------------------------------------
    modal_edit_headers_ui_id <- ns(
        ids()$control_lines_db$outputs[[
            paste0("modal_edit_headers_ui_", type)
        ]]
    )
    modal_edit_headers_btn_id <- ns(
        ids()$control_lines_db$inputs[[
            paste0("modal_edit_headers_btn_", type)
        ]]
    )
    edit_header_panel <- bslib::nav(
        title = "Edit headers",
        div(
            class = "text-muted",
            style = "margin-bottom: 1em;",
            "Assign new names to the columns of the table"
        ),
        uiOutput(
            outputId = modal_edit_headers_ui_id
        ),
        div(
            class = "d-flex w-100 justify-content-end",
            actionButton(
                inputId = modal_edit_headers_btn_id,
                label = "Rename",
                icon = icon("pencil")
            )
        )
    )
    # Assemble UI -----------------------------------------------------------
    div(
        h5("Known integration sites"),
        div(
            class = "container",
            div(
                class = "row",
                div(
                    class = "col-6",
                    reactable::reactableOutput(
                        outputId = known_iss_tbl_id
                    )
                ),
                div(
                    class = "col",
                    bslib::navs_pill_card(
                        edit_content_panel,
                        edit_header_panel
                    )
                )
            )
        )
    )
}

#' Defines the ui portion of the file import (conditioned to selectize)
#' @param type The type of modal to build, either "edit" or "add"
#' @param ns The namespace of the module
#' @return The ui portion of just the card containing file upload controls
#' @noRd
.known_iss_source_file_ui <- function(type = c("edit", "add"), ns) {
    modal_file_input_id <- ns(
        ids()$control_lines_db$inputs[[
            paste0("modal_file_input_", type)
        ]]
    )
    modal_file_input_btn_id <- ns(
        ids()$control_lines_db$inputs[[
            paste0("modal_file_input_btn_", type)
        ]]
    )
    div(
        class = "card",
        style = "margin-bottom: 1rem;",
        div(
            class = "card-body",
            fileInput(
                inputId = modal_file_input_id,
                label = NULL,
                placeholder = "Select file",
                multiple = FALSE,
                width = "100%"
            ),
            div(
                class = "d-flex w-100 justify-content-end",
                actionButton(
                    inputId = modal_file_input_btn_id,
                    label = "Import",
                    icon = icon("upload")
                )
            )
        )
    )
}

#' Performs the import of known is tabular file and eventually
#' displays alerts
#' @param path The path to the file to import
#' @param res_reactive The reactive value object to update
#' @noRd
.import_known_iss <- function(path, res_reactive) {
    ## Read the file
    imported <- .read_file(path, send_confirm_alerts = FALSE)
    if (is.null(imported)) {
        return()
    }
    imported <- tibble::as_tibble(imported)
    # Check imported data is not empty
    if (nrow(imported) == 0) {
        shinyWidgets::sendSweetAlert(
            title = "Something went wrong",
            type = "error",
            text = "The file provided is empty",
            btn_labels = "OK",
            btn_colors = "#dee2e6"
        )
        return()
    }
    res_reactive(imported)
}

#' Defines the server-side logic for managing file import subsection
#' @noRd
.known_iss_source_file_logic <- function(
    input, type = c("edit", "add"),
    temp_tbl) {
    # Import file -----------------------------------------------------------
    flag <- paste0("cl-modal-file-clear-", type)
    gargoyle::init(flag)
    modal_file_input_id <- ids()$control_lines_db$inputs[[
        paste0("modal_file_input_", type)
    ]]
    modal_file_input_btn_id <- ids()$control_lines_db$inputs[[
        paste0("modal_file_input_btn_", type)
    ]]
    imported_iss <- reactiveVal(NULL)
    observe({
        gargoyle::watch(flag)
        imported_iss(NULL)
    })
    observeEvent(input[[modal_file_input_btn_id]], {
        req(input[[modal_file_input_id]])
        .import_known_iss(input[[modal_file_input_id]]$datapath, imported_iss)
    })
    observeEvent(imported_iss(), {
        req(imported_iss())
        temp_tbl(imported_iss())
        shinyWidgets::sendSweetAlert(
            title = "All good!",
            text = "File imported successfully",
            type = "success",
            btn_colors = "#dee2e6"
        )
    })
}

#' Defines the ui portion of the copy from existing cell line
#' (conditioned to selectize)
#' @param type The type of modal to build, either "edit" or "add"
#' @param ns The namespace of the module
#' @return The ui portion of just the card containing lines copy controls
#' @noRd
.known_iss_source_cl_ui <- function(type = c("edit", "add"), ns) {
    modal_cl_selectize_id <- ns(
        ids()$control_lines_db$inputs[[
            paste0("modal_cl_input_selectize_", type)
        ]]
    )
    modal_cl_btn_id <- ns(
        ids()$control_lines_db$inputs[[
            paste0("modal_cl_input_btn_", type)
        ]]
    )
    div(
        class = "card",
        style = "margin-bottom: 1rem;",
        div(
            class = "card-body",
            selectizeInput(
                inputId = modal_cl_selectize_id,
                label = NULL,
                choices = NULL,
                multiple = FALSE,
                width = "100%"
            ),
            div(
                class = "d-flex w-100 justify-content-end",
                actionButton(
                    inputId = modal_cl_btn_id,
                    label = "Copy",
                    icon = icon("copy")
                )
            )
        )
    )
}

#' Defines the server-side logic for managing lines copy subsection
#' @noRd
.known_iss_source_cl_logic <- function(
    input,
    type = c("edit", "add"),
    cldb,
    temp_tbl) {
    # Copy action button ----------------------------------------------------
    flag <- paste0("cl-modal-file-clear-", type)
    modal_cl_input_selectize_id <- ids()$control_lines_db$inputs[[
        paste0("modal_cl_input_selectize_", type)
    ]]
    modal_cl_input_btn_id <- ids()$control_lines_db$inputs[[
        paste0("modal_cl_input_btn_", type)
    ]]
    modal_confirm_copy_id <- ids()$control_lines_db$inputs[[
        paste0("modal_confirm_copy_", type)
    ]]
    observeEvent(input[[modal_cl_input_btn_id]], {
        to_copy <- cldb$get_by_name(input[[modal_cl_input_selectize_id]])
        shinyWidgets::confirmSweetAlert(
            inputId = modal_confirm_copy_id,
            title = "Are you sure?",
            text = "This will overwrite the current content of the table",
            btn_colors = c("#dee2e6", "#d90889")
        )
        observeEvent(input[[modal_confirm_copy_id]], {
            if (isTRUE(input[[modal_confirm_copy_id]])) {
                temp_tbl(
                    to_copy$known_iss
                )
                gargoyle::trigger(flag)
            }
        })
    })
}

#' Generates a list of inputs, based on the column names and types present
#' in the reference table
#' @noRd
.generate_dynamic_inputs <- function(reference, ns, id_prefix) {
    column_types <- purrr::map(reference, class)
    inputs <- purrr::map2(column_types, names(column_types), ~ {
        in_id <- paste0(id_prefix, "_", .y)
        if (.x == "numeric") {
            list(
                input = numericInput(
                    inputId = ns(in_id),
                    label = .y,
                    value = 0
                ),
                id = in_id
            )
        } else {
            list(
                input = textInput(
                    inputId = ns(in_id),
                    label = .y
                ),
                id = in_id
            )
        }
    })
    names(inputs) <- NULL
    ids <- purrr::map(inputs, ~ .x$id) |> purrr::list_c()
    inputs <- purrr::map(inputs, ~ .x$input)
    return(list(inputs = inputs, ids = ids))
}

#' Server-side logic for adding a new row to the table
#' @noRd
.known_iss_add_row_logic <- function(
    input, output, session,
    type = c("edit", "add"),
    temp_tbl) {
    modal_add_row_ui_id <- ids()$control_lines_db$outputs[[
        paste0("modal_add_row_ui_", type)
    ]]
    modal_add_row_btn_id <- ids()$control_lines_db$inputs[[
        paste0("modal_add_row_btn_", type)
    ]]
    add_row_inputs <- reactiveVal(NULL)
    add_row_inputs_ids <- reactiveVal(NULL)
    flag <- paste0("cl-modal-file-clear-", type)
    ## Populate UI with appropriate inputs -------------------------------------
    observeEvent(temp_tbl(), {
        req(temp_tbl())
        gen_in <- .generate_dynamic_inputs(
            reference = temp_tbl(),
            ns = session$ns,
            id_prefix = "modal_add_row_input"
        )
        add_row_inputs_ids(gen_in$ids)
        add_row_inputs(
            div(
                gen_in$inputs
            )
        )
    })
    output[[modal_add_row_ui_id]] <- renderUI({
        req(add_row_inputs())
        add_row_inputs()
    })
    ## Add a row to the table when button is clicked ---------------------------
    observeEvent(input[[modal_add_row_btn_id]], {
        req(add_row_inputs_ids())
        ### All fields should be valid
        all_valid <- purrr::map_lgl(
            add_row_inputs_ids(),
            ~ isTruthy(input[[.x]])
        )
        if (all(all_valid)) {
            ### Create row
            new_row <- purrr::map(colnames(temp_tbl()), ~ {
                input[[paste0("modal_add_row_input_", .x)]]
            }) |>
                purrr::set_names(colnames(temp_tbl())) |>
                tibble::as_tibble_row()
            ### Add row to table
            temp_tbl(
                temp_tbl() |>
                    dplyr::add_row(new_row)
            )
            ### Reset inputs
            purrr::walk(add_row_inputs_ids(), ~ {
                shinyjs::reset(id = .x)
            })
            gargoyle::trigger(flag)
        } else {
            shinyWidgets::sendSweetAlert(
                title = "Something went wrong",
                text = "All fields must be filled in",
                type = "error",
                btn_labels = "OK",
                btn_colors = "#dee2e6"
            )
        }
    })
}

#' Server-side logic for editing a selected row in the table
#' @noRd
.known_iss_edit_row_logic <- function(
    input, output, session,
    type = c("edit", "add"),
    temp_tbl) {
    modal_edit_row_ui_id <- ids()$control_lines_db$outputs[[
        paste0("modal_edit_row_ui_", type)
    ]]
    modal_confirm_row_btn_id <- ids()$control_lines_db$inputs[[
        paste0("modal_confirm_row_btn_", type)
    ]]
    modal_delete_row_btn_id <- ids()$control_lines_db$inputs[[
        paste0("modal_delete_row_btn_", type)
    ]]
    edit_row_inputs <- reactiveVal(NULL)
    edit_row_inputs_ids <- reactiveVal(NULL)
    flag <- paste0("cl-modal-file-clear-", type)
    ## Populate UI with appropriate inputs -------------------------------------
    temp_tbl_colnames <- reactiveVal(NULL)
    observeEvent(temp_tbl(), {
        req(temp_tbl())
        temp_tbl_colnames(colnames(temp_tbl()))
    })
    observeEvent(temp_tbl_colnames(), {
        req(temp_tbl_colnames())
        gen_in <- .generate_dynamic_inputs(
            reference = temp_tbl(),
            ns = session$ns,
            id_prefix = "modal_edit_row_input"
        )
        edit_row_inputs_ids(gen_in$ids)
        edit_row_inputs(
            div(
                gen_in$inputs
            )
        )
    })
    output[[modal_edit_row_ui_id]] <- renderUI({
        req(edit_row_inputs())
        edit_row_inputs()
    })
    ### Stores index of selected row
    selected_row <- reactive({
        reactable::getReactableState(
            outputId = ids()$control_lines_db$outputs[[
                paste0("modal_known_iss_tbl_", type)
            ]], "selected"
        )
    })
    observe({
        req(selected_row())
        selected_row_data <- temp_tbl()[selected_row(), ]
        purrr::walk2(selected_row_data, colnames(selected_row_data), ~ {
            id_in <- paste0("modal_edit_row_input_", .y)
            if (is.numeric(.x)) {
                updateNumericInput(
                    inputId = id_in,
                    value = .x
                )
            } else {
                updateTextInput(
                    inputId = id_in,
                    value = .x
                )
            }
        })
    })
    ## Edit row when button is clicked -----------------------------------------
    observeEvent(input[[modal_confirm_row_btn_id]], {
        req(selected_row())
        req(edit_row_inputs_ids())
        ### All fields should be valid
        all_valid <- purrr::map_lgl(
            edit_row_inputs_ids(),
            ~ isTruthy(input[[.x]])
        )
        if (!all(all_valid)) {
            shinyWidgets::sendSweetAlert(
                title = "Something went wrong",
                text = "All fields must be filled in",
                type = "error",
                btn_labels = "OK",
                btn_colors = "#dee2e6"
            )
            return()
        }
        all_equal <- purrr::map2_lgl(
            temp_tbl()[selected_row(), ],
            colnames(temp_tbl()), ~ {
                input_value <- input[[paste0("modal_edit_row_input_", .y)]]
                if (input_value == .x) {
                    TRUE
                } else {
                    FALSE
                }
            }
        )
        if (!all(all_equal)) {
            names(all_equal) <- colnames(temp_tbl())
            tmp_mod <- temp_tbl()
            for (col in colnames(temp_tbl())) {
                if (!all_equal[[col]]) {
                    tmp_mod[selected_row(), col] <- input[[
                        paste0("modal_edit_row_input_", col)
                    ]]
                }
            }
            temp_tbl(tmp_mod)
            gargoyle::trigger(flag)
        }
        reactable::updateReactable(
            outputId = ids()$control_lines_db$outputs[[
                paste0("modal_known_iss_tbl_", type)
            ]],
            selected = NA
        )
        purrr::walk(edit_row_inputs_ids(), ~ {
            shinyjs::reset(id = .x)
        })
    })
    ## Delete row when button is clicked ---------------------------------------
    confirm_delete_id <- ids()$control_lines_db$inputs[[
        paste0("modal_confirm_delete_", type)
    ]]
    observeEvent(input[[modal_delete_row_btn_id]], {
        req(selected_row())
        shinyWidgets::confirmSweetAlert(
            inputId = confirm_delete_id,
            title = "Are you sure?",
            text = "This will delete the selected row",
            btn_colors = c("#dee2e6", "#d90889")
        )
    })
    observeEvent(input[[confirm_delete_id]], {
        if (isTRUE(input[[confirm_delete_id]])) {
            temp_tbl(
                temp_tbl() |>
                    dplyr::filter(dplyr::row_number() != selected_row())
            )
            gargoyle::trigger(flag)
        }
    })
}

#' Defines the server-side logic for managing the known iss subsection of
#' the modal
#' @param temp_tbl A reactive value containing the temporary table that
#' gets shown on the left side. NOTE: it does not modify immediately
#' the underlying cell line! This is delegated to the parent logic
#' @param current_cl the current cell line name as a reactive value or NULL
#' @noRd
.known_iss_edit_panel_logic <- function(
    input, output,
    session, cldb,
    type = c("edit", "add"),
    current_cl,
    temp_tbl) {
    # Manage visibility of sections (add only) --------------------------------
    manual_edit_section_id <- paste0("manual_edit_section_", type)
    observeEvent(temp_tbl(), {
        if (is.null(temp_tbl())) {
            shinyjs::hide(manual_edit_section_id)
        } else {
            shinyjs::show(manual_edit_section_id)
        }
    })
    # Managing the selectize for source ----------------------------------------
    source_selectize_id <- ids()$control_lines_db$inputs[[
        paste0("modal_source_selectize_", type)
    ]]
    source_ui_id <- ids()$control_lines_db$outputs[[
        paste0("modal_source_ui_", type)
    ]]
    ## Define UIs --------------------------------------------------------------
    ## Based on value of selectize, show one or the other - with their own
    ## separate logic
    from_file_ui <- .known_iss_source_file_ui(type, ns = session$ns)
    .known_iss_source_file_logic(input, type, temp_tbl)
    from_cl_ui <- .known_iss_source_cl_ui(type, ns = session$ns)
    .known_iss_source_cl_logic(input, type, cldb, temp_tbl)
    ## Define logic ------------------------------------------------------------
    modal_cl_selectize_id <- ids()$control_lines_db$inputs[[
        paste0("modal_cl_input_selectize_", type)
    ]]
    source_ui <- reactiveVal(NULL)
    output[[source_ui_id]] <- renderUI({
        req(source_ui())
        source_ui()
    })
    observeEvent(input[[source_selectize_id]], {
        if (input[[source_selectize_id]] == "Import from tabular file") {
            source_ui(from_file_ui)
        } else {
            source_ui(from_cl_ui)
            cl_names <- names(cldb$available_lines)
            if (not_null(current_cl)) {
                cl_names <- cl_names[cl_names != current_cl()$name]
            }
            updateSelectizeInput(
                inputId = modal_cl_selectize_id,
                choices = cl_names,
                server = TRUE
            )
        }
    })
    # Managing the add row section ---------------------------------------------
    .known_iss_add_row_logic(input, output, session, type, temp_tbl)
    # Managing edit row section ------------------------------------------------
    .known_iss_edit_row_logic(input, output, session, type, temp_tbl)
    # Managing edit headers ----------------------------------------------------
    .known_iss_edit_headers_logic(
        input, output, session,
        type = type,
        temp_tbl = temp_tbl
    )
}

#' Defines the server-side logic for editing headers
#' @noRd
.known_iss_edit_headers_logic <- function(
    input, output, session,
    type = c("edit", "add"),
    temp_tbl) {
    # Generate UI --------------------------------------------------------------
    modal_edit_headers_ui_id <- ids()$control_lines_db$outputs[[
        paste0("modal_edit_headers_ui_", type)
    ]]
    prefix <- "modal-edit-headers-input"
    ns <- session$ns
    temp_tbl_colnames <- reactiveVal(NULL)
    observeEvent(temp_tbl(), {
        req(temp_tbl())
        temp_tbl_colnames(colnames(temp_tbl()))
    })
    ui_to_show <- reactiveVal(NULL)
    inputs_ids <- reactiveVal(NULL)
    observeEvent(temp_tbl_colnames(), {
        req(temp_tbl_colnames())
        grid_rows <- purrr::map(temp_tbl_colnames(), ~ {
            div(
                class = "row w-100",
                div(
                    class = "col",
                    div(
                        class = "w-100 text-truncate",
                        .x
                    )
                ),
                div(
                    class = "col-1",
                    icon("right-long")
                ),
                div(
                    class = "col",
                    textInput(
                        inputId = ns(paste0(prefix, "_", .x)),
                        label = NULL,
                        value = "",
                        placeholder = "New header name",
                        width = "100%"
                    )
                )
            )
        })
        inputs_ids(paste0(prefix, "_", temp_tbl_colnames()))
        ui_to_show(div(
            class = "container",
            grid_rows
        ))
    })
    output[[modal_edit_headers_ui_id]] <- renderUI({
        req(ui_to_show())
        ui_to_show()
    })
    # Button logic -------------------------------------------------------------
    modal_edit_headers_btn_id <- ids()$control_lines_db$inputs[[
        paste0("modal_edit_headers_btn_", type)
    ]]
    observeEvent(input[[modal_edit_headers_btn_id]], {
        req(inputs_ids())
        # If all inputs are falsy don't do anything
        inputs_status <- purrr::map_lgl(inputs_ids(), ~ {
            isTruthy(input[[.x]])
        })
        if (!any(inputs_status)) {
            return()
        }
        # If at least one truthy rename the corresponding column
        copy_tbl <- temp_tbl()
        for (index in seq_along(inputs_ids())) {
            if (inputs_status[index]) {
                og_col <- colnames(copy_tbl)[index]
                new_col <- input[[inputs_ids()[index]]]
                copy_tbl <- copy_tbl |>
                    dplyr::rename(!!new_col := og_col)
            }
        }
        temp_tbl(copy_tbl)
        shinyWidgets::sendSweetAlert(
            session = session,
            title = "Success!",
            text = "The headers have been updated",
            type = "success",
            btn_labels = "OK",
            btn_colors = "#dee2e6"
        )
    })
}

## Complete modal --------------------------------------------------------------

### Edit version ---------------------------------------------------------------

#' Builds the ui portion of the edit modal
#' @noRd
.build_cl_modal_ui_edit <- function(
    ns,
    modal_name_input_id,
    modal_desc_input_id,
    modal_btn_confirm_id,
    modal_btn_cancel_id) {
    footer <- tagList(
        tags$button(
            id = modal_btn_cancel_id,
            class = "btn btn-gray action-button",
            "Cancel"
        ),
        actionButton(
            inputId = modal_btn_confirm_id,
            label = "Confirm"
        )
    )
    body <- div(
        textInput(
            inputId = modal_name_input_id,
            label = "Name",
            placeholder = "Enter cell line name"
        ),
        textAreaInput(
            inputId = modal_desc_input_id,
            label = "Description (optional)"
        ),
        .known_iss_edit_panel_ui("edit", ns)
    )
    edit_cl_modal <- modalDialog(
        title = "Edit control line",
        footer = footer,
        size = "xl",
        body
    )
    return(edit_cl_modal)
}

#' Defines the server-side logic for the edit modal
#' @noRd
.modal_cl_logic_edit <- function(input, output, session, cldb) {
    # Get the selected cell line to edit
    temp_tbl <- reactiveVal(NULL)
    selected_name <- reactive({
        input[[ids()$control_lines_db$inputs$edit_modal_name]]
    })
    selected_cl <- reactive({
        req(selected_name())
        cl <- cldb$get_by_name(selected_name())
        temp_tbl(cl$known_iss)
        cl
    })
    known_iss_tbl_id <- ids()$control_lines_db$outputs[[
        "modal_known_iss_tbl_edit"
    ]]
    # Render the known integration sites table
    output[[known_iss_tbl_id]] <- reactable::renderReactable({
        req(temp_tbl())
        reactable::reactable(
            data = temp_tbl(),
            class = "known-iss-mini",
            outlined = TRUE,
            bordered = TRUE,
            striped = TRUE,
            selection = "single",
            onClick = "select",
            defaultColDef = reactable::colDef(
                align = "center"
            )
        )
    })
    observe({
        req(selected_cl())
        updateTextInput(
            inputId = ids()$control_lines_db$inputs$modal_name_input_edit,
            value = selected_cl()$name
        )
        updateTextAreaInput(
            inputId = ids()$control_lines_db$inputs$modal_desc_input_edit,
            value = selected_cl()$description
        )
    })
    observeEvent(temp_tbl(), {
        reactable::updateReactable(
            outputId = ids()$control_lines_db$outputs[[
                "modal_known_iss_tbl_edit"
            ]],
            data = temp_tbl()
        )
    })
    # Logic for known iss controls
    .known_iss_edit_panel_logic(
        input, output, session, cldb,
        type = "edit",
        temp_tbl = temp_tbl,
        current_cl = selected_cl
    )
    # Button observers
    show_edit_modal_id <- ids()$control_lines_db$inputs$show_edit_modal
    observeEvent(
        input[[ids()$control_lines_db$inputs$modal_btn_cancel_edit]],
        {
            gargoyle::trigger("cl-modal-file-clear-edit")
            removeModal()
        }
    )
    observeEvent(
        input[[ids()$control_lines_db$inputs$modal_btn_confirm_edit]],
        {
            req(selected_name())
            edit_successful <- cldb$edit_cell_line(
                name = selected_name(),
                new_name = input[[
                    ids()$control_lines_db$inputs$modal_name_input
                ]],
                new_desc = input[[
                    ids()$control_lines_db$inputs$modal_desc_input
                ]],
                new_iss = temp_tbl()
            )
            if (edit_successful) {
                gargoyle::trigger("cl-tbl-refresh")
                gargoyle::trigger("cl-db-change")
                removeModal()
            }
        }
    )
}

### Add version ----------------------------------------------------------------
#' Builds the ui portion of the add cell line modal
#' @noRd
.build_cl_modal_ui_add <- function(
    ns,
    modal_name_input_id,
    modal_desc_input_id,
    modal_btn_confirm_id,
    modal_btn_cancel_id) {
    footer <- tagList(
        tags$button(
            id = modal_btn_cancel_id,
            class = "btn btn-gray action-button",
            "Cancel"
        ),
        actionButton(
            inputId = modal_btn_confirm_id,
            label = "Confirm"
        )
    )
    body <- div(
        textInput(
            inputId = modal_name_input_id,
            label = "Name",
            placeholder = "Enter cell line name"
        ),
        textAreaInput(
            inputId = modal_desc_input_id,
            label = "Description (optional)"
        ),
        .known_iss_edit_panel_ui("add", ns)
    )
    add_cl_modal <- modalDialog(
        title = "New control line",
        footer = footer,
        size = "xl",
        body
    )
    return(add_cl_modal)
}

#' Defines the server-side logic for the add modal
#' @noRd
.modal_cl_logic_add <- function(input, output, session, cldb) {
    temp_tbl <- reactiveVal(NULL)
    known_iss_tbl_id <- ids()$control_lines_db$outputs[[
        "modal_known_iss_tbl_add"
    ]]
    # Render the known integration sites table
    output[[known_iss_tbl_id]] <- reactable::renderReactable({
        req(temp_tbl())
        reactable::reactable(
            data = temp_tbl(),
            class = "known-iss-mini",
            outlined = TRUE,
            bordered = TRUE,
            striped = TRUE,
            selection = "single",
            onClick = "select",
            defaultColDef = reactable::colDef(
                align = "center"
            )
        )
    })
    observeEvent(temp_tbl(), {
        reactable::updateReactable(
            outputId = ids()$control_lines_db$outputs[[
                "modal_known_iss_tbl_add"
            ]],
            data = temp_tbl()
        )
    })
    # Logic for known iss controls
    .known_iss_edit_panel_logic(
        input, output, session, cldb,
        type = "add",
        temp_tbl = temp_tbl,
        current_cl = NULL
    )
    # Button observers
    observeEvent(
        input[[ids()$control_lines_db$inputs$modal_btn_cancel_add]],
        {
            gargoyle::trigger("cl-modal-file-clear-add")
            removeModal()
        }
    )
    modal_name_input_id <- ids()$control_lines_db$inputs$modal_name_input_add
    modal_desc_input_id <- ids()$control_lines_db$inputs$modal_desc_input_add
    observeEvent(
        input[[ids()$control_lines_db$inputs$modal_btn_confirm_add]],
        {
            cell_line_name <- input[[modal_name_input_id]]
            desc <- input[[modal_desc_input_id]]
            if (!isTruthy(cell_line_name)) {
                shinyWidgets::sendSweetAlert(
                    title = "Error",
                    text = "Please enter a name for the cell line",
                    type = "error",
                    btn_colors = "#dee2e6"
                )
                return()
            }
            if (is.null(temp_tbl())) {
                shinyWidgets::sendSweetAlert(
                    title = "Error",
                    text = "Known integration sites table can't be empty",
                    type = "error",
                    btn_colors = "#dee2e6"
                )
                return()
            }
            add_successful <- cldb$add_cell_line(
                name = cell_line_name,
                known_iss = temp_tbl(),
                description = desc,
                editable = TRUE
            )
            if (add_successful) {
                gargoyle::trigger("cl-tbl-refresh")
                gargoyle::trigger("cl-db-change")
                removeModal()
            }
        }
    )
}

### Generic version ------------------------------------------------------------

#' Builds the modal for editing control lines
#' @noRd
.build_cl_modal_ui <- function(type = c("edit", "add"), ns) {
    type <- rlang::arg_match(type)
    modal_name_input_id <- ns(ids()$control_lines_db$inputs[[
        paste0("modal_name_input_", type)
    ]])
    modal_desc_input_id <- ns(ids()$control_lines_db$inputs[[
        paste0("modal_desc_input_", type)
    ]])
    modal_btn_confirm_id <- ns(
        ids()$control_lines_db$inputs[[paste0("modal_btn_confirm_", type)]]
    )
    modal_btn_cancel_id <- ns(
        ids()$control_lines_db$inputs[[paste0("modal_btn_cancel_", type)]]
    )
    dialog <- if (type == "edit") {
        .build_cl_modal_ui_edit(
            ns,
            modal_name_input_id,
            modal_desc_input_id,
            modal_btn_confirm_id,
            modal_btn_cancel_id
        )
    } else {
        .build_cl_modal_ui_add(
            ns,
            modal_name_input_id,
            modal_desc_input_id,
            modal_btn_confirm_id,
            modal_btn_cancel_id
        )
    }
    return(dialog)
}

#' Defines the server-side logic for the modal
#' @noRd
.attach_modal_cl_logic <- function(
    input, output, session, cldb,
    type = c("edit", "add")) {
    type <- rlang::arg_match(type)
    if (type == "edit") {
        .modal_cl_logic_edit(input, output, session, cldb)
    } else {
        .modal_cl_logic_add(input, output, session, cldb)
    }
}

## Delete modal ----------------------------------------------------------------
#' Displays confirmation alert when deleting a cell line and deletes it
#' from the db
#' @noRd
.delete_cl_logic <- function(input, output, session, cldb) {
    show_delete_modal_id <- ids()$control_lines_db$inputs$show_delete_modal
    delete_modal_name_id <- ids()$control_lines_db$inputs$delete_modal_name
    delete_cl_confirm_btn_id <- ids()$control_lines_db$inputs[[
        "delete_cl_confirm_btn"
    ]]
    observeEvent(input[[show_delete_modal_id]], {
        if (isTRUE(input[[show_delete_modal_id]])) {
            cl_name <- input[[delete_modal_name_id]]
            shinyWidgets::confirmSweetAlert(
                session = session,
                inputId = delete_cl_confirm_btn_id,
                type = "warning",
                title = "Are you sure?",
                text = sprintf(
                    "You are about to delete the cell line '%s'",
                    cl_name
                )
            )
        }
    })
    observeEvent(input[[delete_cl_confirm_btn_id]], {
        if (isTRUE(input[[delete_cl_confirm_btn_id]])) {
            cldb$remove_cell_line(input[[delete_modal_name_id]])
            gargoyle::trigger("cl-tbl-refresh")
            gargoyle::trigger("cl-db-change")
            shinyWidgets::sendSweetAlert(
                session = session,
                title = "Success",
                text = "Cell line deleted",
                type = "success"
            )
        }
    })
}
