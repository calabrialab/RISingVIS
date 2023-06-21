.svg_graph_mini <- function() {
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 72.022 402.5"',
    'preserveAspectRatio="xMinYMin meet" class="svg-content-mini">',
    '<g id="Layer_2" data-name="Layer 2" transform="translate(-237.236)">',
    '<g id="step1" class="mini-node" transform="translate(246.145 2.5)">',
    '<circle id="mini-node-1-main" class="mini-node-main" ',
    'data-name="mini-node-1-main" cx="27.561" cy="27.561" r="27.561" ',
    'transform="translate(0 4.146)"/><path id="mini-node-1-arc" ',
    'data-name="mini-node-1-arc" class="mini-node-arc" d="M374,2.5a31.708,',
    '31.708,0,1,1,0,63.416" transform="translate(-346.458 -2.5)"/>',
    '</g><g id="step2" class="mini-node"',
    'transform="translate(237.236 108.956)">',
    '<circle id="mini-node-2-main" data-name="mini-node-2-main" ',
    'class="mini-node-main" cx="27.561" cy="27.561" r="27.561" ',
    'transform="translate(0 21.094) rotate(-22.5)"/>',
    '<path id="mini-node-2-arc" data-name="mini-node-2-arc" ',
    'class="mini-node-arc"',
    'd="M340.068,229.23a31.708,31.708,0,0,0,0,63.416" ',
    'transform="translate(-303.929 -225.084)"/>',
    '</g><g id="step4" class="mini-node"',
    'transform="translate(237.236 330.479)">',
    '<circle id="mini-node-4-main" data-name="mini-node-4-main" ',
    'class="mini-node-main" cx="27.561" cy="27.561" r="27.561" ',
    'transform="translate(0 21.094) rotate(-22.5)"/>',
    '<path id="mini-node-4-arc" data-name="mini-node-4-arc" ',
    'd="M341.2,683.24a31.708,31.708,0,0,0,0,63.416" ',
    'transform="translate(-304.508 -679.144)" class="mini-node-arc"/>',
    '</g><g id="path1" transform="translate(273.706 73.004)">',
    '<line id="Line_11" data-name="Line 11" y2="33.171" fill="none" ',
    'stroke="#727272" stroke-miterlimit="10" stroke-width="3"/></g>',
    '<g id="step3" class="mini-node" ',
    'transform="translate(246.145 223.938)">',
    '<circle id="mini-node-3-main" ',
    'data-name="mini-node-3-main" cx="27.561" ',
    'cy="27.561" r="27.561" transform="translate(0 4.229)" ',
    'class="mini-node-main"/><path id="mini-node-3-arc" ',
    'data-name="mini-node-3-arc" ',
    'd="M373.1,456.44a31.708,31.708,0,1,1,0,63.416" ',
    'transform="translate(-345.997 -456.44)" class="mini-node-arc"/>',
    '</g><g id="path3" transform="translate(273.706 294.525)">',
    '<line id="Line_13" data-name="Line 13" y2="33.171" fill="none" ',
    'stroke="#727272" stroke-width="3"/></g><g id="path2" ',
    'transform="translate(273.706 183.762)"><line id="Line_14" ',
    'data-name="Line 14" y2="33.171" fill="none" stroke="#727272" ',
    'stroke-width="3"/></g></g></svg>'
  )
}

custom_css <- function() {
  system.file("css", "custom.sass", package = "RISingVIS")
}

#' @importFrom bslib bs_add_variables bs_theme bs_add_rules
#' @importFrom sass sass_file
app_theme <- function() {
  bslib::bs_add_variables(
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
    "white" = "#ffffff",
    "light" = "#e1f2e9",
    "dark" = "#080942",
    "primary" = "#ffc812",
    "secondary" = "#d90889",
    "info" = "#609fe6",
    "success" = "#198754",
    "warning" = "#ffc107",
    "danger" = "#dc3545",
    "alert-bg-scale" = "-80%",
    "alert-border-scale" = "-70%",
    "alert-color-scale" = "40%"
  ) |>
    bslib::bs_add_rules(sass::sass_file(custom_css()))
}

#' Utility for quickly getting colors for sweetalert buttons
#' @noRd
sweetalert_btn_colors <- function() {
  colors <- c(
    "#dee2e6",
    bslib::bs_get_variables(app_theme(), "secondary")
  )
  names(colors) <- NULL
  return(colors)
}

#' Wrapper function for a folder selection input
#' @noRd
.folder_input <- function(
    button_id, display_id,
    label) {
  div(
    div(
      label,
      class = "mb-2"
    ),
    div(
      class = "folder-input-container",
      shinyFiles::shinyDirButton(
        id = button_id,
        class = "dir-choose-btn text-nowrap",
        label = "Choose",
        icon = icon("folder-open"),
        buttonType = "primary",
        title = "Choose folder"
      ),
      tagAppendAttributes(
        textOutput(
          outputId = display_id
        ),
        class = paste(
          "dir-path-display text-muted",
          "ps-3 pe-3 w-100 text-nowrap overflow-scroll"
        )
      )
    )
  )
}

#' Folder selection input handler
#' Changes the text displayed and publishes path on output
#' @noRd
.folder_input_handler <- function(
    dir_btn_id,
    dir_display_id,
    volumes,
    input,
    output,
    flag,
    workflow,
    getter,
    setter,
    getter_args = NULL,
    setter_args = NULL) {
  gargoyle::init(flag)
  passed_args_get <- list()
  if (!is.null(getter_args)) {
    passed_args_get <- append(passed_args_get, getter_args)
  }
  output[[dir_display_id]] <- renderText({
    gargoyle::watch(flag)
    path_displayed <- rlang::exec(workflow()[[getter]], !!!passed_args_get)
    if (is.null(path_displayed)) {
      "No folder selected"
    } else {
      path_displayed
    }
  })
  passed_args_set <- list(flag = flag)
  if (!is.null(setter_args)) {
    passed_args_set <- append(passed_args_set, setter_args)
  }
  observeEvent(input[[dir_btn_id]], {
    if (is.integer(input[[dir_btn_id]])) {
      rlang::exec(workflow()[[setter]], NULL, !!!passed_args_set)
    } else {
      rlang::exec(
        workflow()[[setter]],
        shinyFiles::parseDirPath(volumes, input[[dir_btn_id]]),
        !!!passed_args_set
      )
    }
  })
}

#' Alternative version of handler for the folder input: it uses a reactive
#' value instead of setting the value directly in the workflow. This is useful
#' in those cases where the workflow has to be modified only when the upload or
#' another operation has to be completed first.
#' @param dir_btn_id The id of the button
#' @param dir_display_id The id of the text output to display the path
#' @param volumes The volumes to use
#' @param input The input object
#' @param output The output object
#' @param folder_reactive The reactive value to set
#' @noRd
.folder_input_handler_react <- function(
    dir_btn_id,
    dir_display_id,
    volumes,
    input,
    output,
    folder_reactive) {
  # Path that gets displayed
  output[[dir_display_id]] <- renderText({
    if (is.null(folder_reactive())) {
      "No folder selected"
    } else {
      folder_reactive()
    }
  })
  # Observer for the actual button
  observeEvent(input[[dir_btn_id]], {
    if (is.integer(input[[dir_btn_id]])) {
      folder_reactive(NULL)
    } else {
      folder_reactive(
        shinyFiles::parseDirPath(volumes, input[[dir_btn_id]])
      )
    }
  })
}

#' Alternative to classic fileInput component.
#' Ideal to solve issues with file size limits.
#' @noRd
.file_input <- function(
    button_id, display_id,
    label, multiple = FALSE) {
  div(
    class = "mb-4",
    div(
      label,
      class = "mb-2"
    ),
    div(
      id = paste0(button_id, "-container"),
      class = "folder-input-container",
      shinyFiles::shinyFilesButton(
        id = button_id,
        label = "Browse...",
        title = "Choose file",
        multiple = multiple,
        buttonType = "file-alt",
        class = "text-nowrap"
      ),
      tagAppendAttributes(
        textOutput(
          outputId = display_id
        ),
        class = paste(
          "dir-path-display text-muted",
          "ps-3 pe-3 w-100 text-nowrap overflow-scroll"
        )
      )
    ),
    shinyjs::hidden(
      tagAppendAttributes(
        class = "invalid-feedback",
        textOutput(
          outputId = paste0(button_id, "-message"),
        )
      )
    )
  )
}

.file_input_handler_react <- function(
    file_btn_id,
    file_display_id,
    volumes,
    input,
    output,
    file_reactive) {
  # Path that gets displayed
  output[[file_display_id]] <- renderText({
    if (is.null(file_reactive())) {
      "No file selected"
    } else {
      paste0(file_reactive()$datapath, collapse = ", ")
    }
  })
  # Observer for the actual button
  observeEvent(input[[file_btn_id]], {
    if (is.integer(input[[file_btn_id]])) {
      file_reactive(NULL)
    } else {
      file_reactive(
        shinyFiles::parseFilePaths(volumes, input[[file_btn_id]])
      )
    }
  })
}

#' Custom validator for alternative file input component.
#' @param input The input object
#' @param output The output object
#' @param file_btn_id The id of the button (containing ns)
#' @param file_display_id The id of the text output to display the path
#' (containing ns)
#' @param message The message to display when the input is invalid
#' @return A reactive value that is TRUE when the input is valid, FALSE
#' otherwise
#' @details For changing the displayed style (invalid-valid) based
#' on the status, the function acts on 2 components: the message and the
#' container (outer border). Their ids follow the pattern
#' `<file_btn_id>-message` and `<file_btn_id>-container`.
#' @noRd
.file_input_validator <- function(
    input,
    output,
    file_btn_id,
    file_display_id,
    message = "Required") {
  container_id <- paste0(file_btn_id, "-container")
  message_out_id <- paste0(file_btn_id, "-message")
  file_validator <- reactiveVal(FALSE)
  observeEvent(input[[file_btn_id]], {
    if (is.integer(input[[file_btn_id]])) {
      file_validator(FALSE)
    } else {
      file_validator(TRUE)
    }
  })
  output[[message_out_id]] <- renderText({
    message
  })
  observeEvent(file_validator(), {
    shinyjs::toggleClass(
      id = container_id,
      class = "custom-input-invalid",
      condition = !file_validator()
    )
    shinyjs::toggle(
      id = message_out_id,
      condition = !file_validator()
    )
  })
  return(file_validator)
}

#' Defines inputs filters dropdowns for reactables
#' @noRd
.filter_input <- function(id) {
  function(values, name) {
    tags$select(
      # Set to undefined to clear the filter
      onchange = sprintf(
        paste0(
          "Reactable.setFilter('", id, "', ",
          "'%s', event.target.value || undefined)"
        ),
        name
      ),
      # "All" has an empty value to clear the filter, and is the default option
      tags$option(value = "", "All"),
      lapply(unique(values), tags$option),
      "aria-label" = sprintf("Filter %s", name),
      style = paste(
        "width: 100%; height: 100%;",
        "border: 1px solid rgba(0,0,0,.1); border-radius: 3px;",
        "color: rgb(90, 90, 90); padding-left: 0.4em;"
      )
    )
  }
}

#' Defines strict column filtering for filter dropdowns
#' @noRd
.filter_strictly_equal <- function() {
  reactable::JS(
    "function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
            return row.values[columnId] === filterValue;
        });
    }"
  )
}

#' Utility to quickly create the ui portion of a button that
#' when clicked gets disabled and shows a spinner until
#' the computation is done.
#' @param ns the namespace of the module
#' @param btn_id the id of the button
#' @param position where to place the spinner relative to the button -
#' "before" places the spinner before the button icon and label,
#' "after" places it after
#' @param icon the icon to display in the button (call to shiny::icon())
#' or other ui code
#' @param label the label to display in the button (any ui code or text)
#' @param btn_class classes to assign to the button (single string)
#' @param spinner_class classes to assign to the spinner (single string)
#' @param label_class classes to assign to the label (single string)
#' @return A button tag
#' @noRd
.with_spinner_btn_ui <- function(
    ns,
    btn_id,
    position = c("before", "after"),
    icon = NULL,
    label = NULL,
    btn_class = "btn-secondary",
    spinner_class = "spinner-border spinner-border-sm",
    label_class = "text-truncate") {
  position <- rlang::arg_match(position)
  spinner_id <- ns(paste0(btn_id, "-spinner"))
  content <- if (position == "before") {
    div(
      shinyjs::hidden(
        span(
          id = spinner_id,
          class = spinner_class
        )
      ),
      icon,
      span(
        class = label_class,
        label
      )
    )
  } else {
    div(
      icon,
      span(
        class = label_class,
        label
      ),
      shinyjs::hidden(
        span(
          id = spinner_id,
          class = spinner_class
        )
      )
    )
  }
  tags$button(
    id = ns(btn_id),
    class = paste(
      "shiny-bound-input action-button",
      "btn", btn_class
    ),
    content
  )
}

#' Defines the server-side logic for a spinner button -
#' when clicked the button is disabled and shows the spinner
#' until the computation is done.
#' @param btn_id the id of the button
#' @param expr the expression to evaluate when the button is clicked
#' (can be enclosed in curly brackets)
#' @return the result of the expression
#' @noRd
.with_spinner_btn_logic <- function(btn_id, expr) {
  .activate_spinner(btn_id)
  result <- eval(expr)
  .deactivate_spinner(btn_id)
  return(result)
}

#' Alternative mechanism to activate spinner on load when having
#' to deal with complex reactivity patterns instead of a single
#' expression.
#' NOTE: the button ui must already include the spinner - this is
#' server-side only behavior
#' @param btn_id the id of the button
.activate_spinner <- function(btn_id) {
  spinner_id <- paste0(btn_id, "-spinner")
  shinyjs::disable(
    id = btn_id
  )
  shinyjs::show(
    id = spinner_id
  )
}

.deactivate_spinner <- function(btn_id) {
  spinner_id <- paste0(btn_id, "-spinner")
  shinyjs::hide(
    id = spinner_id
  )
  shinyjs::enable(
    id = btn_id
  )
}
