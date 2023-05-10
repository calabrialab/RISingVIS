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
        label = "Choose folder",
        icon = icon("folder-open"),
        buttonType = "primary",
        title = "Choose folder"
      ),
      div(
        class = "dir_path_display text-muted p-2",
        textOutput(
          outputId = display_id
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
