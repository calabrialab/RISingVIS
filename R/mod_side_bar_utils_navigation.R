
#' Observer for workflow name
#'
#' @description Observer for workflow name button in side bar - on click
#' displays modal for name change
#'
#' @return Nothing
#'
#' @noRd
.wf_name_btn_observer <- function(workflow, input, ns, wf_name_flag) {
  name_change_modal <- .change_name_modal(input, ns, workflow, wf_name_flag)
  observeEvent({
    input[[ids()$side_bar$inputs$wf_name_txt_btn]]
  }, {
    showModal(name_change_modal)
  })
}

#' Modal for workflow name change
#'
#' @description Creates the modal for the workflow change name with the
#' relative observers and validation
#'
#' @return a modalDialog
#'
#' @noRd
.change_name_modal <- function(input, ns, workflow, wf_name_flag) {
  cancel_btn <- modalButton("Cancel")
  cancel_btn$attribs$class <- "btn btn-gray"
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule(ids()$side_bar$inputs$name_modal_textbox,
              shinyvalidate::sv_required())
  iv$enable()
  modal <- modalDialog(
    title = "Change workflow name",
    textInput(inputId = ns(ids()$side_bar$inputs$name_modal_textbox),
              label = "Workflow name"),
    footer = tagList(
      cancel_btn,
      shinyjs::disabled(actionButton(
        inputId = ns(ids()$side_bar$inputs$name_modal_confirm_btn),
        label = "Confirm"))
    )
  )

  observeEvent(
    input[[ids()$side_bar$inputs$name_modal_textbox]], {
      if (iv$is_valid()) {
        shinyjs::enable(id = ids()$side_bar$inputs$name_modal_confirm_btn)
      } else {
        shinyjs::disable(id = ids()$side_bar$inputs$name_modal_confirm_btn)
      }
    }
  )

  observeEvent(
    input[[ids()$side_bar$inputs$name_modal_confirm_btn]], {
      workflow()$change_name(
        new_name = input[[ids()$side_bar$inputs$name_modal_textbox]],
        flag = wf_name_flag
      )
      removeModal()
    }
  )

  return(modal)
}

.side_nav_status_observer <- function(side_nav) {
  observe({
    req(side_nav())
    gargoyle::watch(side_nav()$status_flag)
    for (step in side_nav()$steps) {
      golem::invoke_js("changeStatus", list(
        nodeId = step$node_id,
        nodeStatus = step$status,
        processed = step$processed
      ))
    }
  })
}

.side_btn_observers <- function(input, side_nav) {
  observe({
    gargoyle::watch(side_nav()$status_flag)
    if (side_nav()$current_active > 1) {
      shinyjs::show(id = ids()$side_bar$inputs$back_btn)
      shinyjs::show(id = ids()$side_bar$inputs$new_btn)
    } else {
      shinyjs::hide(id = ids()$side_bar$inputs$back_btn)
      shinyjs::hide(id = ids()$side_bar$inputs$new_btn)
    }
  })
}

.back_btn_observer <- function(input, side_nav) {
  observeEvent(input[[ids()$side_bar$inputs$back_btn]], {
    side_nav()$go_to_previous()
  })
}
