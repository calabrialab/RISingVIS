#' Workflow observer
#'
#' @description Initializes the appropriate workflow object
#'
#' @return Nothing
#' @importFrom shiny observeEvent showModal
#'
#' @noRd
.create_wf_observer <- function(input, session, wf_object, wf_name_flag) {
  modal <- .create_modal_wf_new(session$ns, input, wf_object,
                                wf_name_flag)
  observeEvent(
    input[[
      ids()$workflow_start$inputs$new_wf_btn
    ]], {
      # Create a modal prompt for name
      showModal(modal)
    }
  )
}

#' Creates modal for new workflow
#'
#' @description Creates modal for new workflow with name input and observer
#' for confirmation btn
#'
#' @return A modalDialog
#' @importFrom shiny modalButton textInput tagList actionButton observeEvent
#' @importFrom shiny removeModal
#' @importFrom gargoyle trigger
#'
#' @noRd
.create_modal_wf_new <- function(ns, inputs, outval, wf_name_flag) {
  cancel_btn <- modalButton("Cancel")
  cancel_btn$attribs$class <- "btn btn-gray"
  modal_in <- modalDialog(
    textInput(inputId = ns(ids()$workflow_start$inputs$wf_name_input),
              label = "Workflow name",
              value = "Workflow-1"),
    title = "Enter a name for the new workflow",
    footer = tagList(
      cancel_btn,
      actionButton(
        inputId = ns(ids()$workflow_start$inputs$wf_create_modal_btn),
        label = "Confirm")
    )
  )

  observeEvent(inputs[[ids()$workflow_start$inputs$wf_create_modal_btn]], {
    textbox_val <- inputs[[ids()$workflow_start$inputs$wf_name_input]]
    name_valid <- not_null(textbox_val) & textbox_val != ""
    if (!name_valid) {
      textbox_val <- "Workflow-1"
    }
    new_wf <- WorkFlow$new(name = textbox_val)
    outval(new_wf)
    gargoyle::trigger(wf_name_flag)
    removeModal()
    .page_scroll_next(ids()$workflow_start$section_id,
                      "pages-container")
  })
  return(modal_in)
}


#' Workflow name renderer
#'
#' @description Instantiates logic for workflow name rendering with flags
#'
#' @return Nothing
#' @importFrom shiny renderText req
#' @importFrom gargoyle watch
#'
#' @noRd
.wf_name_renderer <- function(wf, flag, output) {
  output[[
    ids()$workflow_start$outputs$wf_name_out
  ]] <- renderText({

    gargoyle::watch(flag)
    req(wf())
    wf()$get_name()
  })
}
