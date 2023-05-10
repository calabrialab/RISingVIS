# Function to handle errors in case of non-blocking behavior
# desired: value of the desired behavior
# obtained: true value obtained after evaluation
# handler: a function that gets called if desired != obtained
# ...: further args (named) to supply to handler
.error_handler <- function(desired, obtained, handler, ...) {
    if (desired != obtained) {
        args <- rlang::list2(...)
        res <- rlang::exec(handler, !!!args)
        return(invisible(res))
    }
}

# Handlers for control lines management ----------------------------------------

#' Displays appropriate errors when trying to read the user-defined
#' control lines rds fails
#' @noRd
.ud_control_cl_import_handler <- function() {
    if (isRunning()) {
        # If error is thrown from a Shiny session
        shiny::showNotification(
            div(
                strong("Unable to read user-defined control lines"),
                br(),
                "File might be corrupted. ",
                "Re-intialising the db to an empty list."
            ),
            type = "error",
            duration = NULL,
            closeButton = TRUE
        )
    } else {
        warn_msg <- c(
            "Unable to read user-defined control lines",
            i = paste(
                "File might be corrupted.",
                "Re-intialising the db to an empty list."
            )
        )
        rlang::warn(warn_msg, class = "ud-control-fail")
    }
}

#' Displays errors when trying to add a new control line and name is duplicated
#' @noRd
.add_cl_name_error_handler <- function() {
    if (isRunning()) {
        # If error is thrown from a Shiny session
        shinyWidgets::sendSweetAlert(
            title = "Something went wrong",
            type = "error",
            text = paste(
                "The name provided is already used for another",
                "control cell line. Please choose another name"
            ),
            btn_labels = "OK",
            btn_colors = "#dee2e6"
        )
    } else {
        err_msg <- c(
            "Unable to add/edit cell line",
            x = paste(
                "The name provided is already used for another",
                "control cell line. Please choose another name"
            )
        )
        rlang::abort(err_msg, class = "add-cl-name-fail")
    }
}

.add_cl_iss_error_handler <- function(e) {
    if (isRunning()) {
        # If error is thrown from a Shiny session
        shinyWidgets::sendSweetAlert(
            title = "Something went wrong",
            type = "error",
            text = e,
            btn_labels = "OK",
            btn_colors = "#dee2e6"
        )
    } else {
        err_msg <- c(
            "Unable to add cell line",
            x = e$message
        )
        rlang::abort(err_msg, class = "add-cl-iss-fail")
    }
}

.edit_cl_error_handler <- function(e) {
    if (isRunning()) {
        # If error is thrown from a Shiny session
        shinyWidgets::sendSweetAlert(
            title = "Something went wrong",
            type = "error",
            text = e,
            btn_labels = "OK",
            btn_colors = "#dee2e6"
        )
    } else {
        err_msg <- c(
            "Unable to edit cell line",
            x = e$message
        )
        rlang::abort(err_msg, class = "edit-cl-fail")
    }
}
