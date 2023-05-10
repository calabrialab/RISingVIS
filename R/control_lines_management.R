###############################################################################
# Control lines management - classes and functions for control lines          #
###############################################################################

#' R6 class representing a control cell line
#'
#' @description Single cell line characterized by a unique name,
#' a data frame of known integration sites and optionally a description.
#' @export
ControlLine <- R6::R6Class(
    classname = "ControlLine",
    lock_class = TRUE,
    private = list(
        .editable = NULL,
        .name = NULL,
        .known_iss = NULL,
        .description = NULL,
        .errors = list(
            name = c(
                "Control line name must be a character"
            ),
            known_iss = list(
                not_df = c("Known ISs must be supplied as a data frame"),
                empty_df = c("Known ISs data frame is empty")
            ),
            not_edit_error = c(
                "This control line is not editable."
            )
        )
    ),
    active = list(
        #' @field name Name of the control line. This is a readonly field,
        #' to rename the control line use the method `rename`.
        name = function(value) {
            if (missing(value)) {
                return(private$.name)
            } else {
                readonly_msg <- c(
                    "`name` is a read-only property",
                    i = paste(
                        "To rename the control line use",
                        "the method `rename`"
                    )
                )
                rlang::inform(readonly_msg)
            }
        },
        #' @field known_iss Data frame containing the known integration sites.
        #' This is a readonly field, to edit the known integration sites use
        #' the method `edit_known_iss`.
        known_iss = function(value) {
            if (missing(value)) {
                return(private$.known_iss)
            } else {
                readonly_msg <- c(
                    "`known_iss` is a read-only property",
                    i = paste(
                        "To edit the known integration sites use",
                        "the method `edit_known_iss`"
                    )
                )
                rlang::inform(readonly_msg)
            }
        },
        #' @field description Description of the control line.
        description = function(value) {
            if (missing(value)) {
                return(private$.description)
            } else {
                private$.description <- value
                return(invisible(TRUE))
            }
        }
    ),
    public = list(
        #' Instantiates a new ControlLine object
        #' @param name A character string, name of the control line
        #' @param known_iss A data frame containing the known integration sites.
        #' Ensure this data frame does not contain duplicated rows.
        #' Please note that `known_iss` cannot be empty.
        #' @param description An optional character string, description of the
        #' control line.
        #' @param editable A logical, whether the control line is
        #' editable or not.
        #' @return A ControlLine object
        initialize = function(name, known_iss, description = NULL,
                              editable = TRUE) {
            if (!is.character(name)) {
                rlang::abort(private$.errors$name)
            }
            if (!is.data.frame(known_iss)) {
                rlang::abort(private$.errors$known_iss$not_df)
            }
            if (nrow(known_iss) == 0) {
                rlang::abort(private$.errors$known_iss$empty_df)
            }
            private$.name <- name
            private$.known_iss <- known_iss
            private$.editable <- editable
            private$.description <- description
        },
        #' Prints the control line information
        #' @return Nothing
        print = function() {
            known_iss_print <- paste0(
                "\t",
                capture.output(private$.known_iss),
                collapse = "\n"
            )
            cat(
                "Control line name: ", private$.name, "\n",
                "Known integration sites: ", "\n",
                known_iss_print, "\n",
                "Editable: ", private$.editable, "\n",
                "Description: ", private$.description, "\n"
            )
        },
        #' Returns whether the control line is editable or not
        #' @return A logical
        #' @examples
        #' cl <- ControlLine$new("name",
        #' tibble::tibble(chr = c("1", "2"),
        #'                integration_locus = c(102342, 4532231),
        #'                strand = c("+", "-")))
        #' cl
        is_editable = function() {
            return(private$.editable)
        },
        #' Renames the control line (if it is editable)
        #' @param new_name A character string, new name of the control line
        #' @return Nothing
        #' @examples
        #' cl <- ControlLine$new("name",
        #' tibble::tibble(chr = c("1", "2"),
        #'                integration_locus = c(102342, 4532231),
        #'                strand = c("+", "-")))
        #' cl$rename(new_name = "new_name")
        #' cl
        rename = function(new_name) {
            if (!self$is_editable()) {
                rlang::abort(private$.errors$not_edit_error)
            }
            if (!is.character(new_name)) {
                rlang::abort(private$.errors$name)
            }
            private$.name <- new_name
        },
        #' Edits the known integration sites (if the control line is editable)
        #' @param new_known_iss A data frame containing the new known
        #' integration sites. Ensure this data frame does not contain
        #' duplicated rows.
        #' Please note that `new_known_iss` cannot be empty.
        #' @return Nothing
        #' @examples
        #' cl <- ControlLine$new("name",
        #' tibble::tibble(chr = c("1", "2"),
        #'               integration_locus = c(102342, 4532231),
        #'              strand = c("+", "-")))
        #' cl$edit_known_iss(
        #' tibble::tibble(chr = c("3", "4"),
        #'                integration_locus = c(103536, 45353),
        #'                strand = c("+", "-")))
        #' cl
        edit_known_iss = function(new_known_iss) {
            if (!self$is_editable()) {
                rlang::abort(private$.errors$not_edit_error)
            }
            if (!is.data.frame(new_known_iss)) {
                rlang::abort(private$.errors$known_iss$not_df)
            }
            if (nrow(new_known_iss) == 0) {
                rlang::abort(private$.errors$known_iss$empty_df)
            }
            private$.known_iss <- new_known_iss
        }
    )
)

#' R6 class representing a control cell line database
#'
#' @description The class acts as a database of all available control cell lines
#' @export
ControlLinesDb <- R6::R6Class(
    "ControlLinesDb",
    private = list(
        .db_name = "user_lines.rds",
        .defaults = list(
            CEM37 = ControlLine$new(
                name = "CEM37",
                known_iss = tibble::tribble(
                    ~chr, ~integration_locus, ~strand,
                    "8", 8866486, "+",
                    "11", 64537168, "-",
                    "17", 47732339, "-",
                    "2", 73762398, "-",
                    "2", 24546571, "-",
                    "17", 2032352, "-",
                    "16", 28497498, "-",
                ),
                editable = FALSE
            )
        ),
        .user_defined = NULL
    ),
    active = list(
        #' @field default_lines A list of ControlLine objects
        #' provided as defaults. This property is read-only.
        default_lines = function(value) {
            if (missing(value)) {
                return(private$.defaults)
            } else {
                readonly_msg <- c(
                    "`default_lines` is a read-only property"
                )
                rlang::inform(readonly_msg)
            }
        },
        #' @field user_defined_lines A list of ControlLine objects
        #' defined by users. This list is empty by default.
        #' This property is read-only.
        user_defined_lines = function(value) {
            if (missing(value)) {
                return(private$.user_defined)
            } else {
                readonly_msg <- c(
                    "`user_defined_lines` is a read-only property",
                    i = paste(
                        "To add a new custom line use",
                        "the method `add_cell_line`"
                    )
                )
                rlang::inform(readonly_msg)
            }
        },
        #' @field available_lines A list of ControlLine objects containing
        #' all available control lines (default and user-defined).
        #' This property is read-only.
        available_lines = function(value) {
            if (missing(value)) {
                return(
                    append(private$.defaults, private$.user_defined)
                )
            } else {
                readonly_msg <- c(
                    "`available_lines` is a read-only property",
                    i = paste(
                        "To add a new custom line use",
                        "the method `add_cell_line`"
                    )
                )
                rlang::inform(readonly_msg)
            }
        }
    ),
    public = list(
        #' Instantiates a new ControlLinesDb object
        #' @description Upon initialization, the database tries to
        #' load user-defined cell lines from the file system.
        #' If the file does not exist, the database is initialized
        #' with an empty list of user-defined cell lines.
        #' @return A ControlLinesDb object
        initialize = function() {
            # Check if user-defined control lines exist ------------------
            user_cl <- system.file(
                private$.db_name,
                package = "RISingVIS"
            )
            if (fs::file_exists(user_cl)) {
                read_values <- purrr::safely(readRDS)(user_cl)
                if (not_null(read_values$error)) {
                    .error_handler(
                        desired = NULL,
                        obtained = read_values$error,
                        handler = .ud_control_cl_import_handler
                    )
                    # Re-init empty
                    private$.user_defined <- list()
                } else {
                    private$.user_defined <- read_values$result
                }
            } else {
                private$.user_defined <- list()
            }
        },
        #' Retrieve a control cell line by name
        #' @description This method returns a ControlLine object provided the
        #' name exists in the database.
        #' @param name The name of the control cell line to retrieve
        #' @return A ControlLine object
        get_by_name = function(name) {
            if (name %in% names(self$available_lines)) {
                return(self$available_lines[[name]])
            } else {
                info_missing <- c(
                    paste("No control cell line found with name", name)
                )
                rlang::inform(info_missing)
            }
        },
        #' Add a new control cell line to the database
        #' @description This method adds a new control cell line to the
        #' database. The name of the new control cell line must be unique.
        #' @param name The name of the new control cell line. Must be unique.
        #' @param known_iss A data frame containing the known integration
        #' sites of the new control cell line. The data frame must not be empty.
        #' @param description A description of the new control cell line
        #' (optional)
        #' @param editable A boolean indicating whether the new control
        #' cell line can be edited or not (optional, default is TRUE)
        #' @return Invisibly returns TRUE if the add operation was successful,
        #' FALSE otherwise.
        add_cell_line = function(name, known_iss,
                                 description = NULL,
                                 editable = TRUE) {
            # Check if name is already used ------------------------------
            if (!is.character(name)) {
                rlang::abort(
                    "Control line name must be a character"
                )
            }
            if (name %in% names(self$available_lines)) {
                .error_handler(
                    desired = FALSE,
                    obtained = name %in% names(self$available_lines),
                    handler = .add_cl_name_error_handler
                )
                return(invisible(FALSE))
            }
            # Try to create the new control line -------------------------
            tryCatch(
                {
                    new_cl <- ControlLine$new(
                        name = name,
                        known_iss = known_iss,
                        description = description,
                        editable = editable
                    )
                    private$.user_defined[[name]] <- new_cl
                    return(invisible(TRUE))
                },
                error = function(e) {
                    .add_cl_iss_error_handler(e)
                    return(invisible(FALSE))
                }
            )
        },
        #' Remove a control cell line from the database
        #' @description This method removes a control cell line from the
        #' database. The name of the control cell line must exist in the
        #' database (default cell lines can't be deleted).
        #' @param name The name of the control cell line to remove
        #' @return Invisibly returns TRUE if the remove operation was
        #' successful,
        #' FALSE otherwise.
        remove_cell_line = function(name) {
            was_present <- not_null(private$.user_defined[[name]])
            private$.user_defined[[name]] <- NULL
            if (was_present) {
                return(invisible(TRUE))
            }
            return(invisible(FALSE))
        },
        #' Edit a control cell line
        #' @description This method edits a control cell line. The name of the
        #' control cell line must exist in the database.
        #' @param name The name of the control cell line to edit
        #' @param new_name The new name of the control cell line (optional)
        #' @param new_iss A data frame containing the new known integration
        #' sites of the control cell line (optional)
        #' @param new_desc A new description of the control cell line (optional)
        #' @return Invisibly returns TRUE if the edit operation was successful,
        #' FALSE otherwise.
        edit_cell_line = function(name,
                                  new_name = NULL,
                                  new_iss = NULL,
                                  new_desc = NULL) {
            to_mod <- self$available_lines[[name]]
            if (is.null(to_mod)) {
                not_present_err <- c(
                    "The control line you are trying to edit does not exist",
                    i = paste("Did you spell the name correctly?")
                )
                rlang::abort(not_present_err, class = "cl_edit_not_present")
                return(invisible(FALSE))
            }
            if (not_null(new_name)) {
                if (new_name != name &
                    new_name %in% names(self$available_lines)) {
                    .error_handler(
                        desired = FALSE,
                        obtained = TRUE,
                        handler = .add_cl_name_error_handler
                    )
                    return(invisible(FALSE))
                }
            }
            tryCatch(
                {
                    if (not_null(new_iss)) {
                        to_mod$edit_known_iss(new_iss)
                    }
                    if (not_null(new_desc)) {
                        to_mod$description <- new_desc
                    }
                    if (not_null(new_name)) {
                        to_mod$rename(new_name)
                    }
                    names(private$.user_defined)[which(
                        names(private$.user_defined) == name
                    )] <- new_name
                    return(invisible(TRUE))
                },
                error = function(e) {
                    .edit_cl_error_handler(e)
                    return(invisible(FALSE))
                }
            )
        },
        #' Save the database to disk
        #' @description This method saves the database to disk.
        persist = function() {
            # TODO
        }
    )
)
