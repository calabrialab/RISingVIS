
#' R6 class representing a control cell line
#'
#' @description Single cell line characterized by a unique name and
#' a data frame of known integration sites
#'
#' @details
#' The known IS table must contain at least mandatory IS vars, as per options
#' set via `ISAnalytics::mandatory_IS_vars()`
#' @param name TODO
#' @param known_iss TODO
#' @param initialize TODO
ControlLine <- R6::R6Class(
  classname = "ControlLine",
  public = list(
    name = NULL,
    known_iss = NULL,
    initialize = function(name, known_iss) {
      errors = list(
        name = c(
          "Control line name must be a character"
        ),
        known_iss = list(
          not_df = c("Known ISs must be supplied as a data frame"),
          bad_str = c("Known ISs data frame has a bad structure",
                      x = paste("The data frame should contain at least",
                                "these columns:",
                                paste0(ISAnalytics::mandatory_IS_vars(),
                                       collapse = ", "))),
          empty_df = c("Known ISs data frame is empty")
        )
      )
      if (!is.character(name)) {
        rlang::abort(errors$name)
      }
      if (!is.data.frame(known_iss)) {
        rlang::abort(errors$known_iss$not_df)
      }
      if (!all(ISAnalytics::mandatory_IS_vars() %in% colnames(known_iss))) {
        rlang::abort(errors$known_iss$bad_str)
      }
      if (nrow(known_iss) == 0) {
        rlang::abort(errors$known_iss$empty_df)
      }
      self$name <- name
      self$known_iss <- known_iss
    }
  )
)
