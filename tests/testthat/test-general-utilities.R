# General utilities test -------------------------------------------------------


## .get_control_lines ----
test_that(".get_control_lines works with character - ignores not present", {
  control_lines <- list("CEM37")
  result <- .get_control_lines(control_lines, TRUE)
  expect_true(is.list(result) & all(names(result) == c("result", "error")))
  expect_true("ControlLine" %in% class(result$result$CEM37))

  control_lines <- list("CEM37", "control1", "control2")
  result <- .get_control_lines(control_lines, TRUE)
  expect_true(is.list(result) & all(names(result) == c("result", "error")))
  expect_true("ControlLine" %in% class(result$result$CEM37))
  expect_true(all(names(result$result) == "CEM37"))
})

test_that(".get_control_lines works with obj", {
  control_lines <- default_cell_lines()
  result <- .get_control_lines(control_lines, TRUE)
  expect_true(is.list(result) & all(names(result) == c("result", "error")))
  expect_true("ControlLine" %in% class(result$result$CEM37))
})

test_that(".get_control_lines works with mixed", {
  control_lines <- list("CEM37", Control2)
  result <- .get_control_lines(control_lines, TRUE)
  expect_true(is.list(result) & all(names(result) == c("result", "error")))
  expect_true(all(names(result$result) == c("CEM37", "Control2")))
})

test_that(".get_control_lines removes duplicates", {
  control_lines <- list("CEM37", Control2, default_cell_lines()$CEM37,
                        ControlLine$new(
                          name = "Control3",
                          known_iss = Control2$known_iss
                        ))
  result <- .get_control_lines(control_lines, TRUE)
  expect_true(is.list(result) & all(names(result) == c("result", "error")))
  expect_true(all(names(result$result) == c("CEM37", "Control2", "Control3")))
  expect_true(length(result$result) == 3)
})

test_that(".get_control_lines errors if no controls", {
  control_lines <- list("control", "control2")
  expect_error({
    result <- .get_control_lines(control_lines, TRUE)
  }, class = "control_lines_absent")
  result <- .get_control_lines(control_lines, FALSE)
  expect_true(is.null(result$result) & result$error == TRUE)
})

