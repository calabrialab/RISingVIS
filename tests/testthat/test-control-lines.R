################################################################################
# Tests for control lines
################################################################################

test_known_iss <- tibble::tribble(
    ~chr, ~integration_locus, ~strand,
    "8", 8866486, "+",
    "11", 64537168, "-",
    "17", 47732339, "-",
    "2", 73762398, "-",
    "2", 24546571, "-",
    "17", 2032352, "-",
    "16", 28497498, "-",
)

# ControlLine class ------------------------------------------------------------
test_that("ControlLine gets instantiated correctly", {
    cl <- ControlLine$new(
        name = "test_line",
        known_iss = test_known_iss
    )
    expect_true(is(cl, "ControlLine"))
    expect_equal(cl$name, "test_line")
    expect_equal(cl$known_iss, test_known_iss)
    expect_true(cl$is_editable())
})

test_that("ControlLine throws error if name is not a character", {
    expect_error(
        ControlLine$new(
            name = 1,
            known_iss = test_known_iss
        ),
        regexp = ControlLine$self$private_fields$.errors$name
    )
    cl <- ControlLine$new(
        name = "test_line",
        known_iss = test_known_iss
    )
    expect_error(
        cl$rename(1),
        regexp = ControlLine$self$private_fields$.errors$name
    )
})

test_that("ControlLine throws known iss errors", {
    cl <- ControlLine$new(
        name = "test_line",
        known_iss = test_known_iss
    )
    # Known iss not data frame
    expect_error(
        ControlLine$new(
            name = "test_line",
            known_iss = list("a", "b")
        ),
        regexp = ControlLine$self$private_fields$.errors$known_iss$not_df
    )
    expect_error(
        cl$edit_known_iss(list("a", "b")),
        regexp = ControlLine$self$private_fields$.errors$known_iss$not_df
    )
    # Known iss empty data frame
    expect_error(
        ControlLine$new(
            name = "test_line",
            known_iss = tibble::tribble(
                ~chr, ~integration_locus, ~strand
            )
        ),
        regexp = ControlLine$self$private_fields$.errors$known_iss$empty_df
    )
    expect_error(
        cl$edit_known_iss(tibble::tribble(
            ~chr, ~integration_locus, ~strand
        )),
        regexp = ControlLine$self$private_fields$.errors$known_iss$empty_df
    )
})

test_that("ControlLine throws not editable errors", {
    cl <- ControlLine$new(
        name = "test_line",
        known_iss = test_known_iss,
        editable = FALSE
    )
    expect_error(
        cl$edit_known_iss(test_known_iss),
        regexp = ControlLine$self$private_fields$.errors$not_edit_error
    )
    expect_error(
        cl$rename("new_name"),
        regexp = ControlLine$self$private_fields$.errors$not_edit_error
    )
})

test_that("ControlLine has readonly properties", {
    cl <- ControlLine$new(
        name = "test_line",
        known_iss = test_known_iss
    )
    expect_message(
        cl$name <- "new_name"
    )
    expect_true(
        cl$name == "test_line"
    )
    expect_message(
        cl$known_iss <- test_known_iss
    )
    expect_true(
        identical(
            cl$known_iss,
            test_known_iss
        )
    )
})

test_that("ControlLine description property not readonly", {
    cl <- ControlLine$new(
        name = "test_line",
        known_iss = test_known_iss
    )
    cl$description <- "new_description"
    expect_true(
        cl$description == "new_description"
    )
})

test_that("ControlLine set methods work correctly", {
    # Set new name
    cl <- ControlLine$new(
        name = "test_line",
        known_iss = test_known_iss
    )
    cl$rename("new_name")
    expect_true(
        cl$name == "new_name"
    )
    # Set new known iss
    cl$edit_known_iss(test_known_iss[seq(1, 3), ])
    expect_true(
        identical(
            cl$known_iss,
            test_known_iss[seq(1, 3), ]
        )
    )
})

# ControlLinesDb class ---------------------------------------------------------
test_that("ControlLinesDb gets initialised correctly", {
    cldb <- ControlLinesDb$new()
    expect_true(is(cldb, "ControlLinesDb"))
    expect_true("CEM37" %in% names(cldb$default_lines))
    expect_equal(cldb$user_defined_lines, list())
})

test_that("ControlLinesDb reads user defined lines at init", {
    dummy_user_def <- list(
        line1 = ControlLine$new(
            name = "line1",
            known_iss = test_known_iss
        ),
        line2 = ControlLine$new(
            name = "line2",
            known_iss = test_known_iss
        )
    )
    db_path <- fs::path(system.file(
        package = "RISingVIS"
    ), ControlLinesDb$self$private_fields$.db_name)
    saveRDS(
        dummy_user_def,
        file = db_path
    )
    cldb <- ControlLinesDb$new()
    expect_equal(length(cldb$user_defined_lines), 2)
    expect_true(all(c("line1", "line2") %in% names(cldb$user_defined_lines)))
    expect_true(all(purrr::map_lgl(cldb$user_defined_lines, is, "ControlLine")))

    withr::defer(
        fs::file_delete(db_path)
    )
})

test_that("ControlLinesDb active fields are readonly", {
    cldb <- ControlLinesDb$new()
    expect_message(
        cldb$available_lines <- list()
    )
    expect_true(
        identical(
            cldb$available_lines,
            cldb$default_lines
        )
    )
    expect_message(
        cldb$available_lines <- list(
            line1 = ControlLine$new(
                name = "line1",
                known_iss = test_known_iss
            )
        )
    )
    expect_true(
        identical(
            cldb$available_lines,
            cldb$default_lines
        )
    )
    expect_message(
        cldb$user_defined_lines <- list(
            line1 = ControlLine$new(
                name = "line1",
                known_iss = test_known_iss
            )
        )
    )
    expect_equal(
        cldb$user_defined_lines,
        list()
    )
})

test_that("ControlLinesDb add_cell_line works as expected", {
    cldb <- ControlLinesDb$new()
    # Error if name is not a character
    expect_error(
        cldb$add_cell_line(1, test_known_iss),
        regexp = "Control line name must be a character"
    )
    # Error if name is already in use
    expect_error(
        cldb$add_cell_line("CEM37", test_known_iss),
        class = "add-cl-name-fail"
    )
    # Error if known_iss is not a data frame
    expect_error(
        cldb$add_cell_line("new_line", list("a", "b")),
        class = "add-cl-iss-fail"
    )
    # Error if known_iss is empty
    expect_error(
        cldb$add_cell_line("new_line", tibble::tribble(
            ~chr, ~integration_locus, ~strand
        )),
        class = "add-cl-iss-fail"
    )
    # Works otherwise
    cldb$add_cell_line("new_line", test_known_iss)
    expect_true(
        "new_line" %in% names(cldb$user_defined_lines)
    )
    expect_true(
        all(c("CEM37", "new_line") %in% names(cldb$available_lines))
    )
})

test_that("ControlLinesDb get_by_name works as expected", {
    cldb <- ControlLinesDb$new()
    expect_message({
        cldb$get_by_name("test_line")
    })
    found_cl <- cldb$get_by_name("CEM37")
    expect_true(
        is(found_cl, "ControlLine")
    )
})

test_that("ControlLinesDb remove_cell_line works as expected", {
    cldb <- ControlLinesDb$new()
    cldb$add_cell_line("new_line", test_known_iss)
    removed <- cldb$remove_cell_line("CEM37")
    expect_false(removed)
    removed <- cldb$remove_cell_line("new_line")
    expect_true(removed)
})

test_that("ControlLinesDb edit_cell_line works as expected", {
    cldb <- ControlLinesDb$new()
    cldb$add_cell_line("new_line", test_known_iss)
    # Unknown lines throw error
    expect_error(
        {
            cldb$edit_cell_line("test_line", new_name = "myLine")
        },
        class = "cl_edit_not_present"
    )
    # New name must be unique
    expect_error(
        {
            cldb$edit_cell_line("new_line", new_name = "CEM37")
        },
        class = "add-cl-name-fail"
    )
    # Default lines are not editable
    expect_error({
        cldb$edit_cell_line("CEM37", new_name = "CEM371")
    })
    # Other errors are catched from ControlLine
    expect_error(
        {
            cldb$edit_cell_line("new_line", new_iss = list("a", "b"))
        },
        class = "edit-cl-fail"
    )
    # Works otherwise
    cldb$edit_cell_line("new_line",
        new_name = "myLine", new_desc = "test",
        new_iss = test_known_iss[c(1, 2), ]
    )
    expect_equal(
        cldb$user_defined_lines$myLine$description,
        "test"
    )
    expect_equal(
        cldb$user_defined_lines$myLine$known_iss,
        test_known_iss[c(1, 2), ]
    )
})
