#### ---- Internals for reports production ----####

# Name of the folder where templates are stored
.templates_folder <- function() {
    "rmd"
}

# All associated information to each type of report
.templates_info <- function() {
    list(
        standard_report = list(
            template_name = "standard_report.rmd",
            required_pkgs = c("flexdashboard", "reactable", "htmltools"),
            def_filename = "RISingVIS_workflow_report.html"
        )
    )
}

# Gets the default file name for the given report type
.get_default_rep_filename <- function(type) {
    return(.templates_info()[[type]]$def_filename)
}

# Retrieves the template file path
.get_template <- function(type) {
    filename <- .templates_info()[[type]]$template_name
    system.file(.templates_folder(), filename, package = "RISingVIS")
}

# Retrieves all the required packages for the given report type
.get_sugg_packages <- function(type) {
    .templates_info()[[type]]$required_pkgs
}

# Renders the report with the appropriate parameters
.produce_report <- function(report_type, params, path) {
    if (!rlang::is_installed("rmarkdown")) {
        rlang::inform(.missing_pkg_error("rmarkdown"))
        return(NULL)
    }
    pkgs_present <- purrr::map_lgl(
        .get_sugg_packages(report_type),
        ~ rlang::is_installed(.x)
    )
    if (any(pkgs_present == FALSE)) {
        missing_pkgs <- .get_sugg_packages(report_type)[!pkgs_present]
        rlang::inform(.missing_pkg_error(missing_pkgs[1]))
        return(NULL)
    }
    template <- .get_template(report_type)
    path <- .clean_file_path(path, report_type)
    withRestarts(
        {
            rmarkdown::render(
                input = template,
                params = params,
                output_file = path,
                envir = new.env(),
                quiet = TRUE
            )
            rlang::inform(.report_saved_info(path))
        },
        report_fail = function(e) {
            rlang::inform(.report_fail_err(conditionMessage(e)))
        }
    )
}

# Gets a cleaned file path to the report file
.clean_file_path <- function(path, type) {
    if (!fs::is_dir(path)) {
        fs::dir_create(path)
    }
    gen_filename <- .generate_report_filename(type)
    path <- fs::path(path, gen_filename)
    return(path)
}

# Generates a default report filename if one is not provided in input
.generate_report_filename <- function(type) {
    def <- .get_default_rep_filename(type)
    date <- lubridate::today()
    return(paste0(date, "_", def))
}

.report_fail_err <- function(err) {
    c("Failure",
        x = "Report production failed, skipping",
        i = paste("Error: ", err)
    )
}

.report_saved_info <- function(file) {
    c("Report correctly saved",
        i = paste("Report saved to:", file)
    )
}
