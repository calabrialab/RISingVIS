# Error message displayed for suggestion packages that are not installed
# but required by the called function
.missing_pkg_error <- function(pkg, lib = "CRAN") {
    if (!is.null(names(pkg))) {
        pkgs_str <- paste0('"', names(pkg), '"', collapse = ", ")
        from_cran <- pkg[pkg == "CRAN"]
        from_cran <- if (length(from_cran) > 0) {
            paste0(
                "`install.packages(",
                paste0('"', names(from_cran), '"', collapse = ","),
                ")`"
            )
        } else {
            NULL
        }

        from_bioc <- pkg[pkg == "BIOC"]
        from_bioc <- if (length(from_bioc) > 0) {
            paste0(
                "`BiocManager::install(",
                paste0('"', names(from_bioc), '"', collapse = ","),
                ")`"
            )
        } else {
            NULL
        }
        inst_sugg <- paste0("To install: ", from_cran, ", ", from_bioc)
    } else {
        pkgs_str <- paste0('"', pkg, '"', collapse = ", ")
        inst_sugg <- if (lib == "CRAN") {
            paste0("To install: `install.packages(", pkgs_str, ")`")
        } else if (lib == "BIOC") {
            paste0("To install: `BiocManager::install(", pkgs_str, ")`")
        }
    }

    c("Missing package(s)",
        x = paste0(
            "Package(s) ", pkgs_str,
            " are required for this functionality."
        ),
        i = inst_sugg
    )
}
