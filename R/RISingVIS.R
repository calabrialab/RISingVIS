

RISingVIS <- function(...) {
  withr::with_options(list(
    shiny.maxRequestSize = Inf,
    ISAnalytics.reports = FALSE,
    ISAnalytics.verbose = FALSE
    ), {
    app <- shinyApp(
      ui = ui,
      server = server,
      ...
    )
    runApp(app)
  })
}
