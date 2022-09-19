

RISingVIS <- function(...) {
  withr::with_options(list(
    shiny.maxRequestSize = Inf
  ), {
    app <- shinyApp(
      ui = ui,
      server = server,
      ...
    )
    runApp(app)
  })
}
