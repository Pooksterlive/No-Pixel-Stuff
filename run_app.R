#' Run the Hong Kong Pawn Shiny application.
#'
#' @param app_dir Directory containing app.R. Defaults to the current directory.
#' @param host Host interface to bind to. Use "0.0.0.0" for Docker or a server.
#' @param port Port to listen on. Defaults to the PORT environment variable or 3838.
#' @param launch_browser Whether to open the app in a local browser.
#' @param ... Additional arguments passed to shiny::runApp().
#' @return This function runs the app and normally does not return until the app stops.
run_app <- function(
  app_dir = ".",
  host = Sys.getenv("SHINY_HOST", "0.0.0.0"),
  port = as.integer(Sys.getenv("PORT", "3838")),
  launch_browser = interactive(),
  ...
) {
  if (!dir.exists(app_dir)) {
    stop("Application directory does not exist: ", app_dir)
  }

  if (is.na(port) || port < 1L || port > 65535L) {
    stop("port must be an integer between 1 and 65535")
  }

  shiny::runApp(
    appDir = app_dir,
    host = host,
    port = port,
    launch.browser = launch_browser,
    ...
  )
}

# Allow the file to be run directly with:
# Rscript run_app.R
if (sys.nframe() == 0L) {
  run_app()
}
