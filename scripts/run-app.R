host <- Sys.getenv("PHV_LISTEN_HOST", "127.0.0.1")
port <- as.integer(Sys.getenv("PHV_LISTEN_PORT", "8080"))

shiny::runApp(
  appDir = Sys.getenv("PHV_APP_DIR", "/app"),
  host = host,
  port = port,
  launch.browser = FALSE
)

