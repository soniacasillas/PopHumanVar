files <- c("global.R", "ui.R", "server.R", "app.R")

for (file in files) {
  parse(file = file)
  message("Syntax OK: ", file)
}

