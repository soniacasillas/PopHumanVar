if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
message("Created manifest.json for Posit Connect Cloud.")

