cran_packages <- c(
  "data.table", "DBI", "dbplyr", "dplyr", "DT", "ggplot2", "intervals",
  "plotly", "RMariaDB", "shiny", "shinyalert", "shinyBS",
  "shinycssloaders", "shinycustomloader", "shinydashboard",
  "shinydashboardPlus", "shinyhelper", "shinyjs", "shinythemes",
  "shinyWidgets", "splitstackshape", "stringr", "tibble", "tidyverse",
  "waiter"
)

install.packages(cran_packages, repos = "https://cloud.r-project.org")

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos = "https://cloud.r-project.org")
}
BiocManager::install(c("biomaRt", "JBrowseR"), ask = FALSE, update = FALSE)

enable_haplotypes <- tolower(Sys.getenv("INSTALL_HAPLOTYPES", "false")) %in%
  c("1", "true", "yes", "on")
if (enable_haplotypes) {
  install.packages("rehh", repos = "https://cloud.r-project.org")
}

