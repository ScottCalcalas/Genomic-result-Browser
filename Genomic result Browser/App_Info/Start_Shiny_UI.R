# start.R — one-click bootstrapper for XZ DB Browser
# Project layout:
#   ProjectRoot/
#     App_Info/           (app.R, XZ_DB_functions.r, datasets/, IndexedData/, searching_*.csv)
#     Output/             (exports)
#     Start.R             (this file)

# 1) Ensure we run from this script's folder (ProjectRoot)
args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
ix <- grep(file_arg, args, fixed = TRUE)
if (length(ix)) {
  setwd(dirname(normalizePath(sub(file_arg, "", args[ix], fixed = TRUE))))
}

# 2) CRAN mirror (in case none set)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 3) Install missing packages (add httpuv for random free port)
needed <- c(
  "shiny", "bslib", "DT", "readxl", "dplyr", "purrr", "rlang",
  "stringr", "tibble", "ggpubr", "patchwork", "httpuv","tidyverse"
  # "tools" is base, no install needed
)
installed <- rownames(installed.packages())
missing <- setdiff(needed, installed)
if (length(missing)) {
  message("Installing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, Ncpus = max(1, parallel::detectCores() - 1))
}

# 4) Ensure Output/ exists at project root (app also checks, but do it here)
#if (!dir.exists("Output")) dir.create("Output", showWarnings = FALSE)

# 5) Validate App_Info/ and app.R presence
if (!dir.exists("App_Info")) stop("Missing 'App_Info/' directory at project root.")
if (!file.exists(file.path("App_Info", "app.R"))) {
  stop("Missing 'App_Info/app.R'. Place your app file there.")
}

# 6) Launch on a free port and open browser
port <- tryCatch(httpuv::randomPort(), error = function(e) 8888)
url  <- sprintf("http://127.0.0.1:%d/", port)
message(sprintf("Starting app from ./App_Info on %s", url))

shiny::runApp(
  appDir = "App_Info",
  host = "127.0.0.1",
  port = port,
  launch.browser = TRUE
)
