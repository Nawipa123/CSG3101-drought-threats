# scripts/0_setup_packages.R
# Install and load required packages for the project.

pkgs <- c(
  "data.table","tidyverse","lubridate","ggplot2","plotly",
  "randomForest","caret","forecast","anomalize","igraph","ggraph",
  "shiny","shinydashboard"
)

# Find missing packages
new_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]

# Install if missing
if (length(new_pkgs)) {
  install.packages(new_pkgs, dependencies = TRUE)
}

# Load all
for (p in pkgs) {
  suppressPackageStartupMessages(
    library(p, character.only = TRUE)
  )
}

message("✅ All packages installed and loaded successfully.")

