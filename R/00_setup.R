# ============================================================
# 00_setup.R - Package Installation & Loading
# ============================================================

cat("=== Installing required packages ===\n")

required_packages <- c(
  "tidyverse",
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "corrplot",
  "caret",
  "tidymodels",
  "vip",
  "skimr",
  "scales",
  "patchwork",
  "randomForest",
  "lightgbm",
  "smotefamily",
  "themis",
  "e1071"
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
  }
}

lapply(required_packages, install_if_missing)

# Load packages
cat("\n=== Loading packages ===\n")
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(corrplot)
  library(caret)
  library(tidymodels)
  library(vip)
  library(skimr)
  library(scales)
  library(patchwork)
  library(randomForest)
})

cat("All packages loaded successfully!\n")

# Set project paths - works in both RStudio and command line
PROJECT_ROOT <- tryCatch(
  {
    # In RStudio
    dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  },
  error = function(e) {
    # Command line fallback
    if (exists("project_root") && !is.null(project_root)) {
      return(project_root)
    }
    if (basename(getwd()) == "R") dirname(getwd()) else getwd()
  }
)

DATA_DIR    <- PROJECT_ROOT
OUTPUT_DIR  <- file.path(PROJECT_ROOT, "output")
PLOTS_DIR   <- file.path(OUTPUT_DIR, "plots")
MODELS_DIR  <- file.path(OUTPUT_DIR, "model_results")
POWERBI_DIR <- file.path(PROJECT_ROOT, "powerbi")

cat("Project root:", PROJECT_ROOT, "\n")
cat("Output dir :", OUTPUT_DIR, "\n")
cat("Plots dir  :", PLOTS_DIR, "\n")
cat("Ready!\n")
