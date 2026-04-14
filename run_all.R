# ============================================================
# run_all.R - Main Runner Script
# ============================================================
# Execute this script to run the entire pipeline:
# Setup -> Loading -> Cleaning -> Integrity -> KPIs ->
# EDA -> Correlation -> ML (Caret) -> ML (Tidymodels) -> Export
# ============================================================

cat("\n")
cat("############################################################\n")
cat("#  Financial Risk Analysis - R Pipeline                    #\n")
cat("#  Converted from Python (credit.ipynb)                    #\n")
cat("############################################################\n\n")

start_time <- Sys.time()

# Set working directory to project root
args <- commandArgs(trailingOnly = FALSE)
script_dir <- if (length(args) > 0) dirname(sub("--file=", "", args[grep("--file=", args)])) else getwd()
if (script_dir == "" || is.na(script_dir)) script_dir <- getwd()
project_root <- if (basename(script_dir) == "R") dirname(script_dir) else script_dir
setwd(project_root)

cat("Working directory:", getwd(), "\n\n")

# Source scripts in order (each sources the previous)
source("R/00_setup.R")
source("R/01_data_loading.R")
source("R/02_data_cleaning.R")
source("R/03_data_integrity.R")
source("R/04_outliers_kpis.R")
source("R/05_eda.R")
source("R/06_correlation.R")
source("R/07_ml_caret.R")
source("R/08_ml_tidymodels.R")
source("R/09_export_powerbi.R")
source("R/10_ml_advanced.R")

end_time <- Sys.time()

cat("\n")
cat("############################################################\n")
cat("#  Pipeline Complete!                                      #\n")
cat("#  Total time:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")
cat("#                                                          #\n")
cat("#  Outputs:                                                #\n")
cat("#  - Plots:     output/plots/                             #\n")
cat("#  - Models:    output/model_results/                     #\n")
cat("#  - Power BI:  powerbi/                                  #\n")
cat("############################################################\n")
