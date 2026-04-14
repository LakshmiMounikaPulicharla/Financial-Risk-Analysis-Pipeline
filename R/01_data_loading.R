# ============================================================
# 01_data_loading.R - Data Loading & Initial Inspection
# ============================================================
# Source setup if not already loaded
if (!exists("PROJECT_ROOT")) {
  R_DIR <- tryCatch(dirname(parent.frame(2)$ofile), error = function(e) {
    file.path(if (basename(getwd()) == "R") dirname(getwd()) else getwd(), "R")
  })
  source(file.path(R_DIR, "00_setup.R"))
}
setwd(PROJECT_ROOT)

cat("\n============================================================\n")
cat("01 - DATA LOADING & INITIAL INSPECTION\n")
cat("============================================================\n\n")

# --- Load Data ---
cat("Loading dataset...\n")
df <- read_csv("financial_risk_analysis_large.csv", show_col_types = FALSE)

cat("\n--- First 6 rows ---\n")
print(head(df))

# --- Dimensions ---
cat("\n--- Dimensions ---\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

# --- Structure ---
cat("\n--- Data Types & Structure ---\n")
glimpse(df)

# --- Summary Statistics ---
cat("\n--- Summary Statistics ---\n")
print(summary(df))

# --- Detailed Skim ---
cat("\n--- Detailed Skim ---\n")
skim_output <- skim(df)
print(skim_output)

# --- Save raw data in environment for downstream scripts ---
cat("\nData loaded successfully. Shape:", nrow(df), "x", ncol(df), "\n")
cat("Done: 01_data_loading.R\n")
