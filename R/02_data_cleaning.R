# ============================================================
# 02_data_cleaning.R - Data Cleaning
# ============================================================
# Covers: Categorical check, Missing values, Duplicates,
#         Negative values, Financial range validation
# ============================================================

if (!exists("df")) {
  R_DIR <- tryCatch(dirname(parent.frame(2)$ofile), error = function(e) {
    file.path(if (basename(getwd()) == "R") dirname(getwd()) else getwd(), "R")
  })
  source(file.path(R_DIR, "01_data_loading.R"))
}

cat("\n============================================================\n")
cat("02 - DATA CLEANING\n")
cat("============================================================\n\n")

# Categorical Value Check
cat("--- Categorical Value Check ---\n")
cat_cols <- df %>% select(where(is.character)) %>% names()
for (col in cat_cols) {
  cat(col, ":", paste(unique(df[[col]]), collapse = ", "), "\n")
}

# Missing Value Analysis
cat("\n--- Missing Value Analysis ---\n")
missing_count <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing") %>%
  filter(Missing > 0) %>%
  arrange(desc(Missing))
if (nrow(missing_count) == 0) {
  cat("No missing values found.\n")
} else {
  print(missing_count)
}

# Duplicate Check
cat("\n--- Duplicate Check ---\n")
n_dupes <- sum(duplicated(df))
cat("Duplicate rows:", n_dupes, "\n")
df <- df %>% distinct()
cat("Shape after duplicate removal:", nrow(df), "x", ncol(df), "\n")

# Negative Value Checking
cat("\n--- Negative Value Check (Numeric Columns) ---\n")
numeric_cols <- df %>% select(where(is.numeric)) %>% names()
negative_summary <- list()
for (col in numeric_cols) {
  count_neg <- sum(df[[col]] < 0, na.rm = TRUE)
  if (count_neg > 0) negative_summary[[col]] <- count_neg
}
if (length(negative_summary) == 0) {
  cat("No negative values in numeric columns.\n")
} else {
  cat("Columns with negative values:\n")
  for (col in names(negative_summary)) cat(" ", col, ":", negative_summary[[col]], "\n")
}

# Replace Invalid Negatives with NA, then impute with Median
cat("\n--- Handling Invalid Negatives ---\n")
invalid_negative_cols <- c(
  "AnnualIncome", "LoanAmount", "MonthlyDebtPayments", "MonthlySavings",
  "AnnualBonuses", "AnnualExpenses", "MonthlyHousingCosts",
  "MonthlyTransportationCosts", "MonthlyFoodCosts", "MonthlyHealthcareCosts",
  "MonthlyEntertainmentCosts", "TotalAssets", "TotalLiabilities",
  "MortgageBalance", "AutoLoanBalance", "PersonalLoanBalance",
  "StudentLoanBalance", "EmergencyFundBalance"
)
for (col in invalid_negative_cols) {
  if (col %in% names(df)) {
    neg_idx <- which(df[[col]] < 0)
    if (length(neg_idx) > 0) {
      df[[col]][neg_idx] <- NA
      cat(" ", col, ":", length(neg_idx), "negative values -> NA\n")
    }
  }
}
for (col in invalid_negative_cols) {
  if (col %in% names(df)) {
    median_val <- median(df[[col]], na.rm = TRUE)
    df[[col]] <- replace_na(df[[col]], median_val)
  }
}
cat("\nRemaining negatives in invalid columns:\n")
for (col in invalid_negative_cols) {
  if (col %in% names(df)) cat(" ", col, ":", sum(df[[col]] < 0, na.rm = TRUE), "\n")
}

# Financial Range Validation
cat("\n--- Financial Range Validation ---\n")
cat("Invalid Credit Scores (<300 or >850):", nrow(df %>% filter(CreditScore < 300 | CreditScore > 850)), "\n")
cat("Invalid DTI (<0 or >1):", nrow(df %>% filter(DebtToIncomeRatio < 0 | DebtToIncomeRatio > 1)), "\n")
cat("Invalid Credit Card Utilization (<0 or >1):", nrow(df %>% filter(CreditCardUtilizationRate < 0 | CreditCardUtilizationRate > 1)), "\n")
cat("Invalid Interest Rate (<0.01 or >0.3):", nrow(df %>% filter(InterestRate < 0.01 | InterestRate > 0.3)), "\n")

cat("\nDone: 02_data_cleaning.R\n")
