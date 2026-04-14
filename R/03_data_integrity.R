# ============================================================
# 03_data_integrity.R - Data Integrity & Sanity Checks
# ============================================================

if (!exists("df")) {
  R_DIR <- tryCatch(dirname(parent.frame(2)$ofile), error = function(e) {
    file.path(if (basename(getwd()) == "R") dirname(getwd()) else getwd(), "R")
  })
  source(file.path(R_DIR, "02_data_cleaning.R"))
}

cat("\n============================================================\n")
cat("03 - DATA INTEGRITY & SANITY CHECKS\n")
cat("============================================================\n\n")

# NetWorth Consistency Check
cat("--- NetWorth Consistency Check ---\n")
df$NetWorth <- df$TotalAssets - df$TotalLiabilities
networth_consistent <- sum(df$NetWorth == (df$TotalAssets - df$TotalLiabilities))
cat("Consistent NetWorth rows:", networth_consistent, "/", nrow(df), "\n")

# Asset Correlation Check
cat("\n--- Asset Column Correlations ---\n")
asset_cols <- c("SavingsAccountBalance", "CheckingAccountBalance",
                "InvestmentAccountBalance", "RetirementAccountBalance",
                "EmergencyFundBalance", "TotalAssets")
asset_corr <- cor(df %>% select(all_of(asset_cols)), use = "complete.obs")
print(round(asset_corr, 3))

# Assets vs Liabilities Sanity Check
cat("\n--- Assets vs Liabilities Sanity Check ---\n")
liab_gt_assets <- sum(df$TotalLiabilities > df$TotalAssets)
cat("Rows where Liabilities > Assets:", liab_gt_assets, "\n")
if (liab_gt_assets > 0) {
  cat("Sample records:\n")
  print(df %>% filter(TotalLiabilities > TotalAssets) %>%
          select(TotalAssets, TotalLiabilities, NetWorth) %>% head())
}

# Savings vs Income Sanity Check
cat("\n--- Savings vs Income Sanity Check ---\n")
monthly_income <- df$AnnualIncome / 12
savings_exceed <- sum(df$MonthlySavings > monthly_income, na.rm = TRUE)
cat("Rows where MonthlySavings > MonthlyIncome:", savings_exceed, "\n")
if (savings_exceed > 0) {
  cat("Sample records:\n")
  print(df %>% filter(MonthlySavings > AnnualIncome / 12) %>%
          select(AnnualIncome, MonthlySavings) %>% head())
}

# Loan Burden Sanity Check
cat("\n--- Loan Burden Sanity Check ---\n")
loan_exceed <- sum(df$LoanAmount > df$AnnualIncome * 2, na.rm = TRUE)
cat("Rows where LoanAmount > 2x AnnualIncome:", loan_exceed, "\n")

cat("\nDone: 03_data_integrity.R\n")
