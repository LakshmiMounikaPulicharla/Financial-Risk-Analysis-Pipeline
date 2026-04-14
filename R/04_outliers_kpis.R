# ============================================================
# 04_outliers_kpis.R - Outlier Treatment & Financial KPIs
# ============================================================

if (!exists("df")) {
  R_DIR <- tryCatch(dirname(parent.frame(2)$ofile), error = function(e) {
    file.path(if (basename(getwd()) == "R") dirname(getwd()) else getwd(), "R")
  })
  source(file.path(R_DIR, "03_data_integrity.R"))
}

cat("\n============================================================\n")
cat("04 - OUTLIER TREATMENT & FINANCIAL KPIs\n")
cat("============================================================\n\n")

# Outlier Capping (IQR Method)
cat("--- Outlier Capping (IQR Method) ---\n")
outlier_cols <- c("AnnualIncome", "LoanAmount", "TotalAssets",
                  "TotalLiabilities", "MonthlySavings", "AnnualExpenses")

cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  pmin(pmax(x, lower), upper)
}

for (col in outlier_cols) {
  if (col %in% names(df)) df[[col]] <- cap_outliers(df[[col]])
}
cat("Outlier capping completed.\n")

# Financial KPI Feature Engineering
cat("\n--- Financial KPI Feature Engineering ---\n")
df$MonthlyIncome <- df$AnnualIncome / 12
cat("Created: MonthlyIncome\n")

expense_cols <- c("MonthlyHousingCosts", "MonthlyTransportationCosts",
                  "MonthlyFoodCosts", "MonthlyHealthcareCosts",
                  "MonthlyEntertainmentCosts")
df$TotalMonthlyExpenses <- rowSums(df[, expense_cols], na.rm = TRUE)
cat("Created: TotalMonthlyExpenses\n")

df$ExpenseRatio <- df$TotalMonthlyExpenses / df$MonthlyIncome
df$AssetCoverage <- ifelse(df$TotalLiabilities == 0, NA, df$TotalAssets / df$TotalLiabilities)
df$SavingsRatio <- df$MonthlySavings / df$MonthlyIncome
df$LoanBurdenRatio <- df$LoanAmount / df$AnnualIncome
df$NetAssetRatio <- ifelse(df$TotalAssets == 0, NA, df$NetWorth / df$TotalAssets)
cat("Created: ExpenseRatio, AssetCoverage, SavingsRatio, LoanBurdenRatio, NetAssetRatio\n")

df$FinancialHealthScore <- (
  (df$CreditScore / 850) * 0.3 +
  (1 - df$DebtToIncomeRatio) * 0.3 +
  (df$AssetCoverage / (df$AssetCoverage + 1)) * 0.2 +
  (1 - df$ExpenseRatio) * 0.2
)
cat("Created: FinancialHealthScore\n")

# Replace Inf/NaN
df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .)))
df <- df %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))
cat("Inf/NaN values handled.\n")

# Risk Level Segmentation
df <- df %>%
  mutate(RiskLevel = case_when(
    CreditScore < 600 | DebtToIncomeRatio > 0.5 ~ "High Risk",
    CreditScore >= 600 & CreditScore <= 700      ~ "Medium Risk",
    CreditScore > 700 & DebtToIncomeRatio <= 0.5  ~ "Low Risk",
    TRUE                                          ~ "Medium Risk"
  ))
cat("Created: RiskLevel\n")

cat("\n--- KPI Summary Statistics ---\n")
df %>% select(ExpenseRatio, AssetCoverage, SavingsRatio,
              LoanBurdenRatio, FinancialHealthScore) %>% summary()

df <- df %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))

cat("\nDone: 04_outliers_kpis.R\n")
