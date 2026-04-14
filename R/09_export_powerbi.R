# ============================================================
# 09_export_powerbi.R - Export Cleaned Data for Power BI
# ============================================================

if (!exists("df")) {
  R_DIR <- tryCatch(dirname(parent.frame(2)$ofile), error = function(e) {
    file.path(if (basename(getwd()) == "R") dirname(getwd()) else getwd(), "R")
  })
  source(file.path(R_DIR, "08_ml_tidymodels.R"))
}

cat("\n============================================================\n")
cat("09 - EXPORT DATA FOR POWER BI\n")
cat("============================================================\n\n")

df_export <- df %>% select(-any_of(c("MonthlyIncome", "TotalMonthlyExpenses")))
df_export$RiskLevel <- factor(df_export$RiskLevel, levels = c("Low Risk", "Medium Risk", "High Risk"))
df_export$LoanApprovedLabel <- ifelse(df_export$LoanApproved == 1, "Approved", "Rejected")
cat("Export shape:", nrow(df_export), "x", ncol(df_export), "\n")

write_csv(df_export, file.path(POWERBI_DIR, "cleaned_data_for_powerbi.csv"))
write_csv(df_export, file.path(OUTPUT_DIR, "cleaned_data_for_powerbi.csv"))
cat("Exported: cleaned_data_for_powerbi.csv\n")

risk_summary <- df_export %>% group_by(RiskLevel) %>%
  summarise(Count = n(), ApprovalRate = round(mean(LoanApproved) * 100, 2),
    AvgCreditScore = round(mean(CreditScore), 1), AvgDTI = round(mean(DebtToIncomeRatio), 4),
    AvgFinancialHealth = round(mean(FinancialHealthScore), 4),
    AvgIncome = round(mean(AnnualIncome), 2), AvgLoanAmount = round(mean(LoanAmount), 2), .groups = "drop")
write_csv(risk_summary, file.path(POWERBI_DIR, "risk_level_summary.csv"))

edu_summary <- df_export %>% group_by(EducationLevel) %>%
  summarise(Count = n(), ApprovalRate = round(mean(LoanApproved) * 100, 2),
    AvgCreditScore = round(mean(CreditScore), 1), AvgIncome = round(mean(AnnualIncome), 2), .groups = "drop")
write_csv(edu_summary, file.path(POWERBI_DIR, "education_summary.csv"))

emp_summary <- df_export %>% group_by(EmploymentStatus) %>%
  summarise(Count = n(), ApprovalRate = round(mean(LoanApproved) * 100, 2),
    AvgCreditScore = round(mean(CreditScore), 1), AvgIncome = round(mean(AnnualIncome), 2), .groups = "drop")
write_csv(emp_summary, file.path(POWERBI_DIR, "employment_summary.csv"))

marital_summary <- df_export %>% group_by(MaritalStatus) %>%
  summarise(Count = n(), ApprovalRate = round(mean(LoanApproved) * 100, 2),
    AvgCreditScore = round(mean(CreditScore), 1),
    AvgFinancialHealth = round(mean(FinancialHealthScore), 4), .groups = "drop")
write_csv(marital_summary, file.path(POWERBI_DIR, "marital_status_summary.csv"))

home_summary <- df_export %>% group_by(HomeOwnershipStatus) %>%
  summarise(Count = n(), ApprovalRate = round(mean(LoanApproved) * 100, 2),
    AvgCreditScore = round(mean(CreditScore), 1), AvgLoanAmount = round(mean(LoanAmount), 2), .groups = "drop")
write_csv(home_summary, file.path(POWERBI_DIR, "home_ownership_summary.csv"))

df_export$AgeGroup <- cut(df_export$Age, breaks = c(18, 25, 35, 45, 55, 65, 100),
  labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "65+"), include.lowest = TRUE)
age_summary <- df_export %>% group_by(AgeGroup) %>%
  summarise(Count = n(), ApprovalRate = round(mean(LoanApproved) * 100, 2),
    AvgCreditScore = round(mean(CreditScore), 1), AvgIncome = round(mean(AnnualIncome), 2),
    AvgFinancialHealth = round(mean(FinancialHealthScore), 4), .groups = "drop")
write_csv(age_summary, file.path(POWERBI_DIR, "age_group_summary.csv"))

kpi_summary <- df_export %>%
  summarise(TotalRecords = n(), OverallApprovalRate = round(mean(LoanApproved) * 100, 2),
    AvgCreditScore = round(mean(CreditScore), 1), AvgAnnualIncome = round(mean(AnnualIncome), 2),
    AvgLoanAmount = round(mean(LoanAmount), 2), AvgDTI = round(mean(DebtToIncomeRatio), 4),
    AvgExpenseRatio = round(mean(ExpenseRatio), 4), AvgSavingsRatio = round(mean(SavingsRatio), 4),
    AvgAssetCoverage = round(mean(AssetCoverage, na.rm = TRUE), 2),
    AvgFinancialHealthScore = round(mean(FinancialHealthScore), 4)) %>%
  pivot_longer(everything(), names_to = "KPI", values_to = "Value")
write_csv(kpi_summary, file.path(POWERBI_DIR, "kpi_summary.csv"))

if (file.exists(file.path(MODELS_DIR, "combined_model_results.csv"))) {
  file.copy(file.path(MODELS_DIR, "combined_model_results.csv"),
            file.path(POWERBI_DIR, "model_results.csv"), overwrite = TRUE)
}

imp_export <- imp_df %>% rename(FeatureName = Feature, ImportanceScore = Importance)
write_csv(imp_export, file.path(POWERBI_DIR, "feature_importance.csv"))

cat("\n--- Export Summary ---\n")
cat("All files exported to:", POWERBI_DIR, "\n")
for (f in list.files(POWERBI_DIR)) {
  fsize <- file.size(file.path(POWERBI_DIR, f))
  cat(" ", f, sprintf("(%.1f KB)\n", fsize / 1024))
}

cat("\nDone: 09_export_powerbi.R\n")
cat("Ready for Power BI dashboard!\n")
