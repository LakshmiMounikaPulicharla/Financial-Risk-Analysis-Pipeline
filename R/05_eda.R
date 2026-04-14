# ============================================================
# 05_eda.R - Exploratory Data Analysis Visualizations
# ============================================================

if (!exists("df")) {
  R_DIR <- tryCatch(dirname(parent.frame(2)$ofile), error = function(e) {
    file.path(if (basename(getwd()) == "R") dirname(getwd()) else getwd(), "R")
  })
  source(file.path(R_DIR, "04_outliers_kpis.R"))
}

cat("\n============================================================\n")
cat("05 - EXPLORATORY DATA ANALYSIS\n")
cat("============================================================\n\n")

theme_set(theme_minimal(base_size = 14))

cat("--- Loan Approval Rate ---\n")
approval_rate <- mean(df$LoanApproved) * 100
cat(sprintf("Overall Approval Rate: %.2f%%\n", approval_rate))

cat("\n--- Approval Rate by Risk Level ---\n")
risk_approval <- df %>% group_by(RiskLevel) %>%
  summarise(ApprovalRate = mean(LoanApproved) * 100, .groups = "drop")
print(risk_approval)

p1 <- ggplot(risk_approval, aes(x = RiskLevel, y = ApprovalRate, fill = RiskLevel)) +
  geom_col(width = 0.6) + geom_text(aes(label = sprintf("%.1f%%", ApprovalRate)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("High Risk" = "#e74c3c", "Medium Risk" = "#f39c12", "Low Risk" = "#27ae60")) +
  labs(title = "Loan Approval Rate by Risk Level", x = "Risk Level", y = "Approval Rate (%)") +
  theme(legend.position = "none")
ggsave(file.path(PLOTS_DIR, "01_approval_by_risk.png"), p1, width = 8, height = 6)

p2 <- ggplot(df, aes(x = CreditScore)) +
  geom_histogram(bins = 30, fill = "#3498db", color = "white", alpha = 0.8) +
  geom_density(aes(y = after_stat(count)), color = "#e74c3c", linewidth = 1) +
  labs(title = "Credit Score Distribution", x = "Credit Score", y = "Count")
ggsave(file.path(PLOTS_DIR, "02_credit_score_distribution.png"), p2, width = 8, height = 6)

p3 <- ggplot(df, aes(x = factor(LoanApproved), y = AnnualIncome, fill = factor(LoanApproved))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#27ae60"), labels = c("Rejected", "Approved")) +
  labs(title = "Annual Income vs Loan Approval", x = "Loan Approved", y = "Annual Income", fill = "Status")
ggsave(file.path(PLOTS_DIR, "03_income_vs_approval.png"), p3, width = 8, height = 6)

p4 <- ggplot(df, aes(x = factor(LoanApproved), y = DebtToIncomeRatio, fill = factor(LoanApproved))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#27ae60"), labels = c("Rejected", "Approved")) +
  labs(title = "Debt-to-Income Ratio vs Loan Approval", x = "Loan Approved", y = "DTI Ratio", fill = "Status")
ggsave(file.path(PLOTS_DIR, "04_dti_vs_approval.png"), p4, width = 8, height = 6)

p5 <- ggplot(df, aes(x = factor(LoanApproved), y = FinancialHealthScore, fill = factor(LoanApproved))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#27ae60"), labels = c("Rejected", "Approved")) +
  labs(title = "Financial Health Score vs Loan Approval", x = "Loan Approved", y = "Financial Health Score", fill = "Status")
ggsave(file.path(PLOTS_DIR, "05_health_score_vs_approval.png"), p5, width = 8, height = 6)

p6 <- ggplot(df, aes(x = RiskLevel, fill = RiskLevel)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("High Risk" = "#e74c3c", "Medium Risk" = "#f39c12", "Low Risk" = "#27ae60")) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Risk Level Distribution", x = "Risk Level", y = "Count") + theme(legend.position = "none")
ggsave(file.path(PLOTS_DIR, "06_risk_distribution.png"), p6, width = 8, height = 6)

p7 <- ggplot(df, aes(x = ExpenseRatio, fill = factor(LoanApproved))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#27ae60"), labels = c("Rejected", "Approved")) +
  labs(title = "Expense Ratio Distribution by Approval", x = "Expense Ratio", y = "Density", fill = "Status")
ggsave(file.path(PLOTS_DIR, "07_expense_ratio_by_approval.png"), p7, width = 8, height = 6)

p8 <- ggplot(df, aes(x = SavingsRatio, fill = factor(LoanApproved))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#27ae60"), labels = c("Rejected", "Approved")) +
  labs(title = "Savings Ratio Distribution by Approval", x = "Savings Ratio", y = "Density", fill = "Status")
ggsave(file.path(PLOTS_DIR, "08_savings_ratio_by_approval.png"), p8, width = 8, height = 6)

p9 <- ggplot(df, aes(x = RiskLevel, y = LoanBurdenRatio, fill = RiskLevel)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("High Risk" = "#e74c3c", "Medium Risk" = "#f39c12", "Low Risk" = "#27ae60")) +
  labs(title = "Loan Burden Ratio by Risk Level", x = "Risk Level", y = "Loan Burden Ratio") +
  theme(legend.position = "none")
ggsave(file.path(PLOTS_DIR, "09_loan_burden_by_risk.png"), p9, width = 8, height = 6)

cat("Saved 9 plots to:", PLOTS_DIR, "\n")
cat("\nDone: 05_eda.R\n")
