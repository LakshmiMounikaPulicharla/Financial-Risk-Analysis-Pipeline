# ============================================================
# 06_correlation.R - Correlation Analysis
# ============================================================

if (!exists("df")) {
  R_DIR <- tryCatch(dirname(parent.frame(2)$ofile), error = function(e) {
    file.path(if (basename(getwd()) == "R") dirname(getwd()) else getwd(), "R")
  })
  source(file.path(R_DIR, "05_eda.R"))
}

cat("\n============================================================\n")
cat("06 - CORRELATION ANALYSIS\n")
cat("============================================================\n\n")

cat("--- Correlation Matrix ---\n")
numeric_df <- df %>% select(where(is.numeric))
corr_matrix <- cor(numeric_df, use = "complete.obs")

png(file.path(PLOTS_DIR, "10_correlation_heatmap.png"), width = 1200, height = 900, res = 120)
corrplot(corr_matrix, method = "color", type = "lower",
         tl.cex = 0.6, tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#2980b9", "white", "#c0392b"))(200),
         title = "Correlation Matrix", mar = c(0, 0, 2, 0))
dev.off()
cat("Saved: 10_correlation_heatmap.png\n")

cat("\n--- Correlations with LoanApproved ---\n")
target_corr <- corr_matrix[, "LoanApproved"] %>% sort(decreasing = TRUE)
print(target_corr)

cat("\nTop 10 Positive Correlations:\n"); print(head(target_corr, 10))
cat("\nTop 10 Negative Correlations:\n"); print(tail(target_corr, 10))

corr_df <- data.frame(Feature = names(target_corr), Correlation = target_corr) %>%
  filter(Feature != "LoanApproved") %>%
  mutate(Direction = ifelse(Correlation >= 0, "Positive", "Negative"))
p10 <- ggplot(corr_df, aes(x = reorder(Feature, Correlation), y = Correlation, fill = Direction)) +
  geom_col(alpha = 0.8) + coord_flip() +
  scale_fill_manual(values = c("Positive" = "#27ae60", "Negative" = "#e74c3c")) +
  labs(title = "Feature Correlations with Loan Approval", x = "Feature", y = "Correlation") +
  theme(legend.position = "bottom")
ggsave(file.path(PLOTS_DIR, "11_target_correlations.png"), p10, width = 10, height = 10)
cat("Saved: 11_target_correlations.png\n")

cat("\n--- Mean Values by Loan Approval ---\n")
group_stats <- df %>% group_by(LoanApproved) %>%
  summarise(mean_CreditScore = mean(CreditScore), mean_DTI = mean(DebtToIncomeRatio),
    mean_ExpenseRatio = mean(ExpenseRatio), mean_LoanBurdenRatio = mean(LoanBurdenRatio),
    mean_FinancialHealthScore = mean(FinancialHealthScore), .groups = "drop")
print(group_stats)

cat("\n--- Target Variable Balance ---\n")
cat("Counts:\n"); print(table(df$LoanApproved))
cat("Proportions:\n"); print(prop.table(table(df$LoanApproved)))

cat("\n--- Final Integrity Check ---\n")
cat("Remaining Missing Values:", sum(is.na(df)), "\n")
cat("Duplicate Rows:", sum(duplicated(df)), "\n")

cat("\nDone: 06_correlation.R\n")
