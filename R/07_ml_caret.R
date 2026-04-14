# ============================================================
# 07_ml_caret.R - Machine Learning with CARET
# ============================================================

if (!exists("df")) {
  R_DIR <- tryCatch(dirname(parent.frame(2)$ofile), error = function(e) {
    file.path(if (basename(getwd()) == "R") dirname(getwd()) else getwd(), "R")
  })
  source(file.path(R_DIR, "06_correlation.R"))
}

cat("\n============================================================\n")
cat("07 - MACHINE LEARNING (CARET)\n")
cat("============================================================\n\n")

features <- c("CreditScore", "DebtToIncomeRatio", "ExpenseRatio",
  "LoanBurdenRatio", "FinancialHealthScore", "AnnualIncome",
  "TotalAssets", "TotalLiabilities", "MonthlySavings",
  "InterestRate", "LengthOfCreditHistory", "PaymentHistory",
  "PreviousLoanDefaults", "CreditCardUtilizationRate", "JobTenure")

df_ml <- df %>% select(all_of(c(features, "LoanApproved")))
df_ml$LoanApproved <- factor(df_ml$LoanApproved, levels = c(0, 1), labels = c("Rejected", "Approved"))

# Sample for ML (50K rows for memory efficiency)
set.seed(42)
if (nrow(df_ml) > 50000) {
  cat("Sampling 50,000 rows for ML (from", nrow(df_ml), "total)...\n")
  df_ml <- df_ml %>% slice_sample(n = 50000)
}
cat("Feature Matrix:", nrow(df_ml), "x", ncol(df_ml) - 1, "\n")

set.seed(42)
train_idx <- createDataPartition(df_ml$LoanApproved, p = 0.8, list = FALSE)
train_data <- df_ml[train_idx, ]; test_data <- df_ml[-train_idx, ]
cat("Training:", nrow(train_data), " Testing:", nrow(test_data), "\n")

preproc <- preProcess(train_data[, features], method = c("center", "scale"))
train_scaled <- predict(preproc, train_data); test_scaled <- predict(preproc, test_data)

cat("\n--- Logistic Regression (Caret) ---\n")
set.seed(42)
log_model_caret <- train(LoanApproved ~ ., data = train_scaled, method = "glm",
  family = "binomial", trControl = trainControl(method = "cv", number = 5, classProbs = TRUE))
log_pred <- predict(log_model_caret, test_scaled)
log_prob <- predict(log_model_caret, test_scaled, type = "prob")
log_roc <- pROC::roc(test_scaled$LoanApproved, log_prob$Approved)
cat("Accuracy:", confusionMatrix(log_pred, test_scaled$LoanApproved)$overall["Accuracy"], "\n")
cat("ROC-AUC:", pROC::auc(log_roc), "\n")

cat("\n--- Random Forest (Caret) ---\n")
set.seed(42)
rf_model_caret <- train(LoanApproved ~ ., data = train_scaled, method = "rf",
  ntree = 100, trControl = trainControl(method = "cv", number = 3, classProbs = TRUE),
  metric = "Accuracy", importance = TRUE)
rf_pred <- predict(rf_model_caret, test_scaled)
rf_prob <- predict(rf_model_caret, test_scaled, type = "prob")
rf_roc <- pROC::roc(test_scaled$LoanApproved, rf_prob$Approved)
cat("Accuracy:", confusionMatrix(rf_pred, test_scaled$LoanApproved)$overall["Accuracy"], "\n")
cat("ROC-AUC:", pROC::auc(rf_roc), "\n")

rf_importance <- varImp(rf_model_caret)
imp_df <- data.frame(
  Feature = rownames(rf_importance$importance),
  Importance = rf_importance$importance[, 1]
) %>% arrange(desc(Importance))
print(imp_df)

p12 <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "#3498db", alpha = 0.8, width = 0.6) + coord_flip() +
  labs(title = "Feature Importance - Random Forest (Caret)", x = "Feature", y = "Importance")
ggsave(file.path(PLOTS_DIR, "12_feature_importance_caret.png"), p12, width = 8, height = 8)

caret_results <- data.frame(
  Model = c("Logistic Regression", "Random Forest"), Framework = "Caret",
  Accuracy = c(confusionMatrix(log_pred, test_scaled$LoanApproved)$overall["Accuracy"],
               confusionMatrix(rf_pred, test_scaled$LoanApproved)$overall["Accuracy"]),
  ROC_AUC = c(pROC::auc(log_roc), pROC::auc(rf_roc)))
print(caret_results)
write_csv(caret_results, file.path(MODELS_DIR, "caret_model_results.csv"))

# Store predictions (on sampled test set for memory efficiency)
df$Pred_LogReg_Caret <- NA
df$Pred_RF_Caret     <- NA
df$Prob_RF_Caret     <- NA

cat("\nDone: 07_ml_caret.R\n")
