# ============================================================
# 08_ml_tidymodels.R - Machine Learning with TIDYMODELS
# ============================================================

if (!exists("df")) {
  R_DIR <- tryCatch(dirname(parent.frame(2)$ofile), error = function(e) {
    file.path(if (basename(getwd()) == "R") dirname(getwd()) else getwd(), "R")
  })
  source(file.path(R_DIR, "07_ml_caret.R"))
}

cat("\n============================================================\n")
cat("08 - MACHINE LEARNING (TIDYMODELS)\n")
cat("============================================================\n\n")

features <- c("CreditScore", "DebtToIncomeRatio", "ExpenseRatio",
  "LoanBurdenRatio", "FinancialHealthScore", "AnnualIncome",
  "TotalAssets", "TotalLiabilities", "MonthlySavings",
  "InterestRate", "LengthOfCreditHistory", "PaymentHistory",
  "PreviousLoanDefaults", "CreditCardUtilizationRate", "JobTenure")

df_tidymodels <- df %>%
  select(all_of(c(features, "LoanApproved"))) %>%
  mutate(LoanApproved = factor(LoanApproved, levels = c(0, 1), labels = c("Rejected", "Approved")))

# Sample for ML (50K rows for memory efficiency)
set.seed(42)
if (nrow(df_tidymodels) > 50000) {
  cat("Sampling 50,000 rows for ML (from", nrow(df_tidymodels), "total)...\n")
  df_tidymodels <- df_tidymodels %>% slice_sample(n = 50000)
}

set.seed(42)
split <- initial_split(df_tidymodels, prop = 0.8, strata = LoanApproved)
train_tm <- training(split); test_tm <- testing(split)
cat("Training:", nrow(train_tm), " Testing:", nrow(test_tm), "\n")

rec <- recipe(LoanApproved ~ ., data = train_tm) %>%
  step_normalize(all_numeric_predictors()) %>% step_impute_median(all_numeric_predictors())

cat("\n--- Logistic Regression (Tidymodels) ---\n")
log_spec <- logistic_reg() %>% set_engine("glm") %>% set_mode("classification")
log_wf <- workflow() %>% add_recipe(rec) %>% add_model(log_spec)
log_fit <- fit(log_wf, data = train_tm)
log_tm_pred <- predict(log_fit, test_tm, type = "prob") %>%
  bind_cols(predict(log_fit, test_tm)) %>% bind_cols(test_tm %>% select(LoanApproved))
log_tm_accuracy <- log_tm_pred %>% accuracy(truth = LoanApproved, estimate = .pred_class) %>% pull(.estimate)
log_tm_roc <- log_tm_pred %>% roc_auc(truth = LoanApproved, .pred_Approved) %>% pull(.estimate)
cat("Accuracy:", log_tm_accuracy, " ROC-AUC:", log_tm_roc, "\n")

cat("\n--- Random Forest (Tidymodels) ---\n")
rf_spec <- rand_forest(trees = 100, mtry = 5, min_n = 10) %>%
  set_engine("ranger", importance = "impurity") %>% set_mode("classification")
rf_wf <- workflow() %>% add_recipe(rec) %>% add_model(rf_spec)

cat("Fitting Random Forest...\n")
rf_fit <- fit(rf_wf, data = train_tm)
rf_tm_pred <- predict(rf_fit, test_tm, type = "prob") %>%
  bind_cols(predict(rf_fit, test_tm)) %>% bind_cols(test_tm %>% select(LoanApproved))
rf_tm_accuracy <- rf_tm_pred %>% accuracy(truth = LoanApproved, estimate = .pred_class) %>% pull(.estimate)
rf_tm_roc <- rf_tm_pred %>% roc_auc(truth = LoanApproved, .pred_Approved) %>% pull(.estimate)
cat("Accuracy:", rf_tm_accuracy, " ROC-AUC:", rf_tm_roc, "\n")

rf_importance_tm <- extract_fit_parsnip(rf_fit) %>% vip::vi()
p13 <- rf_importance_tm %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#e67e22", alpha = 0.8, width = 0.6) + coord_flip() +
  labs(title = "Feature Importance - Random Forest (Tidymodels)", x = "Feature", y = "Importance")
ggsave(file.path(PLOTS_DIR, "13_feature_importance_tidymodels.png"), p13, width = 8, height = 8)

tidymodels_results <- data.frame(
  Model = c("Logistic Regression", "Random Forest"), Framework = "Tidymodels",
  Accuracy = c(log_tm_accuracy, rf_tm_accuracy), ROC_AUC = c(log_tm_roc, rf_tm_roc))
combined_results <- bind_rows(caret_results, tidymodels_results)
print(combined_results)
write_csv(combined_results, file.path(MODELS_DIR, "combined_model_results.csv"))

# Predictions stored on sampled test set only (for memory efficiency)
df$Pred_LogReg_Tidymodels <- NA
df$Pred_RF_Tidymodels     <- NA
df$Prob_RF_Tidymodels     <- NA

cat("\nDone: 08_ml_tidymodels.R\n")
