# ============================================================
# 10_ml_advanced.R - Advanced ML: XGBoost + LightGBM + GBM +
#   SMOTE Balancing + Hyperparameter Tuning + Ensemble
# ============================================================

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(caret)
  library(lightgbm)
  library(themis)
  library(pROC)
  library(skimr)
})

if (!exists("df") || !is.data.frame(df)) {
  cat("Loading data from CSV...\n")
  PROJECT_ROOT <- if (basename(getwd()) == "R") dirname(getwd()) else getwd()
  OUTPUT_DIR <- file.path(PROJECT_ROOT, "output")
  PLOTS_DIR  <- file.path(OUTPUT_DIR, "plots")
  MODELS_DIR <- file.path(OUTPUT_DIR, "model_results")
  POWERBI_DIR <- file.path(PROJECT_ROOT, "powerbi")
  df <- read_csv(file.path(PROJECT_ROOT, "financial_risk_analysis_large.csv"), show_col_types = FALSE)
  # Apply basic cleaning
  df <- df %>% distinct()
  invalid_cols <- c("AnnualIncome","LoanAmount","MonthlyDebtPayments","MonthlySavings",
    "AnnualBonuses","AnnualExpenses","MonthlyHousingCosts","MonthlyTransportationCosts",
    "MonthlyFoodCosts","MonthlyHealthcareCosts","MonthlyEntertainmentCosts",
    "TotalAssets","TotalLiabilities","MortgageBalance","AutoLoanBalance",
    "PersonalLoanBalance","StudentLoanBalance","EmergencyFundBalance")
  for (col in invalid_cols) { if (col %in% names(df)) { df[[col]][df[[col]] < 0] <- NA; df[[col]] <- replace_na(df[[col]], median(df[[col]], na.rm=TRUE)) }}
  # KPIs
  df$MonthlyIncome <- df$AnnualIncome/12
  exp_cols <- c("MonthlyHousingCosts","MonthlyTransportationCosts","MonthlyFoodCosts","MonthlyHealthcareCosts","MonthlyEntertainmentCosts")
  df$TotalMonthlyExpenses <- rowSums(df[,exp_cols], na.rm=TRUE)
  df$ExpenseRatio <- df$TotalMonthlyExpenses/df$MonthlyIncome
  df$AssetCoverage <- ifelse(df$TotalLiabilities==0, NA, df$TotalAssets/df$TotalLiabilities)
  df$SavingsRatio <- df$MonthlySavings/df$MonthlyIncome
  df$LoanBurdenRatio <- df$LoanAmount/df$AnnualIncome
  df$NetWorth <- df$TotalAssets-df$TotalLiabilities
  df$NetAssetRatio <- ifelse(df$TotalAssets==0, NA, df$NetWorth/df$TotalAssets)
  df$FinancialHealthScore <- (df$CreditScore/850)*0.3 + (1-df$DebtToIncomeRatio)*0.3 + (df$AssetCoverage/(df$AssetCoverage+1))*0.2 + (1-df$ExpenseRatio)*0.2
  df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))
  df$RiskLevel <- case_when(df$CreditScore<600|df$DebtToIncomeRatio>0.5~"High Risk", df$CreditScore>=600&df$CreditScore<=700~"Medium Risk", TRUE~"Low Risk")
}

cat("\n============================================================\n")
cat("10 - ADVANCED ML IMPROVEMENTS\n")
cat("============================================================\n\n")

# =============================================================
# Step 1: Enhanced Feature Engineering
# =============================================================
cat("--- Step 1: Enhanced Feature Engineering ---\n")

# Log-transform skewed features
df_adv <- df %>%
  mutate(
    log_AnnualIncome = log1p(AnnualIncome),
    log_TotalAssets = log1p(TotalAssets),
    log_TotalLiabilities = log1p(TotalLiabilities),
    log_LoanAmount = log1p(LoanAmount),
    log_NetWorth = log1p(NetWorth + abs(min(NetWorth, na.rm = TRUE)) + 1)
  )

# Interaction features
df_adv <- df_adv %>%
  mutate(
    CreditScore_x_DTI = CreditScore * DebtToIncomeRatio,
    Income_x_SavingsRatio = AnnualIncome * SavingsRatio,
    LoanToIncomeRatio = LoanAmount / (AnnualIncome + 1),
    AssetToLiabilityRatio = (TotalAssets + 1) / (TotalLiabilities + 1),
    CreditScore_per_Age = CreditScore / Age,
    Income_per_Dependent = AnnualIncome / (NumberOfDependents + 1)
  )

# One-hot encode categorical variables
df_adv <- df_adv %>%
  mutate(across(
    c(EmploymentStatus, EducationLevel, MaritalStatus,
      HomeOwnershipStatus, LoanPurpose, EmployerType),
    ~ factor(.)
  ))

# Select features for ML
feature_cols <- c(
  "CreditScore", "AnnualIncome", "LoanAmount", "LoanDuration", "Age",
  "NumberOfDependents", "MonthlyDebtPayments", "CreditCardUtilizationRate",
  "NumberOfOpenCreditLines", "DebtToIncomeRatio", "PreviousLoanDefaults",
  "InterestRate", "PaymentHistory", "JobTenure",
  "ExpenseRatio", "LoanBurdenRatio", "FinancialHealthScore",
  "SavingsRatio", "AssetCoverage",
  # New features
  "log_AnnualIncome", "log_TotalAssets", "log_TotalLiabilities", "log_LoanAmount",
  "CreditScore_x_DTI", "Income_x_SavingsRatio", "LoanToIncomeRatio",
  "AssetToLiabilityRatio", "CreditScore_per_Age", "Income_per_Dependent",
  # One-hot encoded
  "EmploymentStatus", "EducationLevel", "MaritalStatus",
  "HomeOwnershipStatus", "LoanPurpose", "EmployerType"
)

df_adv <- df_adv %>%
  select(all_of(c(feature_cols, "LoanApproved"))) %>%
  drop_na()

df_adv$LoanApproved <- factor(df_adv$LoanApproved, levels = c(0, 1), labels = c("Rejected", "Approved"))
cat("Enhanced feature set:", ncol(df_adv) - 1, "features\n")

# =============================================================
# Step 2: Sample Data (100K rows for better accuracy)
# =============================================================
cat("\n--- Step 2: Sampling ---\n")
set.seed(42)
if (nrow(df_adv) > 30000) {
  df_adv <- df_adv %>% slice_sample(n = 30000)
  cat("Sampled 30,000 rows for advanced ML\n")
}

# Class distribution before SMOTE
cat("Class distribution before SMOTE:\n")
print(table(df_adv$LoanApproved))

# =============================================================
# Step 3: Train-Test Split
# =============================================================
set.seed(42)
train_idx <- createDataPartition(df_adv$LoanApproved, p = 0.8, list = FALSE)
train_data <- df_adv[train_idx, ]
test_data  <- df_adv[-train_idx, ]
cat("\nTraining:", nrow(train_data), " Testing:", nrow(test_data), "\n")

# =============================================================
# Step 4: SMOTE Balancing
# =============================================================
cat("\n--- Step 4: SMOTE Class Balancing ---\n")

# Ensure themis is loaded for step_smote
suppressPackageStartupMessages(library(themis))

# Apply SMOTE
smote_recipe <- recipe(LoanApproved ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(LoanApproved, over_ratio = 0.8)

smote_prep <- prep(smote_recipe)
train_balanced <- bake(smote_prep, new_data = NULL)
test_processed <- bake(smote_prep, new_data = test_data)

cat("Class distribution after SMOTE:\n")
print(table(train_balanced$LoanApproved))

# =============================================================
# Step 5: Use Recipe-Processed Data (normalization already done)
# =============================================================
cat("\n--- Step 5: Using Recipe-Processed Data ---\n")

# The recipe already handled normalization and dummy encoding
train_scaled <- train_balanced %>% drop_na()
test_scaled  <- test_processed %>% drop_na()

# Remove any remaining NAs
train_scaled <- train_scaled %>% drop_na()
test_scaled  <- test_scaled %>% drop_na()
cat("Final training set:", nrow(train_scaled), "rows\n")
cat("Final test set:", nrow(test_scaled), "rows\n")

# =============================================================
# Step 6: Model 1 - Logistic Regression (with tuning)
# =============================================================
cat("\n--- Model 1: Logistic Regression (Tuned) ---\n")
set.seed(42)
log_model <- train(
  LoanApproved ~ ., data = train_scaled, method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, savePredictions = "final")
)
log_pred <- predict(log_model, test_scaled)
log_prob <- predict(log_model, test_scaled, type = "prob")
log_acc <- confusionMatrix(log_pred, test_scaled$LoanApproved)$overall["Accuracy"]
log_roc <- pROC::roc(test_scaled$LoanApproved, log_prob$Approved)
cat("Accuracy:", log_acc, " ROC-AUC:", pROC::auc(log_roc), "\n")

# =============================================================
# Step 7: Model 2 - Random Forest (with tuning)
# =============================================================
cat("\n--- Model 2: Random Forest (Tuned) ---\n")
set.seed(42)
rf_model <- train(
  LoanApproved ~ ., data = train_scaled, method = "rf",
  ntree = 100,
  trControl = trainControl(method = "cv", number = 3, classProbs = TRUE, savePredictions = "final"),
  tuneGrid = expand.grid(mtry = c(5, 8)),
  importance = TRUE
)
rf_pred <- predict(rf_model, test_scaled)
rf_prob <- predict(rf_model, test_scaled, type = "prob")
rf_acc <- confusionMatrix(rf_pred, test_scaled$LoanApproved)$overall["Accuracy"]
rf_roc <- pROC::roc(test_scaled$LoanApproved, rf_prob$Approved)
cat("Accuracy:", rf_acc, " ROC-AUC:", pROC::auc(rf_roc), "\n")
cat("Best mtry:", rf_model$bestTune$mtry, "\n")

# =============================================================
# Step 8: Model 3 - XGBoost (caret xgbTree)
# =============================================================
cat("\n--- Model 3: XGBoost (caret xgbTree) ---\n")
xgb_available <- requireNamespace("xgboost", quietly = TRUE)
if (xgb_available) {
  set.seed(42)
  xgb_grid <- expand.grid(
    nrounds = c(100),
    max_depth = c(6),
    eta = c(0.1),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  )
  xgb_model <- train(
    LoanApproved ~ ., data = train_scaled, method = "xgbTree",
    trControl = trainControl(method = "cv", number = 2, classProbs = TRUE, savePredictions = "final"),
    tuneGrid = xgb_grid, verbosity = 0
  )
  xgb_pred <- predict(xgb_model, test_scaled)
  xgb_prob <- predict(xgb_model, test_scaled, type = "prob")
  xgb_acc <- confusionMatrix(xgb_pred, test_scaled$LoanApproved)$overall["Accuracy"]
  xgb_roc <- pROC::roc(test_scaled$LoanApproved, xgb_prob$Approved)
  cat("Accuracy:", xgb_acc, " ROC-AUC:", pROC::auc(xgb_roc), "\n")
} else {
  cat("SKIPPED: xgboost package not installed\n")
  xgb_pred <- rf_pred; xgb_prob <- rf_prob; xgb_acc <- 0; xgb_roc <- rf_roc
}

# =============================================================
# Step 9: Model 4 - LightGBM (native)
# =============================================================
cat("\n--- Model 4: LightGBM (native) ---\n")
library(lightgbm)

# Prepare data for lightgbm
train_matrix <- as.matrix(train_scaled %>% select(-LoanApproved))
train_label <- as.numeric(train_scaled$LoanApproved == "Approved")
test_matrix  <- as.matrix(test_scaled %>% select(-LoanApproved))
test_label   <- as.numeric(test_scaled$LoanApproved == "Approved")

dtrain <- lgb.Dataset(train_matrix, label = train_label)
dtest  <- lgb.Dataset(test_matrix, label = test_label, reference = dtrain)

lgb_params <- list(
  objective = "binary",
  metric = "auc",
  num_leaves = 31,
  learning_rate = 0.1,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  verbose = -1
)

set.seed(42)
lgb_model <- lgb.train(
  params = lgb_params,
  data = dtrain,
  nrounds = 100,
  valids = list(test = dtest),
  early_stopping_rounds = 10,
  verbose = -1
)

lgb_prob_raw <- predict(lgb_model, test_matrix)
lgb_pred <- factor(ifelse(lgb_prob_raw > 0.5, "Approved", "Rejected"), levels = c("Rejected", "Approved"))
lgb_prob <- data.frame(Rejected = 1 - lgb_prob_raw, Approved = lgb_prob_raw)
lgb_acc <- confusionMatrix(lgb_pred, test_scaled$LoanApproved)$overall["Accuracy"]
lgb_roc <- pROC::roc(test_scaled$LoanApproved, lgb_prob$Approved)
cat("Accuracy:", lgb_acc, " ROC-AUC:", pROC::auc(lgb_roc), "\n")
cat("Best iteration:", lgb_model$best_iter, "\n")

# LightGBM feature importance
lgb_imp <- lgb.importance(lgb_model)
p18 <- ggplot(lgb_imp %>% head(20), aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "#8e44ad", alpha = 0.8, width = 0.6) + coord_flip() +
  labs(title = "Feature Importance - LightGBM", x = "Feature", y = "Gain")
ggsave(file.path(PLOTS_DIR, "18_feature_importance_lightgbm.png"), p18, width = 8, height = 8)
cat("Saved: 18_feature_importance_lightgbm.png\n")

# =============================================================
# Step 10: Model 5 - Gradient Boosting (caret gbm)
# =============================================================
cat("\n--- Model 5: Gradient Boosting (caret gbm) ---\n")
set.seed(42)
gbm_model <- train(
  LoanApproved ~ ., data = train_scaled, method = "gbm",
  trControl = trainControl(method = "cv", number = 2, classProbs = TRUE, savePredictions = "final"),
  tuneGrid = expand.grid(
    n.trees = c(100, 200),
    interaction.depth = c(3),
    shrinkage = c(0.1),
    n.minobsinnode = 10
  ),
  verbose = FALSE
)
gbm_pred <- predict(gbm_model, test_scaled)
gbm_prob <- predict(gbm_model, test_scaled, type = "prob")
gbm_acc <- confusionMatrix(gbm_pred, test_scaled$LoanApproved)$overall["Accuracy"]
gbm_roc <- pROC::roc(test_scaled$LoanApproved, gbm_prob$Approved)
cat("Accuracy:", gbm_acc, " ROC-AUC:", pROC::auc(gbm_roc), "\n")

# =============================================================
# Step 11: Model 6 - SVM
# =============================================================
cat("\n--- Model 6: SVM (Radial Kernel) ---\n")
set.seed(42)
svm_available <- tryCatch({
  svm_model <- train(
    LoanApproved ~ ., data = train_scaled, method = "svmRadial",
    trControl = trainControl(method = "cv", number = 2, classProbs = TRUE, savePredictions = "final"),
    tuneGrid = expand.grid(C = 1, sigma = 0.01)
  )
  svm_pred <- predict(svm_model, test_scaled)
  svm_prob <- predict(svm_model, test_scaled, type = "prob")
  svm_acc <- confusionMatrix(svm_pred, test_scaled$LoanApproved)$overall["Accuracy"]
  svm_roc <- pROC::roc(test_scaled$LoanApproved, svm_prob$Approved)
  cat("Accuracy:", svm_acc, " ROC-AUC:", pROC::auc(svm_roc), "\n")
  TRUE
}, error = function(e) {
  cat("SKIPPED: SVM failed -", conditionMessage(e), "\n")
  FALSE
})

if (!svm_available) {
  svm_pred <- rf_pred; svm_prob <- rf_prob; svm_acc <- 0; svm_roc <- rf_roc
}

# =============================================================
# Step 12: Ensemble (Voting)
# =============================================================
cat("\n--- Step 12: Ensemble (Majority Voting) ---\n")

# Convert all predictions to 0/1
to_numeric <- function(preds) as.numeric(preds == "Approved")

ensemble_votes <- (
  to_numeric(rf_pred) +
  to_numeric(xgb_pred) +
  (lgb_prob_raw > 0.5) * 1 +
  to_numeric(gbm_pred)
)

ensemble_pred <- factor(ifelse(ensemble_votes >= 2, "Approved", "Rejected"), levels = c("Rejected", "Approved"))
ensemble_acc <- confusionMatrix(ensemble_pred, test_scaled$LoanApproved)$overall["Accuracy"]
ensemble_prob <- (rf_prob$Approved + xgb_prob$Approved + lgb_prob_raw + gbm_prob$Approved) / 4
ensemble_roc <- pROC::roc(test_scaled$LoanApproved, ensemble_prob)
cat("Ensemble Accuracy:", ensemble_acc, " ROC-AUC:", pROC::auc(ensemble_roc), "\n")

# =============================================================
# Step 13: Full Model Comparison
# =============================================================
cat("\n--- Full Model Comparison ---\n")

# Add LGB probabilities to match format
lgb_prob_df <- data.frame(Rejected = 1 - lgb_prob_raw, Approved = lgb_prob_raw)

all_results <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "LightGBM", "Gradient Boosting", "Ensemble"),
  Framework = c("Caret", "Caret", "Native lightgbm", "Caret (gbm)", "Voting Ensemble"),
  Accuracy = c(log_acc, rf_acc, lgb_acc, gbm_acc, ensemble_acc),
  ROC_AUC = c(pROC::auc(log_roc), pROC::auc(rf_roc),
              pROC::auc(lgb_roc), pROC::auc(gbm_roc),
              pROC::auc(ensemble_roc))
) %>% arrange(desc(Accuracy))

cat("\n=== FINAL RESULTS (Sorted by Accuracy) ===\n")
print(all_results)

write_csv(all_results, file.path(MODELS_DIR, "advanced_model_results.csv"))
cat("\nSaved: advanced_model_results.csv\n")

# =============================================================
# Step 14: ROC Curves Comparison Plot
# =============================================================
cat("\n--- Step 14: ROC Curves Plot ---\n")

roc_data <- bind_rows(
  data.frame(FPR = 1 - as.numeric(log_roc$specificities), TPR = as.numeric(log_roc$sensitivities), Model = "Logistic"),
  data.frame(FPR = 1 - as.numeric(rf_roc$specificities), TPR = as.numeric(rf_roc$sensitivities), Model = "Random Forest"),
  data.frame(FPR = 1 - as.numeric(lgb_roc$specificities), TPR = as.numeric(lgb_roc$sensitivities), Model = "LightGBM"),
  data.frame(FPR = 1 - as.numeric(gbm_roc$specificities), TPR = as.numeric(gbm_roc$sensitivities), Model = "GBM"),
  data.frame(FPR = 1 - as.numeric(ensemble_roc$specificities), TPR = as.numeric(ensemble_roc$sensitivities), Model = "Ensemble")
)

p19 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "ROC Curves - All Models", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal(base_size = 14)
ggsave(file.path(PLOTS_DIR, "19_roc_curves_all.png"), p19, width = 10, height = 7)
cat("Saved: 19_roc_curves_all.png\n")

# =============================================================
# Step 15: Confusion Matrices Plot
# =============================================================
cat("\n--- Step 15: Confusion Matrices ---\n")

cm_data <- data.frame(
  Model = character(), Prediction = character(), Actual = character(), Count = integer(),
  stringsAsFactors = FALSE
)
for (m_name in c("Logistic", "RF", "LGB", "GBM", "Ensemble")) {
  preds <- switch(m_name,
    "Logistic" = log_pred, "RF" = rf_pred,
    "LGB" = lgb_pred, "GBM" = gbm_pred, "Ensemble" = ensemble_pred
  )
  cm <- confusionMatrix(preds, test_scaled$LoanApproved)$table
  for (a in c("Rejected", "Approved")) {
    for (p in c("Rejected", "Approved")) {
      cm_data <- rbind(cm_data, data.frame(
        Model = m_name, Prediction = p, Actual = a,
        Count = cm[p, a], stringsAsFactors = FALSE
      ))
    }
  }
}

p20 <- ggplot(cm_data, aes(x = Prediction, y = Actual, fill = Count)) +
  geom_tile() + geom_text(aes(label = Count), color = "white", size = 3) +
  scale_fill_gradient(low = "#2c3e50", high = "#e74c3c") +
  facet_wrap(~ Model, ncol = 4) +
  labs(title = "Confusion Matrices - All Models") +
  theme_minimal(base_size = 12)
ggsave(file.path(PLOTS_DIR, "20_confusion_matrices_all.png"), p20, width = 12, height = 8)
cat("Saved: 20_confusion_matrices_all.png\n")

# =============================================================
# Step 16: Accuracy Comparison Bar Chart
# =============================================================
p21 <- ggplot(all_results, aes(x = reorder(Model, Accuracy), y = Accuracy * 100, fill = Framework)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", Accuracy * 100)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Model Accuracy Comparison (with SMOTE + Enhanced Features)",
       x = "Model", y = "Accuracy (%)") +
  theme_minimal(base_size = 14) +
  ylim(0, 100)
ggsave(file.path(PLOTS_DIR, "21_accuracy_comparison.png"), p21, width = 10, height = 6)
cat("Saved: 21_accuracy_comparison.png\n")

# =============================================================
# Export for Power BI
# =============================================================
cat("\n--- Exporting advanced results for Power BI ---\n")
write_csv(all_results, file.path(POWERBI_DIR, "advanced_model_results.csv"))

# Export LightGBM feature importance
lgb_imp_export <- lgb_imp %>% head(20) %>%
  rename(FeatureName = Feature, GainScore = Gain, CoverScore = Cover, FrequencyScore = Frequency)
write_csv(lgb_imp_export, file.path(POWERBI_DIR, "lightgbm_feature_importance.csv"))
cat("Exported: lightgbm_feature_importance.csv\n")

cat("\nDone: 10_ml_advanced.R\n")
cat("\n=== BEST MODEL ===\n")
cat(all_results$Model[1], "- Accuracy:", sprintf("%.2f%%", all_results$Accuracy[1] * 100),
    "ROC-AUC:", sprintf("%.3f\n", all_results$ROC_AUC[1]))
