# ============================================================
# generate_powerbi_template.R
# Creates a Power BI Template (.pbit) file with all visuals,
# DAX measures, relationships, and formatting pre-configured.
#
# Usage: Rscript generate_powerbi_template.R
# Output: powerbi/Financial_Risk_Dashboard.pbit
# ============================================================

suppressPackageStartupMessages({
  library(jsonlite)
})

cat("=== Generating Power BI Template (.pbit) ===\n\n")

PROJECT_ROOT <- if (basename(getwd()) == "R") dirname(getwd()) else getwd()
POWERBI_DIR  <- file.path(PROJECT_ROOT, "powerbi")
TEMPLATE_DIR <- file.path(PROJECT_ROOT, "powerbi", "_pbit_build")
PBIT_OUTPUT  <- file.path(POWERBI_DIR, "Financial_Risk_Dashboard.pbit")

if (dir.exists(TEMPLATE_DIR)) unlink(TEMPLATE_DIR, recursive = TRUE)
dir.create(TEMPLATE_DIR, recursive = TRUE)
dir.create(file.path(TEMPLATE_DIR, "Report"), showWarnings = FALSE)

# ============================================================
# Helper: Build a visual container for Report/Layout
# ============================================================
make_vc <- function(name, vtype, category = NULL, values = NULL, legend = NULL, 
                    grouping = NULL, x_cols = NULL, y_cols = NULL,
                    x_pos = 0L, y_pos = 0L, width = 4L, height = 4L) {
  projections <- list()
  if (!is.null(category)) projections$Category <- lapply(category, function(c) list(queryRef = c))
  if (!is.null(values))   projections$Values   <- lapply(values, function(v) list(queryRef = v))
  if (!is.null(legend))   projections$Legend   <- lapply(legend, function(l) list(queryRef = l))
  if (!is.null(grouping)) projections$Grouping <- lapply(grouping, function(g) list(queryRef = g))
  if (!is.null(x_cols))   projections$X        <- lapply(x_cols, function(x) list(queryRef = x))
  if (!is.null(y_cols))   projections$Y        <- lapply(y_cols, function(y) list(queryRef = y))
  
  config <- list(
    name = name,
    singleVisual = list(
      visualType = vtype,
      projections = projections
    )
  )
  
  list(
    config = toJSON(config, auto_unbox = TRUE, null = "null"),
    x = x_pos, y = y_pos, z = 0L,
    width = width, height = height
  )
}

# ============================================================
# 1. DataModelSchema.json
# ============================================================
cat("Creating DataModelSchema.json...\n")

numeric_cols <- list(
  list(name = "Age", dataType = "int64", sourceColumn = "Age", type = "data"),
  list(name = "AnnualIncome", dataType = "double", sourceColumn = "AnnualIncome", type = "data"),
  list(name = "LoanAmount", dataType = "double", sourceColumn = "LoanAmount", type = "data"),
  list(name = "LoanDuration", dataType = "int64", sourceColumn = "LoanDuration", type = "data"),
  list(name = "CreditScore", dataType = "int64", sourceColumn = "CreditScore", type = "data"),
  list(name = "NumberOfDependents", dataType = "int64", sourceColumn = "NumberOfDependents", type = "data"),
  list(name = "InterestRate", dataType = "double", sourceColumn = "InterestRate", type = "data"),
  list(name = "DebtToIncomeRatio", dataType = "double", sourceColumn = "DebtToIncomeRatio", type = "data"),
  list(name = "CreditCardUtilizationRate", dataType = "double", sourceColumn = "CreditCardUtilizationRate", type = "data"),
  list(name = "PaymentHistory", dataType = "double", sourceColumn = "PaymentHistory", type = "data"),
  list(name = "LengthOfCreditHistory", dataType = "int64", sourceColumn = "LengthOfCreditHistory", type = "data"),
  list(name = "PreviousLoanDefaults", dataType = "int64", sourceColumn = "PreviousLoanDefaults", type = "data"),
  list(name = "JobTenure", dataType = "int64", sourceColumn = "JobTenure", type = "data"),
  list(name = "MonthlyDebtPayments", dataType = "double", sourceColumn = "MonthlyDebtPayments", type = "data"),
  list(name = "MonthlySavings", dataType = "double", sourceColumn = "MonthlySavings", type = "data"),
  list(name = "TotalAssets", dataType = "double", sourceColumn = "TotalAssets", type = "data"),
  list(name = "TotalLiabilities", dataType = "double", sourceColumn = "TotalLiabilities", type = "data"),
  list(name = "NetWorth", dataType = "double", sourceColumn = "NetWorth", type = "data"),
  list(name = "LoanApproved", dataType = "int64", sourceColumn = "LoanApproved", type = "data"),
  list(name = "LoanApprovedLabel", dataType = "string", sourceColumn = "LoanApprovedLabel", type = "data"),
  list(name = "ExpenseRatio", dataType = "double", sourceColumn = "ExpenseRatio", type = "data"),
  list(name = "AssetCoverage", dataType = "double", sourceColumn = "AssetCoverage", type = "data"),
  list(name = "SavingsRatio", dataType = "double", sourceColumn = "SavingsRatio", type = "data"),
  list(name = "LoanBurdenRatio", dataType = "double", sourceColumn = "LoanBurdenRatio", type = "data"),
  list(name = "FinancialHealthScore", dataType = "double", sourceColumn = "FinancialHealthScore", type = "data")
)

text_cols <- list(
  list(name = "EmploymentStatus", dataType = "string", sourceColumn = "EmploymentStatus", type = "data"),
  list(name = "EducationLevel", dataType = "string", sourceColumn = "EducationLevel", type = "data"),
  list(name = "MaritalStatus", dataType = "string", sourceColumn = "MaritalStatus", type = "data"),
  list(name = "HomeOwnershipStatus", dataType = "string", sourceColumn = "HomeOwnershipStatus", type = "data"),
  list(name = "LoanPurpose", dataType = "string", sourceColumn = "LoanPurpose", type = "data"),
  list(name = "EmployerType", dataType = "string", sourceColumn = "EmployerType", type = "data"),
  list(name = "RiskLevel", dataType = "string", sourceColumn = "RiskLevel", type = "data"),
  list(name = "AgeGroup", dataType = "string", sourceColumn = "AgeGroup", type = "data")
)

dax_measures <- list(
  list(name = "Approval Rate", expression = "DIVIDE(COUNTROWS(FILTER(cleaned_data_for_powerbi, cleaned_data_for_powerbi[LoanApproved] = 1)), COUNTROWS(cleaned_data_for_powerbi)) * 100", formatString = "0.00", type = "measure"),
  list(name = "Avg Credit Score", expression = "AVERAGE(cleaned_data_for_powerbi[CreditScore])", formatString = "0.0", type = "measure"),
  list(name = "Avg Annual Income", expression = "AVERAGE(cleaned_data_for_powerbi[AnnualIncome])", formatString = "$#,##0", type = "measure"),
  list(name = "Total Records", expression = "COUNTROWS(cleaned_data_for_powerbi)", formatString = "#,##0", type = "measure"),
  list(name = "Avg Financial Health Score", expression = "AVERAGE(cleaned_data_for_powerbi[FinancialHealthScore])", formatString = "0.0000", type = "measure"),
  list(name = "Avg DTI", expression = "AVERAGE(cleaned_data_for_powerbi[DebtToIncomeRatio])", formatString = "0.0000", type = "measure"),
  list(name = "Avg Expense Ratio", expression = "AVERAGE(cleaned_data_for_powerbi[ExpenseRatio])", formatString = "0.0000", type = "measure"),
  list(name = "Avg Savings Ratio", expression = "AVERAGE(cleaned_data_for_powerbi[SavingsRatio])", formatString = "0.0000", type = "measure"),
  list(name = "High Risk Pct", expression = "DIVIDE(COUNTROWS(FILTER(cleaned_data_for_powerbi, cleaned_data_for_powerbi[RiskLevel] = \"High Risk\")), COUNTROWS(cleaned_data_for_powerbi)) * 100", formatString = "0.00", type = "measure"),
  list(name = "Low Risk Pct", expression = "DIVIDE(COUNTROWS(FILTER(cleaned_data_for_powerbi, cleaned_data_for_powerbi[RiskLevel] = \"Low Risk\")), COUNTROWS(cleaned_data_for_powerbi)) * 100", formatString = "0.00", type = "measure")
)

summary_tables <- list(
  list(name = "risk_level_summary", file = "risk_level_summary.csv"),
  list(name = "education_summary", file = "education_summary.csv"),
  list(name = "employment_summary", file = "employment_summary.csv"),
  list(name = "marital_status_summary", file = "marital_status_summary.csv"),
  list(name = "home_ownership_summary", file = "home_ownership_summary.csv"),
  list(name = "age_group_summary", file = "age_group_summary.csv"),
  list(name = "kpi_summary", file = "kpi_summary.csv"),
  list(name = "feature_importance", file = "feature_importance.csv"),
  list(name = "advanced_model_results", file = "advanced_model_results.csv")
)

# M expression for CSV import
m_expr <- function(filename, ncols = 20) {
  paste0("let\n    Source = Csv.Document(File.Contents(\"", filename, 
         "\"),[Delimiter=\",\", Columns=", ncols, 
         ", Encoding=65001, QuoteStyle=QuoteStyle.None]),\n",
         "    #\"Promoted Headers\" = Table.PromoteHeaders(Source, [PromoteAllScalars=true])\n",
         "in\n    #\"Promoted Headers\"")
}

# Build tables
tables_list <- list()

# Main fact table
tables_list[[1]] <- list(
  name = "cleaned_data_for_powerbi",
  columns = c(numeric_cols, text_cols, dax_measures),
  partitions = list(list(name = "Partition", source = list(type = "m", expression = m_expr("cleaned_data_for_powerbi.csv", 65))))
)

# Summary tables
for (i in seq_along(summary_tables)) {
  tables_list[[i + 1]] <- list(
    name = summary_tables[[i]]$name,
    columns = list(),
    partitions = list(list(name = "Partition", source = list(type = "m", expression = m_expr(summary_tables[[i]]$file, 10))))
  )
}

# Data sources
datasources <- lapply(c("cleaned_data_for_powerbi.csv", sapply(summary_tables, `[[`, "file")), function(f) {
  list(Name = basename(f), Connection = list(Protocol = "file", Address = list(Path = f)), Mode = "import")
})

# Relationships
relationships <- list(
  list(name = "rel_risk", fromTable = "cleaned_data_for_powerbi", fromColumn = "RiskLevel", toTable = "risk_level_summary", toColumn = "RiskLevel", crossFilteringBehavior = "bothDirections"),
  list(name = "rel_edu", fromTable = "cleaned_data_for_powerbi", fromColumn = "EducationLevel", toTable = "education_summary", toColumn = "EducationLevel", crossFilteringBehavior = "bothDirections"),
  list(name = "rel_emp", fromTable = "cleaned_data_for_powerbi", fromColumn = "EmploymentStatus", toTable = "employment_summary", toColumn = "EmploymentStatus", crossFilteringBehavior = "bothDirections"),
  list(name = "rel_marital", fromTable = "cleaned_data_for_powerbi", fromColumn = "MaritalStatus", toTable = "marital_status_summary", toColumn = "MaritalStatus", crossFilteringBehavior = "bothDirections"),
  list(name = "rel_home", fromTable = "cleaned_data_for_powerbi", fromColumn = "HomeOwnershipStatus", toTable = "home_ownership_summary", toColumn = "HomeOwnershipStatus", crossFilteringBehavior = "bothDirections"),
  list(name = "rel_age", fromTable = "cleaned_data_for_powerbi", fromColumn = "AgeGroup", toTable = "age_group_summary", toColumn = "AgeGroup", crossFilteringBehavior = "bothDirections")
)

datamodel_schema <- list(
  compatibilityLevel = 1567,
  model = list(culture = "en-US", dataSources = datasources, tables = tables_list, relationships = relationships)
)

write_json(datamodel_schema, file.path(TEMPLATE_DIR, "DataModelSchema.json"), auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)
cat("  DataModelSchema.json created\n")

# ============================================================
# 2. Report/Layout.json
# ============================================================
cat("Creating Report/Layout.json...\n")

# Build visual containers for each page
page1_vcs <- list(
  make_vc("card_approval", "card", values = "cleaned_data_for_powerbi.Approval Rate.Measure", x_pos = 0L, y_pos = 0L, width = 2L, height = 2L),
  make_vc("card_credit", "card", values = "cleaned_data_for_powerbi.Avg Credit Score.Measure", x_pos = 2L, y_pos = 0L, width = 2L, height = 2L),
  make_vc("card_income", "card", values = "cleaned_data_for_powerbi.Avg Annual Income.Measure", x_pos = 4L, y_pos = 0L, width = 2L, height = 2L),
  make_vc("card_total", "card", values = "cleaned_data_for_powerbi.Total Records.Measure", x_pos = 6L, y_pos = 0L, width = 2L, height = 2L),
  make_vc("pie_approval", "pieChart", category = "cleaned_data_for_powerbi.LoanApprovedLabel", values = "cleaned_data_for_powerbi.Total Records.Measure", x_pos = 0L, y_pos = 2L, width = 4L, height = 4L),
  make_vc("slicer_risk", "slicer", values = "cleaned_data_for_powerbi.RiskLevel", x_pos = 6L, y_pos = 2L, width = 2L, height = 2L),
  make_vc("slicer_edu", "slicer", values = "cleaned_data_for_powerbi.EducationLevel", x_pos = 6L, y_pos = 4L, width = 2L, height = 2L),
  make_vc("slicer_emp", "slicer", values = "cleaned_data_for_powerbi.EmploymentStatus", x_pos = 8L, y_pos = 0L, width = 2L, height = 2L),
  make_vc("slicer_marital", "slicer", values = "cleaned_data_for_powerbi.MaritalStatus", x_pos = 8L, y_pos = 2L, width = 2L, height = 2L)
)

page2_vcs <- list(
  make_vc("bar_risk_approval", "stackedBarChart", category = "cleaned_data_for_powerbi.RiskLevel", values = "cleaned_data_for_powerbi.Total Records.Measure", legend = "cleaned_data_for_powerbi.LoanApprovedLabel", x_pos = 0L, y_pos = 0L, width = 8L, height = 4L),
  make_vc("donut_risk", "donutChart", category = "cleaned_data_for_powerbi.RiskLevel", values = "cleaned_data_for_powerbi.Total Records.Measure", x_pos = 8L, y_pos = 0L, width = 4L, height = 4L),
  make_vc("col_credit_score", "columnChart", category = "cleaned_data_for_powerbi.CreditScore", values = "cleaned_data_for_powerbi.Total Records.Measure", x_pos = 0L, y_pos = 4L, width = 6L, height = 4L),
  make_vc("col_approval_risk", "columnChart", category = "cleaned_data_for_powerbi.RiskLevel", values = "cleaned_data_for_powerbi.Approval Rate.Measure", x_pos = 6L, y_pos = 4L, width = 6L, height = 4L)
)

page3_vcs <- list(
  make_vc("col_expense", "columnChart", category = "cleaned_data_for_powerbi.LoanApprovedLabel", values = "cleaned_data_for_powerbi.ExpenseRatio", x_pos = 0L, y_pos = 0L, width = 4L, height = 4L),
  make_vc("col_savings", "columnChart", category = "cleaned_data_for_powerbi.LoanApprovedLabel", values = "cleaned_data_for_powerbi.SavingsRatio", x_pos = 4L, y_pos = 0L, width = 4L, height = 4L),
  make_vc("scatter_health", "scatterChart", x_cols = "cleaned_data_for_powerbi.AssetCoverage", y_cols = "cleaned_data_for_powerbi.FinancialHealthScore", legend = "cleaned_data_for_powerbi.LoanApprovedLabel", x_pos = 8L, y_pos = 0L, width = 4L, height = 4L),
  make_vc("col_burden", "columnChart", category = "cleaned_data_for_powerbi.RiskLevel", values = "cleaned_data_for_powerbi.LoanBurdenRatio", x_pos = 0L, y_pos = 4L, width = 6L, height = 4L),
  make_vc("col_income_approval", "columnChart", category = "cleaned_data_for_powerbi.LoanApprovedLabel", values = "cleaned_data_for_powerbi.Avg Annual Income.Measure", x_pos = 6L, y_pos = 4L, width = 6L, height = 4L)
)

page4_vcs <- list(
  make_vc("table_models", "tableEx", values = c("advanced_model_results.Model", "advanced_model_results.Framework", "advanced_model_results.Accuracy", "advanced_model_results.ROC_AUC"), x_pos = 0L, y_pos = 0L, width = 8L, height = 3L),
  make_vc("bar_accuracy", "barChart", category = "advanced_model_results.Model", values = "advanced_model_results.Accuracy", x_pos = 0L, y_pos = 3L, width = 6L, height = 4L),
  make_vc("bar_importance", "barChart", category = "feature_importance.FeatureName", values = "feature_importance.ImportanceScore", x_pos = 6L, y_pos = 3L, width = 6L, height = 4L),
  make_vc("table_kpi", "tableEx", values = c("kpi_summary.KPI", "kpi_summary.Value"), x_pos = 0L, y_pos = 7L, width = 8L, height = 2L)
)

# Build sections
make_section <- function(name, display_name, vcs) {
  list(
    name = name,
    displayName = display_name,
    width = 16.0,
    height = 9.0,
    displayOption = 1L,
    filters = "[]",
    objects = "{}",
    visualContainers = lapply(vcs, function(vc) {
      list(config = vc$config, x = vc$x, y = vc$y, z = vc$z, width = vc$width, height = vc$height)
    })
  )
}

report_layout <- list(
  schema = "https://developer.microsoft.com/json-schemas/fabric/item/report/definition/report/1.0.0/schema.json",
  config = toJSON(list(version = "5.60", activeSectionIndex = 0L), auto_unbox = TRUE, null = "null"),
  layoutOptimization = 0L,
  sections = list(
    make_section("page1_overview", "Overview", page1_vcs),
    make_section("page2_risk", "Risk Analysis", page2_vcs),
    make_section("page3_finance", "Financial Health", page3_vcs),
    make_section("page4_ml", "ML Insights", page4_vcs)
  )
)

write_json(report_layout, file.path(TEMPLATE_DIR, "Report", "Layout.json"), auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)
cat("  Report/Layout.json created\n")

# ============================================================
# 3. DiagramLayout.json
# ============================================================
cat("Creating DiagramLayout.json...\n")
diagram_layout <- list(
  version = "1.1.0",
  diagrams = list(list(
    ordinal = 0L,
    scrollPosition = list(x = 0L, y = 0L),
    nodes = list(list(nodeIndex = 0L, location = list(x = 0L, y = 0L), size = list(width = 200L, height = 200L), nodeId = list(entity = "cleaned_data_for_powerbi"))),
    name = "All tables",
    displayState = list(layout_mode = 0L, pan_zoom = list(x = 0L, y = 0L, zoom = 1.0))
  ))
)
write_json(diagram_layout, file.path(TEMPLATE_DIR, "DiagramLayout.json"), auto_unbox = TRUE, null = "null", pretty = TRUE)
cat("  DiagramLayout.json created\n")

# ============================================================
# 4. Settings.json
# ============================================================
cat("Creating Settings.json...\n")
settings <- list(version = "5.60", activeSectionIndex = 0L, model = list(culture = "en-US", dataAccessOptions = list(provider = "None", timeout = "00:10:00", type = "import")))
write_json(settings, file.path(TEMPLATE_DIR, "Settings.json"), auto_unbox = TRUE, null = "null", pretty = TRUE)
cat("  Settings.json created\n")

# ============================================================
# 5. Metadata + Version
# ============================================================
write_json(list(version = "1.0"), file.path(TEMPLATE_DIR, "Metadata"), auto_unbox = TRUE, pretty = TRUE)
writeLines("1.0", file.path(TEMPLATE_DIR, "Version"))
cat("  Metadata + Version created\n")

# ============================================================
# 6. Package as .pbit (ZIP)
# ============================================================
cat("\nPackaging .pbit file...\n")

rel_paths <- list.files(TEMPLATE_DIR, recursive = TRUE)
zip_cmd <- sprintf("cd %s && zip -r %s %s", shQuote(TEMPLATE_DIR), shQuote(PBIT_OUTPUT), paste(shQuote(rel_paths), collapse = " "))
system(zip_cmd)

if (file.exists(PBIT_OUTPUT)) {
  fsize <- file.size(PBIT_OUTPUT)
  cat("\n=== SUCCESS ===\n")
  cat("Template created:", PBIT_OUTPUT, "\n")
  cat("File size:", sprintf("%.1f KB\n", fsize / 1024))
  cat("\n=== How to Use on Friend's Laptop ===\n")
  cat("1. Copy these files to friend's Windows machine:\n")
  cat("   - Financial_Risk_Dashboard.pbit\n")
  cat("   - All CSV files from powerbi/ folder\n")
  cat("2. Open Financial_Risk_Dashboard.pbit in Power BI Desktop\n")
  cat("3. When prompted for file path, browse to the CSV folder\n")
  cat("4. Click 'Load' - dashboard builds automatically!\n")
} else {
  cat("ERROR: Failed to create .pbit file\n")
}

unlink(TEMPLATE_DIR, recursive = TRUE)
cat("\nDone!\n")
