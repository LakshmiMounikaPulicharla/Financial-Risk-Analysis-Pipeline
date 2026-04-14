# ============================================================
# Shiny Dashboard - Financial Risk Analysis
# Run: shiny::runApp("R/shiny/app.R")
# ============================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(DT)

# ============================================================
# DATA LOADING
# ============================================================
cat("Loading data for Shiny dashboard...\n")

PROJECT_ROOT <- if (basename(getwd()) == "shiny") {
  # When running from R/shiny/ directory, go up two levels
  dirname(dirname(getwd()))
} else if (basename(getwd()) == "R") {
  # When running from R/ directory
  dirname(getwd())
} else {
  # When running from project root
  getwd()
}
POWERBI_DIR  <- file.path(PROJECT_ROOT, "powerbi")

# Load cleaned data (sampling for performance)
df <- tryCatch({
  read_csv(file.path(POWERBI_DIR, "cleaned_data_for_powerbi.csv"), 
           show_col_types = FALSE, n_max = 100000)
}, error = function(e) {
  cat("Error loading data:", e$message, "\n")
  NULL
})

# Load summary tables
risk_sum    <- read_csv(file.path(POWERBI_DIR, "risk_level_summary.csv"), show_col_types = FALSE)
edu_sum     <- read_csv(file.path(POWERBI_DIR, "education_summary.csv"), show_col_types = FALSE)
emp_sum     <- read_csv(file.path(POWERBI_DIR, "employment_summary.csv"), show_col_types = FALSE)
model_sum   <- read_csv(file.path(POWERBI_DIR, "advanced_model_results.csv"), show_col_types = FALSE)
feat_imp    <- read_csv(file.path(POWERBI_DIR, "feature_importance.csv"), show_col_types = FALSE)

cat("Data loaded successfully. Rows:", nrow(df), "\n")

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Financial Risk Analysis Dashboard",
    tags$li(class = "dropdown",
            tags$a(href = "https://github.com", 
                   icon("github"), 
                   "Project Repository",
                   target = "_blank"))
  ),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Risk Analysis", tabName = "risk", icon = icon("chart-bar")),
      menuItem("Financial Health", tabName = "health", icon = icon("heartbeat")),
      menuItem("ML Insights", tabName = "ml", icon = icon("brain")),
      menuItem("Predict", tabName = "predict", icon = icon("calculator")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    ),
    
    # Filters in sidebar
    h4("Filters", style = "padding: 10px; font-weight: bold;"),
    
    selectInput("risk_filter", "Risk Level:", 
                choices = c("All", unique(df$RiskLevel)), 
                selected = "All"),
    
    selectInput("edu_filter", "Education:", 
                choices = c("All", unique(df$EducationLevel)), 
                selected = "All"),
    
    selectInput("emp_filter", "Employment:", 
                choices = c("All", unique(df$EmploymentStatus)), 
                selected = "All"),
    
    selectInput("marital_filter", "Marital Status:", 
                choices = c("All", unique(df$MaritalStatus)), 
                selected = "All"),
    
    actionButton("reset_filters", "Reset Filters", 
                 style = "width: 100%; margin: 10px;")
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f4f4; }
        .info-box { min-height: 80px; }
        .info-box-icon { height: 80px; line-height: 80px; }
        .info-box-number { font-size: 24px; }
        .box { border-radius: 5px; }
      "))
    ),
    
    tabItems(
      # ==================== PAGE 1: OVERVIEW ====================
      tabItem(tabName = "overview",
              fluidRow(
                # KPI Cards
                valueBoxOutput("kpi_approval", width = 3),
                valueBoxOutput("kpi_credit", width = 3),
                valueBoxOutput("kpi_income", width = 3),
                valueBoxOutput("kpi_records", width = 3)
              ),
              
              fluidRow(
                # Pie Chart
                box(title = "Loan Approval Distribution", 
                    plotlyOutput("pie_approval", height = 400), width = 6),
                
                # Risk Distribution
                box(title = "Risk Level Distribution",
                    plotlyOutput("bar_risk", height = 400), width = 6)
              ),
              
              fluidRow(
                # Credit Score Distribution
                box(title = "Credit Score Distribution",
                    plotlyOutput("hist_credit", height = 300), width = 12)
              )
      ),
      
      # ==================== PAGE 2: RISK ANALYSIS ====================
      tabItem(tabName = "risk",
              fluidRow(
                box(title = "Approval Rate by Risk Level",
                    plotlyOutput("bar_risk_approval", height = 350), width = 8),
                
                box(title = "Risk Statistics",
                    tableOutput("risk_stats"), width = 4)
              ),
              
              fluidRow(
                box(title = "Credit Score vs Approval Status",
                    plotlyOutput("box_credit_approval", height = 350), width = 6),
                
                box(title = "Income by Approval Status",
                    plotlyOutput("box_income_approval", height = 350), width = 6)
              ),
              
              fluidRow(
                box(title = "Education Level Impact on Approval",
                    plotlyOutput("bar_edu_approval", height = 300), width = 12)
              )
      ),
      
      # ==================== PAGE 3: FINANCIAL HEALTH ====================
      tabItem(tabName = "health",
              fluidRow(
                box(title = "Expense Ratio by Approval",
                    plotlyOutput("box_expense", height = 350), width = 6),
                
                box(title = "Savings Ratio by Approval",
                    plotlyOutput("box_savings", height = 350), width = 6)
              ),
              
              fluidRow(
                box(title = "Asset Coverage vs Financial Health Score",
                    plotlyOutput("scatter_assets_health", height = 400), width = 8),
                
                box(title = "Loan Burden by Risk Level",
                    plotlyOutput("bar_burden_risk", height = 400), width = 4)
              ),
              
              fluidRow(
                box(title = "DTI vs Approval Status",
                    plotlyOutput("box_dti", height = 300), width = 12)
              )
      ),
      
      # ==================== PAGE 4: ML INSIGHTS ====================
      tabItem(tabName = "ml",
              fluidRow(
                box(title = "Model Performance Comparison",
                    DTOutput("model_table"), width = 12)
              ),
              
              fluidRow(
                box(title = "Model Accuracy",
                    plotlyOutput("bar_accuracy", height = 350), width = 6),
                
                box(title = "Model ROC-AUC",
                    plotlyOutput("bar_auc", height = 350), width = 6)
              ),
              
              fluidRow(
                box(title = "Feature Importance (Top 15)",
                    plotlyOutput("bar_importance", height = 400), width = 12)
              )
      ),
      
      # ==================== PAGE 5: PREDICTION ====================
      tabItem(tabName = "predict",
              fluidRow(
                column(6,
                       h3("Enter Applicant Details"),
                       wellPanel(
                         numericInput("pred_creditscore", "Credit Score (300-850)", value = 650, min = 300, max = 850),
                         numericInput("pred_income", "Annual Income ($)", value = 50000, min = 0),
                         numericInput("pred_loanamount", "Loan Amount ($)", value = 15000, min = 0),
                         numericInput("pred_loanduration", "Loan Duration (months)", value = 36, min = 1, max = 360),
                         numericInput("pred_interestrate", "Interest Rate (%)", value = 5.5, min = 0, max = 30),
                         selectInput("pred_employment", "Employment Status", 
                                     choices = c("Employed", "Self-Employed", "Unemployed", "Retired")),
                         selectInput("pred_education", "Education Level",
                                     choices = c("High School", "Bachelor", "Master", "Doctorate", "Other")),
                         selectInput("pred_marital", "Marital Status",
                                     choices = c("Married", "Single", "Divorced", "Widowed")),
                         numericInput("pred_dependents", "Number of Dependents", value = 0, min = 0, max = 20),
                         numericInput("pred_debtpayments", "Monthly Debt Payments ($)", value = 500, min = 0),
                         numericInput("pred_savings", "Monthly Savings ($)", value = 1000, min = 0),
                         numericInput("pred_assets", "Total Assets ($)", value = 50000, min = 0),
                         numericInput("pred_liabilities", "Total Liabilities ($)", value = 20000, min = 0),
                         actionButton("predict_btn", "Predict Approval", 
                                      style = "width: 100%; font-size: 16px; margin-top: 10px;",
                                      class = "btn-primary")
                       )
                ),
                column(6,
                       h3("Prediction Result"),
                       wellPanel(
                         uiOutput("prediction_result")
                       )
                )
              ),
              
              fluidRow(
                box(title = "Risk Assessment Details", width = 12,
                    uiOutput("risk_details"))
              )
      ),
      
      # ==================== PAGE 6: RAW DATA ====================
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Filtered Data (Max 1000 rows)",
                    DTOutput("data_table"), width = 12,
                    helpText("Use filters on the left to filter data. Maximum 1000 rows displayed."))
              )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(df)
    
    df_filtered <- df
    
    if (input$risk_filter != "All") {
      df_filtered <- df_filtered %>% filter(RiskLevel == input$risk_filter)
    }
    if (input$edu_filter != "All") {
      df_filtered <- df_filtered %>% filter(EducationLevel == input$edu_filter)
    }
    if (input$emp_filter != "All") {
      df_filtered <- df_filtered %>% filter(EmploymentStatus == input$emp_filter)
    }
    if (input$marital_filter != "All") {
      df_filtered <- df_filtered %>% filter(MaritalStatus == input$marital_filter)
    }
    
    df_filtered
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "risk_filter", selected = "All")
    updateSelectInput(session, "edu_filter", selected = "All")
    updateSelectInput(session, "emp_filter", selected = "All")
    updateSelectInput(session, "marital_filter", selected = "All")
  })
  
  # ==================== OVERVIEW PAGE ====================
  
  # KPI: Approval Rate
  output$kpi_approval <- renderValueBox({
    data <- filtered_data()
    rate <- mean(data$LoanApproved == 1, na.rm = TRUE) * 100
    valueBox(
      sprintf("%.1f%%", rate),
      "Approval Rate",
      icon = icon("check-circle"),
      color = ifelse(rate > 50, "green", "red")
    )
  })
  
  # KPI: Avg Credit Score
  output$kpi_credit <- renderValueBox({
    data <- filtered_data()
    avg <- mean(data$CreditScore, na.rm = TRUE)
    valueBox(
      sprintf("%.0f", avg),
      "Avg Credit Score",
      icon = icon("credit-card"),
      color = "aqua"
    )
  })
  
  # KPI: Avg Income
  output$kpi_income <- renderValueBox({
    data <- filtered_data()
    avg <- mean(data$AnnualIncome, na.rm = TRUE)
    valueBox(
      sprintf("$%.0f", avg),
      "Avg Annual Income",
      icon = icon("money-dollar"),
      color = "green"
    )
  })
  
  # KPI: Total Records
  output$kpi_records <- renderValueBox({
    data <- filtered_data()
    valueBox(
      sprintf("%.0f", nrow(data)),
      "Total Records",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  # Pie Chart: Loan Approval
  output$pie_approval <- renderPlotly({
    data <- filtered_data()
    approval_counts <- data %>% 
      group_by(LoanApprovedLabel) %>% 
      summarise(n = n())
    
    plot_ly(approval_counts, labels = ~LoanApprovedLabel, values = ~n, 
            type = "pie", 
            marker = list(colors = c("Approved" = "#27ae60", "Rejected" = "#e74c3c")),
            textinfo = "percent+label") %>%
      layout(title = "")
  })
  
  # Bar Chart: Risk Distribution
  output$bar_risk <- renderPlotly({
    tryCatch({
      data <- as.data.frame(filtered_data())
      
      if (is.null(data) || nrow(data) == 0) {
        return(plotly::plot_ly() %>% 
                 plotly::add_text(text = "No data available", textposition = "top") %>% 
                 plotly::layout(title = "No data"))
      }
      
      # Find RiskLevel column (case-insensitive)
      risk_col <- grep("RiskLevel|risk_level", names(data), value = TRUE, ignore.case = TRUE)[1]
      
      if (is.na(risk_col)) {
        return(plotly::plot_ly() %>% 
                 plotly::add_text(text = "RiskLevel column not found", textposition = "top") %>% 
                 plotly::layout(title = "Missing column"))
      }
      
      # Use base R table() instead of dplyr group_by
      risk_table <- table(data[[risk_col]], useNA = "no")
      risk_counts <- data.frame(RiskLevel = names(risk_table), n = as.integer(risk_table))
      
      # Colors for risk levels
      colors <- c("High Risk" = "#e74c3c", "Medium Risk" = "#f39c12", "Low Risk" = "#27ae60")
      bar_colors <- unname(colors[risk_counts$RiskLevel])
      bar_colors[is.na(bar_colors)] <- "#95a5a6"  # fallback color
      
      plotly::plot_ly(risk_counts, x = ~RiskLevel, y = ~n, type = "bar",
              marker = list(color = bar_colors),
              text = ~n, textposition = "auto") %>%
        plotly::layout(xaxis = list(title = "Risk Level"), 
               yaxis = list(title = "Count"))
    }, error = function(e) {
      plotly::plot_ly() %>% 
        plotly::add_annotations(text = paste("Error:", e$message), 
                                 x = 0.5, y = 0.5, showarrow = FALSE) %>%
        plotly::layout(title = "Error loading chart")
    })
  })
  
  # Histogram: Credit Score
  output$hist_credit <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~CreditScore, type = "histogram",
            marker = list(color = "#3498db")) %>%
      layout(xaxis = list(title = "Credit Score"),
             yaxis = list(title = "Count"))
  })
  
  # ==================== RISK PAGE ====================
  
  # Bar Chart: Approval by Risk
  output$bar_risk_approval <- renderPlotly({
    tryCatch({
      data <- as.data.frame(filtered_data())
      
      # Find RiskLevel column
      risk_col <- grep("RiskLevel|risk_level", names(data), value = TRUE, ignore.case = TRUE)[1]
      loan_col <- grep("LoanApproved", names(data), value = TRUE, ignore.case = TRUE)[1]
      
      if (is.na(risk_col) || is.na(loan_col)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Missing columns"))
      }
      
      # Calculate approval rate by risk level using base R
      approval_by_risk <- by(data[[loan_col]], data[[risk_col]], function(x) mean(x == 1, na.rm = TRUE) * 100)
      risk_levels <- names(approval_by_risk)
      rates <- as.numeric(approval_by_risk)
      
      colors <- c("High Risk" = "#e74c3c", "Medium Risk" = "#f39c12", "Low Risk" = "#27ae60")
      bar_colors <- unname(colors[risk_levels])
      bar_colors[is.na(bar_colors)] <- "#95a5a6"
      
      df_plot <- data.frame(RiskLevel = risk_levels, rate = rates)
      
      plotly::plot_ly(df_plot, x = ~RiskLevel, y = ~rate, type = "bar",
              marker = list(color = bar_colors),
              text = ~sprintf("%.1f%%", rate), textposition = "auto") %>%
        plotly::layout(xaxis = list(title = "Risk Level"),
               yaxis = list(title = "Approval Rate (%)"))
    }, error = function(e) {
      plotly::plot_ly() %>% plotly::layout(title = paste("Error:", e$message))
    })
  })
  
  # Risk Statistics Table
  output$risk_stats <- renderTable({
    tryCatch({
      data <- as.data.frame(filtered_data())
      
      # Find columns
      risk_col <- grep("RiskLevel|risk_level", names(data), value = TRUE, ignore.case = TRUE)[1]
      credit_col <- grep("CreditScore", names(data), value = TRUE, ignore.case = TRUE)[1]
      income_col <- grep("AnnualIncome", names(data), value = TRUE, ignore.case = TRUE)[1]
      loan_col <- grep("LoanApproved", names(data), value = TRUE, ignore.case = TRUE)[1]
      
      if (is.na(risk_col)) return(data.frame(Message = "RiskLevel column not found"))
      
      # Calculate stats by risk level using base R
      result <- by(data, data[[risk_col]], function(x) {
        data.frame(
          Count = nrow(x),
          ApprovalRate = sprintf("%.1f%%", mean(x[[loan_col]] == 1, na.rm = TRUE) * 100),
          AvgCreditScore = sprintf("%.0f", mean(x[[credit_col]], na.rm = TRUE)),
          AvgIncome = sprintf("$%.0f", mean(x[[income_col]], na.rm = TRUE))
        )
      })
      
      do.call(rbind, result)
    }, error = function(e) {
      data.frame(Error = e$message)
    })
  })
  
  # Box Plot: Credit Score by Approval
  output$box_credit_approval <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~LoanApprovedLabel, y = ~CreditScore, type = "box",
            colors = c("Approved" = "#27ae60", "Rejected" = "#e74c3c")) %>%
      layout(xaxis = list(title = "Loan Status"),
             yaxis = list(title = "Credit Score"))
  })
  
  # Box Plot: Income by Approval
  output$box_income_approval <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~LoanApprovedLabel, y = ~AnnualIncome, type = "box",
            colors = c("Approved" = "#27ae60", "Rejected" = "#e74c3c")) %>%
      layout(xaxis = list(title = "Loan Status"),
             yaxis = list(title = "Annual Income"))
  })
  
  # Bar Chart: Education Impact
  output$bar_edu_approval <- renderPlotly({
    data <- filtered_data()
    edu_approval <- data %>% 
      group_by(EducationLevel) %>% 
      summarise(rate = mean(LoanApproved == 1) * 100) %>%
      arrange(rate)
    
    plot_ly(edu_approval, y = ~EducationLevel, x = ~rate, type = "bar",
            orientation = "h",
            marker = list(color = "#9b59b6"),
            text = ~sprintf("%.1f%%", rate), textposition = "auto") %>%
      layout(yaxis = list(title = "Education Level"),
             xaxis = list(title = "Approval Rate (%)"))
  })
  
  # ==================== HEALTH PAGE ====================
  
  # Box Plot: Expense Ratio
  output$box_expense <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~LoanApprovedLabel, y = ~ExpenseRatio, type = "box",
            colors = c("Approved" = "#27ae60", "Rejected" = "#e74c3c")) %>%
      layout(xaxis = list(title = "Loan Status"),
             yaxis = list(title = "Expense Ratio"))
  })
  
  # Box Plot: Savings Ratio
  output$box_savings <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~LoanApprovedLabel, y = ~SavingsRatio, type = "box",
            colors = c("Approved" = "#27ae60", "Rejected" = "#e74c3c")) %>%
      layout(xaxis = list(title = "Loan Status"),
             yaxis = list(title = "Savings Ratio"))
  })
  
  # Scatter: Asset Coverage vs Health Score
  output$scatter_assets_health <- renderPlotly({
    data <- filtered_data() %>% sample_n(min(5000, n()))
    plot_ly(data, x = ~AssetCoverage, y = ~FinancialHealthScore,
            color = ~LoanApprovedLabel,
            colors = c("Approved" = "#27ae60", "Rejected" = "#e74c3c"),
            type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = "Asset Coverage"),
             yaxis = list(title = "Financial Health Score"))
  })
  
  # Bar: Loan Burden by Risk
  output$bar_burden_risk <- renderPlotly({
    data <- filtered_data()
    burden <- data %>% 
      group_by(RiskLevel) %>% 
      summarise(burden = mean(LoanBurdenRatio, na.rm = TRUE))
    
    plot_ly(burden, x = ~RiskLevel, y = ~burden, type = "bar",
            marker = list(color = c("#e74c3c", "#f39c12", "#27ae60")),
            text = ~sprintf("%.2f", burden), textposition = "auto") %>%
      layout(xaxis = list(title = "Risk Level"),
             yaxis = list(title = "Loan Burden Ratio"))
  })
  
  # Box Plot: DTI
  output$box_dti <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~LoanApprovedLabel, y = ~DebtToIncomeRatio, type = "box",
            colors = c("Approved" = "#27ae60", "Rejected" = "#e74c3c")) %>%
      layout(xaxis = list(title = "Loan Status"),
             yaxis = list(title = "Debt-to-Income Ratio"))
  })
  
  # ==================== ML PAGE ====================
  
  # Model Table
  output$model_table <- renderDT({
    datatable(model_sum, 
              options = list(pageLength = 10, dom = 'tip'),
              rownames = FALSE,
              colnames = c("Model", "Framework", "Accuracy", "ROC-AUC")) %>%
      formatPercentage(c("Accuracy", "ROC_AUC"), 1) %>%
      formatStyle(columns = c("Accuracy", "ROC_AUC"), 
                  backgroundColor = styleInterval(c(0.7, 0.8), c("yellow", "orange", "green")))
  })
  
  # Bar Chart: Accuracy
  output$bar_accuracy <- renderPlotly({
    plot_ly(model_sum, y = ~reorder(Model, Accuracy), x = ~Accuracy, type = "bar",
            orientation = "h",
            marker = list(color = "#3498db"),
            text = ~sprintf("%.1f%%", Accuracy*100), textposition = "auto") %>%
      layout(yaxis = list(title = "Model"),
             xaxis = list(title = "Accuracy"))
  })
  
  # Bar Chart: ROC-AUC
  output$bar_auc <- renderPlotly({
    plot_ly(model_sum, y = ~reorder(Model, ROC_AUC), x = ~ROC_AUC, type = "bar",
            orientation = "h",
            marker = list(color = "#e67e22"),
            text = ~sprintf("%.3f", ROC_AUC), textposition = "auto") %>%
      layout(yaxis = list(title = "Model"),
             xaxis = list(title = "ROC-AUC"))
  })
  
  # Bar Chart: Feature Importance
  output$bar_importance <- renderPlotly({
    feat_top <- feat_imp %>% top_n(15, ImportanceScore)
    plot_ly(feat_top, y = ~reorder(FeatureName, ImportanceScore), 
            x = ~ImportanceScore, type = "bar", orientation = "h",
            marker = list(color = "#8e44ad"),
            text = ~ImportanceScore, textposition = "auto") %>%
      layout(yaxis = list(title = "Feature"),
             xaxis = list(title = "Importance Score"))
  })
  
  # ==================== PREDICTION PAGE ====================
  
  # Prediction logic
  pred_result <- eventReactive(input$predict_btn, {
    # Get input values
    credit_score <- input$pred_creditscore
    annual_income <- input$pred_income
    loan_amount <- input$pred_loanamount
    loan_duration <- input$pred_loanduration
    interest_rate <- input$pred_interestrate
    employment <- input$pred_employment
    education <- input$pred_education
    marital <- input$pred_marital
    dependents <- input$pred_dependents
    monthly_debt <- input$pred_debtpayments
    monthly_savings <- input$pred_savings
    total_assets <- input$pred_assets
    total_liabilities <- input$pred_liabilities
    
    # Calculate derived features (same as training data)
    monthly_income <- annual_income / 12
    monthly_housing <- monthly_income * 0.3  # Estimated
    monthly_food <- monthly_income * 0.1     # Estimated
    monthly_transport <- monthly_income * 0.05  # Estimated
    monthly_healthcare <- monthly_income * 0.03  # Estimated
    total_monthly_expenses <- monthly_housing + monthly_food + monthly_transport + monthly_healthcare + monthly_debt
    
    dti <- monthly_debt / monthly_income
    expense_ratio <- total_monthly_expenses / monthly_income
    savings_ratio <- monthly_savings / monthly_income
    loan_burden <- loan_amount / annual_income
    asset_coverage <- ifelse(total_liabilities > 0, total_assets / total_liabilities, 10)
    net_worth <- total_assets - total_liabilities
    
    # Financial health score calculation (same as training)
    financial_health <- (credit_score / 850) * 0.3 + 
                        (1 - min(dti, 1)) * 0.3 + 
                        (asset_coverage / (asset_coverage + 1)) * 0.2 + 
                        (1 - min(expense_ratio, 1)) * 0.2
    
    # Determine risk level
    if (credit_score < 600 || dti > 0.5) {
      risk_level <- "High Risk"
    } else if (credit_score >= 600 && credit_score <= 700) {
      risk_level <- "Medium Risk"
    } else {
      risk_level <- "Low Risk"
    }
    
    # Simple rule-based prediction (based on feature importance)
    # Top features: CreditScore, DebtToIncomeRatio, FinancialHealthScore, LoanBurdenRatio
    
    score <- 0
    
    # Credit Score contribution (0-40 points)
    if (credit_score >= 750) score <- score + 40
    else if (credit_score >= 700) score <- score + 30
    else if (credit_score >= 650) score <- score + 20
    else if (credit_score >= 600) score <- score + 10
    else score <- score + 0
    
    # DTI contribution (0-25 points) - lower is better
    if (dti < 0.2) score <- score + 25
    else if (dti < 0.3) score <- score + 20
    else if (dti < 0.4) score <- score + 15
    else if (dti < 0.5) score <- score + 10
    else score <- score + 0
    
    # Financial Health contribution (0-20 points)
    if (financial_health > 0.8) score <- score + 20
    else if (financial_health > 0.6) score <- score + 15
    else if (financial_health > 0.4) score <- score + 10
    else score <- score + 5
    
    # Loan Burden contribution (0-15 points) - lower is better
    if (loan_burden < 0.2) score <- score + 15
    else if (loan_burden < 0.3) score <- score + 12
    else if (loan_burden < 0.4) score <- score + 8
    else if (loan_burden < 0.5) score <- score + 4
    else score <- score + 0
    
    # Employment status contribution (0-10 points)
    if (employment == "Employed") score <- score + 10
    else if (employment == "Self-Employed") score <- score + 8
    else if (employment == "Retired") score <- score + 5
    else score <- score + 0
    
    # Calculate probability and prediction
    probability <- score / 110  # Max score is 110
    probability <- min(max(probability, 0.05), 0.95)  # Bound between 5% and 95%
    
    if (score >= 60) {
      prediction <- "APPROVED"
      prob_approved <- probability * 100
    } else {
      prediction <- "REJECTED"
      prob_approved <- probability * 100
    }
    
    # Generate reasons
    reasons <- list()
    
    # Check Credit Score
    if (credit_score < 600) {
      reasons <- c(reasons, paste0("Credit score (", credit_score, ") is below 600 threshold"))
    } else if (credit_score >= 700) {
      reasons <- c(reasons, paste0("Strong credit score (", credit_score, ")"))
    }
    
    # Check DTI
    if (dti > 0.5) {
      reasons <- c(reasons, paste0("High debt-to-income ratio (", sprintf("%.1f", dti * 100), "%)"))
    } else if (dti < 0.3) {
      reasons <- c(reasons, paste0("Healthy DTI ratio (", sprintf("%.1f", dti * 100), "%)"))
    }
    
    # Check Financial Health
    if (financial_health > 0.7) {
      reasons <- c(reasons, paste0("Excellent financial health score (", sprintf("%.2f", financial_health), ")"))
    } else if (financial_health < 0.4) {
      reasons <- c(reasons, paste0("Low financial health score (", sprintf("%.2f", financial_health), ")"))
    }
    
    # Check Loan Burden
    if (loan_burden > 0.4) {
      reasons <- c(reasons, paste0("High loan burden ratio (", sprintf("%.1f", loan_burden * 100), "%)"))
    } else if (loan_burden < 0.25) {
      reasons <- c(reasons, paste0("Manageable loan burden (", sprintf("%.1f", loan_burden * 100), "%)"))
    }
    
    # Check Employment
    if (employment == "Unemployed") {
      reasons <- c(reasons, "Unemployment status")
    } else if (employment == "Employed") {
      reasons <- c(reasons, "Stable employment")
    }
    
    # Check Assets
    if (total_assets > total_liabilities * 2) {
      reasons <- c(reasons, paste0("Strong asset coverage ($", format(total_assets, big.mark = ","), ")"))
    }
    
    list(
      prediction = prediction,
      probability = prob_approved,
      score = score,
      risk_level = risk_level,
      financial_health = financial_health,
      dti = dti,
      expense_ratio = expense_ratio,
      savings_ratio = savings_ratio,
      loan_burden = loan_burden,
      reasons = reasons
    )
  })
  
  # Output prediction result
  output$prediction_result <- renderUI({
    req(pred_result())
    result <- pred_result()
    
    if (result$prediction == "APPROVED") {
      HTML(paste0(
        '<div style="text-align: center; padding: 20px;">',
        '<h2 style="color: #27ae60; font-size: 36px;">✅ APPROVED</h2>',
        '<p style="font-size: 24px;">Confidence: ', sprintf("%.0f", result$probability), '%</p>',
        '<hr>',
        '<p><strong>Risk Level:</strong> ', result$risk_level, '</p>',
        '<p><strong>Credit Score:</strong> ', input$pred_creditscore, '</p>',
        '<p><strong>Loan Amount:</strong> $', format(input$pred_loanamount, big.mark = ","), '</p>',
        '</div>'
      ))
    } else {
      HTML(paste0(
        '<div style="text-align: center; padding: 20px;">',
        '<h2 style="color: #e74c3c; font-size: 36px;">❌ REJECTED</h2>',
        '<p style="font-size: 24px;">Confidence: ', sprintf("%.0f", 100 - result$probability), '%</p>',
        '<hr>',
        '<p><strong>Risk Level:</strong> ', result$risk_level, '</p>',
        '<p><strong>Credit Score:</strong> ', input$pred_creditscore, '</p>',
        '<p><strong>Loan Amount:</strong> $', format(input$pred_loanamount, big.mark = ","), '</p>',
        '</div>'
      ))
    }
  })
  
  # Output risk details
  output$risk_details <- renderUI({
    req(pred_result())
    result <- pred_result()
    
    HTML(paste0(
      '<table style="width: 100%; border-collapse: collapse;">',
      '<tr style="background-color: #f8f9fa;">',
      '<th style="padding: 10px; border: 1px solid #ddd; text-align: left;">Metric</th>',
      '<th style="padding: 10px; border: 1px solid #ddd; text-align: left;">Value</th>',
      '<th style="padding: 10px; border: 1px solid #ddd; text-align: left;">Assessment</th>',
      '</tr>',
      '<tr>',
      '<td style="padding: 10px; border: 1px solid #ddd;">Credit Score</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', input$pred_creditscore, '</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', ifelse(input$pred_creditscore >= 700, "✅ Good", ifelse(input$pred_creditscore >= 600, "⚠️ Fair", "❌ Poor")), '</td>',
      '</tr>',
      '<tr>',
      '<td style="padding: 10px; border: 1px solid #ddd;">Debt-to-Income Ratio</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', sprintf("%.1f", result$dti * 100), '%</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', ifelse(result$dti < 0.3, "✅ Healthy", ifelse(result$dti < 0.5, "⚠️ Moderate", "❌ High")), '</td>',
      '</tr>',
      '<tr>',
      '<td style="padding: 10px; border: 1px solid #ddd;">Financial Health Score</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', sprintf("%.3f", result$financial_health), '</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', ifelse(result$financial_health > 0.7, "✅ Strong", ifelse(result$financial_health > 0.4, "⚠️ Moderate", "❌ Weak")), '</td>',
      '</tr>',
      '<tr>',
      '<td style="padding: 10px; border: 1px solid #ddd;">Loan Burden Ratio</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', sprintf("%.1f", result$loan_burden * 100), '%</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', ifelse(result$loan_burden < 0.3, "✅ Manageable", ifelse(result$loan_burden < 0.5, "⚠️ High", "❌ Very High")), '</td>',
      '</tr>',
      '<tr>',
      '<td style="padding: 10px; border: 1px solid #ddd;">Expense Ratio</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', sprintf("%.1f", result$expense_ratio * 100), '%</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', ifelse(result$expense_ratio < 0.5, "✅ Reasonable", ifelse(result$expense_ratio < 0.8, "⚠️ High", "❌ Very High")), '</td>',
      '</tr>',
      '<tr>',
      '<td style="padding: 10px; border: 1px solid #ddd;">Savings Ratio</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', sprintf("%.1f", result$savings_ratio * 100), '%</td>',
      '<td style="padding: 10px; border: 1px solid #ddd;">', ifelse(result$savings_ratio > 0.2, "✅ Good", ifelse(result$savings_ratio > 0.1, "⚠️ Low", "❌ Very Low")), '</td>',
      '</tr>',
      '</table>',
      '<h4 style="margin-top: 20px;">Decision Factors:</h4>',
      '<ul>',
      paste0('<li>', result$reasons, '</li>', collapse = ''),
      '</ul>'
    ))
  })
  
  # ==================== DATA PAGE ====================
  
  # Data Table
  output$data_table <- renderDT({
    data <- filtered_data() %>% head(1000)
    datatable(data, 
              options = list(
                pageLength = 20,
                scrollX = TRUE,
                dom = 'tip'
              ),
              rownames = FALSE) %>%
      formatStyle(columns = "LoanApprovedLabel",
                  backgroundColor = styleEqual(
                    c("Approved", "Rejected"),
                    c("#27ae60", "#e74c3c")
                  ))
  })
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui, server)