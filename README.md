# Financial Risk Analysis & Loan Approval Prediction

## Project Overview
This project focuses on analyzing financial data to assess risk levels and support loan approval decisions. It includes exploratory data analysis (EDA), KPI computation, and an interactive dashboard built using R Shiny.

The goal is to derive **data-driven insights** and visualize key financial indicators to assist in risk assessment and decision making.

---

## Objectives
- Analyze financial datasets to identify risk patterns
- Perform exploratory data analysis and outlier detection
- Compute key performance indicators (KPIs)
- Build an interactive dashboard for visualization
- Support data-driven decision making in financial risk analysis

---

## Dashboard Features

### Pages

| Page | Features |
|------|----------|
| **Overview** | KPI cards, risk distribution, credit score analysis |
| **Risk Analysis** | Approval by risk level, credit score and income analysis |
| **Financial Health** | Expense ratio, loan burden, asset vs financial health |
| **ML Insights** | Model comparison, accuracy, ROC-AUC, feature importance |
| **Raw Data** | Interactive data table with filtering |

---

## Workflow

### 1. Data Processing
- Data cleaning and preprocessing
- Handling missing values and inconsistencies
- Feature selection and transformation

### 2. Exploratory Data Analysis (EDA)
- Distribution analysis
- Correlation analysis
- Outlier detection
- Statistical visualizations

### 3. Dashboard Development
- Built interactive dashboard using R Shiny
- Integrated Plotly for dynamic visualizations
- Added filters and interactivity for better insights

---

## Technologies Used

- R
- Shiny
- Plotly
- DT (Data Tables)
- dplyr, readr
- Data Visualization

---

## Project Structure

R/
├── shiny/
│ └── app.R # Main Shiny dashboard
├── 00_setup.R
├── 01_data_loading.R
├── ...
powerbi/
├── cleaned_data_for_powerbi.csv
├── risk_level_summary.csv
├── ...
run_all.R
README.md


---

## Running the Dashboard

### Run Locally
```bash
Rscript -e "shiny::runApp('R/shiny/app.R', port=3838)"
Open in browser: http://localhost:3838
