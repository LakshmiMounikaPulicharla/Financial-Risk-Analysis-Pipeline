# R Shiny Dashboard - Deployment Guide

## Overview

This document provides instructions on how to run and deploy the R Shiny dashboard for the Financial Risk Analysis project.

---

## Local Deployment (Ubuntu)

### Method 1: From RStudio

1. Open RStudio
2. File → Open File → Select `R/shiny/app.R`
3. Click **Run App** button (top right of the script window)
4. Dashboard opens in RStudio Viewer or browser

### Method 2: From Terminal

```bash
cd /home/sunoj-9/Documents/datawarehouse
Rscript -e "shiny::runApp('R/shiny/app.R', port=3838)"
```

Then open browser to: `http://localhost:3838`

---

## Online Deployment (shinyapps.io - FREE)

### Prerequisites

1. Create account: https://shinyapps.io (free tier available)
2. Install rsconnect package:
   ```r
   install.packages("rsconnect")
   ```

### Step-by-Step Deployment

#### Step 1: Configure Account

```r
library(rsconnect)
rsconnect::setAccountInfo(name="your_username", 
                         token="your_token", 
                         secret="your_secret")
```

You get these from your shinyapps.io account dashboard → Tokens

#### Step 2: Deploy App

```r
library(shiny)
rsconnect::deployApp("R/shiny/app.R")
```

Or from RStudio:
1. Click **Publish** button
2. Select shinyapps.io
3. Choose application name
4. Click **Publish**

#### Step 3: Access Your Dashboard

After deployment, you'll get a URL like:
```
https://your-username.shinyapps.io/financial-risk-dashboard/
```

Share this URL with anyone - they can view the dashboard online!

---

## Dashboard Features

### Pages

| Page | Features |
|------|----------|
| **Overview** | KPI cards (Approval Rate, Credit Score, Income, Records), Pie chart, Risk distribution, Credit score histogram |
| **Risk Analysis** | Approval by risk, Credit score boxplots, Income boxplots, Education impact |
| **Financial Health** | Expense/Savings ratio boxplots, Asset vs Health scatter, Loan burden bar chart, DTI boxplots |
| **ML Insights** | Model comparison table, Accuracy bar chart, ROC-AUC bar chart, Feature importance |
| **Raw Data** | Interactive data table with filtering (max 1000 rows) |

### Interactivity

- **Filters (Sidebar):** Filter by Risk Level, Education, Employment, Marital Status
- **Reset Button:** Clear all filters
- **Hover:** View exact values on all charts
- **Zoom:** Scroll to zoom on scatter plots
- **Download:** Export data from tables

---

## Troubleshooting

### Issue: App Won't Load

**Solution:** Check data files exist:
```bash
ls -la /home/sunoj-9/Documents/datawarehouse/powerbi/*.csv
```

### Issue: Charts Not Displaying

**Solution:** Check plotly package:
```r
library(plotly)
```

### Issue: Slow Performance

**Solution:** Data is sampled to 100,000 rows. Modify in app.R:
```r
read_csv(..., n_max = 50000)  # Reduce to 50K for faster loading
```

### Issue: Port Already in Use

**Solution:** Change port in run command:
```r
shiny::runApp("R/shiny/app.R", port = 4000)
```

---

## System Requirements

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| R Version | 4.0+ | 4.3+ |
| RAM | 4 GB | 8 GB+ |
| Packages | shiny, shinydashboard, plotly, DT, dplyr, readr | Latest versions |

---

## File Structure

```
datawarehouse/
├── R/
│   ├── shiny/
│   │   └── app.R           ← Main Shiny app
│   ├── 00_setup.R
│   ├── 01_data_loading.R
│   └── ... (other R scripts)
├── powerbi/
│   ├── cleaned_data_for_powerbi.csv
│   ├── risk_level_summary.csv
│   └── ... (other CSVs)
└── run_all.R
```

---

## Quick Start Command

```bash
cd /home/sunoj-9/Documents/datawarehouse
Rscript -e "shiny::runApp('R/shiny/app.R', port=3838, launch.browser=TRUE)"
```

Then open: http://localhost:3838

---

**Generated:** March 2026  
**Project:** Financial Risk Analysis & Loan Approval Prediction  
**Dashboard:** R Shiny + Plotly + DT