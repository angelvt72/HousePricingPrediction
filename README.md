# Idealista House Price Analyzer

## 📖 Overview
Professional Shiny application for real estate market analysis in Spain's Murcia region. Provides price predictions, profitability insights, and interactive visualizations using machine learning models and geospatial data.

## 🚀 Key Features

### 🔮 Price Prediction Engine
- **Dual Prediction Models**  
  Ridge Regression algorithms for:
  - Property purchase/sale prices
  - Monthly rental valuations
- **Investment Analysis**  
  - Automated annual gross yield calculations
  - ROI timeline estimation

### 🌍 Geographic Insights
- **Interactive Heatmaps**  
  Visualize profitability hotspots using Leaflet
- **Location Intelligence**  
  - Zone comparison tools
  - Customizable area filters
  - Demographic overlay support

### 📊 Analytics Dashboard
- **Four Integrated Modules**  
  1. **Market Trends** - Regional price developments  
  2. **Zone Diagnostics** - Area-specific metrics  
  3. **Comparative Analysis** - Cross-location benchmarking  
  4. **Price Estimator** - Custom property valuation  

## ⚙️ Technical Setup

### Requirements
- **R Environment** (v4.0+ recommended)
- **Essential Packages**:
  ```r
  install.packages(c("shiny", "shinydashboard", "leaflet", "dplyr", 
                    "ggplot2", "plotly", "caret", "glmnet", "sf", 
                    "rmapshaper"))
