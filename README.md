# Liquidity Risk Modeling: Forecasting Bank Deposits with Multiple Regression

## 🎯 Project Objective

This project focuses on modeling **Liquidity Risk** by analyzing and forecasting the behavior of bank deposit products (Demand, Time, and Traditional Deposits) from various financial institutions. The objective is to construct robust predictive models using **Multiple Linear Regression (MLR)** to quantify the impact of macroeconomic factors on key liquidity metrics.

This analysis provides critical insight for cash flow planning, interest rate risk management, and regulatory reporting (e.g., liquidity coverage ratios).

## 🛠️ Key Methodologies and R Packages Utilized

The entire analysis was implemented in R, emphasizing statistical discipline and modeling expertise:

* **Multiple Linear Regression (`lm`):** Utilized as the primary predictive tool to establish the relationship between numerous macroeconomic variables and bank deposit volumes.
* **Model Assumptions Validation:** Critical statistical validation was performed by applying the **Kolmogorov-Smirnov Test (`ks.test`)** on model residuals to verify the assumption of normality. This ensures the reliability and accuracy of the regression estimates.
* **R Packages:** Proficiency demonstrated with **`readxl`**, **`ggplot2`** (for visualization), and **`broom`** (for tidying model output).

## 📊 Core Variables Modeled

The model predicts the percentage change in various bank deposits based on:

* **Dependent Variables:** Percentage Change in Demand, Time, and Traditional Deposits (e.g., `Depositos_Vista_TBM_ln`).
* **Key Predictors:** A wide array of macroeconomic time-series data, including Inflation, Unemployment Rate, TIIE (Interbank Interest Rate), GDP, various consumer confidence indices, and exchange rate metrics.

## ✨ Value Proposition for the Analyst Role

* **Expertise in Liquidity Risk:** Direct, proven experience in a specialized area of financial risk management.
* **Statistical Rigor:** Demonstrated discipline in building and formally validating predictive models using advanced statistical testing (Kolmogorov-Smirnov).
* **R Proficiency for Financial Modeling:** Confirmed ability to use R for production-grade financial and statistical analysis, complementing Python skills.
