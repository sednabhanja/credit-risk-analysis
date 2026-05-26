# Credit Risk Analysis — Logistic Regression
## MSc Statistics Project

## Overview
This project analyzes credit risk using the German Credit Dataset (n=1000)
using Logistic Regression to predict loan default probability.

## Dataset
- Source: German Credit Dataset
- Observations: 1000
- Variables: 10 (9 predictors + 1 target)
- Target: Risk (0 = Good, 1 = Bad)

## Files
- logistic_regression_credit_risk.R — Main R code
- cleaned_credit_data_FINAL.xlsx — Cleaned dataset
- Credit_Risk_Logistic_Regression_Report.docx — Full report

## Results
- Accuracy : 74.1%
- AUC      : 0.7636
- Sensitivity : 37.67%
- Specificity : 89.71%

## Key Findings
- Duration is the strongest predictor of credit risk
- Higher checking/saving account balance reduces default risk
- Older applicants are less likely to default

## Tools Used
- Language: R
- Packages: readxl, ggplot2, dplyr, lmtest, car, pROC
