# =============================================================
#   CREDIT RISK ANALYSIS — LOGISTIC REGRESSION 
#   Dataset: German Credit Data
#   MSc Statistics Project
# =============================================================

# ─────────────────────────────────────────
# STEP 0: Install & Load Required Packages
# ─────────────────────────────────────────
# Run this once if packages are not installed:
# install.packages(c("readxl", "ggplot2", "dplyr", "caret", "lmtest", "car"))

library(readxl)
library(ggplot2)
library(dplyr)
library(lmtest)   # for lrtest (Likelihood Ratio Test)
library(car)      # for Wald Test / vif


# ─────────────────────────────────────────
# STEP 1: Load Dataset
# ─────────────────────────────────────────
df <- read_excel("C:/Users/Sedna/OneDrive/Music/Desktop/my project/german_credit_data project sem 2.xlsx")

# Remove the index column
df <- df[ , -1]

# View structure
str(df)
head(df)
summary(df)
View(df)

# ─────────────────────────────────────────
# STEP 2: EDA — Distribution of Risk (YOUR SHARE)
# ─────────────────────────────────────────

# Count of Good vs Bad
risk_counts <- table(df$Risk)
print(risk_counts)

# Bar Chart
ggplot(df, aes(x = Risk, fill = Risk)) +
  geom_bar(width = 0.5, color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("good" = "#2ecc71", "bad" = "#e74c3c")) +
  labs(
    title = "Distribution of Credit Risk",
    subtitle = "German Credit Dataset (n = 1000)",
    x = "Risk Category",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Pie Chart (optional)
pie(risk_counts, 
    labels = paste(names(risk_counts), "\n", risk_counts, 
                   "(", round(100 * risk_counts / sum(risk_counts), 1), "%)"),
    col = c("#e74c3c", "#2ecc71"),
    main = "Credit Risk Distribution")


# ─────────────────────────────────────────
# STEP 3: Data Preprocessing
# ─────────────────────────────────────────

# 3a. Convert Risk to binary: good = 0, bad = 1
df$Risk <- ifelse(df$Risk == "bad", 1, 0)
df$Risk <- as.factor(df$Risk)
table(df$Risk)  # check: 0 = good (700), 1 = bad (300)

# 3b. Handle Missing Values
# Saving accounts: 183 missing → fill with "unknown"
df$`Saving accounts`[is.na(df$`Saving accounts`)] <- "unknown"

# Checking account: 394 missing → fill with "unknown"
df$`Checking account`[is.na(df$`Checking account`)] <- "unknown"

# Verify no more missing values
colSums(is.na(df))

# 3c. Convert categorical variables to factors
df$Sex              <- as.factor(df$Sex)
df$Housing          <- as.factor(df$Housing)
df$Purpose          <- as.factor(df$Purpose)
df$`Saving accounts`   <- as.factor(df$`Saving accounts`)
df$`Checking account`  <- as.factor(df$`Checking account`)
df$Job              <- as.factor(df$Job)

# 3d. Rename columns with spaces for easier handling in R
colnames(df) <- c("Age", "Sex", "Job", "Housing", 
                  "Saving_accounts", "Checking_account",
                  "Credit_amount", "Duration", "Purpose", "Risk")

# Check final structure
str(df)


# ─────────────────────────────────────────
# STEP 4: Fit FULL Logistic Regression Model
# ─────────────────────────────────────────

model_full <- glm(Risk ~ ., 
                  data = df, 
                  family = binomial(link = "logit"))

summary(model_full)

# Wald Test p-values are shown in summary()
# Significant predictors: those with p < 0.05 (marked with * or **)


# ─────────────────────────────────────────
# STEP 5: Wald Test (Formal)
# ─────────────────────────────────────────

# The summary() already gives Wald z-statistics and p-values.
# For a formal Wald Test table:
cat("\n===== WALD TEST RESULTS (Full Model) =====\n")
coef_summary <- summary(model_full)$coefficients
print(round(coef_summary, 4))

# Identify significant variables (p < 0.05)
significant <- rownames(coef_summary)[coef_summary[, 4] < 0.05]
cat("\nSignificant Predictors (p < 0.05):\n")
print(significant)


# ─────────────────────────────────────────
# STEP 6: Fit REDUCED / FINAL Model
#         (Keep only significant variables)
# ─────────────────────────────────────────

# Based on Wald test results, we keep the significant variables.
# Common significant variables in German Credit Data:
# Duration, Credit_amount, Age, Checking_account, Saving_accounts

model_final <- glm(Risk ~ Duration + Credit_amount + Age + 
                     Checking_account + Saving_accounts,
                   data = df,
                   family = binomial(link = "logit"))

summary(model_final)

# Likelihood Ratio Test: Compare Full vs Reduced
lrtest(model_full, model_final)
# If p > 0.05 → reduced model is adequate (no significant loss of fit)


# ─────────────────────────────────────────
# STEP 7: Final Model Equation
# ─────────────────────────────────────────

cat("\n===== FINAL MODEL COEFFICIENTS =====\n")
print(round(coef(model_final), 4))

# The logit equation:
# log(p / 1-p) = β0 + β1*Duration + β2*Credit_amount + β3*Age + ...
# where p = P(Risk = bad | X)


# ─────────────────────────────────────────
# STEP 8: Odds Ratios (e^β)
# ─────────────────────────────────────────

cat("\n===== ODDS RATIOS (exp(β)) WITH 95% CI =====\n")
odds_ratios <- exp(cbind(OR = coef(model_final), 
                         confint(model_final)))
print(round(odds_ratios, 4))

# Interpretation Guide:
# OR > 1 → increases odds of being BAD risk
# OR < 1 → decreases odds of being BAD risk
# e.g., Duration OR = 1.03 means each extra month increases 
#        odds of default by 3%


# ─────────────────────────────────────────
# STEP 9: Odds Ratio Plot
# ─────────────────────────────────────────

or_df <- as.data.frame(odds_ratios)
or_df$Variable <- rownames(or_df)
or_df <- or_df[or_df$Variable != "(Intercept)", ]
colnames(or_df) <- c("OR", "Lower", "Upper", "Variable")

ggplot(or_df, aes(x = reorder(Variable, OR), y = OR)) +
  geom_point(size = 3, color = "#2c3e50") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                width = 0.2, color = "#2c3e50") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Odds Ratios with 95% Confidence Intervals",
    subtitle = "Final Logistic Regression Model",
    x = "Predictor Variables",
    y = "Odds Ratio (exp(β))"
  ) +
  theme_minimal(base_size = 13)

# ─────────────────────────────────────────
# STEP 10: Model Evaluation
# ─────────────────────────────────────────
# Predicted probabilities
pred_prob <- predict(model_final, type = "response")

# Classify: prob > 0.5 → Bad (1), else Good (0)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# Confusion Matrix (base R)
conf_matrix <- table(Predicted = pred_class, Actual = df$Risk)
print(conf_matrix)

# Calculate metrics manually
TP <- conf_matrix[2,2]  # predicted bad, actually bad
TN <- conf_matrix[1,1]  # predicted good, actually good
FP <- conf_matrix[2,1]  # predicted bad, actually good
FN <- conf_matrix[1,2]  # predicted good, actually bad

Accuracy    <- (TP + TN) / sum(conf_matrix)
Sensitivity <- TP / (TP + FN)
Specificity <- TN / (TN + FP)

cat("===== MODEL PERFORMANCE =====\n")
cat("Accuracy    :", round(Accuracy, 4), "\n")
cat("Sensitivity :", round(Sensitivity, 4), "\n")
cat("Specificity :", round(Specificity, 4), "\n")




# ─────────────────────────────────────────
# STEP 11: ROC Curve & AUC
# ─────────────────────────────────────────

# install.packages("pROC") # run once
library(pROC)

roc_obj <- roc(df$Risk, pred_prob)
auc_val <- auc(roc_obj)
cat("\nAUC:", round(auc_val, 4), "\n")

# Plot ROC curve
plot(roc_obj, 
     col = "#e74c3c", lwd = 2,
     main = paste("ROC Curve — Logistic Regression\nAUC =", round(auc_val, 4)))
abline(a = 0, b = 1, lty = 2, col = "gray")


# ─────────────────────────────────────────
# STEP 12: Predicted Probability Distribution
# ─────────────────────────────────────────

df$pred_prob <- pred_prob

ggplot(df, aes(x = pred_prob, fill = Risk)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity", color = "white") +
  scale_fill_manual(values = c("0" = "#2ecc71", "1" = "#e74c3c"),
                    labels = c("Good", "Bad")) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black") +
  labs(
    title = "Predicted Probability of Default",
    x = "Predicted Probability (P(Bad))",
    y = "Count",
    fill = "Actual Risk"
  ) +
  theme_minimal(base_size = 13)


