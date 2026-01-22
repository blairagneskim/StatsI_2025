###############################################################################
# Title:        Stats I - Week 11
# Description:  Regression Diagnostics
# Author:       Elena Karagianni
# R version:    R 4.5.2
############################################################################### 


# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
    basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
    package.list <- setdiff(package.list, basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
    }
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg,  dependencies = TRUE)
    sapply(pkg,  require,  character.only = TRUE)
    }

# Load any necessary packages
lapply(c("car"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Agenda
# (1) Influential cases/outliers
# (2) OLS assumptions
#     - Normality
#     - Constant variance
#     - Linearity
#     - Multicollinearity 

# Research question: 
# What is the relationship between education and Euroscepticism?

# Load data
df <- read.csv("ess_euroscepticism.csv", row.names=1)
# Note: Changed from "../../datasets/ess_euroscepticism.csv" to local file
# and row.names="X" to row.names=1 (first column)
View(df)

# Convert categorical variables into factor 
df$edu_cat <- factor(df$edu_cat)
df$gndr <- ifelse(df$gndr == 2, 1, 0)
df$gndr <- factor(df$gndr, labels = c("Male", "Female"))
df$brncntr <- ifelse(df$brncntr == 2, 1, 0)
df$brncntr <- factor(df$brncntr, labels = c("Born in country", "Not born in country"))

# Complete case analysis
df_na <- df[complete.cases(df), ] 

# Reset index
rownames(df_na) <- 1:nrow(df_na) 

# Final model
model_final <- lm(euftf_re~eduyrs + 
                           hinctnta + 
                           trstplt + 
                           imwbcnt + 
                           gndr + 
                           agea + 
                           brncntr, data=df_na)
summary(model_final)

# (1) Influential cases/outliers ---------------

### Cook's Distance ###
# Difference in predicted values when observation
# i is included and not included
# Threshold > 4/(n-k-1)

# Get Cook's Distance for all observations
cooks_d <- cooks.distance(model_final)
cooks_d

# Plot 
par(mar=c(5,4,3,3)) # Reset figure margins
plot(model_final, which=4)

# Get top 10 highest Cook's Distance values
head(sort(cooks_d, decreasing=TRUE),10)
  
# Calculate threshold
thres <- 4/(nobs(model_final)-(length(coef(model_final))-1)-1)

# Get observations above threshold
which(sort(cooks_d, decreasing=TRUE)>thres)

# What to do now?
# ANSWER: When you find observations with high Cook's Distance, you need to investigate them systematically:
# 1. Check if they are coding errors (e.g., impossible values like 51 years of education)
# 2. Verify if they are legitimate outliers (extreme but real values)
# 3. Consider if important variables are missing from the model
# 4. Decide on action: correct errors, remove outliers, or keep them if they're valid
#    - For coding errors: correct the data if possible, or recode as missing
#    - For legitimate outliers: consider robust regression or report them separately
#    - For missing variables: consider adding relevant predictors to the model

# Subsetting data frames, df[row,column]
df_na[159,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[159] # Predicted outcome
# Case 159: eduyrs=51 (Z-score=7.53, extremely unusual!), euftf_re=0, predicted=3.43
# This is likely a CODING ERROR - 51 years of education is implausible (should be ~15 or 5)

df_na[458,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[458] # Predicted outcome
# Case 458: eduyrs=35 (Z-score=4.15, very unusual), hinctnta=1 (very low income), 
# euftf_re=10 (very high), predicted=5.34
# This is likely a CODING ERROR - 35 years of education is suspicious

df_na[263,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[263] # Predicted outcome
# Case 263: eduyrs=13 (normal), imwbcnt=10 (very high immigration concerns),
# euftf_re=10 (very high), predicted=2.94, residual=7.06 (LARGEST!)
# This might be a LEGITIMATE OUTLIER - normal X values but extreme Y value
# Could indicate missing variables or a subgroup not well captured by the model

# KEY TAKEAWAYS - Cook's Distance:
# - Measures how much removing an observation changes ALL predicted values
# - Threshold: 4/(n-k-1) - observations above this are influential
# - High Cook's D doesn't mean "delete it" - investigate WHY it's influential
# - Common causes: coding errors, legitimate outliers, omitted variables
# - Action depends on the cause: correct errors, consider robust methods, or add variables

### Difference in betas ####
# Difference in coefficients when observation 
# i is included and not included

# We repeat the same process

# Get DFBeta for all observations
dfbeta <- dfbeta(model_final)
View(dfbeta)

# Print results for some observations
dfbeta[1, c("eduyrs")]
dfbeta[2, c("eduyrs")]
sprintf("%.10f", dfbeta[1, c("eduyrs")])

# Find maximum absolute values for each coefficient 
dfbeta[,c("eduyrs")][which.max(abs(dfbeta[,c("eduyrs")]))]
# Row 159 has max impact on eduyrs coefficient: DFBeta = -0.0069
dfbeta[,c("hinctnta")][which.max(abs(dfbeta[,c("hinctnta")]))]
# Row 458 has max impact on hinctnta coefficient: DFBeta = -0.0052
dfbeta[,c("trstplt")][which.max(abs(dfbeta[,c("trstplt")]))]
# Row 404 has max impact on trstplt coefficient: DFBeta = 0.0056
dfbeta[,c("imwbcnt")][which.max(abs(dfbeta[,c("imwbcnt")]))]
# Row 344 has max impact on imwbcnt coefficient: DFBeta = 0.0074

# What to do now?
# ANSWER: DFBeta helps identify which observations are "pulling" specific coefficients
# in different directions. If removing one observation changes a coefficient from
# significant to non-significant (or vice versa), that's a red flag.
# 1. Check if the observation is a coding error (like cases 159 and 458 with extreme education)
# 2. Consider if the coefficient is unstable due to multicollinearity
# 3. If the DFBeta is large relative to the coefficient itself, the model may be sensitive
# 4. Compare with Cook's Distance - observations affecting both predictions AND coefficients
#    are particularly problematic
# 5. Consider robust regression methods if many observations have large DFBeta values

# Subsetting data frames, df[row,column]
df_na[404,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[404] # Predicted outcome
# Case 404: Has largest impact on trstplt (trust in politics) coefficient

df_na[344,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[344] # Predicted outcome
# Case 344: Has largest impact on imwbcnt (immigration concerns) coefficient

# KEY TAKEAWAYS - DFBeta:
# - Measures how much each COEFFICIENT (β) changes when you remove observation i
# - Unlike Cook's Distance (which looks at predictions), DFBeta looks at coefficient stability
# - Large DFBeta for a specific variable means that observation is "pulling" that coefficient
# - If removing one case changes significance of a coefficient, investigate carefully
# - Compare DFBeta values to the coefficient size - small changes are usually fine
# - Use alongside Cook's Distance for comprehensive diagnostic assessment

### Leverage versus residual plot ###
# Leverage: Unusual value on X
# Discrepancy: Unusual value on Y, given value on X
# Influence = Leverage x Discrepancy
# --> unusual value on X and Y 

# Plot 
plot(model_final, which=5)

# Look at case with very high leverage out of curiosity
# but has low discrepancy, so it is not an influential case
which(hatvalues(model_final)>0.13)
# Row 352 has leverage > 0.13 (very high leverage)

df_na[352,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[352] # Predicted outcome
# Case 352: Has unusual predictor values (high leverage) but the model predicts it well
# (low discrepancy), so it's NOT influential - it doesn't affect the regression line much

# What to do now?
# ANSWER: The leverage vs residual plot (which=5) shows the relationship between:
# - Leverage (unusual X values) on X-axis
# - Standardized residuals (unusual Y given X) on Y-axis
# - Contour lines showing Cook's Distance thresholds
# 
# Interpretation:
# 1. Top-right or top-left: High leverage + High residual = INFLUENTIAL (problematic)
#    - These observations can significantly change the regression line
#    - Investigate for coding errors or consider robust methods
# 2. Top-center: High leverage + Low residual = NOT influential (OK)
#    - Unusual X values, but model predicts them well
#    - These don't affect the regression line much
# 3. Bottom: Low leverage = Not influential regardless of residual
#    - Even large residuals don't affect the regression line if leverage is low
# 
# For high-leverage cases (like row 352), check if they're:
# - Coding errors (impossible values)
# - Legitimate extreme values (keep them)
# - Missing important variables (consider adding predictors)

# KEY TAKEAWAYS - Leverage vs Residual Plot:
# - Leverage = unusual values on X (predictors) - measures how far from mean of X
# - Discrepancy = unusual values on Y given X (large residuals)
# - Influence = Leverage × Discrepancy - both must be high for an observation to be influential
# - High leverage alone is NOT bad - it's when combined with high discrepancy that problems arise
# - The plot (which=5) shows standardized residuals vs leverage with Cook's D contours
# - Points outside the 0.5 and 1.0 Cook's D contours are highly influential
# - Use this plot to visually identify which observations need investigation

# (2) OLS assumptions ---------------

### Normality ###
# The error is normally distributed 

# Histogram of error
hist(model_final$residuals)
# Check: Should be roughly bell-shaped and centered at 0
# Look for: Strong skewness, extreme outliers, or bimodal distributions

# QQ (Quantile-quantile) plot
plot(model_final, which=2)
# Check: Points should follow the diagonal line
# Deviations indicate:
# - Points above line = heavier tails (more extreme values than expected under normality)
# - Points below line = lighter tails (fewer extreme values)
# - S-shaped curve = skewed distribution
# - Curved pattern = non-normal distribution

# KEY TAKEAWAYS - Normality Assumption:
# - Assumption: The error term (ε) is normally distributed
# - Why it matters: Needed for valid hypothesis tests (t-tests, F-tests) and confidence intervals
# - With large samples (n > 30-50), this is less critical due to Central Limit Theorem
# - Perfect normality is rare in real data - focus on SEVERE violations
# - What to look for: Strong skew, extreme outliers, clear non-normal patterns
# - What to do if violated:
#   - For large samples: Usually OK, slight violations are acceptable
#   - For small samples: Consider transformations or robust methods
#   - For severe violations: Consider non-parametric alternatives or robust regression
# - Mean of residuals should be ≈ 0 (expected), SD tells you about spread

### Constant variance ###
# The error has a constant variance (homoscedasticity)

# Residual versus fitted plot
plot(model_final, which=1)
# Check: Should show random scatter around 0 with constant spread
# Good pattern: Random cloud centered at 0, no systematic pattern
# Bad patterns:
#   - Funnel shape: Variance increases/decreases with fitted values (heteroscedasticity)
#   - U-shape or curve: Non-linear relationship (violates linearity too)
#   - Fanning out: Variance increases with fitted values
#   - Systematic patterns: Suggest model misspecification

# What to do if labels of observations are overlapping?
which(model_final$residuals>6.35 & model_final$fitted.values<4.5)

# Option 1: 
overlapping <- which(model_final$residuals>6.35 & model_final$fitted.values<4.5)
plot(model_final, which=1, id.n=0)  # Turn off automatic labels
text(model_final$fitted.values[overlapping], 
     model_final$residuals[overlapping], 
     labels=overlapping, pos=3, cex=0.7)

# Option 2: 
plot(model_final, which=1)
identify(model_final$fitted.values, model_final$residuals, 
         labels=rownames(df_na))
# click on points, then press ESC when done

# KEY TAKEAWAYS - Constant Variance (Homoscedasticity):
# - Assumption: The variance of errors is constant across all values of X
# - Homoscedasticity = constant variance (good)
# - Heteroscedasticity = non-constant variance (bad)
# - Why it matters: Violations lead to inefficient estimates and wrong standard errors
# - The residual vs fitted plot (which=1) is the main diagnostic tool
# - Should look like a "random cloud" centered at 0 with constant spread
# - Any systematic pattern (funnel, fanning, curves) suggests a problem
# - What to do if violated:
#   - Consider transformations (log, square root)
#   - Use robust standard errors (heteroscedasticity-consistent)
# - Note: Slight violations are common and often acceptable

### Linearity ###
# The effect between X and Y is linear

# Scatter plots 
plot(df_na$eduyrs,jitter(df_na$euftf_re,2))
plot(df_na$hinctnta,jitter(df_na$euftf_re,2))
plot(df_na$trstplt,jitter(df_na$euftf_re,2))
plot(df_na$imwbcnt,jitter(df_na$euftf_re,2))
plot(df_na$agea,jitter(df_na$euftf_re,2))

# Residual plot
residualPlots(model_final)

# Add a quadratic term for trust in politics
df_na$trstplt_trstplt <- df_na$trstplt^2

# Fit model
model_quad <- lm(euftf_re~eduyrs + 
                   hinctnta + 
                   trstplt + 
                   trstplt_trstplt +
                   imwbcnt +         
                   gndr + 
                   agea + 
                   brncntr, data=df_na)
summary(model_quad)

# Compare residual plot for quadratic model
residualPlots(model_quad)

# We might also want to log-transform education years. 
# This variable is right/positively skewed. 
hist(df_na$eduyrs) 

# Log-transform education years
# +1 because log(0) = -Inf
hist(log(df_na$eduyrs+1)) 
min(df_na$eduyrs)
log(0)

# Fit model
model_log <- lm(euftf_re~log(eduyrs+1) + 
                         hinctnta + 
                         trstplt + 
                         imwbcnt +         
                         gndr + 
                         agea + 
                         brncntr, data=df_na)
summary(model_log)

# Compare residual plot for log model
residualPlots(model_log)

# But be careful, if we transform X we need to 
# adjust interpretation. There is a trade-off between
# fit and interpretability.
# ANSWER: When you transform variables, interpretation changes:
# - Log transformation: "A 1% increase in X leads to approximately β change in Y"
# - Quadratic terms: "The effect of X depends on the level of X" (non-linear relationship)
# - Always consider: Better fit vs. harder interpretation
# - Report both transformed and original scale if possible

# KEY TAKEAWAYS - Linearity Assumption:
# - Assumption: The relationship between X and Y is linear
# - Why it matters: OLS assumes linear relationships - violations lead to biased estimates
# - Diagnostic tools:
#   - Scatter plots: Visual check of X vs Y relationships
#   - Residual plots (residualPlots()): More formal test - should show random scatter
# - What to look for: Curves, U-shapes, or systematic patterns in residual plots
# - Solutions if violated:
#   - Add polynomial terms (e.g., X²) for non-linear relationships
#   - Transform variables (log, square root) for skewed relationships
#   - Use splines or other flexible functional forms
# - Trade-off: Better fit vs. harder interpretation
# - Always check residual plots after transformations to see if they improved
# - Remember: Perfect linearity is rare - focus on severe violations 

### Multicollinearity ###
# Independent variables are strongly correlated

# Correlation matrix
cor(df_na[, c("eduyrs","hinctnta","trstplt","imwbcnt","agea")])

# Variance Inflation Factor
vif(model_final)

# Create a variable with high correlation
cor(df_na$trstplt,df_na$imwbcnt)
df_na$trust_att <- df_na$trstplt + df_na$imwbcnt
cor(df_na$trust_att,df_na$trstplt)
cor(df_na$trust_att,df_na$imwbcnt)

# Refit model with highly correlated variables
model_collin <- lm(euftf_re~eduyrs + 
                   hinctnta + 
                   trstplt + 
                   imwbcnt +
                   trust_att, data=df_na)
summary(model_collin)
# Notice: trust_att coefficient shows as NA (not defined)
# This is because trust_att is a perfect linear combination of trstplt and imwbcnt
# R automatically drops one variable due to "singularity" or "perfect multicollinearity"
# VIF cannot be calculated for this model (error: "aliased coefficients")

# KEY TAKEAWAYS - Multicollinearity:
# - Assumption: Independent variables should not be too highly correlated
# - Why it matters: High correlation makes it hard to separate effects of predictors
#   - Coefficients become unstable (small data changes → large coefficient changes)
#   - Standard errors inflate (harder to find significant effects)
#   - Can't determine which variable is driving the relationship
# - Diagnostic tools:
#   - Correlation matrix: Check pairwise correlations (watch for r > 0.7-0.8)
#   - Variance Inflation Factor (VIF): More comprehensive measure
# - VIF interpretation:
#   - VIF = 1: No multicollinearity
#   - VIF > 1: Some multicollinearity
#   - VIF > 5: Moderate multicollinearity (concerning)
#   - VIF > 10: Severe multicollinearity (problematic)
# - In this model: All VIF values < 1.26, so multicollinearity is NOT a problem
# - Perfect multicollinearity: One variable is a linear combination of others
#   - R automatically drops redundant variables (shows as NA)
#   - Example: trust_att = trstplt + imwbcnt creates perfect multicollinearity
# - What to do if multicollinearity is high:
#   - Remove redundant variables (keep the most theoretically important)
#   - Combine highly correlated variables into a single index
# - Remember: Some correlation is normal and expected (e.g., education and income)
# - Focus on severe multicollinearity (VIF > 10) or perfect multicollinearity

###############################################################################
# SUMMARY: REGRESSION DIAGNOSTICS CHECKLIST
###############################################################################

# DIAGNOSTIC CHECKLIST FOR OLS ASSUMPTIONS:

# 1. INFLUENTIAL CASES/OUTLIERS:
#    ✓ Check Cook's Distance (threshold: 4/(n-k-1))
#    ✓ Check DFBeta for coefficient stability
#    ✓ Check leverage vs residual plot (which=5)
#    ✓ Investigate high-leverage cases
#    → Action: Correct coding errors, consider robust methods, or add variables

# 2. NORMALITY:
#    ✓ Histogram of residuals (should be bell-shaped, centered at 0)
#    ✓ Q-Q plot (which=2) - points should follow diagonal line
#    → Action: Less critical with large samples; consider transformations if severe

# 3. CONSTANT VARIANCE (HOMOSCEDASTICITY):
#    ✓ Residuals vs Fitted plot (which=1) - should be random scatter
#    → Action: Use robust standard errors or transformations if violated

# 4. LINEARITY:
#    ✓ Scatter plots of X vs Y
#    ✓ Residual plots (residualPlots()) - should show random scatter
#    → Action: Add polynomial terms or transform variables if non-linear

# 5. MULTICOLLINEARITY:
#    ✓ Correlation matrix (watch for r > 0.7-0.8)
#    ✓ VIF (watch for VIF > 5-10)
#    → Action: Remove redundant variables or use regularization if severe

# KEY PRINCIPLES:
# - Diagnostics are tools, not rules - use judgment
# - No model is perfect - focus on severe violations
# - Context matters - what's acceptable depends on your research question
# - Large samples are more robust to assumption violations
# - Always investigate influential cases - don't automatically delete them

###############################################################################


