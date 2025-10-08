###############################################################################
# Title:        Stats I - Week 4
# Description:  Contingency tables, correlation & bivariate regression
# Author:       Elena Karagianni
# R version:    R 4.4.0
###############################################################################

# -------------------------------#
# 1. Setup
# -------------------------------#

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
lapply(c("readr", "ggplot2", "dplyr", "viridis", "foreign"),  pkgTest)

# Get working directory
getwd()

# Set working directory
setwd("/Users/elkarag/Desktop/Teaching/Applied Stats I/Week 4")

# Agenda
# (0.0) Data-wrangling 
# (a.) Contingency tables
# (b.) Chi-square test
# (c.) Correlation
# (d.) Bivariate regression 

# -------------------------------#
# a. Contingency tables
# -------------------------------#

# Load data 
df_not_tidy <- read.csv("data/movies.csv")

# First step, look at data
View(df_not_tidy)
str(df_not_tidy)
head(df_not_tidy)
summary(df_not_tidy)

# Research question:
# Do different genres receive different critical appreciation?

# Load the tidy version of thedata
# (Prepared using the data_wrangling.R script)
df <- readRDS("data/movies.rds")
str(df)

# First step, look at data
View(df)
class(df$genre)
levels(df$genre)

# --- Contingency table ---
table(df$genre,            # Genre
      df$critics_rating)   # Rating


# ==========================================================
# Subset data: keep only Comedy, Documentary, Drama
# ==========================================================

## Option 1: Base R subsetting
# Dataframe subsetting: df[rows, columns]
df_s <- df[df$genre == "Comedy" |
             df$genre == "Drama" |
             df$genre == "Documentary", ]
View(df_s)


# Step by step
# a) Select column
df$genre  

# b) Logical test
df$genre == "Comedy" | df$genre == "Drama" | df$genre == "Documentary" 

# c) Apply filter
df[df$genre == "Comedy"|df$genre == "Drama"|df$genre == "Documentary", ] 

## Option 2: Using tidyverse-style subset
# Install and load tidyverse
# Adopted from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

df_s <- subset(df, genre %in% c("Comedy", "Documentary", "Drama"))

# Let's check: 
View(df_s)

# More on the 'subset' command
?subset

# Step by step
# a) Select column
df$genre     

# b) Horror in vector? 
"Horror" %in% c("Comedy", "Documentary", "Drama")  

# c) Comedy in vector?
"Comedy" %in% c("Comedy", "Documentary", "Drama")

# d) Row-wise check
df$genre %in% c("Comedy", "Documentary", "Drama")   

# --- Contingency table on subset ---
table(df_s$genre, 
      df_s$critics_rating)

# Problem: Although we filtered our data, the underlying levels still exist.
# We need to use the 'droplevels' function to drop them. 
class(df_s$genre)
levels(df_s$genre)

df_s$genre <- droplevels(df_s$genre)

# Contingency table (after dropping levels)
table(df_s$genre, 
      df_s$critics_rating)

# Add marginal distributions
addmargins(table(df_s$genre,
                 df_s$critics_rating))

# ==========================================================
# Probabilities
# ==========================================================

# Joint probability 
prop.table(table(df_s$genre, 
                 df_s$critics_rating))

# - manual check for 'comedy' genre: which values do we use? 
# [Answer here]

# Interpretation:
# Estimated probability of two specific values co-occurring.

# Probability of a Comedy AND "Rotten"?

# Conditional probability 1.1
# Probability of "Rotten" given Comedy
?prop.table

# Over rows --> Rating conditional on genre
prop.table(table(df_s$genre,           # rows
                 df_s$critics_rating), # columns
           margin = 1) # over rows

# - manual check for 'comedy' AND 'rotten': which values do we use? 
# [Answer here]

# Add marginal distributions (conditional on rows)
addmargins(prop.table(table(df_s$genre, 
                            df_s$critics_rating), 
                      margin = 1)) # over rows

# Round
round(addmargins(prop.table(table(df_s$genre, 
                                  df_s$critics_rating), 
                            margin = 1)), 2)

# To get rid of the Sum-Sum row-column pair:
round(prop.table(table(df_s$genre, df_s$critics_rating), margin = 1), 2)

# Simple rounding example
round(0.72413793, 2)

# Conditional probability 1.2
# Research question: 
# What is the probability of each GENRE given that a movie 
# has a certain CRITICS RATING? (e.g., Rotten)?

# Reminder: 
# prop.table(x, margin = 1): proportions across rows (conditional on row)
# prop.table(x, margin = 2): proportions across columns (conditional on column)

# What do we use here then? 
# Over [answer] --> Genre conditional on [answer]
addmargins(prop.table(table(df_s$genre,           #row
                            df_s$critics_rating), # column
                      margin = 2)) 
                      
# - manual check for 'comedy' given 'rotten' : which values do we use?                       
# [Answer here] 

# ==========================================================
# Visualizing conditional distributions with a bar plot
# ==========================================================
png(filename = "barplot.png",
    width = 600,
    height = 350)
barplot(prop.table(table(df_s$genre, 
                         df_s$critics_rating), margin=1),
        xlab="Ranking",
        ylab="Proportions",
        main="Critics Rating by Genre",
        beside=TRUE,
        legend.text = TRUE,
        args.legend = list(x=12, 
                           y=0.7, 
                           cex = 0.8, 
                           box.col = "white"))
dev.off()


# -------------------------------#
# b. Chi-square test
# -------------------------------#

# Test whether genre and critics rating are independent.
# State the hypotheses: 
# H0 (null): 
# H1 (alt):  

# Run Chi-square test
chi <- chisq.test(df_s$genre, df_s$critics_rating)
chi

# Extract p-value
chi$p.value
sprintf("%.20f", chi$p.value)  # print with high precision

# Step 1: Assumptions
# Step 2: Hypotheses (done)
# Step 3: Test statistic (Pearson chi-square - done)
# Step 4: P-value (done)
# Step 5: Conclusion

# Check residuals to see which cells contribute most
# Residual = (Observed - Expected) / sqrt(Expected)
chi$residuals
?chisq.test  # help page

# -------------------------------#
# c. Correlation
# -------------------------------#

# We go back to our fictional data. 
# Question: Is there an association between education and income?

# Load dataset
df <- read.csv("fictional_data.csv")

# Quick scatter plot
plot(df$income,df$edu)
plot(df$income,df$edu,
     col=df$cap+1) # Color over third variable (+1, because first color in R is white)

# Let's improve this visualization: 
# Add a third variable (cap: 0 = non-capital, 1 = capital city)
# Color: black (0) vs red (1)
plot(df$income, df$edu,
     col = df$cap + 1,     # +1 because color 1 = black, 2 = red
     xlab="Monthly net income (in Euro)",
     ylab="University level education (in years)",
     main="The relationship between education and income")

# Save a nicer plot
png(file = "scatter_plot.png")
plot(df$income, df$edu,
     col = df$cap + 1,
     xlab = "Monthly net income (in Euro)",
     ylab = "University level education (in years)",
     main = "The Relationship between Education and Income")
legend(1000, 8,
       legend = c("Non-capital", "Capital"),
       col = c("black", "red"),
       pch = 1)           # marker type
dev.off()

# Calculate correlation coefficient (Pearson)
cor(df$income, df$edu)

# -> r ranges from -1 to +1: negative, none, or positive linear association

# Add correlation value to scatterplot
plot(df$income, df$edu)
text(1200, 7, sprintf("Correlation = %.4f", cor(df$income, df$edu)))


# -------------------------------#
# d. Bivariate regression
# -------------------------------#

# Question: Is there a relationship between education and income?
# Model: income = b0 + b1 * education + e
summary(lm(df$income ~ df$edu))

# Output interpretation:
#  - Intercept (b0): expected income when education = 0
#  - Slope (b1): average change in income per additional year of education
#  - R-squared: proportion of income variance explained by education