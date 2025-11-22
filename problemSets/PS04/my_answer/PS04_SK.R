#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############################
# Question 1: Economics
############################
data(Prestige)      
?Prestige            

# (a)
# create professional dummy: 1 = professional, 0 = blue/white collar
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
# quick check
table(Prestige$type, Prestige$professional)

# (b)
# linear model with interaction: prestige ~ income * professional
q1_mod <- lm(prestige ~ income * professional, data = Prestige)

# regression output
summary(q1_mod)

# (c)
# look at estimated coefficients
coef(q1_mod)

# store coefficients for later use
b <- coef(q1_mod)
b
# prediction equation:
# y_hat = b0 + b1 * income + b2 * professional + b3 * (income * professional)

# (d)
# marginal effect of income for non-professional jobs (professional = 0)
income_effect_nonprof <- b["income"]
income_effect_nonprof

# marginal effect of income for professional jobs (professional = 1)
income_effect_prof <- b["income"] + b["income:professional"]
income_effect_prof

# (e)
# effect of professional when income = 0
b["professional"]

# (f)
# effect of a 1,000 increase in income for professional jobs
# (professional = 1)
ME_income_1000_prof <- income_effect_prof * 1000
ME_income_1000_prof

# (g)
# effect of switching from non-professional to professional
# when income = 6000
ME_prof_at6000 <- b["professional"] + b["income:professional"] * 6000
ME_prof_at6000

############################
# Question 2: Political Science
############################
# coefficients and standard errors are given in the problem

# (a)
n  <- 131
k  <- 2
df <- n - (k + 1)

beta_treat <- 0.042
se_treat   <- 0.016

# t-test for H0: beta_treat = 0
t_treat <- beta_treat / se_treat
t_treat

# two-sided p-value
p_treat <- 2 * pt(abs(t_treat), df = df, lower.tail = FALSE)
p_treat

# (b)
beta_adj <- 0.042
se_adj   <- 0.013

# t-test for H0: beta_adj = 0
t_adj <- beta_adj / se_adj
t_adj

# two-sided p-value
p_adj <- 2 * pt(abs(t_adj), df = df, lower.tail = FALSE)
p_adj

# (c)
beta_const <- 0.302
se_const   <- 0.011

beta_const
se_const

# (d)
R2 <- 0.094
N  <- 131

R2
N
