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
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# QUESTUON 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# [1] Find a 90\% CI(confidence interval) for the average student IQ in the school
mean_y <- mean(y) # sample mean 
n <- length(y)    # sample size == 25
s <- sd(y)        # sample standard deviation
SE <- s/sqrt(n)   # standard error of the mean
alpha <- 0.10
df<- n-1          # degrees of freedom == 24

# 90% two-sided CI: t_{1 - alpha/2, df}
tcrit_1 <- qt(1 - alpha/2, df) #two-sided 90% CI -> qt(0.95.24)
tcrit_1           # == 1.710882

ci_90_lower <- mean_y - tcrit_1 * SE
ci_90_upper <- mean_y + tcrit_1 * SE

cat("90% CI = (",
    round(ci_90_lower, 2), ",", round(ci_90_upper, 2), ")\n")

print(round(mean_y,2))
print(round(n,2))
print(round(s,2))
print(round(SE, 2))
print(round(tcrit_1,2))
print(round(ci_90_lower,2))
print(round(ci_90_upper,2))


#[2] Next, the school counselor was curious whether the average student IQ in her school
# is higher than the average IQ score (100) among all the schools in the country.
# Using the same sample, conduct the appropriate hypothesis test with α= 0.05.

mu0 <- 100        # H0: mu=100, H1: mu > 100
t_stat <- (mean_y - mu0) / SE
t_crit <- qt(0.95, df) # because alpha is 0.05
p_value <- 1 - pt(t_stat,df)

# --- RESULT ---
print(round(n, 2))
print(round(mean_y, 2))
print(round(s, 2))
print(round(SE, 2))
print(round(t_stat, 2))
print(round(t_crit, 2))
print(round(p_value, 2))

#another option) using R funtion "t.test"
t.test(y, mu = 100, alternative = "greater")

#####################
# Problem 2
#####################
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
expenditure

#####################
# Q1.Relationships among Y, X1, X2, and X3
#####################

#Select the relevant variables
vars <- expenditure[, c("Y", "X1", "X2", "X3")]

#(1) Pairwise scatterplot matrix
pdf("q2_pairs.pdf", width = 7, height = 7)
pairs(vars, main = "Pairwise Plots: Y, X1, X2, X3")
dev.off()

#(2) Scatterplots: Y vs each X
pdf("q2_Y_vs_Xs.pdf", width = 9, height = 3)
op <- par(mfrow = c(1,3))
plot(vars$X1, vars$Y, xlab="X1", ylab="Y", main="Y vs X1", pch=19, col="gray40")
plot(vars$X2, vars$Y, xlab="X2", ylab="Y", main="Y vs X2", pch=19, col="gray40")
plot(vars$X3, vars$Y, xlab="X3", ylab="Y", main="Y vs X3", pch=19, col="gray40")
par(op)
dev.off()

#(3) Correlation matrix
cor_mat <- cor(vars, use = "pairwise.complete.obs", method = "pearson")
round(cor_mat, 3)

cat("corr(Y, X1) =", round(cor_mat["Y","X1"], 3), "\n")
cat("corr(Y, X2) =", round(cor_mat["Y","X2"], 3), "\n")
cat("corr(Y, X3) =", round(cor_mat["Y","X3"], 3), "\n")

#####################
# Q2 Y vs Region
#####################
expenditure$Region <- factor(expenditure$Region)
# Boxplot
pdf("q2_Y_by_region.pdf", width=6, height=6)
boxplot(Y ~ Region, data=expenditure,
        main="Per-capita Expenditure (Y) by Region",
        xlab="Region", ylab="Y (per capita)")
region_means <- tapply(expenditure$Y, expenditure$Region, mean, na.rm = TRUE)
points(1:length(region_means), region_means, col="red", pch=19)
dev.off()

# Print region means
print(round(region_means,2))

#####################
# Q2 (3) Y vs X1 (+ Region)
#####################

# Scatterplot Y vs X1
pdf("q2_Y_vs_X1.pdf", width=6, height=6)
plot(expenditure$X1, expenditure$Y,
     main="Y vs X1",
     xlab="Per-capita personal income (X1)",
     ylab="Per-capita expenditure (Y)",
     pch=19, col="gray40")
abline(lm(Y ~ X1, data=expenditure), lty=2, lwd=2)
dev.off()

# Scatterplot Y vs X1 by Region
pdf("q2_Y_vs_X1_by_region.pdf", width=6.5, height=6)
cols <- c("blue", "orange", "green", "purple")
pchv <- c(16, 17, 15, 18)
plot(expenditure$X1, expenditure$Y,
     main="Y vs X1 by Region",
     xlab="Per-capita personal income (X1)",
     ylab="Per-capita expenditure (Y)",
     col=cols[as.integer(expenditure$Region)],
     pch=pchv[as.integer(expenditure$Region)])
abline(lm(Y ~ X1, data=expenditure), lty=2, lwd=2)
legend("bottomright",
       legend=levels(expenditure$Region),
       col=cols, pch=pchv, bty="n",
       title="Region")
dev.off()
