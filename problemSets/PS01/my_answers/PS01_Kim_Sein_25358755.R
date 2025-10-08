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
tcrit_1 <- qt(1 - alpha/2, df) #two-sided 90% CI -> qt(0.95.24)
tcrit_1           # == 1.710882

ci_90_lower <- mean_y - tcrit_1 * SE
ci_90_upper <- mean_y + tcrit_1 * SE

print(mean_y)     # 98.44
print(n)          # 25
print(s)          # 13.09339...
print(SE)         # 2.6186...
print(tcrit_1)      # 1.710882
print(ci_90_lower)  # ~93.95993
print(ci_90_upper)  # ~102.92010


#[2] Next, the school counselor was curious whether the average student IQ in her school
# is higher than the average IQ score (100) among all the schools in the country.
# Using the same sample, conduct the appropriate hypothesis test with α= 0.05.

mu0 <- 100        # H0: mu=100, H1: mu > 100
t_stat <- (mean_y - mu0) / SE
t_crit <- qt(0.95, df) # because alpha is 0.05
p_value <- 1 - pt(t_stat,df)

# --- RESULT ---
print(n)
print(mean_y)
print(s)
print(SE)
print(t_stat)
print(t_crit)
print(p_value)

#another option) using R funtion "t.test"
t.test(y, mu = 100, alternative = "greater")

#####################
# Problem 2
#####################
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
expenditure # print to check

#####################
# Q1 (1) Relationships among Y, X1, X2, and X3
#####################

# Keep only Y, X1, X2, X3
vars <- expenditure[, c("Y", "X1", "X2", "X3")]

# Pairwise scatterplot matrix (base R)
pdf("q2_pairs.pdf", width = 7, height = 7)  # save to pdf
pairs(vars,
      main = "Pairwise Plots: Y, X1, X2, X3")  # simple, transparent
dev.off()

# Focused scatterplots of Y vs each X (side by side)
pdf("q2_Y_vs_Xs.pdf", width = 9, height = 3)  # save to pdf
op <- par(mfrow = c(1, 3))  # three panels in one row

plot(vars$X1, vars$Y,
     xlab = "X1: Per-capita personal income",
     ylab = "Y: Per-capita expenditure",
     main = "Y vs X1",
     pch = 19, col = "lightpink")

plot(vars$X2, vars$Y,
     xlab = "X2: Financially insecure per 100,000",
     ylab = "Y: Per-capita expenditure",
     main = "Y vs X2",
     pch = 19, col = "skyblue")

plot(vars$X3, vars$Y,
     xlab = "X3: Urban residents per 1,000",
     ylab = "Y: Per-capita expenditure",
     main = "Y vs X3",
     pch = 19, col = "lightgreen")

par(op)  # restore graphics setting
dev.off()

# Correlation matrix (Pearson, base R)
cor_mat <- cor(vars, use = "pairwise.complete.obs", method = "pearson")
cor_mat            # raw numbers
round(cor_mat, 3)  # rounded to 3 decimals

# Print each correlation explicitly
cat("corr(Y, X1) =", round(cor_mat["Y","X1"], 3), "\n")
cat("corr(Y, X2) =", round(cor_mat["Y","X2"], 3), "\n")
cat("corr(Y, X3) =", round(cor_mat["Y","X3"], 3), "\n")

#####################
# Q2 (2) Y vs Region
#####################

# Boxplot of Y by Region
pdf("q2_Y_by_region.pdf", width = 6, height = 6)
boxplot(Y ~ Region, data = expenditure,
        main = "Per-capita Expenditure (Y) by Region",
        xlab = "Region", ylab = "Y (per capita)")
region_means <- tapply(expenditure$Y, expenditure$Region, mean, na.rm=TRUE)
points(1:length(region_means), region_means, col="red", pch=19)
dev.off()

# Print means per Region
print(region_means)

#####################
# Q2 (3) Y vs X1 (+ Region)
#####################

# Scatterplot Y vs X1
pdf("q2_Y_vs_X1.pdf", width = 6, height = 6)
plot(expenditure$X1, expenditure$Y,
     main = "Y vs X1",
     xlab = "Per-capita personal income (X1)",
     ylab = "Per-capita expenditure (Y)",
     pch = 19, col = "gray40")
abline(lm(Y ~ X1, data = expenditure), lty = 2, lwd = 2)
dev.off()

# Scatterplot Y vs X1 by Region
pdf("q2_Y_vs_X1_by_region.pdf", width = 6.5, height = 6)
cols <- c("blue", "orange", "green", "purple")
pchv <- c(16, 17, 15, 18)
plot(expenditure$X1, expenditure$Y,
     main = "Y vs X1 by Region",
     xlab = "Per-capita personal income (X1)",
     ylab = "Per-capita expenditure (Y)",
     col = cols[as.integer(expenditure$Region)],
     pch = pchv[as.integer(expenditure$Region)])
abline(lm(Y ~ X1, data = expenditure), lty = 2, lwd = 2)
legend("bottomright",
       legend = levels(factor(expenditure$Region)),
       col = cols, pch = pchv, bty = "n",
       title = "Region")
dev.off()
