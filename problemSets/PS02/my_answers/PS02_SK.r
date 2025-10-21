###############################################################################
# Title:        PS02 - Applied Stats / Quant Methods 1
# Description:  PS02 solutions (Q1: Chi-square; Q2: Bivariate regression)
# Author:       Sein Kim
# R version:    4.rr
###############################################################################

rm(list = ls())

detachAllPackages <- function() {
  basic <- c("package:stats","package:graphics","package:grDevices",
             "package:utils","package:datasets","package:methods","package:base")
  pkg <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  pkg <- setdiff(pkg, basic)
  if (length(pkg) > 0) for (p in pkg) detach(p, character.only = TRUE)
}
detachAllPackages()

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
lapply(c(), pkgTest)

###############################################################################
# Question 1: Political Science
###############################################################################

obs <- rbind(
  Upper = c(`Not Stopped` = 14, `Bribe requested` = 6, `Stopped/given warning` = 7),
  Lower = c(`Not Stopped` = 7,  `Bribe requested` = 7, `Stopped/given warning` = 1)
)

row_tot <- rowSums(obs)
col_tot <- colSums(obs)
N       <- sum(obs)
expct   <- outer(row_tot, col_tot) / N
chi_man <- sum((obs - expct)^2 / expct)
cat("Q1 (a) Manual chi-square =", round(chi_man, 4), "\n")

df_q1  <- (nrow(obs)-1)*(ncol(obs)-1)
p_man  <- 1 - pchisq(chi_man, df = df_q1)
cat("Q1 (b) p-value (manual) =", round(p_man, 4), "\n\n")

ct <- suppressWarnings(chisq.test(obs, correct = FALSE))
cat("Q1 check: chisq.test$stat =", round(ct$statistic, 4),
    " df =", ct$parameter, " p-value =", round(ct$p.value, 4), "\n\n")

std_res <- ct$stdres
cat("Q1 (c) Standardized residuals:\n")
print(round(std_res, 3))
cat("\n")

###############################################################################
# Question 2: Economics
###############################################################################

url_women <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
dat <- read.csv(url_women)

fit <- lm(water ~ reserved, data = dat)
smry <- summary(fit)
print(smry)

b1  <- coef(fit)["reserved"]
se1 <- smry$coefficients["reserved", "Std. Error"]
t1  <- smry$coefficients["reserved", "t value"]
p1  <- smry$coefficients["reserved", "Pr(>|t|)"]
cat("coef(reserved) =", round(b1, 3),
    "SE =", round(se1, 3),
    "t =", round(t1, 3),
    "p =", round(p1, 4), "\n\n")

pdf("q2_water_by_reserved.pdf", width = 6, height = 5)
boxplot(water ~ reserved, data = dat,
        main = "Water Facilities by Reservation Status",
        xlab = "Reserved for Women (0 = No, 1 = Yes)",
        ylab = "Number of New/Repaired Water Facilities",
        col  = c("lightgray", "lightpink"))
grp_means <- tapply(dat$water, dat$reserved, mean, na.rm = TRUE)
points(1:2, grp_means, pch = 19, col = "red")
dev.off()
cat("Saved figure: q2_water_by_reserved.pdf\n")
par(cex.axis=0.9, cex.lab=0.9, cex.main=1)
