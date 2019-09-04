library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(GGally)
library(ggthemes)
library(formattable)
library(scales)
library(reshape2)
library(skimr)
library(gridExtra)

#####################################################################
######################### Assignment 3 ##############################
#####################################################################

path.w <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.h <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.h)) {
  setwd(path.h)
} else {
  setwd(path.w)
}

theme_set(theme_light())

# t-test for beta1
n <- 12
p <- 3

ssr <- 588.92 + 103.90 + 0.24
sse <- 195.19
sst <- ssr + sse

((sst - sse) / p) / (sse / (n - p - 1))

alpha1 <- 0.01
beta1 <- 2.186

T <- beta1 / 0.4104
T <- round(T, 4)

c(beta1 - alpha1, beta1 + alpha1)

crit.value <- abs(qt(alpha1 / 2, 72 - 4 - 2)) # 99% confidence, 2 sided
round(crit.value, 4)

abs(T) > crit.value

# rsq

ssr <- 1974.53 + 118.8642568 + 32.47012585 + 0.435606985
sse <- 630.36
sst <- ssr + sse

rsq <- ssr / sst
#rsq <- round(rsq, 4)

n <- 72
adj_r2 <- 1 - ((1 - rsq) * (n - 1) / (n - 4 - 1))

round(adj_r2, 4)

# F-test

n <- 72
p <- 4

ssr <- 1974.53 + 118.8642568 + 32.47012585 + 0.435606985
sse <- 630.36
sst <- ssr + sse

F <- ((sst - sse) / p) / (sse / (n - p - 1))
round(F, 4)

p.val <- df(F, p, n - p - 1)

round(p.val, 4)

# F-test compare

n <- 72
dff <- 65
dfr <- 67
dfn <- dfr - dfn
ss_r <- 630.36
ss_f <- 572.6091

f.val <- ((ss_r - ss_f) / dfn) / (ss_f / dff)
round(f.val, 4) # 3.278

alpha = .05
f.crit <- qf(1 - alpha, dfn, dff)
round(f.crit, 4)

f.val > f.crit

# Model 3

data.m3 <- data.model[, .(SalePrice, HouseAge, YearBuilt, YearRemodel)]

model_3 <- lm(formula = SalePrice ~ HouseAge + YearBuilt + YearRemodel, data = data.m3)
summary(model_3)
anova(model_3)

n <- nrow(data.m3)
k <- 3

t.val <- -2291.54 / 1002.40
round(t.val, 4)

crit.value <- abs(qt(0.05 / 2, n - k - 2))
crit.value <- round(crit.value, 4)

abs(round(t.val, 4)) > crit.value

t.val <- -1172.34 / 1006.12
round(t.val, 4)

crit.value <- abs(qt(0.05 / 2, n - k - 2))
crit.value <- round(crit.value, 4)

abs(round(t.val, 4)) > crit.value

t.val <- 1108.88 / 76.64
round(t.val, 4)

crit.value <- abs(qt(0.05 / 2, n - k - 2))
crit.value <- round(crit.value, 4)

abs(round(t.val, 4)) > crit.value

# F-test

options(scipen = 5)

anova3 <- anova(model_3)

rss <- sum(anova3$`Sum Sq`[0:3])
sse <- sum(model_3$residuals ^ 2)
sst <- rss + sse

cbind(anova3, 'CriticalValue' = qf(1 - .05, anova3[1, 1], anova3[2, 1]))

f.val <- ((sst - sse) / k) / (sse / (n - k - 1))
round(f.val, 4)

p.val <- df(f.val, k, n - k - 1)

round(p.val, 4)

alpha = .01
f.crit <- qf(1 - alpha, 3, (n - (k + 1)))
round(f.crit, 4)

f.val > f.crit

# Model 4

data.m4 <- data.model[, .(SalePrice, HouseAge, YearBuilt, YearRemodel, OverallQual, QualityIndex)]

model_4 <- lm(formula = SalePrice ~ HouseAge + YearBuilt + YearRemodel + OverallQual + QualityIndex, data = data.m4)
summary(model_4)
anova(model_4)

n <- nrow(data.m3)
k <- 5

t.val <- -1172.86 / 733.18
round(t.val, 4)

crit.value <- abs(qt(0.05 / 2, n - k - 2))
crit.value <- round(crit.value, 4)

abs(round(t.val, 4)) > crit.value

t.val <- -854.39 / 735.14
round(t.val, 4)

crit.value <- abs(qt(0.05 / 2, n - k - 2))
crit.value <- round(crit.value, 4)

abs(round(t.val, 4)) > crit.value

t.val <- 287.52 / 62.53
round(t.val, 4)

crit.value <- abs(qt(0.05 / 2, n - k - 2))
crit.value <- round(crit.value, 4)

abs(round(t.val, 4)) > crit.value

t.val <- 41471.70 / 1317.60
round(t.val, 4)

crit.value <- abs(qt(0.05 / 2, n - k - 2))
crit.value <- round(crit.value, 4)

abs(round(t.val, 4)) > crit.value

t.val <- -144.15 / 172.51
round(t.val, 4)

crit.value <- abs(qt(0.05 / 2, n - k - 2))
crit.value <- round(crit.value, 4)

abs(round(t.val, 4)) > crit.value

# model 4 F-statistic

anova4 <- anova(model_4)

rss <- sum(anova4$`Sum Sq`[0:5])
sse <- sum(model_4$residuals ^ 2)
sst <- rss + sse

cbind(anova4, 'CriticalValue' = qf(1 - .05, anova4[1, 1], anova4[2, 1]))

k <- 5
f.val <- ((sst - sse) / k) / (sse / (n - k - 1))
round(f.val, 4)

p.val <- df(f.val, k, n - k - 1)

round(p.val, 4)

# Nested Model3 & 4

ssr <- sum(model_3$residuals ^ 2)
ssc <- sum(model_4$residuals ^ 2)

k <- 5
f.val <- (ssr - ssc) / (ssc / (n - (k + 1)))
round(f.val, 4)

alpha = .01
f.crit <- qf(1 - alpha, 4, (n - (k + 1)))
round(f.crit, 4)

f.val > f.crit

