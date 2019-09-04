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
library(lessR)
library(ggiraphExtra)
library(olsrr)

#####################################################################
######################### Modeling 2 ################################
#####################################################################

path.w <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.h <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.h)) {
  setwd(path.h)
} else {
  setwd(path.w)
}

theme_set(theme_light())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

data.housing <- as.data.table(read.csv(file = "ames_housing_data.csv", head = TRUE, sep = ","))

# Utility Functions

getCorTable <- function(cols) {
  # selected features correlation matrix
  sale.cor <- cor(cols, use = "pairwise.complete.obs")[, "SalePrice"]
  sale.cor <- sort(sale.cor, decreasing = T)

  sale.cor <- sale.cor[-1] # remove SalePrice
  sale.cor <- sale.cor[-1] # remove LogSalePrice

  tbl.sale.cor <- melt(sale.cor)
  colnames(tbl.sale.cor) <- c("Correlation to Sale Price")

  formattable(tbl.sale.cor, align = c("l", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
  ))
}

g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Basic Data Structure

ncol(data.housing)
head(data.housing)
names(data.housing)

# New Features

data.housing[, TotalFloorSF := FirstFlrSF + SecondFlrSF]
data.housing[, HouseAge := YrSold - YearBuilt]
data.housing[, QualityIndex := OverallQual * OverallCond]
data.housing[, logSalePrice := log(SalePrice)]
data.housing[, Price_Sqft := SalePrice / TotalFloorSF]
data.housing[, TotalBath := FullBath + HalfBath]

summary(data.housing)

# Data Survey

ggplot(data.housing) +
  geom_histogram(aes(data.housing$Price_Sqft, fill = ..count..), breaks = pretty(data.housing$Price_Sqft)) +
  labs(x = "Price / Sqft", y = "Count")

housing.numeric.col <- unlist(lapply(data.housing, is.numeric))
data.housing.numeric <- data.housing[, housing.numeric.col, with = F]

str(data.housing.numeric)

# getCorTable(data.housing.numeric)

skim(data.housing)

# Data Exploration

ggplot(data.housing) +
  geom_boxplot(aes(x = SaleType, y = SalePrice, fill = Neighborhood)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.housing) +
  geom_boxplot(aes(x = Zoning, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.housing) +
  geom_histogram(aes(data.housing$MasVnrArea, fill = ..count..), breaks = pretty(data.housing$MasVnrArea)) +
  labs(x = "Price / Sqft", y = "Count")

summary(data.housing$MasVnrArea)

missing.masvnr <- data.housing[is.na(data.housing$MasVnrArea)]

ggplot(missing.masvnr) +
  geom_histogram(aes(missing.masvnr$SalePrice, fill = ..count..), breaks = pretty(missing.masvnr$SalePrice)) +
  labs(x = "Price / Sqft", y = "Count") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  scale_x_continuous(labels = dollar_format(largest_with_cents = .2))


# Drop Conditions

data.cleaned <- data.housing
data.clean.stats <- data.table(Step = "Baseline", Records = nrow(data.cleaned))

ggplot(data.cleaned) +
  geom_boxplot(aes(x = BldgType, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

data.cleaned <- data.housing[BldgType == "1Fam"]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "BldgType", Records = nrow(data.cleaned)))

# Zoning

data.cleaned <- data.cleaned[!(Zoning %in% c("A (agr)", "I (all)", "C (all)"))]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "Zoning", Records = nrow(data.cleaned)))

unique(data.cleaned$Zoning)

ggplot(data.cleaned) +
  geom_boxplot(aes(x = Zoning, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Sale Condition

skim(data.cleaned)

ggplot(data.cleaned) +
  geom_boxplot(aes(x = SaleCondition, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

unique(data.cleaned$SaleCondition)

data.cleaned <- data.cleaned[!(SaleCondition %in% c("Abnorml"))]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "Sale Condition", Records = nrow(data.cleaned)))

ggplot(data.cleaned) +
  geom_boxplot(aes(x = LotShape, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = BsmtCond, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = HouseStyle, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = TotRmsAbvGrd, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = TotRmsAbvGrd, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = RoofStyle, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = CentralAir, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Sale Price (outliers)

summary(data.cleaned$SalePrice)

ggplot(data.cleaned, aes(SalePrice, fill = ..count..)) +
  geom_histogram(breaks = pretty(data.cleaned$SalePrice)) +
  labs(x = "SalePrice", y = "Count") +
  scale_x_continuous(labels = dollar_format(largest_with_cents = .2))

ggplot(data.cleaned, aes(y = SalePrice)) +
  geom_boxplot(outlier.colour = "red", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
             coord_flip() +
             labs(x = "", y = "Sale Price") +
             scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

data.model <- data.cleaned[data.cleaned$SalePrice < 700000]

ggplot(data.model, aes(y = SalePrice)) +
  geom_boxplot(outlier.colour = "red", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
             coord_flip() +
             labs(x = "", y = "Sale Price") +
             scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

# Drop Waterfall

data.clean.stats$Step <- factor(data.clean.stats$Step, levels = data.clean.stats$Step)
data.clean.stats$id <- seq_along(data.clean.stats$Records)

ggplot(data.clean.stats, aes(x = id, y = Records, fill = Step)) +
  geom_rect(aes(xmin = id - 0.45, xmax = id + .45, ymin = 0, ymax = Records)) +
  theme(legend.position = "bottom")

# Model Data

skim(data.model)

ggplot(data.model) +
  geom_histogram(aes(data.model$BedroomAbvGr, fill = ..count..), breaks = pretty(data.model$BedroomAbvGr)) +
  labs(y = "Count")

summary(data.model$BsmtExposure)

ggplot(data.cleaned) +
  geom_boxplot(aes(x = BsmtExposure, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

ggplot(data.model) +
  geom_histogram(aes(data.model$WoodDeckSF, fill = ..count..), breaks = pretty(data.model$WoodDeckSF)) +
  labs(y = "Count")

ggplot(data.cleaned) +
  geom_boxplot(aes(x = BsmtExposure, y = SalePrice, fill = SaleCondition)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

data.model <- subset(data.clean.stats, select = c(
# Response
    "SalePrice", "logSalePrice",

    # Quality
    "OverallQual", "QualityIndex",
    "Price_Sqft", "Neighborhood",

    # Size Related
    "TotalFloorSF", "TotalBsmtSF", "FirstFlrSF",
    "GrLivArea",

    # High Value Features
    "GarageCars", "GarageArea", "GarageYrBlt",
    "FullBath", "HalfBath",
    "MasVnrArea", "MasVnrType",
    "PoolArea", "Fireplaces",
    "KitchenQual", "BedroomAbvGr",
    "Heating", "Utilities",

    # Temporal
    "HouseAge", "YearRemodel", "YearBuilt",

    # Housing Lot
    "LotArea", "LotShape", "LotFrontage",

    # Housing Style
    "HouseStyle", "BldgType"))

str(data.model)

# Model Data Correlations

model.numeric.col <- unlist(lapply(data.model, is.numeric))
data.model.numeric <- data.model[, model.numeric.col, with = F]

str(data.model.numeric)

# Simple Linear Models

ggplotRegression <- function(fit) {
  p1 <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                      "Intercept =", signif(fit$coef[[1]], 5),
                      " Slope =", signif(fit$coef[[2]], 5),
                      " P =", signif(summary(fit)$coef[2, 4], 5))) +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

  res <- data.table(Value = residuals(fit))
  res.norm <- data.table(Value = (res$Value - mean(res$Value)) / sd(res$Value))
  
  p2 <- ggplot(res, aes(Value, fill = ..count..)) +
    geom_histogram(breaks = pretty(res.norm$Value)) +
    labs(title = "Standardized Residuals")

  grid.arrange(p1, p2)
}

# "Best" continious predictor

model1_data <- data.model[, .(TotalFloorSF, SalePrice)]
model1_fit <- lm(formula = SalePrice ~ TotalFloorSF, data = model1_data)

yhat <- data.table(x = 10212 + 116.862 * model1_data$SalePrice, y = model1_data$SalePrice)
yhat[, diff := x - y]

# y-bar vs y
ggplot(yhat) +
  geom_point(aes(x, y, color = -diff)) +
  scale_x_continuous(labels = dollar_format(largest_with_cents = .2)) +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "none")

res <- data.table(Value = residuals(model1_fit))
res.norm <- data.table(Value = (res$Value - mean(res$Value)) / sd(res$Value))

ggplotRegression(model1_fit)

summary(model1_fit)
model1_anova <- anova(model1_fit)

# tests

options(scipen = 12)

n <- nrow(data.model)
p <- 1
nt <- n - p - 2

alpha1 <- 0.01

# t-test
t1 <- 116.862 / 2.215
round(t1, 4)

crit.value <- abs(qt(alpha1 / 2, nt)) # 99% confidence, 2 sided
round(crit.value, 4)

abs(t1) > crit.value

# f-test
ssr <- model1_anova$`Sum Sq`[1:p]
sse <- model1_anova$`Sum Sq`[p + 1]
sst <- ssr + sse

f1 <- ((sst - sse) / p) / (sse / (n - p - 1))
round(f1, 4)

p.val <- df(f1, p, n - p - 1)

round(p.val, 4)

# Overall Qual discrete value

model2_data <- data.model[, .(OverallQual, SalePrice)]
model2_fit <- lm(formula = SalePrice ~ OverallQual, data = model2_data)

ggplotRegression(model2_fit)

summary(model2_fit)
model2_anova <- anova(model2_fit)

# t-test
t2 <- 47086.6 / 715.4
round(t2, 4)

crit.value <- abs(qt(alpha1 / 2, nt)) # 99% confidence, 2 sided
round(crit.value, 4)

abs(t2) > crit.value

# f-test
ssr <- model2_anova$`Sum Sq`[1:p]
sse <- model2_anova$`Sum Sq`[p + 1]
sst <- ssr + sse

f2 <- ((sst - sse) / p) / (sse / (n - p - 1))
round(f2, 4)

p.val <- df(f1, p, n - p - 1)

round(p.val, 4)

# y-bar vs y
yhat <- data.table(x = 10.6184 + 0.2352 * model1_data$SalePrice, y = model1_data$SalePrice)
yhat[, diff := x - y]

ggplot(yhat) +
  geom_point(aes(x, y, color = -diff)) +
  scale_x_continuous(labels = dollar_format(largest_with_cents = .2)) +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "none")

plot(model2_fit)

#d <- model2_data
#reg(SalePrice ~ OverallQual)

# comparison

anova(model1_fit, model2_fit)

# Multiple Linear Regression

model3_data <- data.model[, .(TotalFloorSF, OverallQual, SalePrice)]
model3_fit <- lm(formula = SalePrice ~ TotalFloorSF + OverallQual, data = model3_data)

summary(model3_fit)

p1 <- ggplot(model3_data, aes(y = SalePrice, x = TotalFloorSF, color = OverallQual)) +
       geom_point() +
       stat_smooth(method = "lm", se = FALSE, col = "red") +
       labs(title = paste("Adj R2 = ", signif(summary(model3_fit)$adj.r.squared, 5),
                      "Intercept =", signif(model3_fit$coef[[1]], 5),
                      " Slope (B1) =", signif(model3_fit$coef[[2]], 5),
                      " Slope (B2) =", signif(model3_fit$coef[[3]], 5),
                      " P =", signif(summary(model3_fit)$coef[2, 4], 5))) +
                      scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
        theme(legend.title = element_text(face = "bold"))

res <- data.table(Value = residuals(model3_fit))
res.norm <- data.table(Value = (res$Value - mean(res$Value)) / sd(res$Value))

p2 <- ggplot(res.norm, aes(Value, fill = ..count..)) +
    geom_histogram(breaks = pretty(res.norm$Value)) +
    labs(title = "Standardized Residuals")

grid.arrange(p1, p2)

min(model3_data$OverallQual)

rsq.diff <- summary(model3_fit)$r.squared - summary(model1_fit)$r.squared
round(rsq.diff, 4)

model3_anova <- anova(model3_fit)

rss <- sum(model3_anova$`Sum Sq`[0:2])
sse <- sum(model3_fit$residuals ^ 2)
sst <- rss + sse

cbind(model3_anova, 'CriticalValue' = qf(1 - .05, model3_anova[1, 1], model3_anova[2, 1]))

n
k <- 2
f.val <- ((sst - sse) / k) / (sse / (n - k - 1))
round(f.val, 4)

p.val <- df(f.val, k, n - k - 1)

round(p.val, 4)

ggplot(res) +
  geom_qq(aes(sample = Value, col = model3_data$SalePrice)) +
  geom_qq_line(aes(sample = Value)) +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  labs(title = "Q-Q Plot of Residuals")

ggplot(model3_data, aes(x = TotalFloorSF, y = OverallQual)) +
  geom_point(aes(color = SalePrice)) +
  labs(title = "Overall Quality vs. Total Floor SF")

# Transformed Response Variable

model4_data <- data.model[, .(TotalFloorSF, logSalePrice)]
model4_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model4_data)

summary(model4_fit)

model5_data <- data.model[, .(OverallQual, logSalePrice)]
model5_fit <- lm(formula = logSalePrice ~ OverallQual, data = model5_data)

model6_data <- data.model[, .(TotalFloorSF, OverallQual, logSalePrice)]
model6_fit <- lm(formula = logSalePrice ~ TotalFloorSF + OverallQual, data = model6_data)

rsq.diff <- data.table(Model = 1, NormRsq = summary(model1_fit)$r.squared, LogRsq = summary(model4_fit)$r.squared)
rsq.diff <- rbind(rsq.diff, data.table(Model = 2, NormRsq = summary(model2_fit)$r.squared, LogRsq = summary(model5_fit)$r.squared))
rsq.diff <- rbind(rsq.diff, data.table(Model = 3, NormRsq = summary(model3_fit)$r.squared, LogRsq = summary(model6_fit)$r.squared))

rsq.diff$NormRsq <- round(rsq.diff$NormRsq, 4)
rsq.diff$LogRsq <- round(rsq.diff$LogRsq, 4)

rsq.diff[, Delta := LogRsq - NormRsq]

formattable(rsq.diff, align = c("l", "c", "c", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

p1 <- ggplot(model3_data, aes(SalePrice)) +
  geom_histogram(aes(fill = ..count..)) +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

p2 <- ggplot(model6_data, aes(logSalePrice)) +
  geom_histogram(aes(fill = ..count..)) +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

grid.arrange(p1, p2)

# influental points

getValueHist <- function(data, desc, cap) {
  lower_q <- quantile(data$Value, 0.025)
  upper_q <- quantile(data$Value, 0.975)
  ggplot(data, aes(Value, fill = ..count..)) +
    geom_histogram(bins = 15) +
    geom_vline(xintercept = lower_q, linetype = 11, color = "firebrick3") +
    geom_vline(xintercept = upper_q, linetype = 11, color = "firebrick3") +
    geom_rug(aes(x = Value), color = "firebrick", inherit.aes = FALSE) +
    labs(title = paste("Histogram of", desc), caption = cap) +
    theme(axis.text.x = element_text(size = 8))
}

getValueHist(res, "", "")

model7_data <- data.model[, .(TotalFloorSF, OverallQual, YearBuilt, GrLivArea, MasVnrArea, LotArea, logSalePrice, SalePrice)]
model7_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model7_data)

summary(model7_fit)

ols_plot_cooksd_bar(model7_fit)

model7_data <- model7_data[!1706]

model7_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model7_data)
ols_plot_cooksd_bar(model7_fit)

model7_data <- model7_data[!1706]

model7_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model7_data)
ols_plot_cooksd_bar(model7_fit)

model7_data <- model7_data[!1154]

model7_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model7_data)
ols_plot_cooksd_bar(model7_fit)

model7_data <- model7_data[!892]

model7_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model7_data)
ols_plot_cooksd_bar(model7_fit)

model7_data <- model7_data[!994]

model7_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model7_data)
ols_plot_cooksd_bar(model7_fit)

model7_data <- model7_data[!1151]

model7_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model7_data)
ols_plot_cooksd_bar(model7_fit)

model7_data <- model7_data[!2195]

model7_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model7_data)
ols_plot_cooksd_bar(model7_fit)

model7_data <- model7_data[!2111]

model7_fit <- lm(formula = logSalePrice ~ TotalFloorSF, data = model7_data)
ols_plot_cooksd_bar(model7_fit)

summary(model7_fit)

ggplotRegression(model7_fit)

summary(model7_fit)$r.squared - summary(model4_fit)$r.squared

# Final Model

m1 <- lm(formula = SalePrice ~ TotalFloorSF, data = model7_data)
rsq.diff <- data.table(Model = 1, Features = names(m1$coefficients)[-1], Rsq = summary(m1)$r.squared, Change = NA)

m2 <- lm(formula = SalePrice ~ TotalFloorSF + GrLivArea, data = model7_data)
rsq.diff <- rbind(rsq.diff, data.table(Model = 2, Features = "TotalFloorSF + GrLivArea", Rsq = summary(m2)$r.squared, Change = summary(m2)$r.squared - summary(m1)$r.squared))

m3 <- lm(formula = SalePrice ~ TotalFloorSF + GrLivArea + OverallQual, data = model7_data)
rsq.diff <- rbind(rsq.diff, data.table(Model = 3, Features = "SalePrice ~ TotalFloorSF + GrLivArea + OverallQual", Rsq = summary(m3)$r.squared, Change = summary(m3)$r.squared - summary(m2)$r.squared))

m4 <- lm(formula = SalePrice ~ TotalFloorSF + OverallQual, data = model7_data)
rsq.diff <- rbind(rsq.diff, data.table(Model = 4, Features = "SalePrice ~ TotalFloorSF + OverallQual", Rsq = summary(m4)$r.squared, Change = summary(m4)$r.squared - summary(m3)$r.squared))

m5 <- lm(formula = SalePrice ~ TotalFloorSF + OverallQual + YearBuilt, data = model7_data)
rsq.diff <- rbind(rsq.diff, data.table(Model = 5, Features = "SalePrice ~ TotalFloorSF + OverallQual + YearBuilt", Rsq = summary(m5)$r.squared, Change = summary(m5)$r.squared - summary(m4)$r.squared))

m6 <- lm(formula = SalePrice ~ TotalFloorSF + OverallQual + MasVnrArea, data = model7_data)
rsq.diff <- rbind(rsq.diff, data.table(Model = 6, Features = "SalePrice ~ TotalFloorSF + OverallQual + MasVnrArea", Rsq = summary(m6)$r.squared, Change = summary(m6)$r.squared - summary(m5)$r.squared))

m7 <- lm(formula = SalePrice ~ TotalFloorSF + OverallQual + LotArea, data = model7_data)
rsq.diff <- rbind(rsq.diff, data.table(Model = 7, Features = "SalePrice ~ TotalFloorSF + OverallQual + LotArea", Rsq = summary(m7)$r.squared, Change = summary(m7)$r.squared - summary(m6)$r.squared))

rsq.diff

formattable(rsq.diff, align = c("l", "c", "c", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

model8_data <- model7_data
model8_fit <- lm(formula = SalePrice ~ TotalFloorSF + YearBuilt + OverallQual, data = model8_data)

summary(model8_fit)

p1 <- ggplot(model8_data, aes(y = SalePrice, x = TotalFloorSF, size = OverallQual, color = YearBuilt)) +
       geom_point() +
       stat_smooth(method = "lm", se = FALSE, col = "red") +
       labs(title = paste("Adj R2 = ", signif(summary(model8_fit)$adj.r.squared, 5),
                      "Intercept =", signif(model8_fit$coef[[1]], 5),
                      " B1: =", signif(model8_fit$coef[[2]], 5),
                      " B2: =", signif(model8_fit$coef[[3]], 5),
                      " B3: =", signif(model8_fit$coef[[4]], 5),
                      " P =", signif(summary(model8_fit)$coef[2, 4], 5))) +
                      scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
                      theme(legend.title = element_text(face = "bold"))

res <- data.table(Value = residuals(model8_fit))
res.norm <- data.table(Value = (res$Value - mean(res$Value)) / sd(res$Value))

p2 <- ggplot(res.norm, aes(Value, fill = ..count..)) +
    geom_histogram(breaks = pretty(res.norm$Value)) +
    labs(title = "Standardized Residuals")

grid.arrange(p1, p2)

ggplot(res) +
  geom_qq(aes(sample = Value, col = model8_data$SalePrice)) +
  geom_qq_line(aes(sample = Value)) +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  labs(title = "Q-Q Plot of Residuals")

summary(model8_fit)
anova(model8_fit)

