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
######################### Modeling 1 ################################
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

getCorTable(data.housing.numeric)

# General EDA

data.types <- data.frame(Type = c('Nominal', 'Ordinal', 'Discrete', 'Continious'), Count = c(23, 23, 14, 20))

unique(data.housing$BldgType)

ggplot(data.types, aes(Type, Count, fill = Count)) +
  geom_bar(stat = 'identity') +
  labs(title = "Data Types for Ames Housing",
    caption = "MSDS 410: Regression Analysis 1") +
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))

building.info <- data.housing[, .(Count = .N, Pct = percent((.N / nrow(data.housing))), AvgSalePrice = currency(mean(SalePrice))), by = BldgType]

formattable(building.info, align = c("l", "c", "c", "c", "r"),
            list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

str(building.info)

# Model Data

data.model <- subset(data.housing, select = c(
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

getCorTable(data.model.numeric)

# Sample Population

data.model <- data.model[BldgType == "1Fam"]

ggplot(data.model, aes(y = data.model$SalePrice)) +
  geom_boxplot(outlier.colour = "red", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
  coord_flip() +
  labs(x = "", y = "Sale Price") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

sd(data.model$SalePrice) * 3
mean(data.model$SalePrice)

# Data Quality Checks

summary(data.model$SalePrice)

ggplot(data.model, aes(data.model$SalePrice, fill = ..count..)) +
  geom_histogram(breaks = pretty(data.model$SalePrice)) +
  labs(x = "SalePrice", y = "Count") +
  scale_x_continuous(labels = dollar_format(largest_with_cents = .2))

ggplot(data.model, aes(y = data.model$SalePrice)) +
  geom_boxplot(outlier.colour = "red", fill = "#1C93D1", outlier.shape = 16,
             outlier.size = 2, notch = FALSE) +
  coord_flip() +
  labs(x = "", y = "Sale Price") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

data.model <- data.model[SalePrice < 700000]

data.model.quality <- subset(data.housing, select = c(
    
    # Quality
    "OverallQual", "QualityIndex",
    "Price_Sqft",

    # Size Related
    "TotalFloorSF", "TotalBsmtSF", "FirstFlrSF",
    "GrLivArea",

    # High Value Features
    "GarageCars", "GarageArea", "GarageYrBlt",
    "FullBath", "HalfBath",
    "MasVnrArea",
    "Fireplaces", "KitchenQual",

    # Temporal
    "HouseAge", "YearRemodel", "YearBuilt",

    # Housing Lot
    "LotArea", "LotFrontage"))

summary(data.model.quality)

melt(colnames(data.model.quality))

data.model.quality %>% skim()

ggpairs(subset(data.model.quality, select = c(
  # Continious
  "Price_Sqft",

  # Size Related
  "TotalFloorSF", "TotalBsmtSF", "FirstFlrSF",
  "GrLivArea", "MasVnrArea", "LotArea",
  "LotFrontage")))

ggpairs(subset(data.model.quality, select = c(
  # Temperal
  "YearRemodel", "YearBuilt", 
  "GarageYrBlt")))

ggpairs(subset(data.model.quality, select = c(
  # Discrete
  "HouseAge",
  "FullBath", "HalfBath",
  "Fireplaces", "GarageCars")))

ggpairs(subset(data.model.quality, select = c(
  # Ordinal
  "OverallQual", "QualityIndex",
  "KitchenQual")))

# Exploratory Data Analysis

# Categorical

nh_1 <- ggplot(data.model) +
  geom_boxplot(aes(x = Neighborhood, y = SalePrice, fill = Neighborhood)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

nh_2 <- ggplot(data.model) +
  geom_boxplot(aes(x = Neighborhood, y = Price_Sqft, fill = Neighborhood)) +
  coord_flip()

nh_leg <- g_legend(nh_1)

grid.arrange(nh_1 + theme(legend.position = "none"),
             nh_2 + theme(legend.position = "none"),
             nh_leg, heights = c(6, 6, 2))

hs_1 <- ggplot(data.model) +
  geom_histogram(aes( x = HouseStyle, fill = ..count..), stat = "count")

hs_2 <- ggplot(data.model) +
  geom_boxplot(aes(HouseStyle, SalePrice, fill = HouseStyle), outlier.color = "black") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

grid.arrange(hs_1, hs_2)

nrow(data.model[HouseStyle %in% c('1Story', '2Story')]) / nrow(data.model) * 100

# Discrete Variables

yr_1 <- ggplot(data.model) +
  geom_point(aes(YearBuilt, SalePrice)) +
  geom_smooth(aes(YearBuilt, SalePrice), method = "gam", formula = y ~ s(x, bs = "cs")) +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

yr_2 <- ggplot(data.model) +
  geom_point(aes(YearRemodel, SalePrice)) +
  geom_smooth(aes(YearRemodel, SalePrice), method = "lm") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2))

grid.arrange(yr_1, yr_2)

# Continious Variables

ggpairs(subset(data.model, select = c(
  "SalePrice",
  # Continious
  "Price_Sqft",

  # Size Related
  "TotalFloorSF", "TotalBsmtSF", "FirstFlrSF",
  "GrLivArea", "MasVnrArea", "LotArea",
  "LotFrontage")))

# Grliv

grlv_1 <- ggplot(data.model) +
  geom_point(aes(GrLivArea, SalePrice, color = HouseAge)) +
  geom_smooth(aes(GrLivArea, SalePrice), method = "lm") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.title = element_text(size = 10))

grlv_2 <- ggplot(data.model) +
  geom_point(aes(GrLivArea, SalePrice, color = OverallQual)) +
  geom_smooth(aes(GrLivArea, SalePrice), method = "loess") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.title = element_text(size = 10))

grid.arrange(grlv_1, grlv_2)

# TotalFloorSF

totsf_1 <- ggplot(data.model) +
  geom_point(aes(TotalFloorSF, SalePrice, color = HouseAge)) +
  geom_smooth(aes(TotalFloorSF, SalePrice), method = "lm") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.title = element_text(size = 10))

totsf_2 <- ggplot(data.model) +
  geom_point(aes(TotalFloorSF, SalePrice, color = OverallQual)) +
  geom_smooth(aes(TotalFloorSF, SalePrice), method = "loess") +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.title = element_text(size = 10))

grid.arrange(totsf_1, totsf_2)

# Modeling

# Single Variable Linear Model

ggpairs(subset(data.model, select = c(
  "logSalePrice",

  # Predictors
  "TotalFloorSF", "GrLivArea", 
  "MasVnrArea", "LotArea",
  "LotFrontage")))

ggplotRegression <- function(fit) {
  p1 <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                      "Intercept =", signif(fit$coef[[1]], 5),
                      " Slope =", signif(fit$coef[[2]], 5),
                      " P =", signif(summary(fit)$coef[2, 4], 5)))

  res <- as.data.table(residuals(fit))
  colnames(res) <- c("value")
  
  p2 <- ggplot(res, aes(value, fill = ..count..)) +
    geom_histogram(breaks = pretty(res$value)) +
    labs(title = "Residuals")

  grid.arrange(p1, p2)
}

ggplotRegression(lm(formula = logSalePrice ~ TotalFloorSF, data = data.model[, .(TotalFloorSF, logSalePrice)]))
ggplotRegression(lm(formula = logSalePrice ~ GrLivArea, data = data.model[, .(GrLivArea, logSalePrice)]))
ggplotRegression(lm(formula = logSalePrice ~ YearBuilt, data = data.model[, .(YearBuilt, logSalePrice)]))
ggplotRegression(lm(formula = logSalePrice ~ YearRemodel, data = data.model[, .(YearRemodel, logSalePrice)]))
ggplotRegression(lm(formula = logSalePrice ~ OverallQual, data = data.model[, .(OverallQual, logSalePrice)]))

# Multiple Linear Regression

ggplotMultRegression <- function(data, fit) {

  p1 <- ggplot(melt(data, id.vars = 'logSalePrice')) +
    geom_jitter(aes(value, logSalePrice, colour = variable),) + geom_smooth(aes(value, logSalePrice, colour = variable), method = lm, se = FALSE) +
    facet_wrap(~variable, scales = "free_x") +
    labs(x = "Variable", y = "Sale Price",
         title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                        "Intercept =", signif(fit$coef[[1]], 5),
                        " Slope =", signif(fit$coef[[2]], 5),
                        " P =", signif(summary(fit)$coef[2, 4], 5)))

  res <- as.data.table(residuals(fit))
  colnames(res) <- c("value")

  p2 <- ggplot(res, aes(value, fill = ..count..)) +
    geom_histogram(breaks = pretty(res$value)) +
    labs(title = "Residuals")

  grid.arrange(p1, p2)
}

mlr <- data.model[, .(GrLivArea, YearRemodel, logSalePrice)]
ggplotMultRegression(mlr, lm(formula = logSalePrice ~ GrLivArea + YearRemodel, data = mlr))

mlr <- data.model[, .(GrLivArea, YearBuilt, logSalePrice)]
ggplotMultRegression(mlr, lm(formula = logSalePrice ~ GrLivArea + YearBuilt, data = mlr))

mlr <- data.model[, .(GrLivArea, YearBuilt, OverallQual, logSalePrice)]
ggplotMultRegression(mlr, lm(formula = logSalePrice ~ GrLivArea + YearBuilt + OverallQual, data = mlr))

