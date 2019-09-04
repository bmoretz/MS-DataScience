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
library(caret)
library(sjPlot)
library(sjmisc)
library(car)
library(WVPlots)
library(MASS)
library(Metrics)
library(stringr)

#####################################################################
######################### Modeling 3 ################################
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

# Utility Function
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


data.housing <- as.data.table(read.csv(file = "ames_housing_data.csv", head = TRUE, sep = ","))

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

housing.numeric.col <- unlist(lapply(data.housing, is.numeric))
data.housing.numeric <- data.housing[, housing.numeric.col, with = F]

str(data.housing.numeric)
skim(data.housing)

# Drop Conditions

data.cleaned <- data.housing
data.clean.stats <- data.table(Step = "Baseline", Records = nrow(data.cleaned))

data.cleaned <- data.housing[BldgType == "1Fam"]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "BldgType", Records = nrow(data.cleaned)))

# Zoning

data.cleaned <- data.cleaned[!(Zoning %in% c("A (agr)", "I (all)", "C (all)"))]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "Zoning", Records = nrow(data.cleaned)))

# Sale Condition

data.cleaned <- data.cleaned[!(SaleCondition %in% c("Abnorml"))]
data.clean.stats <- rbind(data.clean.stats, data.table(Step = "Sale Condition", Records = nrow(data.cleaned)))

data.model <- data.cleaned[data.cleaned$SalePrice < 700000]

# Drop Waterfall

data.clean.stats$Step <- factor(data.clean.stats$Step, levels = data.clean.stats$Step)
data.clean.stats$id <- seq_along(data.clean.stats$Records)

# Categorical Variables

# Get all factor columns in the data

getCategoryRelationships <- function(data, response) {

  catCols <- names(data)[sapply(data, is.factor)]
  print(length(catCols))

  results <- data.table( Column = character(), RSq = numeric(), RSE = numeric(), MeanDiff = numeric(), Levels = numeric(), PctPopulated = numeric())

  for (col in catCols) {

    tryCatch({
      fmla <- as.formula(paste0(response, " ~ ", col))
      fit <- lm(fmla, data)
      pct_value <- round((1 - sum(is.na(data[[col]])) / nrow(data)) * 100, 1)
      vals <- data.table(value = tapply(data.model[[response]], data.model[[col]], mean))
      mean.diff <- mean(vals[!is.na(value)]$value)

      ret <- data.table(Column = col, RSq = round(summary(fit)$r.squared * 100, 2), RSE = dollar(sd(residuals(fit))), MeanDiff = dollar(mean.diff), Levels = length(coef(fit)), PctPopulated = pct_value)
  
      results <- rbind(results, ret, use.names = T)
    }, error = function(e) {
      print(e)
    })
  }

  setorder(results, - RSq, RSE, PctPopulated, MeanDiff)
  results
}

cat.relationships <- getCategoryRelationships(data.model, "SalePrice")

formattable(cat.relationships, align = c("l", "c", "c", "c", "c", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

category_model <- function(col, data, response) {

  data <- data.model[, SalePrice, by = c(col)]
  print(summary(data))

  fmla <- as.formula(paste0(response, " ~ ", col))
  category <- dummyVars(fmla, data = data)

  model_fit <- lm(category, data = data)
  
  print(summary(model_fit))
  print(Anova(model_fit, type = "II"))

  plot_model(model_fit, type = "pred")
}

# Neighborhood

category_model("Neighborhood", data.model, "SalePrice")

neighborhood.mean <- melt(tapply(data.model$SalePrice, data.model$Neighborhood, mean))
colnames(neighborhood.mean) <- c("Neighborhood", "MeanPrice")
neighborhood.mean <- neighborhood.mean[!is.na(neighborhood.mean$MeanPrice),]

neighborhood.mean$MeanPrice <- dollar(neighborhood.mean$MeanPrice)

formattable(neighborhood.mean, align = c("l", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

ggplot(data.model) +
  geom_boxplot(aes(x = Neighborhood, y = SalePrice, fill = Neighborhood)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Basement Quality

ggplot(data.model) +
  geom_boxplot(aes(x = BsmtQual, y = SalePrice, fill = BsmtQual)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

category_model("BsmtQual", data.model, "SalePrice")

# Kitchen Qual
category_model("KitchenQual", data.model, "SalePrice")

ggplot(data.model) +
  geom_boxplot(aes(x = KitchenQual, y = SalePrice, fill = KitchenQual)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Exterior Qual
category_model("ExterQual", data.model, "SalePrice")

ggplot(data.model) +
  geom_boxplot(aes(x = ExterQual, y = SalePrice, fill = ExterQual)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Foundation
category_model("Foundation", data.model, "SalePrice")

ggplot(data.model) +
  geom_boxplot(aes(x = Foundation, y = SalePrice, fill = Foundation)) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(largest_with_cents = .2)) +
  theme(legend.position = "bottom")

# Fence
category_model("Fence", data.model, "SalePrice")

category_model("PoolQC", data.model, "SalePrice")
category_model("ExterQual", data.model, "SalePrice")
category_model("Foundation", data.model, "SalePrice")

category_model("HeatingQC", data.model, "SalePrice")
category_model("GarageType", data.model, "SalePrice")

# Predictive Model

# Additional cleaning steps

data.clean <- data.model[!is.na(BsmtQual)]
data.clean <- data.clean[!is.na(KitchenQual)]
data.clean <- data.clean[!is.na(Foundation)]
data.clean <- data.clean[!is.na(MasVnrType)]
data.clean <- data.clean[!is.na(FullBath)]

# dummy variables for selected categorical values
dmy <- dummyVars("SalePrice ~ BsmtQual + KitchenQual + ExterQual + Foundation + MasVnrType", data = data.clean)
data.categorical <- data.table(predict(dmy, newdata = data.clean))

head(data.categorical)

# Define these two variables for later use;
data.clean$QualityIndex <- data.clean$OverallQual * data.clean$OverallCond
data.clean$TotalSqftCalc <- data.clean$BsmtFinSF1 + data.clean$BsmtFinSF2 + data.clean$GrLivArea

data.clean <- cbind(data.clean, data.categorical)

set.seed(123)

n.total <- nrow(data.clean)

data.clean$u <- runif(n = n.total, min = 0, max = 1)

# Create train/test split;
data.train <- subset(data.clean, u < 0.70)
data.test <- subset(data.clean, u >= 0.70)

n.train <-nrow(data.train)
n.test <- nrow(data.test)

# assert tran + test = total
stopifnot(n.train + n.test == n.total)

tbl.count <- data.table(Total = n.total, Train = n.train, Test = n.test)

getDataSplit <- function( cnt ) {
  formattable(cnt, align = c("l", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
  ))
}

# getDataSplit(tbl.count)

data.train.num <- data.train[, .(SalePrice, QualityIndex, TotalSqftCalc, YearBuilt, YearRemodel, LotArea, GrLivArea, TotalBath, TotalBsmtSF, HouseAge, FullBath, HalfBath)]
data.train.cat <- data.train[, colnames(data.train) %in% colnames(data.categorical), with = F]
data.train.clean <- cbind(data.train.num, data.train.cat)

ncol(data.train.clean)

model.cols <- data.table(Column = colnames(data.train.clean))
model.cols$Type = sapply(data.train.clean, typeof)

# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ ., data = data.train.clean)
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1, data = data.train.clean)
summary(lower.lm)

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc, data = data.train.clean)
summary(sqft.lm)

# Model Definitions
?stepAIC

forward.lm <- stepAIC(object = lower.lm, scope = list(upper = formula(upper.lm), lower = ~1), direction = c('forward'))
summary(forward.lm)

backward.lm <- stepAIC(object = upper.lm, direction = c('backward'))
summary(backward.lm)

stepwise.lm <- stepAIC(object = sqft.lm, scope = list(upper = formula(upper.lm), lower = ~1), direction = c('both'))
summary(stepwise.lm)

# Junk Model

junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data = data.train)
summary(junk.lm)

# VIF

cbind.fill <- function(...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x)
        rbind(x, matrix(, n - nrow(x), ncol(x)))))
}

vif.fwd <- vif(forward.lm)
vif.fwd.dt <- data.table(FwdColumn = names(vif.fwd), FwdValue = vif.fwd)
setorder(vif.fwd.dt, - FwdValue)

vif.bwd <- vif(backward.lm)
vif.bwd.dt <- data.table(BwdColumn = names(vif.bwd), BwdValue = vif.bwd)
setorder(vif.bwd.dt, -BwdValue)

vif.step <- vif(stepwise.lm)
vif.step.dt <- data.table(StepColumn = names(vif.step), StepValue = vif.step)
setorder(vif.step.dt, -StepValue)

vif.junk <- vif(junk.lm)
vif.junk.dt <- data.table(JunkColumn = names(vif.junk), JunkValue = vif.junk)
setorder(vif.junk.dt, -JunkValue)

vif.dt <- cbind.fill(fiv.fwd.dt, vif.bwd.dt, vif.step.dt, vif.junk.dt)

getVIFResults <- function(vif) {
  formattable(vif.dt, align = c("l", "c", "c", "c", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
 ))
}

# getVIFResults(as.data.table(vif.dt))

sort(vif(forward.lm), decreasing = TRUE)
sort(vif(backward.lm), decreasing = TRUE)
sort(vif(stepwise.lm), decreasing = TRUE)
sort(vif(junk.lm), decreasing = TRUE)

# Evaluation

insample_fit <- function(name, fit) {
  return(data.table(Model = name, AdjRSq = summary(fit)$adj.r.squared, AIC = AIC(fit), BIC = BIC(fit), MSE = mean(summary(fit)$residuals ^ 2), MAE = mean(abs(fit$residuals))))
}

consolidated.diag <- rbind(insample_fit("Forward", forward.lm), insample_fit("Backward", backward.lm), insample_fit("Stepwise", stepwise.lm), insample_fit("Junk", junk.lm))

consolidated.diag$AdjRSq_Rank <- rank(-consolidated.diag$AdjRSq)
consolidated.diag$AIC_Rank <- rank(consolidated.diag$AIC)
consolidated.diag$BIC_Rank <- rank(consolidated.diag$BIC)
consolidated.diag$MSE_Rank <- rank(consolidated.diag$MSE)
consolidated.diag$MAE_Rank <- rank(consolidated.diag$MAE)

consolidated.diag$AIC <- dollar(consolidated.diag$AIC)
consolidated.diag$BIC <- dollar(consolidated.diag$BIC)
consolidated.diag$MAE <- dollar(consolidated.diag$MAE)
consolidated.diag$MSE <- dollar(consolidated.diag$MSE)

consolidated.diag <- consolidated.diag[, .(Model, AdjRSq, AdjRSq_Rank, AIC, AIC_Rank, BIC, BIC_Rank, MSE, MSE_Rank, MAE, MAE_Rank)]

formattable(consolidated.diag, align = c("l", "c", "c", "c", "c", "c", "c", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

dollar(mse(data.train$SalePrice, predict(forward.lm)))

# Predictive Accuracy

outsample_fit <- function(name, fit, test.data) {
  y_hat <- predict(fit, newdata = test.data)
  residuals <- y_hat - test.data$SalePrice

  return(data.table(Model = name, MAE = mean(abs(residuals)), MSE = mse(test.data$SalePrice, y_hat)))
}

outsample.diag <- rbind(outsample_fit("Forward", forward.lm, data.test), outsample_fit("Backward", backward.lm, data.test), outsample_fit("Stepwise", stepwise.lm, data.test), outsample_fit("Junk", junk.lm, data.test))

outsample.diag$MSE_Rank <- rank(outsample.diag$MSE)
outsample.diag$MAE_Rank <- rank(outsample.diag$MAE)

outsample.diag <- outsample.diag[, .(Model, MSE, MSE_Rank, MAE, MAE_Rank)]
outsample.diag$MAE <- dollar(outsample.diag$MAE)
outsample.diag$MSE <- dollar(outsample.diag$MSE)

formattable(outsample.diag, align = c("l", "c", "c", "c", "c", "c", "c", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

# Operational Validation

# Training Data
# Abs Pct Error

insample.grade.model <- function(fit, train) {
  model.pct <- abs(fit$residuals) / train$SalePrice;

  prediction.grade <- ifelse(model.pct <= 0.10, 'Grade 1: [0.0.10]',
          ifelse(model.pct <= 0.15, 'Grade 2: (0.10,0.15]',
            ifelse(model.pct <= 0.25, 'Grade 3: (0.15,0.25]',
            'Grade 4: (0.25+]')))

  trainTable <- table(prediction.grade)
  round(trainTable / sum(trainTable) * 100, 4)
}

insample.grades <- data.table(rbind(insample.grade.model(forward.lm, data.train), insample.grade.model(backward.lm, data.train), insample.grade.model(stepwise.lm, data.train), insample.grade.model(junk.lm, data.train)))
insample.grades <- cbind(c("Forward", "Backward", "Stepwise", "Junk"), insample.grades)
colnames(insample.grades)[1] <- "Model"

formattable(insample.grades, align = c("l", "c", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

outsample.grade.model <- function(fit, test.data, actual) {
  options(warn = -1)
  predicted <- predict(fit, newdata = test.data)
  options(warn = 1)

  model.pct <- abs(actual - predicted) / actual;

  prediction.grade <- ifelse(model.pct <= 0.10, 'Grade 1: [0.0.10]',
          ifelse(model.pct <= 0.15, 'Grade 2: (0.10,0.15]',
            ifelse(model.pct <= 0.25, 'Grade 3: (0.15,0.25]',
            'Grade 4: (0.25+]')))

  trainTable <- table(prediction.grade)
  round(trainTable / sum(trainTable) * 100, 4)
}

outsample.grades <- data.table(rbind(outsample.grade.model(forward.lm, data.test, data.test$SalePrice),
                                  outsample.grade.model(backward.lm, data.test, data.test$SalePrice),
                                  outsample.grade.model(stepwise.lm, data.test, data.test$SalePrice),
                                  outsample.grade.model(junk.lm, data.test, data.test$SalePrice)))

outsample.grades <- cbind(c("Forward", "Backward", "Stepwise", "Junk"), outsample.grades)
colnames(outsample.grades)[1] <- "Model"

formattable(outsample.grades, align = c("l", "c", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

# Model Revision

colnames(data.train)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + GrLivArea + TotalBsmtSF + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model.baseline <- lm(formula = fmla.backward, data = data.train)

f.pred.train <- data.table(actual = data.train$SalePrice, pred = predict(f.model.baseline))
GainCurvePlot(f.pred.train, "pred", "actual", "Predicted Sale Price (Train)")

coef(f.model.baseline)

summary(f.model.baseline)
summary(backward.lm)

# MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone
# KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA
# ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA
# Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab
# BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA

model.fit.stats <- function(name, fit, test.data) {

  in.sample <- insample_fit(paste(name, "(IS)"), fit)
  options(warn = -1)
  out.sample <- outsample_fit(paste(name, "(OS)"), fit, test.data)
  options(warn = 1)

  ret <- rbind(in.sample, out.sample, fill = T)
  ret$AdjRSq <- round(ret$AdjRSq, 4)
  ret$AIC <- dollar(ret$AIC)
  ret$BIC <- dollar(ret$BIC)
  ret$MSE <- dollar(ret$MSE)
  ret$MAE <- dollar(ret$MAE)

  ret
}

model.fit.stats("Final Baseline", f.model.baseline, data.test)

# Term Reduction

diff <- data.table(Variable = character(), RSq = numeric(), Diff = numeric())

baseline.rsq <- summary(f.model.baseline)$adj.r.squared
baseline <- data.table(Variable = "Baseline", RSq = rsq, Diff = 0)
diff <- rbind(diff, baseline)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + GrLivArea + TotalBsmtSF + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + GrLivArea + TotalBsmtSF + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + GrLivArea + TotalBsmtSF + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + GrLivArea + TotalBsmtSF + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + GrLivArea + TotalBsmtSF + HouseAge + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + GrLivArea + TotalBsmtSF + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "HouseAge", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ QualityIndex + LotArea + GrLivArea + TotalBsmtSF + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "TotalSqftCalc", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + TotalBsmtSF + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "GrLivArea", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + GrLivArea + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "TotalBsmtSF", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + GrLivArea + TotalBsmtSF + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "LotArea", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

fmla.backward = as.formula("SalePrice ~ TotalSqftCalc + LotArea + TotalBsmtSF + GrLivArea + HouseAge + BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + Foundation.Slab + MasVnrType. + MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.CBlock + MasVnrType.None + MasVnrType.Stone")
f.model <- lm(formula = fmla.backward, data = data.train)
new.rsq <- summary(f.model)$adj.r.squared
vars <- data.table(Variable = "QualityIndex", RSq = new.rsq, Diff = baseline.rsq - new.rsq)
diff <- rbind(diff, vars)

summary(f.model)

setorder(diff, Diff)

diff$Diff <- round(diff$Diff, 4)

formattable(diff, align = c("l", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

# Interaction Terms

# Full

fmla.int.full = as.formula("SalePrice ~ TotalBsmtSF + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Ex*TotalBsmtSF + BsmtQual.Fa*TotalBsmtSF + BsmtQual.Gd*TotalBsmtSF")
full.model <- lm(fmla.int.full, data = data.test)
anova(full.model)
summary(full.model)

mean(data.test$SalePrice)

# Reduced

fmla.int.reduced = as.formula("SalePrice ~ TotalBsmtSF + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd")
reduced.model <- lm(fmla.int.reduced, data = data.test)
anova(reduced.model)
summary(reduced.model)

plot_model(lm(formula = fmla.int.full, data = data.train), type = "int")
plot_model(lm(formula = fmla.final.int, data = data.train), type = "pred", terms = c("BsmtQual", "TotalBsmtSF"))

Plot(SalePrice, TotalBsmtSF, by = BsmtQual, fit = TRUE, data = data.train)

aov_full <- anova(full.model)
aov_reduced <- anova(reduced.model)

anova(full.model, reduced.model)

f.val <- ((1630694755062 - 1603752680650) / 3) / (1603752680650 / 644)

alpha = .05
f.crit <- qf(1 - alpha, 3, 644)
round(f.crit, 4)

ifelse(f.val > f.crit, "Reject the Null", "Cannot reject the null")

# Final Model Tuning

fmla.final = as.formula("SalePrice ~ QualityIndex + TotalSqftCalc + TotalBsmtSF + GrLivArea + HouseAge + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Fa + ExterQual.Ex + ExterQual.Fa + ExterQual.Gd  + BsmtQual.Ex*TotalBsmtSF + BsmtQual.Fa*TotalBsmtSF + BsmtQual.Gd*TotalBsmtSF")
f.model <- lm(formula = fmla.final, data = data.train)

summary(f.model)

coef(f.model)

baseline.score <- model.fit.stats("Final Baseline", f.model.baseline, data.test)
final.score <- model.fit.stats("Final Tuned", f.model, data.test)

formattable(rbind(baseline.score, final.score), align = c("l", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

final.grades.in <- rbind(insample.grade.model(f.model.baseline, data.train), insample.grade.model(f.model, data.train))
final.grades.in <- as.data.table(cbind(c("Baseline", "Tuned"), final.grades.in))
colnames(final.grades.in)[1] <- "Model"

formattable(final.grades.in, align = c("l", "c", "c", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

final.grades.out <- rbind(outsample.grade.model(f.model.baseline, data.test, data.test$SalePrice), outsample.grade.model(f.model, data.test, data.test$SalePrice))
final.grades.out <- as.data.table(cbind(c("Baseline", "Tuned"), final.grades.out))
colnames(final.grades.out)[1] <- "Model"

formattable(final.grades.out, align = c("l", "c", "c", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

f.pred.train <- data.table(actual = data.train$SalePrice, pred = predict(f.model))
GainCurvePlot(f.pred.train, "pred", "actual", "Predicted Sale Price (Train)")

f.pred.test <- data.table(actual = data.test$SalePrice, pred = predict(f.model, newdata = data.test))
GainCurvePlot(f.pred.test, "pred", "actual", "Predicted Sale Price (Test)")

# Residual Diagnostics

options(warn = -1)
data.clean$pred <- predict(f.model, newdata = data.clean)
options(warn = 1)

p1 <- ggplot(data.clean, aes(x = SalePrice, y = pred)) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(f.model)$adj.r.squared, 5),
                      "Intercept =", signif(f.model$coef[[1]], 5),
                      " Slope =", signif(f.model$coef[[2]], 5),
                      " P =", signif(summary(f.model)$coef[2, 4], 5))) +
    scale_x_continuous(labels = dollar_format(largest_with_cents = .2)) +
    scale_y_continuous(labels = dollar_format(largest_with_cents = .2))
    
res <- data.table(Value = residuals(f.model))
res.norm <- data.table(Value = (res$Value - mean(res$Value)) / sd(res$Value))

p2 <- ggplot(res, aes(Value, fill = ..count..)) +
    geom_histogram(breaks = pretty(res.norm$Value)) +
    labs(title = "Standardized Residuals")

grid.arrange(p1, p2)

f.pred.combined <- data.table(actual = data.clean$SalePrice, pred = predict(f.model, newdata = data.clean))
GainCurvePlot(f.pred.combined, "pred", "actual", "Predicted Sale Price (Combined)")
