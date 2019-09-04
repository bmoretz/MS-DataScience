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
library(caret)
library(sjPlot)
library(sjmisc)
library(car)
library(WVPlots)
library(lessR)
library(MASS)
library(RColorBrewer)
library(ggcorrplot)
library(glmulti)
library(glmnet)
library(gbm)
library(Metrics)
library(pscl)

#####################################################################
#########################   Modeling 4  #############################
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

data.wine <- as.data.table(read.csv(file = "wine.csv", head = TRUE, sep = ","))
data.wine$`ï..INDEX` <- NULL # remove index column

#####################################################################
### Variable Transformations

summary(data.wine)

skim(data.wine)

# Transform Label Appeal, standardize label appeal to 1-5

data.wine$LabelAppeal <- data.wine$LabelAppeal + 3

ggplot(data.wine, aes(LabelAppeal, fill = ..count..)) +
  geom_histogram()

alcohol.removed <- nrow(data.wine[Alcohol < 0 | is.na(Alcohol)])

# Remove negative ABV values.
data.wine <- data.wine[Alcohol >= 0]

alcohol.removed / nrow(data.wine)

# Binary Good/Bad Quality Indicator

data.wine$Quality <- ifelse(data.wine$STARS >= 3, "Good", "Bad")
data.wine$Quality <- as.factor(data.wine$Quality)

######################################################################################
### Univariate3 EDA
#####################################################################################

skim(data.wine)

getDistribution <- function(data, col, lab) {
  p1 <- ggplot(data, aes(x = 1, col)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(alpha = 0.2, color = 'red') +
    labs(y = lab, x = '')

  p2 <- ggplot(data, aes(col, fill = ..count..)) +
    geom_histogram() +
    labs(x = lab)

  grid.arrange(p1, p2, nrow = 1)
}

# ggpairs(data.wine)

normalize <- function(var) {
  (var - mean(var, na.rm = T)) / sd(var, na.rm = T)
}

# Alcohol, Continious

getDistribution(data.wine, data.wine$Alcohol, 'Alcohol')

### Chlorides, Continious

summary(data.wine$Chlorides)

getDistribution(data.wine, data.wine$Chlorides, 'Chlorides')

data.wine$Norm_Chlorides <- scale(data.wine$Chlorides)

getDistribution(data.wine, data.wine$Norm_Chlorides, 'Norm Chlorides')

# CitricAcid, Continuous

summary(data.wine$CitricAcid)

getDistribution(data.wine, data.wine$CitricAcid, 'Citric Acid')

data.wine$Norm_CitricAcid <- scale(data.wine$CitricAcid)

getDistribution(data.wine, data.wine$Norm_CitricAcid, 'Norm Citric Acid')

# Fixed Acidity

summary(data.wine$FixedAcidity)

getDistribution(data.wine, data.wine$FixedAcidity, 'Fixed Acidity')

data.wine$Norm_FixedAcidity <- scale(data.wine$FixedAcidity)

getDistribution(data.wine, data.wine$Norm_FixedAcidity, 'Fixed Acidity')

# Free Sulfur Dioxide, Continuous

summary(data.wine$FreeSulfurDioxide)

getDistribution(data.wine, data.wine$FreeSulfurDioxide, 'Free-Sulfur Dioxide')

data.wine$Norm_FreeSulfurDioxide <- scale(data.wine$FreeSulfurDioxide)

getDistribution(data.wine, data.wine$Norm_FSD, 'Free-Sulfur Dioxide')

# ResidualSugar, Continious

summary(data.wine$ResidualSugar)

getDistribution(data.wine, data.wine$ResidualSugar, 'ResidualSugar')

data.wine$Norm_ResidualSugar <- scale(data.wine$ResidualSugar)

getDistribution(data.wine, data.wine$Norm_ResidualSugar, 'Norm ResidualSugar')

# Total Sulfur Dioxide, Continious

summary(data.wine$TotalSulfurDioxide)

getDistribution(data.wine, data.wine$TotalSulfurDioxide, 'Total Sulfur Dioxide')

data.wine$Norm_TotalSulfurDioxide <- scale(data.wine$TotalSulfurDioxide)

getDistribution(data.wine, data.wine$Norm_TotalSulfurDioxide, 'Norm Total Sulfur Dioxide')

# Sulphates, Continious

summary(data.wine$Sulphates)

getDistribution(data.wine, data.wine$Sulphates, 'Sulphates')

data.wine$Norm_Sulphates <- scale(data.wine$Sulphates)

getDistribution(data.wine, data.wine$Norm_Sulphates, 'Sulphates')

# Volatile Acidity, Continuous

summary(data.wine$VolatileAcidity)

getDistribution(data.wine, data.wine$VolatileAcidity, 'Volatile Acidity')

data.wine$Norm_VolAcidity <- scale(data.wine$VolatileAcidity)

getDistribution(data.wine, data.wine$Norm_VolAcidity, 'Norm Volatile Acidity')

# Density, Continious

getDistribution(data.wine, data.wine$Density, 'Density')

# pH, Continious

ggplot(data.wine, aes(x = pH, fill = Quality)) +
  geom_density(alpha = .3)

getDistribution(data.wine, data.wine$pH, 'pH')

# Label Appeal, Normal Discrete

ggplot(data.wine, aes(LabelAppeal, fill = ..count..)) +
  geom_histogram()

getDistribution(data.wine, data.wine$LabelAppeal, 'Label Appeal')

# Acid Index, Poisson Discrete

ggplot(data.wine, aes(AcidIndex, fill = ..count..)) +
  geom_histogram()

getDistribution(data.wine, data.wine$AcidIndex, 'Acid Index')

######################################################################################
### Model Data Correlations
#####################################################################################

getCorTable <- function(cols) {
  # selected features correlation matrix
  sale.cor <- cor(cols, use = "pairwise.complete.obs")[, "STARS"]
  sale.cor <- sort(sale.cor, decreasing = T)

  sale.cor <- sale.cor[-1] # remove SalePrice
  sale.cor <- sale.cor[-1] # remove LogSalePrice

  tbl.sale.cor <- melt(sale.cor)
  colnames(tbl.sale.cor) <- c("Correlation to STARS")

  formattable(tbl.sale.cor, align = c("l", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
  ))
}

skim(data.wine)

model.numeric.col <- unlist(lapply(data.wine, is.numeric))
data.wine.numeric <- data.wine[, model.numeric.col, with = F]

str(data.wine.numeric)

getCorTable(data.wine.numeric)

# Correlation matrix
ggcorrplot(round(cor(data.wine.numeric[, 1:16]), 1),
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Wine Metrics")

ggplot(data = data.wine,
       aes(y = Density, x = Alcohol,
           color = STARS)) +
           geom_point(alpha = 0.8, size = 1) +
           geom_smooth(method = "lm", se = FALSE, size = 1) +
           scale_color_brewer(type = 'seq',
                   guide = guide_legend(title = 'Quality'))

PairPlot(data.wine,
         colnames(data.wine)[4:10],
         "Chemical Properties by Quality",
         group_var = "Quality")

######################################################################################
### Bivariate EDA
#####################################################################################

data.complete <- data.wine[complete.cases(data.wine)]

ggplot(data.complete, aes(x = CitricAcid, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Chlorides, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = FixedAcidity, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = FreeSulfurDioxide, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = ResidualSugar, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = TotalSulfurDioxide, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Sulphates, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = VolatileAcidity, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Density, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = pH, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = AcidIndex, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = AcidIndex, fill = as.factor(STARS))) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Alcohol, fill = Quality)) +
  geom_density(alpha = .3)

ggplot(data.complete, aes(x = Alcohol, fill = as.factor(STARS))) +
  geom_density(alpha = .3)

# Alcohol ~ Label

ggplot(data.wine, aes(Alcohol, fill = LabelAppeal, colour = LabelAppeal)) +
  geom_density(alpha = 0.1) + facet_grid(LabelAppeal ~ .)

# STARS

ggplot(data.wine, aes(STARS, CitricAcid, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, Chlorides, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, FixedAcidity, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, FreeSulfurDioxide, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, ResidualSugar, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, TotalSulfurDioxide, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, Sulphates, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, VolatileAcidity, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, pH, group = STARS, fill = STARS)) +
  geom_boxplot(varwidth = T)

ggplot(data.wine, aes(STARS, AcidIndex, group = STARS, fill = STARS)) +
  geom_boxplot()

ggplot(data.wine, aes(STARS, Density, group = STARS, fill = STARS)) +
  geom_boxplot()

# Quality

ggplot(data = data.wine, aes(x = Quality, y = FixedAcidity, group = Quality)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.complete, aes(x = Quality, y = FixedAcidity, group = Quality)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Quality, y = FixedAcidity, group = Quality)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Quality, y = VolatileAcidity, group = Quality)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = pH, y = VolatileAcidity, group = Quality)) +
  geom_point()

# STARS

ggplot(data = data.wine, aes(x = STARS, y = FixedAcidity, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = LabelAppeal, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = VolatileAcidity, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = ResidualSugar, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = TotalSulfurDioxide, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = pH, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = STARS, y = CitricAcid, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

# CASES

ggplot(data = data.wine, aes(x = Cases, y = FixedAcidity, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = VolatileAcidity, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = CitricAcid, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = TotalSulfurDioxide, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = ResidualSugar, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = Sulphates, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = AcidIndex, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = Density, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = pH, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = STARS, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = Alcohol, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = LabelAppeal, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = data.wine, aes(x = Cases, y = LabelAppeal, group = Cases)) +
  geom_jitter() +
  geom_smooth(aes(Cases, LabelAppeal), method = "lm")

#####################################################################
### Response, STARS

data.stars <- data.wine[!is.na(STARS)]

nrow(data.stars) - nrow(data.wine)

n.total.stars <- nrow(data.stars)

data.stars$u <- runif(n = n.total.stars, min = 0, max = 1)

# Create train/test split;
stars.train <- subset(data.stars, u < 0.70)
stars.test <- subset(data.stars, u >= 0.70)

# Poisson Distribution
ggplot(stars.train, aes(STARS, fill = ..count..)) +
  geom_histogram() +
  labs(title = "STARS Rating")

nrow(stars.train[STARS <= 2]) / nrow(stars.train)

dist <- stars.train[, .(Count = .N / nrow(stars.train)), by = STARS]
dist[, Poisson := dpois(STARS, 1)]

ggplot(dist) +
  geom_line(aes(STARS, Count, color = "Stars"), lwd = 1.5) +
  geom_line(aes(STARS, Poisson, color = "Poisson"), lwd = 1.5, linetype = "dashed") +
  labs(title = "Stars Distribution vs Possion Distribution") +
  theme(legend.position = "bottom")

# STARS EDA

ggplot(data = stars.train, aes(x = STARS, y = FixedAcidity, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = LabelAppeal, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = VolatileAcidity, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = ResidualSugar, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = TotalSulfurDioxide, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = pH, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = stars.train, aes(x = STARS, y = CitricAcid, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

skim(stars.train)

stars.numeric.col <- unlist(lapply(stars.train, is.numeric))
stars.wine.numeric <- stars.train[, stars.numeric.col, with = F]

str(stars.wine.numeric)

getCorTable(stars.wine.numeric)

# Correlation matrix
ggcorrplot(round(cor(stars.wine.numeric[, 1:16]), 1),
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Wine Metrics ~ Stars")

stars.train <- stars.train[complete.cases(stars.train)]

train.stars <- stars.train$STARS
train.stars.vars <- stars.train[, !c("STARS", "u", "Quality", "Purchase")] # , "Cases"

summary(stars.null <- glm(STARS ~ 1, family = poisson, data = stars.train))

stars.train.cols <- paste(paste(colnames(train.stars.vars), collapse = " + "))

# Define the upper model as the FULL model
upper.lm <- glm(paste("STARS ~ ", stars.train.cols), data = stars.train, family = poisson)
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- glm(STARS ~ 1, data = stars.train, family = poisson)
summary(lower.lm)

#Backward selection of variables
backward.stars <- train(x = train.stars.vars,
                        y = train.stars,
                        scope = list(upper = formula(upper.lm), lower = ~1),
                        method = "glmStepAIC",
                        family = poisson,
                        direction = c('backward'))

summary(backward.stars)

anova(stars.null, backward.stars, test = "Chisq")

#Forward selection of variables
forward.stars <- train(x = train.stars.vars,
                       y = train.stars,
                       method = "glmStepAIC",
                       family = poisson,
                       direction = c('forward'))

summary(forward.stars)

anova(stars.null, forward.stars, test = "Chisq")

# Stepwise selection of variables
stepwise.stars <- train(x = train.stars.vars,
                       y = train.stars,
                       method = "glmStepAIC",
                       family = poisson,
                       direction = c('both'))

summary(stepwise.stars)

anova(stars.null, stepwise.stars, test = "Chisq")

stars.m1 <- stars.train[, .(STARS)]

stars.m1$deviation <- resid(backward.stars, type = "deviance")
stars.m1$pred <- predict(backward.stars, type = "raw")

p1 <- ggplot(stars.m1, aes(pred, deviation)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Deviation vs Fitted") +
  theme(legend.position = 'none')

p2 <- ggplot(stars.m1, aes(sample = stars.m1$deviation)) +
  geom_qq() +
  geom_qq_line()

grid.arrange(p1, p2, nrow = 2)

stars.test.m1 <- stars.test[, .(STARS)]
stars.test.m1$pred <- round(predict(backward.stars, newdata = stars.test, type = "raw"))
stars.test.m1$accurate <- stars.test.m1$STARS == stars.test.m1$pred

round(sum(stars.test.m1$accurate) / nrow(stars.test.m1) * 100, 1)

m1.results <- stars.test.m1[, .(PctAccurate = (sum(accurate) / .N) * 100), by = STARS]
setorder(m1.results, STARS)
m1.results

formattable(m1.results, align = c("l", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))))

p1 <- ggplot(stars.test.m1, aes(STARS, fill = ..count..)) +
  geom_histogram()

p2 <- ggplot(stars.test.m1, aes(pred, fill = ..count..)) +
  geom_histogram()

grid.arrange(p1, p2, nrow = 2)

# Holdout Cases variable

train.stars <- stars.train$STARS
train.stars.vars <- stars.train[, !c("STARS", "u", "Quality", "Purchase", "Cases")]

stars.train.cols <- paste(paste(colnames(train.stars.vars), collapse = " + "))

# Define the upper model as the FULL model
upper.lm <- glm(paste("STARS ~ ", stars.train.cols), data = stars.train, family = poisson)
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- glm(STARS ~ 1, data = stars.train, family = poisson)
summary(lower.lm)

#Backward selection of variables
backward.stars <- train(x = train.stars.vars,
                        y = train.stars,
                        scope = list(upper = formula(upper.lm), lower = ~1),
                        method = "glmStepAIC",
                        family = poisson,
                        direction = c('backward'))

summary(backward.stars)

anova(stars.null, backward.stars, test = "Chisq")

#Forward selection of variables
forward.stars <- train(x = train.stars.vars,
                       y = train.stars,
                       method = "glmStepAIC",
                       family = poisson,
                       direction = c('forward'))

summary(forward.stars)

anova(stars.null, forward.stars, test = "Chisq")

# Stepwise selection of variables
stepwise.stars <- train(x = train.stars.vars,
                       y = train.stars,
                       method = "glmStepAIC",
                       family = poisson,
                       direction = c('both'))

summary(stepwise.stars)

anova(stars.null, stepwise.stars, test = "Chisq")

data.table(Method = "Forward", AIC = forward.stars) 

stars.m2 <- stars.train[, .(STARS)]

stars.m2$deviation <- resid(stepwise.stars, type = "deviance")
stars.m2$pred <- round(predict(stepwise.stars, type = "raw"), 0)

p1 <- ggplot(stars.m2, aes(pred, deviation)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Deviation vs Fitted") +
  theme(legend.position = 'none')

p2 <- ggplot(stars.m2, aes(sample = stars.m2$deviation)) +
  geom_qq() +
  geom_qq_line()

grid.arrange(p1, p2, nrow = 2)

stars.test.m2 <- stars.test[, .(STARS)]
stars.test.m2$pred <- round(predict(stepwise.stars, newdata = stars.test, type = "raw"))
stars.test.m2$accurate <- stars.test.m2$STARS == stars.test.m2$pred

round(sum(stars.test.m2$accurate) / nrow(stars.test.m2) * 100, 1)

m2.results <- stars.test.m2[, .(PctAccurate = (sum(accurate) / .N) * 100), by = STARS]
setorder(m2.results, STARS)
m2.results

formattable(m2.results, align = c("l", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))))

p1 <- ggplot(stars.test.m2, aes(STARS, fill = ..count..)) +
  geom_histogram()

p2 <- ggplot(stars.test.m2, aes(pred, fill = ..count..)) +
  geom_histogram()

grid.arrange(p1, p2, nrow = 2)

# Final Model

summary(m3 <- glm(STARS ~ Cases + Alcohol + LabelAppeal + AcidIndex, data = stars.train, family = poisson))

AIC(m3)

stars.m3 <- stars.train[, .(STARS)]

stars.m3$deviation <- resid(m3, type = "deviance")
stars.m3$pred <- predict(m3, type = "response")

p1 <- ggplot(stars.m3, aes(pred, deviation)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Deviation vs Fitted") +
  theme(legend.position = 'none')

p2 <- ggplot(stars.m3, aes(sample = stars.m3$deviation)) +
  geom_qq() +
  geom_qq_line()

grid.arrange(p1, p2, nrow = 2)

stars.test.m3 <- stars.test[, .(STARS)]
stars.test.m3$pred <- round(predict(m3, newdata = stars.test, type = "response"))
stars.test.m3$accurate <- stars.test.m3$STARS == stars.test.m3$pred

round(sum(stars.test.m3$accurate) / nrow(stars.test.m3) * 100, 1)

m3.results <- stars.test.m3[, .(PctAccurate = (sum(accurate) / .N) * 100), by = STARS]
setorder(m3.results, STARS)
m3.results

formattable(m3.results, align = c("l", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))))

p1 <- ggplot(stars.test.m3, aes(STARS, fill = ..count..)) +
  geom_histogram()

p2 <- ggplot(stars.test.m3, aes(pred, fill = ..count..)) +
  geom_histogram()

grid.arrange(p1, p2, nrow = 2)

#####################################################################
### Purchase Decision

data.purchase <- data.wine[!is.na(Purchase)]

nrow(data.purchase) - nrow(data.purchase)

n.total.purchase <- nrow(data.purchase)

data.purchase$u <- runif(n = n.total.purchase, min = 0, max = 1)

# Create train/test split;
purchase.train <- subset(data.stars, u < 0.70)
purchase.test <- subset(data.stars, u >= 0.70)

# Logistic Regression
ggplot(purchase.test, aes(Purchase, fill = ..count..)) +
  geom_histogram() +
  labs(title = "Purchase Decisions")

dist <- purchase.train[, .(Count = .N / nrow(purchase.train)), by = Purchase]
dist[, Poisson := dbinom(Purchase, size = n.total.purchase, prob = 1 - .07)]

ggplot(dist) +
  geom_line(aes(Purchase, Count, color = "Purchase"), lwd = 1.5) +
  geom_line(aes(Purchase, Poisson, color = "Binomial"), lwd = 1.5, linetype = "dashed") +
  labs(title = "Stars Distribution vs Possion Distribution") +
  theme(legend.position = "bottom")

# Purchase EDA

ggplot(data = purchase.train, aes(x = Purchase, y = FixedAcidity, group = Purchase)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = purchase.train, aes(x = Purchase, y = VolatileAcidity, group = Purchase)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = purchase.train, aes(x = Purchase, y = VolatileAcidity, group = Purchase)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = purchase.train, aes(x = Purchase, y = ResidualSugar, group = Purchase)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = purchase.train, aes(x = Purchase, y = TotalSulfurDioxide, group = Purchase)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = purchase.train, aes(x = Purchase, y = pH, group = Purchase)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = purchase.train, aes(x = Purchase, y = CitricAcid, group = Purchase)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = purchase.train, aes(x = LabelAppeal, y = Purchase, group = LabelAppeal)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = purchase.train, aes(x = STARS, y = Purchase, group = STARS)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

skim(purchase.train)

purchase.numeric.col <- unlist(lapply(purchase.train, is.numeric))
purchase.wine.numeric <- stars.train[, stars.numeric.col, with = F]

str(purchase.wine.numeric)

# Correlation matrix
ggcorrplot(round(cor(purchase.wine.numeric[, 1:16]), 1),
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Wine Metrics ~ Purchase")

purchase.train <- purchase.train[complete.cases(purchase.train)]

train.purchase <- as.factor(purchase.train$Purchase)
train.purchase.vars <- purchase.train[, !c("u", "Quality", "Purchase", "Cases")]

summary(purchase.null <- glm(Purchase ~ 1, family = binomial, data = purchase.train))

purchase.train.cols <- paste(paste(colnames(train.purchase.vars), collapse = " + "))

# Define the upper model as the FULL model
upper.lm <- glm(paste("Purchase ~ ", purchase.train.cols), data = purchase.train, family = binomial)
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- glm(Purchase ~ 1, data = purchase.train, family = binomial)
summary(lower.lm)

#Backward selection of variables
backward.purchase <- train(x = train.purchase.vars,
                        y = train.purchase,
                        scope = list(upper = formula(upper.lm), lower = ~1),
                        method = "glmStepAIC",
                        family = binomial,
                        direction = c('backward'),
                        trace = F)

summary(backward.purchase)

#Forward selection of variables
forward.purchase <- train(x = train.purchase.vars,
                       y = train.purchase,
                       method = "glmStepAIC",
                       family = binomial,
                       direction = c('forward'),
                       trace = F)

summary(forward.purchase)

# Stepwise selection of variables
stepwise.purchase <- train(x = train.purchase.vars,
                       y = train.purchase,
                       method = "glmStepAIC",
                       family = binomial,
                       direction = c('both'),
                       trace = F)

summary(stepwise.purchase)


# Model summary functions

log_fit <- function(name, fit) {
  return(data.table(Model = name, AIC = AIC(fit), BIC = BIC(fit)))
}

logit2prob <- function(logit) {
  odds <- exp(logit)
  prob <- round(odds / (1 + odds), 3)

  return(prob)
}

model.summary <- rbind(log_fit("Forward Selection", backward.purchase$finalModel),
                      log_fit("Backward Selection", forward.purchase$finalModel),
                      log_fit("Stepwise Selection", stepwise.purchase$finalModel))

formattable(model.summary, align = c("l", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

summary(stepwise.purchase$finalModel)

round(coef(stepwise.purchase$finalModel), 3)

b0 <- coef(stepwise.purchase$finalModel)[1]
b0_logit <- exp(b0)
round((b0_logit / (1 + b0_logit)), 3) * 100 # convert to probability

X1 <- coef(stepwise.purchase$finalModel)[2]
X1_logit <- exp(X1)
round((X1_logit - 1) * 100, 2)
round(X1_logit / (1 + X1_logit), 3) * 100 # convert to probability

X2 <- coef(stepwise.purchase$finalModel)[3]
X2_logit <- exp(X2)
round((X2_logit - 1) * 100, 2)
round(X2_logit / (1 + X2_logit), 3) * 100 # convert to probability

X3 <- coef(stepwise.purchase$finalModel)[4]
X3_logit <- exp(X3)
round((X3_logit - 1) * 100, 2)
round(X3_logit / (1 + X3_logit), 3) * 100 # convert to probability

X4 <- coef(stepwise.purchase$finalModel)[5]
X4_logit <- exp(X4)
round((X4_logit - 1) * 100, 2)
round(X4_logit / (1 + X4_logit), 3) * 100 # convert to probability

X5 <- coef(stepwise.purchase$finalModel)[6]
X5_logit <- exp(X5)
round((X5_logit - 1) * 100, 2)
round(X5_logit / (1 + X5_logit), 3) * 100 # convert to probability

X6 <- coef(stepwise.purchase$finalModel)[7]
X6_logit <- exp(X6)
round((X6_logit - 1) * 100, 2)
round(X6_logit / (1 + X6_logit), 3) * 100 # convert to probability

X7 <- coef(stepwise.purchase$finalModel)[8]
X7_logit <- exp(X7)
round((X7_logit - 1) * 100, 2)
round(X7_logit / (1 + X7_logit), 3) * 100 # convert to probability

#####################################################################
### Evaluation

purchase.m1 <- purchase.train[, .(Purchase)]

purchase.m1$deviation <- resid(stepwise.purchase$finalModel, type = "deviance")
purchase.m1$pred <- predict(stepwise.purchase$finalModel, type = "response")

p1 <- ggplot(purchase.m1, aes(pred, deviation)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Deviation vs Fitted") +
  theme(legend.position = 'none')

p2 <- ggplot(purchase.m1, aes(sample = purchase.m1$deviation)) +
  geom_qq() +
  geom_qq_line()

grid.arrange(p1, p2, nrow = 2)

purchase.test.m1 <- purchase.test[, .(Purchase, STARS)]
purchase.test.m1$pred <- ifelse( predict(stepwise.purchase$finalModel, newdata = purchase.test, type = "response") > .5, 1, 0 )
purchase.test.m1 <- purchase.test.m1[!is.na(pred)]

purchase.test.m1$accurate <- purchase.test.m1$Purchase == purchase.test.m1$pred

round(sum(purchase.test.m1$accurate) / nrow(purchase.test.m1) * 100, 1)

m1.results <- purchase.test.m1[, .(PctAccurate = (sum(accurate) / .N) * 100), by = Purchase]
m1.results$PctAccurate <- round(m1.results$PctAccurate, 3)
setorder(m1.results, Purchase)
m1.results

formattable(m1.results, align = c("l", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))))

m1.results <- purchase.test.m1[, .(PctAccurate = (sum(accurate) / .N) * 100), by = STARS]
m1.results$PctAccurate <- round(m1.results$PctAccurate, 3)
setorder(m1.results, STARS)
m1.results

formattable(m1.results, align = c("l", "r"),
  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))))

coef(stepwise.purchase$finalModel)

X2_val <- mean(purchase.test$VolatileAcidity, na.rm = T)
X3_val <- mean(purchase.test$ResidualSugar, na.rm = T)
X4_val <- mean(purchase.test$TotalSulfurDioxide, na.rm = T)
X5_val <- mean(purchase.test$Alcohol, na.rm = T)
X6_val <- mean(purchase.test$LabelAppeal, na.rm = T)
X7_val <- mean(purchase.test$AcidIndex, na.rm = T)

logits <- b0 + X1 * purchase.test$STARS + X2 * X2_val + X3 + X3 * X3_val + X4 * X4_val + X5 * X5_val + X6 * X6_val + X7 * X7_val

purchase.test[, pi := exp(logits) / (1 + exp(logits))]

ggplot(purchase.test, aes(STARS, pi)) +
  geom_point() +
  geom_line() +
  labs(x = "STARS", y = "P(outcome)", title = "Probability of Purchase Decision by Stars Rating")

#####################################################################
### Number of Cases Sold

# Zero Inflated Binomial (ZIP)

data.cases <- data.wine[!is.na(Cases)]

nrow(data.cases) - nrow(data.wine)

n.total.cases <- nrow(data.cases)

data.cases$u <- runif(n = n.total.purchase, min = 0, max = 1)

# Create train/test split;
cases.train <- subset(data.stars, u < 0.70)
cases.test <- subset(data.stars, u >= 0.70)

# Zero Inflated Binomial (ZIP)
ggplot(data.cases, aes(Cases, fill = ..count..)) +
  geom_histogram() +
  labs(title = "Cases Distribution")

skim(cases.train)

purchase.numeric.col <- unlist(lapply(cases.train, is.numeric))
purchase.cases.numeric <- stars.train[, purchase.numeric.col, with = F]

str(purchase.cases.numeric)

# Correlation matrix
ggcorrplot(round(cor(purchase.cases.numeric[, 1:16]), 1),
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Wine Metrics ~ Cases")

cases.train <- purchase.train[complete.cases(purchase.train)]

train.cases <- as.factor(purchase.train$Purchase)
train.purchase.vars <- purchase.train[, !c("u", "Quality", "Purchase", "Cases")]

ggplot(data = cases.train, aes(x = Cases, y = VolatileAcidity, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = cases.train, aes(x = Cases, y = ResidualSugar, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = cases.train, aes(x = Cases, y = TotalSulfurDioxide, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = cases.train, aes(x = Cases, y = Alcohol, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = cases.train, aes(x = Cases, y = LabelAppeal, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

ggplot(data = cases.train, aes(x = Cases, y = STARS, group = Cases)) +
  geom_jitter(alpha = .3) +
  geom_boxplot(alpha = .5, color = 'blue') +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)


ggplot(data = cases.train, aes(x = Cases, y = STARS, group = Cases)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_summary(fun.y = "mean",
               geom = "point",
               color = "red",
               shape = 8,
               size = 4)

summary(cases.train)

summary(stepwise.purchase)

summary(base.zero <- zeroinfl(Cases ~ STARS + VolatileAcidity + ResidualSugar + TotalSulfurDioxide + Alcohol + LabelAppeal + AcidIndex, data = cases.train))

summary(zero1 <- zeroinfl(Cases ~ STARS + Alcohol + LabelAppeal + AcidIndex, dist = "poisson", data = cases.train))

round(coef(zero1), 3)

exp(coef(zero1)[7])

b0 <- coef(zero1)[1]
b0_logit <- exp(b0)
round((b0_logit - 1) * 100, 2)
round((b0_logit / (1 + b0_logit)), 3) * 100 # convert to probability

X1 <- coef(zero1)[2]
X1_logit <- exp(X1)
round((X1_logit - 1) * 100, 2)
round(X1_logit / (1 + X1_logit), 3) * 100 # convert to probability

X2 <- coef(zero1)[3]
X2_logit <- exp(X2)
round((X2_logit - 1) * 100, 2)
round(X2_logit / (1 + X2_logit), 3) * 100 # convert to probability

X3 <- coef(zero1)[4]
X3_logit <- exp(X3)
round((X3_logit - 1) * 100, 2)
round(X3_logit / (1 + X3_logit), 3) * 100 # convert to probability

X4 <- coef(zero1)[5]
X4_logit <- exp(X4)
round((X4_logit - 1) * 100, 2)
round(X4_logit / (1 + X4_logit), 3) * 100 # convert to probability

b6 <- coef(zero1)[6]
b6_logit <- exp(b6)
round((b6_logit - 1) * 100, 2)
round((b6_logit / (1 + b6_logit)), 3) * 100 # convert to probability

b7 <- coef(zero1)[7]
b7_logit <- exp(b7)
round((b7_logit - 1) * 100, 2)
round(b7_logit / (1 + b7_logit), 3) * 100 # convert to probability

b8 <- coef(zero1)[8]
b8_logit <- exp(b8)
round((b8_logit - 1) * 100, 2)
round(b8_logit / (1 + b8_logit), 3) * 100 # convert to probability

b9 <- coef(zero1)[9]
b9_logit <- exp(b9)
round((b9_logit - 1) * 100, 2)
round(b9_logit / (1 + b9_logit), 3) * 100 # convert to probability

b10 <- coef(zero1)[10]
v10_logit <- exp(b10)
round((v10_logit - 1) * 100, 2)
round(v10_logit / (1 + v10_logit), 3) * 100 # convert to probability

cases.train$pred <- round(predict(zero1, type = "response"))
cases.train$residual <- resid(zero1, type = "response")

ggplot(cases.train, aes(pred, Cases)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
   geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
   labs(title = "Predicted vs Actual") +
   theme(legend.position = 'none')

p1 <- ggplot(cases.train, aes(sample = pred)) +
  geom_qq() +
  geom_qq_line()

p2 <- ggplot(cases.train, aes(residual, fill = ..count..)) +
  geom_histogram()

grid.arrange(p1, p2, nrow = 2)

zero1.data <- cases.test
zero1.data$pred <- round(predict(zero1, newdata = zero1.data, type = "response"))
zero1.data$accurate <- zero1.data$Cases == zero1.data$pred

sum(zero1.data$accurate) / nrow(zero1.data)

ggplot(zero1.data, aes(pred, Cases)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
   geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
   labs(title = "Predicted vs Actual") +
   theme(legend.position = 'none')