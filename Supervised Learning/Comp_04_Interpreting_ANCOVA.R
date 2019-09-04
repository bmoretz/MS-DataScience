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

#####################################################################
######################### Computation 4 #############################
#####################################################################

path.w <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.h <- "D:/Projects/MSDS-RegressionAnalysis/data"

if (file.exists(path.h)) {
  setwd(path.h)
} else {
  setwd(path.w)
}

theme_set(theme_sjplot())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

# Utility

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

  grid.arrange(p1, p2, nrow = 1)
}

# Data of interest

data.nutrition <- as.data.table(read.csv(file = "NutritionStudy.csv", head = TRUE, sep = ","))

fVsC <- cor(data.nutrition$Cholesterol, data.nutrition$Fiber)
fVsC <- round(fVsC, 4)

ggplot(data.nutrition, aes(Fiber, Cholesterol)) +
  geom_point() +
  labs(title = paste("Correlation: ", fVsC))

model1_fit <- lm(formula = Cholesterol ~ Fiber, data = data.nutrition)
summary(model1_fit)
anova(model1_fit)

glance(model1_fit)
wrapFTest(model1_fit)

mean(data.nutrition$Cholesterol)
mean(data.nutrition$Fiber)


193.701 + 12.78857 * 3.813

ggplotRegression(model1_fit)

coef(model1_fit)

round(model1_fit$coefficients, 4)

summary(model1_fit)$r.squared * 100

# Model 2

# Dummy Coded Alcohol

data.nutrition$AlcoholUse <- ifelse(data.nutrition$Alcohol == 0, "None", ifelse(data.nutrition$Alcohol < 10, "Moderate", "Heavy"))
data.nutrition$AlcoholUse <- factor(data.nutrition$AlcoholUse, labels = c("Heavy", "Moderate", "None"), levels = )
levels(data.nutrition$AlcoholUse) <- c("None", "Moderate", "Heavy")

tapply(data.nutrition$Cholesterol, data.nutrition$AlcoholUse, mean)

data.nutrition[, .(Count = .N), by = AlcoholUse]

ggplot(data.nutrition, aes(Cholesterol, AlcoholUse)) +
  geom_boxplot(outlier.colour = "blue", outlier.shape = 1) +
  geom_jitter(width = 0.2) +
  coord_flip()

alcohol <- model.matrix(~AlcoholUse, data = data.nutrition)

head(alcohol)

contrasts(data.nutrition$AlcoholUse) = matrix(c(-1, 1, 0, -1, 0, 1), ncol = 2)

model2_data <- data.table(Cholesterol = data.nutrition$Cholesterol, AlcoholUse = data.nutrition$AlcoholUse, alcohol[, 2:3], Fiber = data.nutrition$Fiber)
model2_fit <- lm(formula = Cholesterol ~ Fiber + AlcoholUseModerate + AlcoholUseHeavy, data = model2_data)

summary(model2_fit)
Anova(model2_fit, type = "II")

round(summary(model2_fit)$r.squared * 100, 4)
round(coef(model2_fit), 4)

ggplotRegression(model2_fit)

ggplot(model2_fit$model, aes_string(x = names(model2_fit$model)[2], y = names(model2_fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(model2_fit)$adj.r.squared, 5),
                      "Intercept =", signif(model2_fit$coef[[1]], 5),
                      " Slope =", signif(model2_fit$coef[[2]], 5),
                      " P =", signif(summary(model2_fit)$coef[2, 4], 5)))

alpha1 <- 0.01
beta1 <- 2.868

t.val <- beta1 / 0.4104
t.val <- round(T, 4)

c(beta1 - alpha1, beta1 + alpha1)

crit.value <- abs(qt(alpha1 / 2, 72 - 4 - 2)) # 99% confidence, 2 sided
round(crit.value, 4)

model2_data$pred <- predict(model2_fit)

ggplot(model2_data, aes(Fiber, pred, color = AlcoholUse)) +
  geom_point()

GainCurvePlot(model2_data, "pred", "Cholesterol", "Cholesterol model")

ggplot(data.nutrition, aes(Fiber, Cholesterol, color = AlcoholUse)) +
  geom_point()

# Model 3

model3_data <- data.table(Cholesterol = data.nutrition$Cholesterol, AlcoholUse = data.nutrition$AlcoholUse, alcohol[, 2:3], Fiber = data.nutrition$Fiber)
model3_fit <- lm(formula = Cholesterol ~ Fiber + AlcoholUseModerate + AlcoholUseHeavy + Fiber * AlcoholUseModerate + Fiber * AlcoholUseHeavy, data = model3_data)
summary(model3_fit)

round(coef(model3_fit), 4)

round(summary(model3_fit)$r.squared, 4) * 100

plot_model(model3_fit, type = "int")

m1_aov <- anova(model3_fit)
m2_aov <- anova(model2_fit)

ss_f <- m1_aov$`Sum Sq`[length(m1_aov$`Sum Sq`)]

ss_r <- m2_aov$`Sum Sq`[length(m2_aov$`Sum Sq`)]
msf <- sum(m1_aov$`Mean Sq`)

dff <- m1_aov$Df[length(m1_aov$Df)]
dfr <- m2_aov$Df[length(m2_aov$Df)]
dfn <- dfr - dff

f.val <- ((ss_r - ss_f) / dfn) / (ss_f / dff)
round(f.val, 4)

alpha = .05
f.crit <- qf(1 - alpha, dfn, dff)
round(f.crit, 4)

ifelse(f.val > f.crit, "Reject the Null", "Cannot reject the null")

anova(model3_fit, model2_fit)

# plot(model3_fit)

# Fiber/Smoke, Model 4

smoke <- dummyVars(Cholesterol ~ Fiber + Smoke + Fiber*Smoke, data = data.nutrition)

model4_fit <- lm(smoke, data = data.nutrition)
Anova(model4_fit, type = "II")
summary(model4_fit)

round(coef(model4_fit), 4)

round(summary(model4_fit)$r.squared, 4) * 100

plot_model(model4_fit, type = "int")

# Fiber/Vitamin, Model 5

vitamin <- dummyVars(Cholesterol ~ Fiber + VitaminUse + Fiber*VitaminUse, data = data.nutrition)

model5_fit <- lm(vitamin, data = data.nutrition)
summary(model5_fit)
Anova(model5_fit, type = "II")

round(coef(model5_fit), 4)

round(summary(model5_fit)$r.squared, 4) * 100

plot_model(model5_fit, type = "int")

# Model 6, Fiber/Gender

gender <- dummyVars(Cholesterol ~ Fiber + Gender + Fiber*Gender, data = data.nutrition)

model6_fit <- lm(gender, data = data.nutrition)
summary(model6_fit)
Anova(model6_fit)
round(coef(model6_fit), 4)

round(summary(model6_fit)$r.squared, 4) * 100

plot_model(model6_fit, type = "int")

# Bonus

ggpairs(data.nutrition)

bonus1 <- dummyVars(Cholesterol ~ Calories + Fat + Fiber + Gender + Fiber * Gender, data = data.nutrition)

bonus1_fit <- lm(bonus1, data = data.nutrition)
summary(bonus1_fit)
Anova(bonus1_fit)

plot_model(bonus1_fit, type = "pred", terms = c("Gender"))

round(coef(bonus1_fit), 4)

ggplotRegression(bonus1_fit)

bonus1_data <- data.table( actual = data.nutrition$Cholesterol, pred = predict(bonus1_fit))

GainCurvePlot(bonus1_data, "pred", "actual", "Predicted Cholesterol Levels")

summary(data.nutrition)

b0 <- 242.5

b1 <- 0.04305

b0 + b1 * 1796.7

tapply(data.nutrition$Cholesterol, data.nutrition$Gender, mean)

# calories + gender

calories_gender <- dummyVars(Cholesterol ~ Calories + Gender + Calories * Gender, data = data.nutrition)

model7_fit <- lm(calories_gender, data = data.nutrition)
summary(model7_fit)
Anova(model7_fit)
round(coef(model7_fit), 4)

round(summary(model7_fit)$r.squared, 4) * 100

plot_model(model7_fit, type = "int")

# fat + gender

fat_gender <- dummyVars(Cholesterol ~ Fat + Gender + Fat * Gender, data = data.nutrition)

model8_fit <- lm(fat_gender, data = data.nutrition)
summary(model8_fit)
Anova(model8_fit)
round(coef(model8_fit), 4)

round(summary(model7_fit)$r.squared, 4) * 100

plot_model(model8_fit, type = "int")

# Bonus 2

bonus2 <- dummyVars(Cholesterol ~ Calories + Fat + Fiber + Gender + Fiber * Gender + Calories * Gender, data = data.nutrition)

bonus2_fit <- lm(bonus2, data = data.nutrition)
summary(bonus2_fit)
Anova(bonus2_fit)

plot_model(bonus2_fit, type = "pred", terms = c("Gender"))

round(coef(bonus1_fit), 4)

ggplotRegression(bonus2_fit)

bonus2_data <- data.table(actual = data.nutrition$Cholesterol, pred = predict(bonus2_fit))

GainCurvePlot(bonus2_data, "pred", "actual", "Predicted Cholesterol Levels")

# F-test

m1_aov <- anova(bonus2_fit)
m2_aov <- anova(bonus1_fit)

ss_f <- m1_aov$`Sum Sq`[length(m1_aov$`Sum Sq`)]

ss_r <- m2_aov$`Sum Sq`[length(m2_aov$`Sum Sq`)]
msf <- sum(m1_aov$`Mean Sq`)

dff <- m1_aov$Df[length(m1_aov$Df)]
dfr <- m2_aov$Df[length(m2_aov$Df)]
dfn <- dfr - dff

f.val <- ((ss_r - ss_f) / dfn) / (ss_f / dff)
round(f.val, 4)

alpha = .05
f.crit <- qf(1 - alpha, dfn, dff)
round(f.crit, 4)

ifelse(f.val > f.crit, "Reject the Null", "Cannot reject the null")

anova(bonus2_fit, bonus1_fit)
