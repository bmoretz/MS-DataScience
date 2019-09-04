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

#####################################################################
######################### Computation 3 #############################
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

# Utility

ggplotRegression <- function(fit) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                      "Intercept =", signif(fit$coef[[1]], 5),
                      " Slope =", signif(fit$coef[[2]], 5),
                      " P =", signif(summary(fit)$coef[2, 4], 5)))
}

# Data of interest

data.nutrition <- as.data.table(read.csv(file = "NutritionStudy.csv", head = TRUE, sep = ","))

# Recode Variables
data.nutrition$VitaminUse <- as.character(data.nutrition$VitaminUse)
typeof(data.nutrition$VitaminUse)

unique(data.nutrition$Gender)
data.nutrition$GenderCoded <- as.factor(data.nutrition$Gender)
levels(data.nutrition$GenderCoded) = c(0, 1)

data.nutrition$SmokeCoded <- as.factor(data.nutrition$Smoke)
levels(data.nutrition$SmokeCoded) <- c(0, 1)

data.nutrition$PriorSmokeCoded <- as.factor(data.nutrition$PriorSmoke)
levels(data.nutrition$PriorSmoke) <- c(0, 1)

# formattable(data.nutrition, align = c("l", "r"),
#  list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
# ))

# Linear Model #1

data.nutrition$VitaminCoded <- ifelse(data.nutrition$VitaminUse == 'No', 3, ifelse(data.nutrition$VitaminUse == 'Occasional', 2, 1))
model1_fit <- lm(formula = Cholesterol ~ VitaminCoded, data = data.nutrition)

summary(model1_fit)
anova(model1_fit)

summary(model1_fit)$coeff

tapply(data.nutrition$Cholesterol, data.nutrition$VitaminUse, mean)

intercept <- 232.634
b1 <- 5.001 * 1

intercept + b1 * 3

mean(data.nutrition$Cholesterol)

ybar <- mean(data.nutrition[VitaminUse == 'No']$Cholesterol)
ybar_1 <- mean(data.nutrition[VitaminUse == 'Occasional']$Cholesterol)
ybar_2 <- mean(data.nutrition[VitaminUse == 'Regular']$Cholesterol)

ggplot(data.nutrition, aes(VitaminCoded, Cholesterol)) +
  geom_point(show.legend = T) +
  stat_smooth(method = "lm", col = "red") +
  geom_point(aes(x = 1, y = ybar, size = 2), color = "lightblue") +
  geom_point(aes(x = 2, y = ybar_1, size = 2), color = "lightblue") +
  geom_point(aes(x = 3, y = ybar_2, size = 2), color = "lightblue") +
  theme(legend.position = "none")

summary(model1_fit)$r.squared * 100

ggplotRegression(model1_fit)

# Linear Model #2

tapply(data.nutrition$Cholesterol, data.nutrition$VitaminUse, mean)

data.nutrition$VitaminCoded2 <- ifelse(data.nutrition$VitaminUse == 'No', 1, ifelse(data.nutrition$VitaminUse == 'Occasional', 2, 3))

model2_fit <- lm(formula = Cholesterol ~ VitaminCoded2, data = data.nutrition)
summary(model2_fit)

intercept <- 252.6378
b1 <- 5.001 * 1

252.6378 - 232.634

intercept - b1 * 3

ggplot(data.nutrition, aes(VitaminCoded2, Cholesterol)) +
  geom_point(show.legend = T) +
  stat_smooth(method = "lm", col = "red") +
  geom_point(aes(x = 1, y = ybar, size = 2), color = "lightblue") +
  geom_point(aes(x = 2, y = ybar_1, size = 2), color = "lightblue") +
  geom_point(aes(x = 3, y = ybar_2, size = 2), color = "lightblue") +
  theme(legend.position = "none")

ggplotRegression(model2_fit)

# Dummy Coded

tapply(data.nutrition$Cholesterol, data.nutrition$VitaminUse, mean)

vit <- model.matrix(~VitaminUse, data = data.nutrition)

model4_data <- data.table(Cholesterol = data.nutrition$Cholesterol, VitaminUse = data.nutrition$VitaminUse, vit[, -1])

model4_fit <- lm(formula = Cholesterol ~ VitaminUseOccasional + VitaminUseRegular, data = model4_data)
summary(model4_fit)
anova(model4_fit)

summary(model4_fit)$r.squared * 100

ybar <- mean(model4_data[VitaminUseOccasional == 0 & VitaminUseRegular == 0]$Cholesterol)
ybar - model4_fit$coefficients[1]

ybar_1 <- mean(model4_data[VitaminUseOccasional == 1]$Cholesterol)
ybar_1 - model4_fit$coefficients[1]

ybar_2 <- mean(data.nutrition[VitaminCoded == 1]$Cholesterol)
ybar_2 - model4_fit$coefficients[1]

ggplot(model4_data, aes(VitaminUse, Cholesterol)) +
  geom_point(show.legend = T) +
  geom_hline(aes(yintercept = ybar, lwd = 1.5, color = "blue")) +
  geom_point(aes(x = 1, y = ybar, size = 3, color = "red")) +
  geom_point(aes(x = 2, y = ybar_1, size = 3, color = "red")) +
  geom_point(aes(x = 3, y = ybar_2, size = 3, color = "red")) +
  theme(legend.position = "none")

# Effect Coding

tapply(data.nutrition$Cholesterol, data.nutrition$VitaminUse, mean)


data.nutrition$Vitamin <- as.factor(data.nutrition$VitaminUse)
contrasts(data.nutrition$Vitamin) = matrix(c(-1, 1, 0, -1, 0, 1), ncol = 2) # contr.sum(3)
contrasts(data.nutrition$Vitamin)
model5_fit <- lm(Cholesterol ~ Vitamin, data.nutrition)
summary(model5_fit)

coef(model5_fit)

summary(model5_fit)$r.squared * 100

intercept <- 242.911
beta1 <- 2.532
beta2 <- -6.220

intercept + beta1
intercept + beta2

mean(data.nutrition$Cholesterol)

ybar <- mean(data.nutrition[VitaminUse == 'No']$Cholesterol)
ybar_1 <- mean(data.nutrition[VitaminUse == 'Occasional']$Cholesterol)
ybar_2 <- mean(data.nutrition[VitaminUse == 'Regular']$Cholesterol)

ggplot(data.nutrition, aes(VitaminUse, Cholesterol, fill = VitaminUse)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 241.0668, lwd = .7), color = "blue") +
  geom_point(aes(x = 1, y = intercept, size = 1), color = "blue") +
  geom_point(aes(x = 2, y = intercept + beta1, size = 1), color = "blue") +
  geom_point(aes(x = 3, y = intercept + beta2, size = 1), color = "blue")

# Alcohol

data.nutrition$AlcoholUse <- ifelse(data.nutrition$Alcohol == 0, "None", ifelse(data.nutrition$Alcohol < 10, "Moderate", "Heavy"))
data.nutrition$AlcoholUse <- factor(data.nutrition$AlcoholUse, labels = c("Heavy", "Moderate", "None"))
levels(data.nutrition$AlcoholUse) <- c("None", "Moderate", "Heavy")
contrasts(data.nutrition$AlcoholUse) = matrix(c(-1, 1, 0, -1, 0, 1), ncol = 2)

data.nutrition$AlcoholHeavy <- ifelse(data.nutrition$AlcoholUse == 'Heavy', 1, 0)
data.nutrition$AlcoholModerate <- ifelse(data.nutrition$AlcoholUse == 'Moderate', 1, 0)

data.nutrition$VitaminOccasional <- ifelse(data.nutrition$VitaminUse == 'Occasional', 1, 0)
data.nutrition$VitaminRegular <- ifelse(data.nutrition$VitaminUse == 'Regular', 1, 0)

model6_fit <- lm(formula = Cholesterol ~ AlcoholUse, data = data.nutrition)
summary(model6_fit)

tapply(data.nutrition$Cholesterol, data.nutrition$AlcoholUse, mean)

data.interaction <- data.nutrition[, .(Cholesterol, AlcoholHeavy, AlcoholModerate, VitaminOccasional, VitaminRegular)]

data.interaction[, ':='(HO = AlcoholHeavy * VitaminOccasional, HR = AlcoholHeavy * VitaminRegular, MO = AlcoholModerate * VitaminOccasional, MR = AlcoholModerate * VitaminRegular)]

full_fit <- lm(formula = Cholesterol ~ AlcoholModerate + AlcoholHeavy + VitaminOccasional + VitaminRegular + HO + HR + MO + MR, data = data.interaction)
summary(full_fit)
anova(full_fit)

reduced_fit <- lm(formula = Cholesterol ~ AlcoholModerate + AlcoholHeavy + VitaminOccasional + VitaminRegular, data = data.interaction)
summary(reduced_fit)

# F-test

m1_aov <- anova(full_fit)
m2_aov <- anova(reduced_fit)

df_1 <- 4
df_2 <- 2

ss_f <- m1_aov$`Sum Sq`[length(m1_aov$`Sum Sq`)]
ss_r <- m2_aov$`Sum Sq`[length(m2_aov$`Sum Sq`)]
msf <- sum( m1_aov$`Mean Sq` )

n <- nrow(data.nutrition)

k <- 4
p <- 8

f.val <- ((ss_r - ss_f) / k) / (ss_f / (n - (p + 1)))
round(f.val, 4)

dff <- 306
dfr <- 310
dfn <- dfr - dff

f.val <- ((ss_r - ss_f) / dfn) / (ss_f / dff)
f.val

alpha = .05
f.crit <- qf(1 - alpha, dfn, dff)
round(f.crit, 4)

f.val > f.crit

anova(full_fit, reduced_fit)

alcohol.no <- mean(data.nutrition[AlcoholUse == 'None']$Cholesterol)
alcohol.heavy <- mean(data.nutrition[AlcoholUse == 'Heavy']$Cholesterol)

ggplot(data.nutrition, aes(AlcoholUse, Cholesterol, fill = AlcoholUse)) +
  geom_boxplot() +
  coord_flip()

ggplot(data.nutrition, aes(VitaminUse, Cholesterol, fill = AlcoholUse)) +
  geom_boxplot() +
  coord_flip()

# Gender / Smoke

data.interaction$Gender <- data.nutrition$GenderCoded
data.interaction$Smoke <- data.nutrition$SmokeCoded

full_fit <- lm(formula = Cholesterol ~ AlcoholModerate + AlcoholHeavy + VitaminOccasional + VitaminRegular + HO + HR + MO + MR + Gender + Smoke, data = data.interaction)
summary(full_fit)

reduced_fit <- lm(formula = Cholesterol ~ AlcoholModerate + AlcoholHeavy + VitaminOccasional + VitaminRegular + HO + HR + MO + MR, data = data.interaction)
summary(reduced_fit)

m1_aov <- anova(full_fit)
m2_aov <- anova(reduced_fit)

dff <- 304
dfr <- 306
dfn <- dfr - dff

ss_f <- m1_aov$`Sum Sq`[length(m1_aov$`Sum Sq`)]
ss_r <- m2_aov$`Sum Sq`[length(m2_aov$`Sum Sq`)]

f.val <- ((ss_r - ss_f) / dfn) / (ss_f / dff)
round(f.val, 4)

alpha = .05
f.crit <- qf(1 - alpha, dfn, dff)
round(f.crit, 4)

f.val > f.crit

anova(full_fit, reduced_fit)