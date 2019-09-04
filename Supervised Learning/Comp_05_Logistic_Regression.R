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

#####################################################################
######################### Computation 5 #############################
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

# Data set of interest
data.religion <- as.data.table(read.csv(file = "RELIGION.csv", head = TRUE, sep = ","))

# EDA

attach(data.religion)

str(data.religion)

# Religious School prob / odds
rel.table <- table(RELSCHOL)
rel.prob <- rel.table / nrow(data.religion)
rel.odds <- rel.prob / (1 - rel.prob)

# Q2

Plot(INCOME, RELSCHOL, data = data.religion)

by_income <- data.religion[, .(Prob = sum(RELSCHOL) / .N, Count = .N), by = INCOME]
setorder(by_income, INCOME)
by_income

mean(by_income[INCOME >= 8]$Prob) * 100 # Cut-off point
mean(by_income[INCOME <= 7]$Prob) * 100

sort(unique(data.religion$INCOME))

tbl.income <- table(RELSCHOL, INCOME)
round(prop.table(tbl.income), 3)

p1 <- ggplot(data.religion[, .(Prob = sum(RELSCHOL) / .N), by = INCOME], aes(INCOME, Prob)) +
  geom_point() +
  geom_line() +
  labs( title = "Proportion of Religious School by Income")

p2 <- ggplot(data.religion, aes(INCOME, fill = ..count..)) +
  geom_histogram() +
  labs(title = "Income Distribution")

grid.arrange(p1, p2, nrow = 2)

data.religion$D_INCOME <- ifelse(data.religion$INCOME >= 8, 1, 0)

attach(data.religion)

tbl.d_income <- table(RELSCHOL, D_INCOME)
round(tbl.d_income, 3)

by_attendance <- data.religion[, .(Value = sum(RELSCHOL), Prob = sum(RELSCHOL) / .N, Count = .N), by = ATTEND]
setorder(by_attendance, ATTEND)
by_attendance

tbl.attend <- table(RELSCHOL, ATTEND)
tbl.attend / nrow(data.religion)
round(prop.table(tbl.attend, 1), 3)
round(prop.table(tbl.attend, 2), 3)

ggplot(data.religion, aes(ATTEND, RELSCHOL)) +
  geom_point()

p1 <- ggplot(data.religion[, .(Prob = sum(RELSCHOL) / .N), by = ATTEND], aes(ATTEND, Prob)) +
  geom_point() +
  geom_line() +
  labs(title = "Proportion of Religious School by Attendance")

p2 <- ggplot(data.religion, aes(ATTEND, fill = ..count..)) +
  geom_histogram() +
  labs(title = "Attendence Distribution")

grid.arrange(p1, p2, nrow = 2)

# Utility Functions

log_fit <- function(name, fit) {
  return(data.table(Model = name, AIC = AIC(fit), BIC = BIC(fit)))
}

logit2prob <- function(logit) {
  odds <- exp(logit)
  prob <- round(odds / (1 + odds), 3)

  return(prob)
}

# Logistic Regression

summary(data.religion)

model1_fit <- glm(RELSCHOL ~ RACE, family = binomial, data = data.religion)
summary(model1_fit)

formattable(log_fit("Model 1", model1_fit), align = c("l", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

# Race Coefficent

logit2prob(coef(model1_fit))

b0 <- coef(model1_fit)[1]
b0_odds <- round(exp(b0), 3) # convert to odds
b0_odds

X1 <- coef(model1_fit)[2]
X1_logit <- exp(X1)
round((b0_logit - 1) * 100, 2) # convert to probability

by_race <- data.religion[, .(Value = sum(RELSCHOL), Prob = sum(RELSCHOL) / .N, Count = .N), by = RACE]
setorder(by_race, RACE)
by_race[, Prob := round(Value / Count, 3) ]
by_race

tbl.race <- table(RELSCHOL, RACE)
round(prop.table(tbl.race, 1), 3)

model1_data <- data.table(prob = data.religion$RELSCHOL,
                       race = data.religion$RACE,
                       fit = predict(model1_fit, data.religion))

model1_data$fit_prob <- exp(model1_data$fit) / (1 + exp(model1_data$fit))

ggplot(model1_data, aes(x = race, y = prob)) +
  geom_point() +
  geom_line(aes(x = race, y = fit_prob))

# Income

model2_fit <- glm(RELSCHOL ~ INCOME, family = binomial, data = data.religion)
summary(model2_fit)

logit2prob(coef(model2_fit))

model2_data <- data.table(prob = data.religion$RELSCHOL,
                       income = data.religion$INCOME,
                       fit = )

model2_data$fit_prob <- exp(model2_data$fit) / (1 + exp(model2_data$fit))

ggplot(model2_data) +
  geom_point(aes(x = income, y = fit_prob), lwd = 1.5) +
  labs(title = "Probability of Religious School by Income Bracket")

b0 <- coef(model2_fit)[1]
b0_logit <- exp(b0)
round((b0_logit / (1 + b0_logit)), 3) * 100 # convert to probability

X1 <- coef(model2_fit)[2]
X1_logit <- exp(X1)
round(X1_logit / (1 + X1_logit), 3) * 100 # convert to probability

logits <- b0 + X1 * model2_data$income

model2_data[, pi := exp(logits) / (1 + exp(logits))]

ggplot(model2_data, aes(income, pi)) +
  geom_point() +
  labs(x = "X1", y = "P(outcome)", title = "Probability of Religious School by Income Bracket")

max(model2_data$pi, na.rm = T)

# Attend

model3_fit <- glm(RELSCHOL ~ ATTEND, family = binomial, data = data.religion)
summary(model3_fit)

logit2prob(coef(model3_fit))

b0 <- coef(model3_fit)[1]
b0_logit <- exp(b0)
round((b0_logit / (1 + b0_logit)), 3) * 100 # convert to probability

X1 <- coef(model3_fit)[2]
X1_logit <- exp(X1)
round(X1_logit / (1 + X1_logit), 3) * 100 # convert to probability

model3_data <- data.table(prob = data.religion$RELSCHOL,
                       attend = data.religion$ATTEND,
                       fit = predict(model3_fit, data.religion))

logits <- b0 + X1 * model3_data$attend

model3_data[, pi := exp(logits) / (1 + exp(logits))]

ggplot(model3_data, aes(attend, pi)) +
  geom_point() +
  labs(x = "X1", y = "P(outcome)", title = "Probability of Religious School by Attendance")

formattable(log_fit("Model 3", model3_fit), align = c("l", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

xattend <- seq(0, 6, 0.01)
yattend <- predict(model3_fit, list(ATTEND = xattend), type = "response")

ggplot(data.table(xattend, yattend), aes(xattend, yattend)) +
  geom_point() +
  labs(x = "X1", y = "P(outcome)", title = "Probability of Religious School by Attendance")

# Race + Income + Attend

model4_fit <- glm(RELSCHOL ~ RACE + INCOME + ATTEND, family = binomial, data = data.religion)
summary(model4_fit)

logit2prob(coef(model4_fit))

b0 <- coef(model4_fit)[1]
b0_logit <- exp(b0)
round((b0_logit / (1 + b0_logit)), 3) * 100 # convert to probability

X1 <- coef(model4_fit)[2]
X1_logit <- exp(X1)
round(X1_logit / (1 + X1_logit), 3) * 100 # convert to probability

X2 <- coef(model4_fit)[3]
X2_logit <- exp(X2)
round(X2_logit / (1 + X2_logit), 3) * 100 # convert to probability

X3 <- coef(model4_fit)[4]
X3_logit <- exp(X3)
round((X3_logit - 1) * 100, 2)
round(X3_logit / (1 + X3_logit), 3) * 100 # convert to probability

model4_data <- data.table(prob = data.religion$RELSCHOL,
                       attend = data.religion$ATTEND,
                       income = data.religion$INCOME,
                       race = data.religion$RACE,
                       fit = predict(model4_fit, data.religion))

model4_data$fit_prob <- exp(model4_data$fit) / (1 + exp(model4_data$fit))

X2_val <- mean(model4_data$income, na.rm = T)
X3_val <- mean(model4_data$attend, na.rm = T)

logits <- b0 + X1 * data.religion$RACE + X2 * model4_data$income + X3 * model4_data$attend

prob <- b0 + X1 * 1 + X2 * 4 + X3 * 5
odds <- exp(prob)
odds / (1 + odds)

model4_data[, pi := exp(logits) / (1 + exp(logits))]

max(model4_data$pi, na.rm = T)

ggplot(model4_data, aes(income, pi)) +
  geom_point() +
  labs(x = "X1", y = "P(outcome)", title = "Probability of Religious School by Attendance")

formattable(log_fit("Model 4", model4_fit), align = c("l", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

ggplot(model4_data, aes(x = attend, y = prob)) +
  geom_point() +
  geom_line(aes(x = attend, y = fit_prob))

# Research

model.fit <- data.table(model1_fit = predict(model1_fit, data.religion), model2_fit = predict(model2_fit, data.religion), model3_fit = predict(model2_fit, data.religion))
model.prob <- model.fit[, .(model1_prob = exp(model1_fit) / (1 + exp(model1_fit)),
               model2_prob = exp(model2_fit) / (1 + exp(model2_fit)),
               model3_prob = exp(model3_fit) / (1 + exp(model3_fit)))]

model.pred <- model.prob[, .(model1_pred = model1_prob > .5, model2_pred = model2_prob > .5, model3_pred = model3_prob > .5)]
model.pred <- model.pred[complete.cases(model.pred)]

model.pivot <- melt(model.pred)

pred.summary <- model.pivot[, .(Yes = sum(value) / .N, No = .N - sum(value)), by = c('variable')]
colnames(pred.summary)[1] <- 'Model'

formattable(pred.summary, align = c("l", "c", "r"),
    list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))
