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
######################### Computation 1 #############################
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

data.states <- as.data.table(read.csv(file = "USStates.csv", head = TRUE, sep = ","))

# 2.)

skim(data.nondemo)

data.nondemo <- data.states[, !c('State', 'Region', 'Population')]

stats <- as.data.frame(summarytools::descr(data.nondemo, display.labels = colnames(data.nondemo)))

formattable(stats, align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "r"),
            list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

data.income <- melt(data.nondemo, id.vars = c('HouseholdIncome'))

ggplot(data.income) +
    geom_jitter(aes(value, HouseholdIncome, colour = variable),) +
    geom_smooth(aes(value, HouseholdIncome, colour = variable), method = lm, se = FALSE) +
    facet_wrap(~variable, scales = "free_x")

# 3.)

getCorTable <- function(cols) {
  # selected features correlation matrix
  sale.cor <- cor(cols, use = "pairwise.complete.obs")[, "HouseholdIncome"]
  sale.cor <- sort(sale.cor, decreasing = T)

  sale.cor <- sale.cor[-1] # remove SalePrice

  tbl.sale.cor <- melt(sale.cor)
  colnames(tbl.sale.cor) <- c("Correlation to Household Income")

  formattable(tbl.sale.cor, align = c("l", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
  ))
}

getCorTable(data.nondemo)

ggpairs(data.nondemo)

# 4.)

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

  grid.arrange(p1)
}

# Model #1
m1 <- data.nondemo[, .(College, HouseholdIncome)]

model_1 <- lm(formula = HouseholdIncome ~ College, data = m1)
ggplotRegression(model_1)

anova(model_1)
aov(HouseholdIncome ~ College, m1)

summary(model_1)

# Long-hand LM
model_1_slope <- cor(m1$College, m1$HouseholdIncome) * (sd(m1$HouseholdIncome) / sd(m1$College))
model_1_intercept <- mean(m1$HouseholdIncome) - (model_1_slope * mean(m1$College))

# 5.)

# SSE

m1$Y_Hat <- predict(model_1)
m1$residual <- m1$HouseholdIncome - m1$Y_Hat

sse <- sum(m1$residual ** 2)

# SST

y_bar <- mean(m1$HouseholdIncome)

sst <- sum((m1$HouseholdIncome - y_bar) ** 2)

# SSR

ssr <- sum((m1$Y_Hat - y_bar) ** 2)

# R2

(ssr / sst)

ssr + sse

stopifnot(round(sst, 3) == round(ssr + sse, 3))

# 6.)

m2 <- data.nondemo[, .(College, Insured, HouseholdIncome)]

model_2 <- lm(formula = HouseholdIncome ~ College + Insured, data = m2)

summary(model_2)

ggplotRegression(model_2)

aov(formula = HouseholdIncome ~ College + Insured, data = m2)

anova(model_2)

nrow(m2)

# 7.)

model.values <- data.table(Model = "College", R2 = signif(summary(model_1)$r.squared, 5))
model.values <- rbind(model.values, data.table(Model = "College + Insured", R2 = signif(summary(model_2)$r.squared, 5)))

model_3 <- lm(formula = HouseholdIncome ~ College + Insured + Smokers, data = data.nondemo)
summary(model_3)
model.values <- rbind(model.values, data.table(Model = "College + Insured + Smokers", R2 = signif(summary(model_3)$r.squared, 5)))

model_4 <- lm(formula = HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity, data = data.nondemo)
summary(model_4)
model.values <- rbind(model.values, data.table(Model = "College + Insured + Smokers + PhysicalActivity", R2 = signif(summary(model_4)$r.squared, 5)))

model_5 <- lm(formula = HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity + TwoParents, data = data.nondemo)
summary(model_5)
model.values <- rbind(model.values, data.table(Model = "College + Insured + Smokers + PhysicalActivity + TwoParents", R2 = signif(summary(model_5)$r.squared, 5)))

model_6 <- lm(formula = HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity + TwoParents  + HeavyDrinkers, data = data.nondemo)
summary(model_6)
model.values <- rbind(model.values, data.table(Model = "College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers", R2 = signif(summary(model_6)$r.squared, 5)))

model_7 <- lm(formula = HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers + HighSchool, data = data.nondemo)
summary(model_7)
model.values <- rbind(model.values, data.table(Model = "College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers + High School", R2 = signif(summary(model_7)$r.squared, 5)))

model_7 <- lm(formula = HouseholdIncome ~ College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers + HighSchool, data = data.nondemo)
summary(model_7)
model.values <- rbind(model.values, data.table(Model = "College + Insured + Smokers + PhysicalActivity + TwoParents + HeavyDrinkers + High School", R2 = signif(summary(model_7)$r.squared, 5)))

formattable(model.values, align = c("l", "r"),
            list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
))

# 8.)

ggplotMultRegression <- function(data, fit) {

  p1 <- ggplot(melt(data, id.vars = 'HouseholdIncome')) +
    geom_jitter(aes(value, HouseholdIncome, colour = variable),) + geom_smooth(aes(value, HouseholdIncome, colour = variable), method = lm, se = FALSE) +
    facet_wrap(~variable, scales = "free_x") +
    labs(x = "Variable", y = "Household Income",
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

m3 <- data.nondemo[, .(College, Obese, HouseholdIncome)]

model_3 <- lm(formula = HouseholdIncome ~ College + Smokers, data = m3)

summary(model_3)

ggplotMultRegression(m3, lm(formula = HouseholdIncome ~ College + Smokers, data = m3))

ggplotMultRegression(m3, lm(formula = HouseholdIncome ~ College + Obese, data = m3))

aov(formula = HouseholdIncome ~ College + Smokers, data = m3)

anova(model_3)

cor(m3$College, m3$Smokers)

cs_fit <- lm(formula = College ~ Smokers, data = m3)
summary(cs_fit)

# 9.)

data.smokers <- melt(data.nondemo, id.vars = c('Smokers'))

ggplot(data.smokers) +
    geom_jitter(aes(value, Smokers, colour = variable),) +
    geom_smooth(aes(value, Smokers, colour = variable), method = lm, se = FALSE) +
    facet_wrap(~variable, scales = "free_x")

smokers_fit <- lm(formula = Smokers ~ HouseholdIncome, data = data.nondemo)
summary(smokers_fit)

# 10.)

