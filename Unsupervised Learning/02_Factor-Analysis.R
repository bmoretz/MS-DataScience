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
library(Rtsne)
library(plotly)
library(Quandl)
library(tidyverse)
library(quantmod)
library(psych)
library(ggcorrplot)

#####################################################################
######################### EFA #######################################
#####################################################################

data.path <- "D:/Projects/MSDS-Supervised-Learning/data"

setwd(data.path)

theme_set(theme_light())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())


bfi_data <- bfi

#Remove rows with missing values and keep only complete cases

bfi_data = bfi_data[complete.cases(bfi_data),]

nrow(bfi_data) / ncol(bfi_data) * 20

bfi_cor <- cor(bfi_data[, 1:25])

ggcorrplot(bfi_cor,
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Personality Variables")

bfi_matrix <- matrix(bfi_cor, nrow = 25, ncol = 25, byrow = T)

# Check that object is a matrix object;
is.matrix(bfi_matrix)

# Check that matrix is symmetric;
# This check helps check for typos;
isSymmetric(bfi_matrix)

bfi_Z <- eigen(bfi_matrix)
sum((bfi_Z$val * 4)[1:19])

bfi_Z$vec
bfi_Z$val

sum(bfi_Z$val)

sum(bfi_Z$val >= 1)

bfi_Z$val[Z_keep]

bfi_fa <- fa(bfi_cor, nfactors = 4, )
bfi_fa$loadings

bfi_fa$loadings

nrow(bfi_data)

fa.parallel(bfi_cor, 
            n.obs=2236, 
            fa="both", 
            n.iter=100,
            fm="ml",
            show.legend=TRUE, main="Scree plot with parallel analysis")

colnames(bfi_cor)[bfi_Z$val >= 1]

bfi_data <- as.data.table(bfi_data)

bfi2 <- bfi_data[complete.cases(bfi2), .(A1, A2, A3, A4, A5, C1)][]

bfi2_m <- as.matrix(bfi2)
bfi2_cor <- cor(bfi2_m)

bfi_fa2 <- fa(bfi_cor, nfactors=4, fm="ml", scores=TRUE)
bfi_fa2

bfi_fa3 <- factanal(covmat=bfi_cor, n.obs=1442, factors=4, fm="ml", rotation='varimax');
bfi_fa3
round(bfi_fa3$PVAL,10)

bfi_fa4 <- factanal(covmat=bfi_cor, n.obs=1442, factors=4, fm="ml", rotation='promax');
round(bfi_fa4$PVAL,10)

bfi_fa5 <- factanal(covmat=bfi_cor, n.obs=1442, factors=4, fm="ml", rotation='varimax');
bfi_fa5
round(bfi_fa5$PVAL,10)

bfi_fa6 <- factanal(covmat=bfi_cor, n.obs=1442, factors=11, fm="ml", rotation='varimax');
bfi_fa6
round(bfi_fa6$PVAL,10)

bfi_fa_reg <- fa(bfi_cor, nfactors=5, rotate="varimax", fm="ml", scores=TRUE)
bfi_fa_reg

weights(bfi_fa_reg)

bfi_data

fa_reg_w <- weights(bfi_fa_reg)

for(col in 1:5){
  result <- ""
  for( i in 1:25) {
    result <- paste0(result, ifelse(i > 1, ifelse(fa_reg_w[i, col] >= 0, "+ ", "- "), ""), abs(fa_reg_w[i, col]), "*", "bfi_data$", rownames(fa_reg_w)[i], " ")
  }
  print(result)
}

bfi_reg <- bfi_data
bfi_reg$F1 <- 0.00485186403636336*bfi_data$A1 + 0.0439509115888879*bfi_data$A2 + 0.0535504996092088*bfi_data$A3 + 0.017272398230521*bfi_data$A4 + 0.0074519966421024*bfi_data$A5 + 0.0252462686339109*bfi_data$C1 + 0.0518488974381662*bfi_data$C2 + 0.0192126354910501*bfi_data$C3 + 0.0248198159503421*bfi_data$C4 + 0.0342974397686449*bfi_data$C5 - 0.00553801536730915*bfi_data$E1 + 0.0428209464873776*bfi_data$E2 + 0.030825239140118*bfi_data$E3 + 0.0000566497986031562*bfi_data$E4 + 0.0377968228453992*bfi_data$E5 + 0.377367222968841*bfi_data$N1 + 0.29597607447315*bfi_data$N2 + 0.200684880149919*bfi_data$N3 + 0.137747274002561*bfi_data$N4 + 0.107893393693958*bfi_data$N5 + 0.00632144597543837*bfi_data$O1 + 0.0322476441651689*bfi_data$O2 + 0.0210446069437554*bfi_data$O3 + 0.0394030006413875*bfi_data$O4 + 0.0106592692505423*bfi_data$O5
bfi_reg$F2 <- 0.0613137005020194*bfi_data$A1 - 0.0307611169969791*bfi_data$A2 - 0.00867815803084172*bfi_data$A3 - 0.0126825556059969*bfi_data$A4 + 0.0319187655548396*bfi_data$A5 - 0.0388091400747599*bfi_data$C1 - 0.079592745565058*bfi_data$C2 - 0.0464750202742938*bfi_data$C3 + 0.0186999700541815*bfi_data$C4 - 0.0329945013179998*bfi_data$C5 - 0.198268441670893*bfi_data$E1 - 0.312980415522295*bfi_data$E2 + 0.136955655024875*bfi_data$E3 + 0.228880928737629*bfi_data$E4 + 0.150602531354767*bfi_data$E5 + 0.200966532409799*bfi_data$N1 + 0.121156965627104*bfi_data$N2 - 0.0255906741093831*bfi_data$N3 - 0.165041468236482*bfi_data$N4 - 0.0674419086574876*bfi_data$N5 + 0.0153428257008637*bfi_data$O1 + 0.014032480156739*bfi_data$O2 + 0.0499437868875145*bfi_data$O3 - 0.10340627599202*bfi_data$O4 + 0.0277309345782637*bfi_data$O5
bfi_reg$F3 <- 0.0217158081378257*bfi_data$A1 + 0.0188450018737517*bfi_data$A2 - 0.0063212990719091*bfi_data$A3 + 0.0599379454305706*bfi_data$A4 - 0.0322924502039964*bfi_data$A5 + 0.187187641861763*bfi_data$C1 + 0.264418184510979*bfi_data$C2 + 0.20551655902179*bfi_data$C3 - 0.299528196575604*bfi_data$C4 - 0.237226675132443*bfi_data$C5 + 0.0625947905861941*bfi_data$E1 + 0.0345816685259505*bfi_data$E2 - 0.0406471307366264*bfi_data$E3 - 0.0338314162750519*bfi_data$E4 + 0.0840161096021418*bfi_data$E5 + 0.0540639456917907*bfi_data$N1 + 0.0566109636657724*bfi_data$N2 + 0.00813169850860995*bfi_data$N3 - 0.0434591236323194*bfi_data$N4 + 0.0107039716236466*bfi_data$N5 - 0.00121808951953656*bfi_data$O1 - 0.0150553560109464*bfi_data$O2 - 0.0467672393472964*bfi_data$O3 - 0.0194171741167208*bfi_data$O4 + 0.0038282081285296*bfi_data$O5
bfi_reg$F4 <- 0.127121172434877*bfi_data$A1 + 0.23273111257271*bfi_data$A2 + 0.312611744315078*bfi_data$A3 + 0.150973283681902*bfi_data$A4 + 0.23519375219494*bfi_data$A5 - 0.00963982550512237*bfi_data$C1 + 0.0368902710012546*bfi_data$C2 + 0.0218955074926059*bfi_data$C3 + 0.0655462579439102*bfi_data$C4 + 0.0544472013201967*bfi_data$C5 + 0.0328656653824565*bfi_data$E1 + 0.0769544094173374*bfi_data$E2 + 0.0631256684332213*bfi_data$E3 + 0.0943394175058026*bfi_data$E4 - 0.0450501710299877*bfi_data$E5 - 0.160427280319466*bfi_data$N1 - 0.121211527370228*bfi_data$N2 + 0.0596057729032588*bfi_data$N3 + 0.113577212998949*bfi_data$N4 + 0.103672604015875*bfi_data$N5 - 0.0126212609623174*bfi_data$O1 + 0.0694121577910594*bfi_data$O2 + 0.0054316463735921*bfi_data$O3 + 0.0798366338056761*bfi_data$O4 + 0.0247999978459472*bfi_data$O5 
bfi_reg$F5 <- 0.0196676398794862*bfi_data$A1 - 0.0163575589809591*bfi_data$A2 - 0.0189659257312259*bfi_data$A3 - 0.0910161138010228*bfi_data$A4 - 0.00834450657618106*bfi_data$A5 + 0.0648831984976353*bfi_data$C1 + 0.0227688855519571*bfi_data$C2 - 0.0344052977831143*bfi_data$C3 + 0.00535998237986989*bfi_data$C4 + 0.0745923047900234*bfi_data$C5 + 0.00202411596717473*bfi_data$E1 + 0.0276164218637017*bfi_data$E2 + 0.124486552655877*bfi_data$E3 - 0.101302639752146*bfi_data$E4 + 0.0610213174207739*bfi_data$E5 - 0.0799447641764505*bfi_data$N1 - 0.00669917963777876*bfi_data$N2 + 0.0148283826685744*bfi_data$N3 + 0.0892522395513818*bfi_data$N4 - 0.0575221976518634*bfi_data$N5 + 0.218706360916561*bfi_data$O1 - 0.193572218803958*bfi_data$O2 + 0.338612507605922*bfi_data$O3 + 0.156785018738474*bfi_data$O4 - 0.220819237998739*bfi_data$O5

bfi_reg

n.total <- nrow(bfi_reg)
bfi_reg$u <- runif(n = n.total, min = 0, max = 1)
bfi_reg$gender <- bfi_data$gender - 1

bfi.train <- bfi_reg[bfi_reg$u <= .7,]
bfi.test <- bfi_reg[bfi_reg$u > .7,]

stopifnot(nrow(bfi.train) + nrow(bfi.test) == n.total)

#####################################################################
### Evaluation

# Gender

summary(gender_fit <- glm(data = bfi.train, formula = gender ~ F1 + F2 + F3 + F4 + F5), distribution = binomial)

mse(bfi.train$gender, predict(gender_fit))
AIC(gender_fit)
BIC(gender_fit)

gender.test <- bfi.test

summary(gender_fit)

round(coef(gender_fit), 3)

gender <- bfi.train[, .(gender)]

gender$deviation <- resid(gender_fit, type = "deviance")
gender$pred <- predict(gender_fit, type = "response")

p1 <- ggplot(gender, aes(pred, deviation)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Deviation vs Fitted") +
  theme(legend.position = 'none')

p2 <- ggplot(gender, aes(sample = deviation)) +
  geom_qq() +
  geom_qq_line()

grid.arrange(p1, p2, nrow = 2)

round(sum(bfi.train$gender == ifelse(predict(gender_fit, type = "response") > .5, 1, 0 )) / nrow(bfi.train), 4) * 100

gender.test <- bfi.test[, .(Gender, F1, F2, F3, F4, F5)]
gender.test$pred <- ifelse(predict(gender_fit, newdata = bfi.test, type = "response") > .5, 1, 0 )
gender.test <- gender.test[!is.na(pred)]

gender.test$accurate <- gender.test$gender == gender.test$pred

round(sum(gender.test$accurate) / nrow(gender.test) * 100, 1)

m1.results <- gender.test[, .(PctAccurate = (sum(accurate) / .N) * 100), by = gender]
m1.results$PctAccurate <- round(m1.results$PctAccurate, 3)
setorder(m1.results, gender)
m1.results

formattable(m1.results, align = c("l", "r"),
            list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))))

#####################################################################
### Personality

ggplot(bfi.train, aes(education)) +
  geom_histogram()

summary(education_fit <- lm(data = bfi.train, formula = education ~ F1 + F2 + F3 + F4 + F5))

mse(bfi.train$education, predict(education_fit))
AIC(education_fit)
BIC(education_fit)

summary(education_fit)

round(coef(education_fit), 3)

education <- bfi.train[, .(education)]

education$deviation <- resid(education_fit, type = "deviance")
education$pred <- predict(education_fit, type = "response")

p1 <- ggplot(education, aes(pred, deviation)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Deviation vs Fitted") +
  theme(legend.position = 'none')

p2 <- ggplot(education, aes(sample = deviation)) +
  geom_qq() +
  geom_qq_line()

grid.arrange(p1, p2, nrow = 2)

AIC(education_fit)
BIC(education_fit)

mse(bfi.train$education, predict(education_fit))

round(sum(bfi.train$education == as.vector(round(predict(education_fit, type = "response")))) / nrow(bfi.train), 4) * 100

ggplot(education) +
  geom_point(aes(education, pred))


#####################################################################
### Personality

ggplot(bfi.train, aes(education)) +
  geom_histogram()

summary(education_fit <- lm(data = bfi.train, formula = education ~ F1 + F2 + F3 + F4 + F5))

mse(bfi.train$education, predict(education_fit))
AIC(education_fit)
BIC(education_fit)

summary(education_fit)

round(coef(education_fit), 3)

education <- bfi.train[, .(education)]

education$deviation <- resid(education_fit, type = "deviance")
education$pred <- predict(education_fit, type = "response")

p1 <- ggplot(education, aes(pred, deviation)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Deviation vs Fitted") +
  theme(legend.position = 'none')

p2 <- ggplot(education, aes(sample = deviation)) +
  geom_qq() +
  geom_qq_line()

grid.arrange(p1, p2, nrow = 2)

AIC(education_fit)
BIC(education_fit)

mse(bfi.train$education, predict(education_fit))

round(sum(bfi.train$education == as.vector(round(predict(education_fit, type = "response")))) / nrow(bfi.train), 4) * 100

ggplot(education) +
  geom_point(aes(education, pred))

#####################################################################
### Personality

ggplot(bfi.train, aes(age)) +
  geom_histogram()

summary(age_fit <- glm(data = bfi.train, formula = age ~ F1 + F2 + F3 + F4 + F5, family = "quasipoisson"))

mse(bfi.train$age, predict(age_fit))
AIC(age_fit)
BIC(age_fit)

summary(age_fit)

round(coef(age_fit), 3)

age <- bfi.train[, .(age)]

age$deviation <- resid(age_fit, type = "deviance")
age$pred <- predict(age_fit, type = "response")

p1 <- ggplot(age, aes(pred, deviation)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, col = "red"), lwd = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Deviation vs Fitted") +
  theme(legend.position = 'none')

p2 <- ggplot(age, aes(sample = deviation)) +
  geom_qq() +
  geom_qq_line()

grid.arrange(p1, p2, nrow = 2)

AIC(age_fit)
BIC(age_fit)

mse(bfi.train$age, predict(age_fit))

round(sum(bfi.train$age == as.vector(round(predict(age_fit, type = "response")))) / nrow(bfi.train), 4) * 100

ggplot(education) +
  geom_point(aes(education, pred))
