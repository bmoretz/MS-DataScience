library(data.table)
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(RColorBrewer)
library(formattable)

theme_set(theme_light())

# Theme Overrides
theme_update(axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "darkgreen"),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             plot.subtitle = element_text(face = "bold", size = 8, colour = "darkred"),
             legend.title = element_text(size = 12, color = "darkred", face = "bold"),
             legend.position = "right", legend.title.align=0.5,
             panel.border = element_rect(linetype = "solid", 
                                         colour = "lightgray"), 
             plot.margin = unit(c( 0.1, 0.1, 0.1, 0.1), "inches"))

path.data <- "D:/Projects/MSDS-Unsupervised-Learning/datasets/"
setwd(path.data)

# Load Stock Data

stock.data <- read_excel(paste0(path.data, "stockdata.xlsx"),
                         col_types = c("date", rep("numeric", 42)))

head(stock.data)
str(stock.data)

sorted.df <- stock.data[order(stock.data$Date),];
head(sorted.df)

AA <- log(sorted.df$AA[-1]/sorted.df$AA[-dim(sorted.df)[1]]);

# Manually check the first entry: log(9.45/9.23)
# Type cast the array as a data frame;

returns.df <- as.data.frame(AA);
returns.df$BAC <- log(sorted.df$BAC[-1]/sorted.df$BAC[-dim(sorted.df)[1]]);
returns.df$BHI <- log(sorted.df$BHI[-1]/sorted.df$BHI[-dim(sorted.df)[1]]);
returns.df$CVX <- log(sorted.df$CVX[-1]/sorted.df$CVX[-dim(sorted.df)[1]]);
returns.df$DD  <- log(sorted.df$DD[-1]/sorted.df$DD[-dim(sorted.df)[1]]);
returns.df$DOW <- log(sorted.df$DOW[-1]/sorted.df$DOW[-dim(sorted.df)[1]]);
returns.df$DPS <- log(sorted.df$DPS[-1]/sorted.df$DPS[-dim(sorted.df)[1]]);
returns.df$GS  <- log(sorted.df$GS[-1]/sorted.df$GS[-dim(sorted.df)[1]]);
returns.df$HAL <- log(sorted.df$HAL[-1]/sorted.df$HAL[-dim(sorted.df)[1]]);
returns.df$HES <- log(sorted.df$HES[-1]/sorted.df$HES[-dim(sorted.df)[1]]);
returns.df$HON <- log(sorted.df$HON[-1]/sorted.df$HON[-dim(sorted.df)[1]]);
returns.df$HUN <- log(sorted.df$HUN[-1]/sorted.df$HUN[-dim(sorted.df)[1]]);
returns.df$JPM <- log(sorted.df$JPM[-1]/sorted.df$JPM[-dim(sorted.df)[1]]);
returns.df$KO  <- log(sorted.df$KO[-1]/sorted.df$KO[-dim(sorted.df)[1]]);
returns.df$MMM <- log(sorted.df$MMM[-1]/sorted.df$MMM[-dim(sorted.df)[1]]);
returns.df$MPC <- log(sorted.df$MPC[-1]/sorted.df$MPC[-dim(sorted.df)[1]]);
returns.df$PEP <- log(sorted.df$PEP[-1]/sorted.df$PEP[-dim(sorted.df)[1]]);
returns.df$SLB <- log(sorted.df$SLB[-1]/sorted.df$SLB[-dim(sorted.df)[1]]);
returns.df$WFC <- log(sorted.df$WFC[-1]/sorted.df$WFC[-dim(sorted.df)[1]]);
returns.df$XOM <- log(sorted.df$XOM[-1]/sorted.df$XOM[-dim(sorted.df)[1]]);
returns.df$VV  <- log(sorted.df$VV[-1]/sorted.df$VV[-dim(sorted.df)[1]]);

# Compute correlation matrix for returns;
returns.cor <- cor(returns.df)
returns.cor[, c('VV')]

round(returns.cor, 2)

tail(sort(returns.cor[which(returns.cor < 1 )]), 5)

returns.cor[2:21, c('AA')]

# Barplot the last column to visualize magnitude of correlations;
barplot(returns.cor[1:20,c('VV')],las=2,ylim=c(0,1.0))
title('Correlations with VV')

# Make correlation plot for returns;
# If you need to install corrplot package;  Note how many dependencies this package has;
#install.packages('corrplot', dependencies=TRUE)

corrplot(returns.cor)

# load car package
require(car)

# Fit some model
model.1 <- lm(VV ~ GS+DD+DOW+HON+HUN+JPM+KO+MMM+XOM, data=returns.df)
summary(model.1)
vif.1 <- vif(model.1)
vif.1[which(vif.1 > 2.5)]

# Fit the full model
model.2 <- lm(VV ~ BAC+GS+JPM+WFC+BHI+CVX+DD+DOW+DPS+HAL+HES+HON+HUN+KO+MMM+MPC+PEP+SLB+XOM,data=returns.df)
summary(model.2)
vif.2 <- vif(model.2)

vif.2[which(vif.2 > 2.5)]

# Principal Component Analysis

returns.pca <- princomp(x=returns.df[,-21],cor=TRUE)
# See the output components returned by princomp();
names(returns.pca)

pc.1 <- returns.pca$loadings[,1];
pc.2 <- returns.pca$loadings[,2];
names(pc.1)

dev.off()
plot( -10,10, type='p', xlim=c(-0.27,-0.12), ylim=c(-0.27,0.6),xlab='PC 1',ylab='PC 2')
text(-pc.1, -pc.2, labels=names(pc.1), cex=0.75)

# Load meta data for symbols.

symbol.info <- data.table( Symbol = c("AA", "BAC", "BHI", "CVX", "DD", "DOW", "DPS", "GS", "HAL", "HES", "HON", "HUN", "JPM", "KO", "MMM", "MPC", "PEP", "SLB", "WFC", "XOM", "VV"),
                           Name = c('Alcoa Aluminum', 'Bank of America', 'Baker Hughes Incorprated', 'Chevron', 'Dupont', 'Dow Chemical', 'DrPepper Snapple', 'Goldman Sachs', 'Halliburton', 'Hess Energy', 'Honeywell International', 'Huntsman Corporation', 'JPMorgan Chase', 'The Coca-Cola Company', '3M Company', 'Marathon Petroleum Corp', 'Pepsi Company', 'Schlumberger', 'Wells Fargo', 'Exxon-Mobile', 'Vanguard Large Cap Index'),
                           Industry = c('Industrial - Metals', 'Banking', 'Oil Field Services', 'Oil Refining', 'Industrial - Chemical', 'Industrial - Chemical', 'Soft Drinks', 'Banking', 'Oil Field Services', 'Oil Refining', 'Manufacturing', 'Industrial - Chemical', 'Banking', 'Soft Drinks', 'Manufacturing', 'Oil Refining', 'Soft Drinks', 'Oil Field Services', 'Banking', 'Oil Refining', 'Market Index' ))

industries <- unique(symbol.info$Industry)
industry.color <- brewer.pal(length(industries), "BrBG")

col.table <- data.table( Industry = industries, Shade = industry.color)

symbol.info <- merge(symbol.info, col.table, on = c("Industry"))

pc.1.long <- data.table(melt(pc.1), keep.rownames = T)
colnames(pc.1.long) <- c("Symbol", "PC1")

pc.2.long <- data.table(melt(pc.2), keep.rownames = T)
colnames(pc.2.long) <- c("Symbol", "PC2")

symbol.info <- merge(symbol.info, pc.1.long, by = c("Symbol"))s
symbol.info <- merge(symbol.info, pc.2.long, by = c("Symbol"))

# Plot PCA by Symbol, colored by Industry
ggplot(symbol.info, aes(x = -PC1, y = -PC2, label = Symbol, color = Industry)) +
  geom_text() +
  xlab("PC1") +
  ylab("PC2") +
  xlim(-0.27, -0.125) +
  ylim(-0.27, 0.6) +
  ggtitle("Principal Component Loadings") +
  theme_gdocs(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#F2F2F2"),
    panel.background = element_rect(fill = "#FFFFFF", colour = "grey50"),
    plot.title = element_text(size = 25,
                              face = "bold",
                              color = "#1C93D1",
                              hjust = 0.5,
                              lineheight = 1.2), # title
    panel.border = element_rect(colour = "#d3d3d3"),
    plot.subtitle = element_text(size = 15,
                                 face = "bold",
                                 hjust = 0.5), # subtitle
    plot.caption = element_text(size = 15), # caption
    axis.title.x = element_text(vjust = 0,
                                size = 22,
                                color = "#1C93D1",
                                face = "bold"), # X axis title
    axis.title.y = element_text(size = 22,
                                color = "#1C93D1",
                                face = "bold"), # Y axis title
    axis.text.x = element_text(size = 12,
                               face = "bold",
                               vjust = 0), # X axis text
    axis.text.y = element_text(size = 12,
                               face = "bold"), # Y axis text
    plot.margin = unit(c(0, 1, 1, 1), "cm")
  )

# Plot the default scree plot;
plot(returns.pca)

# Make Scree Plot
scree.values <- (returns.pca$sdev^2)/sum(returns.pca$sdev^2);

plot(scree.values, xlab='Number of Components',ylab='',type='l',lwd=2)
points(scree.values,lwd=2,cex=1.5)
title('Scree Plot')

ggplot(data.table( y = scree.values)[, x := .I], aes(x, y)) +
  geom_path() +
  geom_point(aes(color = "red")) +
  labs(title = "Scree Plot of Principal Compoenents", x = "Components", y = "") +
  #geom_hline(yintercept = 1, color = "red", type = "dashed")
  theme_gdocs(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#F2F2F2"),
    panel.background = element_rect(fill = "#FFFFFF", colour = "grey50"),
    plot.title = element_text(size = 22,
                              face = "bold",
                              color = "#1C93D1",
                              hjust = 0.5,
                              lineheight = 1.2), # title
    panel.border = element_rect(colour = "#d3d3d3"),
    plot.subtitle = element_text(size = 15,
                                 face = "bold",
                                 hjust = 0.5), # subtitle
    plot.caption = element_text(size = 15), # caption
    axis.title.x = element_text(vjust = 0,
                                size = 16,
                                color = "#1C93D1",
                                face = "bold"), # X axis title
    axis.title.y = element_text(size = 16,
                                color = "#1C93D1",
                                face = "bold"), # Y axis title
    axis.text.x = element_text(size = 12,
                               face = "bold",
                               vjust = 0), # X axis text
    axis.text.y = element_text(size = 12,
                               face = "bold"), # Y axis text
    plot.margin = unit(c(0, 1, 1, 1), "cm"),
    legend.position = "none"
  )


# Make Proportion of Variance Explained
variance.values <- cumsum(returns.pca$sdev^2)/sum(returns.pca$sdev^2);

plot(variance.values,xlab='Number of Components',ylab='',type='l',lwd=2)
points(variance.values,lwd=2,cex=1.5)
abline(h=0.8,lwd=1.5,col='red')
abline(v=8,lwd=1.5,col='red')
text(13,0.5,'Keep 8 Principal Components',col='red')
title('Total Variance Explained Plot')

ggplot(data.table(y = variance.values)[, x := .I], aes(x,y)) + 
  geom_path(lwd = .8) +
  geom_point(color = "red", lwd = 4, shape = 1) +
  geom_vline(xintercept = 8, color = "cornflowerblue", lwd = 1.2) +
  geom_hline(yintercept = .8, color = "cornflowerblue", lwd = 1.2) +
  theme_gdocs(base_size = 12) +
  labs(title = "Total Variance Explained Plot", x = "Number of Componenets", y = "Total Variance Explained") +
  theme(
    plot.background = element_rect(fill = "#F2F2F2"),
    panel.background = element_rect(fill = "#FFFFFF", colour = "grey50"),
    plot.title = element_text(size = 22,
                              face = "bold",
                              color = "#1C93D1",
                              hjust = 0.5,
                              lineheight = 1.2), # title
    panel.border = element_rect(colour = "#d3d3d3"),
    plot.subtitle = element_text(size = 15,
                                 face = "bold",
                                 hjust = 0.5), # subtitle
    plot.caption = element_text(size = 15), # caption
    axis.title.x = element_text(vjust = 0,
                                size = 16,
                                color = "#1C93D1",
                                face = "bold"), # X axis title
    axis.title.y = element_text(size = 16,
                                color = "#1C93D1",
                                face = "bold"), # Y axis title
    axis.text.x = element_text(size = 12,
                               face = "bold",
                               vjust = 0), # X axis text
    axis.text.y = element_text(size = 12,
                               face = "bold"), # Y axis text
    plot.margin = unit(c(0, 1, 1, 1), "cm"),
    legend.position = "none"
  )

# Create the data frame of PCA predictor variables;
return.scores <- as.data.frame(returns.pca$scores);
return.scores$VV <- returns.df$VV;
return.scores$u <- runif(n=dim(return.scores)[1],min=0,max=1);
head(return.scores)

return.scores[, c("Comp.1", "Comp.2")]

ggplot(return.scores, aes( x = Comp.1, y = Comp.2)) +
  geom_point()

# Split the data set into train and test data sets;
train.scores <- subset(return.scores,u<0.70);
test.scores <- subset(return.scores,u>=0.70);
dim(train.scores)
dim(test.scores)
dim(train.scores)+dim(test.scores)
dim(return.scores)

# Fit a linear regression model using the first 8 principal components;
pca1.lm <- lm(VV ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.8, data=train.scores);
summary(pca1.lm)

# Compute the Mean Absolute Error on the training sample;
pca1.mae.train <- mean(abs(train.scores$VV-pca1.lm$fitted.values));
vif(pca1.lm)

# Score the model out-of-sample and compute MAE;
pca1.test <- predict(pca1.lm,newdata=test.scores);
pca1.mae.test <- mean(abs(test.scores$VV-pca1.test));

# Let's compare the PCA regression model with a 'raw' regression model;
# Create a train/test split of the returns data set to match the scores data set;
returns.df$u <- return.scores$u;
train.returns <- subset(returns.df,u<0.70);
test.returns <- subset(returns.df,u>=0.70);
dim(train.returns)
dim(test.returns)
dim(train.returns)+dim(test.returns)
dim(returns.df)


# Fit model.1 on train data set and score on test data;
model.1 <- lm(VV ~ GS+DD+DOW+HON+HUN+JPM+KO+MMM+XOM, data=train.returns)
model1.mae.train <- mean(abs(train.returns$VV-model.1$fitted.values));
model1.test <- predict(model.1,newdata=test.returns);
model1.mae.test <- mean(abs(test.returns$VV-model1.test));

length(model.1$coefficients)

# Fit model.2 on train data set and score on test data;
model.2 <- lm(VV ~ AA+BAC+GS+JPM+WFC+BHI+CVX+DD+DOW+DPS+HAL+HES+HON+HUN+KO+MMM+MPC+PEP+SLB+XOM, data=train.returns)
model2.mae.train <- mean(abs(train.returns$VV-model.2$fitted.values));
model2.test <- predict(model.2,newdata=test.returns);
model2.mae.test <- mean(abs(test.returns$VV-model2.test));

length(model.2$coefficients)

model.scores <- data.table( model = c("pca1.lm", "model.1", "model.2"), 
                            train = c(pca1.mae.train, model1.mae.train, model2.mae.train ),
                            test = c(pca1.mae.test, model1.mae.test, model2.mae.test))

model.scores$train <- round(model.scores$train, 4)
model.scores$test <- round(model.scores$test, 4)

formattable(model.scores, align = c("l", "c", "c", "c", "c", "r"),
            list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
            ))

full.lm <- lm(VV ~ ., data=train.scores);
summary(full.lm)

library(MASS)
backward.lm <- stepAIC(full.lm,direction=c('backward'))
summary(backward.lm)
backward.mae.train <- mean(abs(train.scores$VV-backward.lm$fitted.values));
vif(backward.lm)

backward.test <- predict(backward.lm,newdata=test.scores);
backward.mae.test <- mean(abs(test.scores$VV-backward.test));

##############################
# t-SNE Analysis
##############################

tsne.cor <- returns.cor[-21,-21]

tsne_vis <- function( perplexity = 1, learning = 20, iterations = 5000) {
  
  set.seed(1)
  tsne <- Rtsne(tsne.cor, dims = 2, perplexity=perplexity, verbose =TRUE, max_iter = iterations, learning = learning)
  tsne.results <- data.table(x = tsne$Y[,1], y = tsne$Y[,2], Symbol = row.names(tsne.cor))
  tsne.results <- merge(tsne.results, symbol.info, by = c("Symbol"))[, c("PC1", "PC2") := NULL]
  
  ggplot(tsne.results, aes(x, y, label = Symbol, color = Industry)) +
    geom_text() +
    labs(title = paste("t-SNE: P=", perplexity, " L=", learning, "Iterations=", iterations ), x = "tSNE dimension 1", y = "tSNE dimension 2") +
    theme(legend.position = "right")
}

tsne_vis(perplexity = 6, learning = 50, iterations = 5000)

tsne.ret <- t(returns.df[, -c(21, 22)])

tsne_ret_vis <- function( perplexity = 1, learning = 20, iterations = 5000) {
  
  set.seed(1)
  tsne <- Rtsne(tsne.ret, dims = 2, perplexity=perplexity, verbose =TRUE, max_iter = iterations, learning = learning)
  tsne.results <- data.table(x = tsne$Y[,1], y = tsne$Y[,2], Symbol = row.names(tsne.ret))
  tsne.results <- merge(tsne.results, symbol.info, by = c("Symbol"))[, c("PC1", "PC2") := NULL]
  
  ggplot(tsne.results, aes(x, y, label = Symbol, color = Industry)) +
    geom_text() +
    labs(title = paste("t-SNE: P=", perplexity, " L=", learning, "Iterations=", iterations ), x = "tSNE dimension 1", y = "tSNE dimension 2") +
    theme(legend.position = "right")
  
  tsne.results
}

tsne.results <- tsne_ret_vis(3, 15, 5000)


