library(data.table)
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(RColorBrewer)
library(formattable)
library(readxl)
library(MASS)
library(GGally)
library(ggcorrplot)
library(ggdendro)
library(dendextend)
library(fpc)
library(cluster)
library(maptree)
library(ggridges)

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

path.data <- "D:/Projects/MSDS-Unsupervised-Learning/datasets"

setwd(path.data)

eur.employment <- read.csv("EuropeanEmployment.csv")

print(eur.employment)

# ggpairs(eur.employment, cardinality_threshold = 30)

ggpairs(eur.employment[, -1], aes(col = Group), 
        cardinality_threshold = 30)

ggpairs(eur.employment, aes(col = Country), 
        cardinality_threshold = 30)

ggscatmat(eur.employment) +
  ggtitle("Scatterplot Matrix")

# A

ggplot(eur.employment, aes(SER, FIN, color = Group, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), hjust = 0, vjust = 0) +
  ggtitle("Financial vs Services") +
  guides(size = "none")

# B

ggplot(eur.employment, aes(MAN, FIN, color = Group, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), hjust = 0, vjust = 0) +
  ggtitle("Manufacturing vs Services") +
  guides(size = "none")


ggplot(eur.employment, aes(MAN, FIN, col = Group)) +
  geom_point() +
  ggtitle("Manufactering vs Financials")

ggplot(eur.employment, aes(SPS, SER, col = Group)) +
  geom_point() +
  ggtitle("Social vs Service")

ggplot(eur.employment, aes(TC, FIN, col = Group)) +
  geom_point() +
  ggtitle("Transport vs Financials")

# Principal Component Analysis

# Normal

apply(my.data[,-c(1,2)],MARGIN=1,FUN=sum)
pca.out <- princomp(x=eur.employment[,-c(1,2)],cor=FALSE);
names(pca.out)

pc.1 <- pca.out$scores[,1];
pc.2 <- pca.out$scores[,2];
str(pc.1)
pcdf = data.frame(pc1=pc.1, pc2=pc.2)
pcdf1 = cbind(pcdf,eur.employment$Country)
pcdf2 = cbind(pcdf1,eur.employment$Group)
str(pcdf2)

colnames(pcdf2) <- c("PC1", "PC2", "Country", "Group")

ggplot(pcdf2, aes(x= PC1, y = PC2, colour = Group, label = Country)) + 
  geom_point() + geom_text(aes(label=eur.employment$Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Scaled

apply(eur.employment[,-c(1,2)],MARGIN=1,FUN=sum)
pca.out <- princomp( x = scale(eur.employment[,-c(1,2)]), cor=FALSE);
names(pca.out)

pc.1 <- pca.out$scores[,1];
pc.2 <- pca.out$scores[,2];
str(pc.1)
pcdf = data.frame(pc1=pc.1, pc2=pc.2)
pcdf1 = cbind(pcdf,eur.employment$Country)
pcdf2 = cbind(pcdf1,eur.employment$Group)
str(pcdf2)

colnames(pcdf2) <- c("PC1", "PC2", "Country", "Group")

ggplot(pcdf2, aes(x= PC1, y = PC2, colour = Group, label = Country)) + 
  geom_point() + geom_text(aes(label=eur.employment$Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2 (Scaled)") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

employment.pca <- princomp( x = eur.employment[, -c(1,2)], cor=TRUE)

# See the output components returned by princomp();
names(employment.pca)

pc.1 <- employment.pca$loadings[,1];
pc.2 <- employment.pca$loadings[,2];

names(pc.1)

plot( -10,10, type='p', 
      xlim=c(-.6, .6), 
      ylim=c(-.6, .6),
      xlab='PC 1',ylab='PC 2')
text(pc.1, pc.2, labels=names(pc.1), cex=0.75)


pc <- data.table( Name = names(pc.1), X = pc.1, Y = pc.2)
pc$col <- brewer.pal(nrow(pc), "BrBG")

ggplot(pc, aes(X, Y)) +
  geom_text(aes(label = Name, col = col, size = 15)) +
  labs(x = "Component 1", y = "Component 2") +
  guides(col = "none", size = "none") +
  ggtitle("European Employment Principal Components")


employment.pca.std <- princomp( x = scale(eur.employment[, -c(1,2)]), cor = TRUE)

std.pc.1 <- employment.pca.std$loadings[,1];
std.pc.2 <- employment.pca.std$loadings[,2];

std.pc <- data.table( Name = names(pc.1), X = pc.1, Y = pc.2)
std.pc$col <- brewer.pal(nrow(pc), "BrBG")

ggplot(std.pc, aes(X, Y)) +
  geom_text(aes(label = Name, col = col, size = 15)) +
  labs(x = "Component 1", y = "Component 2") +
  guides(col = "none", size = "none") +
  ggtitle("European Employment Principal Components (Standardized)")

# (4)	Hierarchical Clustering Analysis

clusters <- hclust(dist(eur.employment[, -c(1,2)]))
plot(clusters)

ggdendrogram(clusters) +
  ggtitle("European Employment Dendrogram")

dend <- as.dendrogram(clusters)

d3 = color_branches(dend, k=3)
plot(d3) +
  title("Dendrogram: K=3")

d6 = color_branches(dend, k=6)
plot(d6) + 
  title("Dendrogram: K=6")


pca.clusters <- hclust(dist(pc[, 2:3]))

ggdendrogram(pca.clusters) +
  ggtitle("European Employment Dendrogram - PCA")

cutree(pca.clusters, k = 3)

dend <- as.dendrogram(pca.clusters)

d3 = color_branches(dend, k=3)
plot(d3) +
  title("PCA Dendrogram: K=3")

d6 = color_branches(dend, k=6)
plot(d6) + 
  title("PCA Dendrogram: K=6")

# Hirerarchical clustreing

hier.dist = dist(eur.employment[,-c(1,2)])

hclustmodel <- hclust(hier.dist, method = 'complete')
plot(hclustmodel, labels = eur.employment$Country)

hc <- dendro_data(hclustmodel)
dict <- setNames(eur.employment$Country, 1:30)

hc$labels$label <- sapply(hc$labels$label, function(x) dict[[as.character(x)]])

ggdendrogram(hc) +
  ggtitle("European Employment Dendrogram")

dend <- as.dendrogram(hclustmodel)

d3 = color_branches(dend, k=3)
plot(d3) +
  title("PCA Dendrogram: K=3")

d6 = color_branches(dend, k=6)
plot(d6) + 
  title("PCA Dendrogram: K=6")


# choose the number of clusters k = 3
cut.3 <- cutree(hclustmodel, k=3)
pcdf3 <- cbind(pcdf2, K3 = cut.3)
pcdf3

cut.6 <- cutree(hclustmodel, k = 6)
pcdf4 <- cbind(pcdf3, K6 = cut.6)

# cross tab of clusters vs Group

table(pcdf3$Group)

table(pcdf3$Group, pcdf3$K3)

table(pcdf4$Group, pcdf4$K6)


# accuracy - Between % ss

subdat <- eur.employment[,-c(1,2)]
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS

complete3 <- cutree(hclust(hier.dist),3)
WSS3 <- cluster.stats(hier.dist, 
                      complete3, 
                      alt.clustering=NULL)$within.cluster.ss
WSS3

BetSSPer3 <- (TSS-WSS3)/TSS
BetSSPer3

complete6 <- cutree(hclust(hier.dist), 6)
WSS6 <- cluster.stats(hier.dist, 
                      complete6, 
                      alt.clustering=NULL)$within.cluster.ss
WSS6

BetSSPer6 <- (TSS-WSS6)/TSS
BetSSPer6

norm.r <- data.table(Method = c("Std k=3", "Std k=6"), Error = c(WSS3, WSS6), Pct = c(BetSSPer3, BetSSPer6))
formattable(norm.r)


### PCA

pca.dist <- dist(pc[, 2:3])
pca.hclustmodel <- hclust(pca.dist, method = 'complete')
plot(pca.hclustmodel)

pca.hc <- dendro_data(pca.hclustmodel)
dict <- setNames(eur.employment$Country, 1:30)

pca.hc$labels$label <- sapply(hc$labels$label, function(x) dict[[as.character(x)]])

ggdendrogram(pca.hc) +
  ggtitle("European Employment Dendrogram (PCA)")


# accuracy - Between % ss

subdat <- pc[, 2:3]
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS

complete3 <- cutree(hclust(pca.dist),3)
WSS3 <- cluster.stats(pca.dist, 
                      complete3, 
                      alt.clustering=NULL)$within.cluster.ss
WSS3

BetSSPer3 <- (TSS-WSS3)/TSS
BetSSPer3

complete6 <- cutree(hclust(pca.dist), 6)
WSS6 <- cluster.stats(pca.dist, 
                      complete6, 
                      alt.clustering=NULL)$within.cluster.ss
WSS6

BetSSPer6 <- (TSS-WSS6)/TSS
BetSSPer6

pca.r <- data.table(Method = c("PCA k=3", "PCA k=6"), Error = c(WSS3, WSS6), Pct = c(BetSSPer3, BetSSPer6))
formattable(pca.r)

formattable(rbind(norm.r, pca.r)[, .(Method, Pct)])

# kmeans clustering with k=3 clusters

k3.results <- kmeans(eur.employment[,-c(1,2)],3)
names(k3.results)
knn3.BetSSPer <- k3.results$betweenss/k3.results$totss
knn3.BetSSPer
k3.results$totss

# cluster plots for kmeans

clusplot(eur.employment[,-c(1,2)], k3.results$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

k3.result <- data.table( Country = eur.employment$Country, Group = eur.employment$Group, Cluster = k3.results$cluster)

ggplot(k3.result) +
  geom_text(aes(Country, Cluster))

# kmeans clustering with k=6 clusters

k6.results <- kmeans(eur.employment[,-c(1,2)], 6)
names(k6.results)
knn6.BetSSPer <- k6.results$betweenss/k6.results$totss
knn6.BetSSPer
k6.results$totss

# cluster plots for kmeans

clusplot(eur.employment[,-c(1,2)], k6.results$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

k6.result <- data.table( Country = eur.employment$Country, Group = eur.employment$Group, Cluster = k6.results$cluster)

r.kmeans <- data.table(Method = c("KNN k=3", "KNN k=6"), Pct = c(knn3.BetSSPer, knn6.BetSSPer))

formattable(rbind(rbind(norm.r, pca.r)[, .(Method, Pct)], r.kmeans))

formattable(r.kmeans)


# pca / kmeans clustering with k=3 clusters

pca.k3.results <- kmeans(pc[, 2:3],3)
names(k3.results)
pca.knn3.BetSSPer <- pca.k3.results$betweenss/pca.k3.results$totss
pca.knn3.BetSSPer
pca.k3.results$totss

# cluster plots for kmeans

clusplot(pc[, 2:3], pca.k3.results$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# pca / kmeans clustering with k=6 clusters

pca.k6.results <- kmeans(pc[, 2:3], 6)
names(pca.k6.results)
pca.knn6.BetSSPer <- pca.k6.results$betweenss/pca.k6.results$totss
pca.knn6.BetSSPer
pca.k6.results$totss

# cluster plots for kmeans

clusplot(pc[, 2:3], pca.k6.results$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

r.kmeans6 <- data.table(Method = c("PCA KNN k=3", "PCA KNN k=6"), Pct = c(knn3.BetSSPer, knn6.BetSSPer))

formattable(rbind(rbind(rbind(norm.r, pca.r)[, .(Method, Pct)], r.kmeans), r.kmeans6))

wssplot <- function(subdat, nc=15, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(subdat, centers=i)$withinss)}
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")} 

subdat <- eur.employment[,-c(1,2)]

wssplot(subdat)

## Hierarchical clustering

wssplot <- function(subdat, nc=15, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    require(fpc)
    set.seed(seed)
    hier.dist <- dist(subdat)
    complete3 <- cutree(hclust(hier.dist),i)
    wss[i] <- cluster.stats(hier.dist,complete3, alt.clustering=NULL)$within.cluster.ss}
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")
  return(wss)
}

wssplot(subdat, nc=20)
abline(v=8, col="darkred")
abline(h=.8, col="cornflowerblue")

## USStates

data.states <- read.csv("USStates.csv")

subdata <- data.states[, -c(1,)]

wssplot(subdat)
abline(v=6, col="darkred")
abline(h=.8, col="cornflowerblue")

hier.dist = dist(subdat[,-c(1,2)])

hclustmodel <- hclust(hier.dist, method = 'complete')
plot(hclustmodel, labels = data.states$State)

hc <- dendro_data(hclustmodel)
dict <- setNames(data.states$State, 1:50)

hc$labels$label <- sapply(hc$labels$label, function(x) dict[[as.character(x)]])

ggdendrogram(hc) +
  ggtitle("US States Dendrogram")

dend <- as.dendrogram(hclustmodel)

d3 = color_branches(dend, k=3)
plot(d3) +
  title("States Dendrogram: K=3")

d6 = color_branches(dend, k=6)
plot(d6) + 
  title("States Dendrogram: K=6")

TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS

complete3 <- cutree(hclust(hier.dist),3)
WSS3 <- cluster.stats(hier.dist, 
                      complete3, 
                      alt.clustering=NULL)$within.cluster.ss
WSS3

BetSSPer3 <- (TSS-WSS3)/TSS
BetSSPer3

complete6 <- cutree(hclust(hier.dist), 6)
WSS6 <- cluster.stats(hier.dist, 
                      complete6, 
                      alt.clustering=NULL)$within.cluster.ss
WSS6

BetSSPer6 <- (TSS-WSS6)/TSS
BetSSPer6

norm.r <- data.table(Method = c("Std k=3", "Std k=6"), Error = c(WSS3, WSS6), Pct = c(BetSSPer3, BetSSPer6))
formattable(norm.r)

# RECIDIVISM

data.recid <- read.csv("recidivism.csv")

max(data.recid$durat)

ggplot(data.recid, aes(durat)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  geom_density(aes(y = ..density..)) +
  labs(title = "Duration Density")

data.recid$durcat <- as.numeric(cut(data.recid$durat, seq(0, 81, 9)))

ggplot(data.recid, aes(x = durcat, y = ..count.., fill = ..count..)) +
  geom_bar() +
  labs(title = "Duration Category")

k3.results <- kmeans(data.recid, 3)
names(k3.results)
knn3.BetSSPer <- k3.results$betweenss/k3.results$totss
knn3.BetSSPer
k3.results$totss

# cluster plots for kmeans

clusplot(data.recid, k3.results$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

k3.result <- data.table( Country = eur.employment$Country, Group = eur.employment$Group, Cluster = k3.results$cluster)

# kmeans clustering with k=6 clusters

recid_knn <- function(data = data.recid, k = 5 ) {

  results <- kmeans(data, k)
  
  BetSSPer <- results$betweenss/results$totss
  print(BetSSPer)
  results$totss
  
  # cluster plots for kmeans
  
  clusplot(data, results$cluster, color=TRUE, shade=TRUE, 
           labels=2, lines=0)
  
}

recid_knn(data.recid, 5)

recid.pca <- princomp( x = data.recid[, -c(18, 19)], cor=TRUE)

# See the output components returned by princomp();
names(recid.pca)

pc.1 <- recid.pca$loadings[,1];
pc.2 <- recid.pca$loadings[,2];

plot( -10,10, type='p', 
      xlim=c(-.6, .6), 
      ylim=c(-.6, .6),
      xlab='PC 1',ylab='PC 2')
text(pc.1, pc.2, labels=names(pc.1), cex=0.75)

pc <- data.table( Name = names(pc.1), X = pc.1, Y = pc.2)
pc$col <- c(brewer.pal(9, "PuBuGn"), brewer.pal(8, "YlGnBu"))

ggplot(pc, aes(X, Y)) +
  geom_text(aes(label = Name, col = col, size = 15)) +
  labs(x = "Component 1", y = "Component 2") +
  guides(col = "none", size = "none") +
  ggtitle("Recidivism Principal Components")

# pca / kmeans clustering

recid_knn(pc[, 2:3], 4)
