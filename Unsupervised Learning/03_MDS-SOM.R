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

#############
##### MDS
#############

recidivism <- read.csv("recidivism.csv")

ggplot(recidivism, aes(durat, y = ..density..)) +
  geom_histogram(aes(fill = ..count..)) +
  geom_density(col = "red", lwd = 1) +
  labs(title = "Duration Until Return")

ggscatmat(recidivism)

ggplot(recidivism, aes(tserved, durat)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(recidivism, aes(y = tserved, group = black)) +
  geom_boxplot()

ggplot(recidivism, aes(y = durat, group = black)) +
  geom_boxplot()

ggplot(recidivism, aes(y = tserved, group = alcohol)) +
  geom_boxplot()

ggplot(recidivism, aes(y = durat, group = alcohol)) +
  geom_boxplot()

ggplot(recidivism, aes(y = durat, group = drugs)) +
  geom_boxplot(aes(fill = drugs)) +
  facet_grid(. ~ alcohol) +
  labs(title = "Duration by Drugs / Alcohol Use") +
  theme(legend.position = "top")

ggplot(recidivism, aes(y = durat, group = property)) +
  geom_boxplot(aes(fill = property)) +
  facet_grid(. ~ person) +
  labs(title = "Duration by Person / Property Crimes") +
  theme(legend.position = "top")

ggplot(recidivism, aes(y = durat, group = felon)) +
  geom_boxplot(aes(fill = felon)) +
  facet_grid(. ~ workprg ) +
  labs(title = "Duration by Felony / Work Program") +
  theme(legend.position = "top")

p1 <- ggplot(recidivism, aes(durat, rules)) +
  geom_point() + 
  geom_smooth()

p2 <- ggplot(recidivism, aes(durat, priors)) +
  geom_point() + 
  geom_smooth()

p3 <- ggplot(recidivism, aes(durat, educ)) +
  geom_point() + 
  geom_smooth()

grid.arrange(p1, p2, p3, nrow = 3)


p1 <- ggplot(recidivism, aes(durat, age)) +
  geom_point() + 
  geom_smooth()

p2 <- ggplot(recidivism, aes(durat, tserved)) +
  geom_point() + 
  geom_smooth()

p3 <- ggplot(recidivism, aes(durat, follow)) +
  geom_point() + 
  geom_smooth()

grid.arrange(p1, p2, p3, nrow = 3)

# 2.)

recid.dist <- dist(recidivism)

fit <- cmdscale(recid.dist, eig=TRUE, k=2) # k is the number of dim
fit # view results

par(mfrow = c(1,1))

ggplot(data.table(x = fit$points[, 1], y = fit$points[, 2]), aes(x, y)) +
  geom_point(aes(color = recidivism$durat)) +
  geom_hline(yintercept = 0, col = "darkred") +
  geom_vline(xintercept = 0, col = "darkred") +
  labs(title = "Duration (months) by Dimension", subtitle = "Metric MDS", 
       x = "Dimension 1", y = "Dimension 2", color = "Duration")

fit2 <- isoMDS(recid.dist, k=2) # k is the number of dim
fit2 # view results

ggplot(data.table(x = fit2$points[, 1], y = fit2$points[, 2]), aes(x, y)) +
  geom_point(aes(color = recidivism$durat)) +
  geom_hline(yintercept = 0, col = "darkred") +
  geom_vline(xintercept = 0, col = "darkred") +
  labs(title = "Duration (months) by Dimension", subtitle = "Nonmetric MDS",
       x = "Dimension 1", y = "Dimension 2", color = "Duration")

#############
##### Self-Organizing Maps
#############

data.college <- as.data.table(read.csv("college_acceptance.csv"))

data.college %>%
  gather(variable, value, -admit) %>%
  ggplot(aes(y = as.factor(variable),
             fill = as.factor(admit),
             x = percent_rank(value))) +
  geom_density_ridges() +
  ggtitle('Admittance Distributions') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = 'variable', y = 'value', fill = 'Admit (Yes/No)')

ggplot(data.college, aes(rank, y = ..density..)) +
  geom_histogram(aes(fill = ..count..)) +
  geom_density(col = "red", lwd = 1) +
  labs(title = "College Ranks")

ggplot(data.college, aes(gpa, y = ..density..)) +
  geom_histogram(aes(fill = ..count..)) +
  geom_density(col = "red", lwd = 1) +
  labs(title = "Student GPA")

ggplot(data.college, aes(gre, y = ..density..)) +
  geom_histogram(aes(fill = ..count..)) +
  geom_density(col = "red", lwd = 1) +
  labs(title = "Student GRE")

p1 <- ggplot(data.college, aes(y = gre, group = admit)) +
  geom_boxplot(aes(fill = admit)) +
  labs(title = "Student GRE by Admit")

p2 <- ggplot(data.college, aes(y = gpa, group = admit)) +
  geom_boxplot(aes(fill = admit)) +
  labs(title = "Student GPA by Admit")

grid.arrange(p1, p2, nrow = 1)

data.college$s_gre <- scale(data.college$gre)
data.college$s_gpa <- scale(data.college$gpa)
data.college$s_rank <- scale(data.college$rank)

data.college %>%
  select(c(-gpa, -gre, -rank)) %>%
  gather(variable, value, -admit) %>%
  ggplot(aes(y = as.factor(variable),
             fill = as.factor(admit),
             x = percent_rank(value))) +
  geom_density_ridges() +
  ggtitle('Admittance Distributions') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = 'variable', y = 'value', fill = 'Admit (Yes/No)')


distances <- c("sumofsquares")
data.list = list()
factors <- c("admit")

data.list[["admit"]] <- classvec2classmat(data.college$admit)
data.list[["numerics"]] <- as.matrix(data.college[, .(admit, s_gpa, s_gre, s_rank)])

map.dimension = find_grid_size(dim(data.college)[1])

college.epochs <- 2000

# create a grid onto which the som will be mapped
som.grid <- somgrid(xdim = 8,
                    ydim = 9,
                    topo = "rectangular")

# train the SOM
cc.som <- supersom(data.list,
                   grid = som.grid,
                   rlen = college.epochs,
                   alpha = c(0.1, 0.01),
                   whatmap = c('admit', 'numerics'),
                   dist.fcts = c(distances, 'tanimoto'),
                   keep.data = TRUE
)

plot(cc.som, type = "changes")

plot(cc.som, type = "counts", palette.name = coolBlueHotRed)

cc.som$unit.classif
observations_by_node <- get_node_counts(cc.som$unit.classif)

round(mean(observations_by_node$observations), 2)

plot(cc.som, type = "dist.neighbours", palette.name = coolBlueHotRed)

plot(cc.som, type = "codes", palette.name = coolBlueHotRed)
