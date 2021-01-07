# Required packages
library(magrittr)
library(dplyr)
library(ggpubr)

###-------------------------------------------------------swiss cmd
data("swiss")
head(swiss)

# MDS
mds <- swiss %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")


# Plot MDS
# Plot1 with label and Plot2 without label
ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          xlab = "x",
          ylab = "y",
          title = "swiss dataset classical MDS",
          size = 2,
          repel = TRUE)

ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
                  xlab = "x",
                  ylab = "y",
                  title = "swiss dataset classical MDS",
                  size = 1,
                  repel = TRUE)

### MDS presents the samples by converting the linear distances among samples in to 2D
### Clustering is done based on minimizing the linear distances (euclidean distance)
### Check how each ob is similar from each other

### visualization's 2 dimensions represent variations
### x = largest variability in sample . y = second largest variability in sample

### According to direct observation, points like Rive Gauche, Rive Droite,
### Echallens, Moutier, V.DE Geneve are outliers and dissimilar from others
### At least two clusters can be determined
### Check the distances between the loose groups

## ADD CODE HERE ABOUT HOW TO DETERMNIE HOW MANY CLUSTERS TO MAKE 
# clust <- kmeans(mds_IST, 2)$cluster %>% as.factor()
# fviz_nbclust(mds_IST, FUNcluster = kmeans)

# K-means clustering -- to show 3 main clusters
   # this is similar to textbook example with 25 US universities where the data
  # generally fell into clusters -- private vs. public university
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds
mds <- mds %>%
  mutate(groups = clust)
mds
# Plot and color
ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          color = "groups",
          xlab = "x",
          ylab = "y",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

#Classical (metric) MDS
###-----------------------------------------------------Wine cmd
wine = read.csv("C:/Users/MancaiNanjolno/Desktop/140/wine.csv")
head(wine)

mds <- wine[,2:14] %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

# Plot MDS
# Plot1 with label and Plot2 without label
ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
                  label = wine[,1],
                  xlab = "x",
                  ylab = "y",
                  shape = 2,
                  title = "Wine Dataset Classical MDS",
                  size = 1,
                  repel = TRUE)

clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)

class <- wine[,1]

mds$wine <- class

ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
                  label = "wine",
                  color = "groups",
                  #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                  xlab = "x",
                  ylab = "y",
                  size = 1, 
                  ellipse = TRUE,
                  #ellipse.type = "convex",
                  repel = TRUE)

#------------------------------------------------------------------------------

#wcmdscale from vegan swiss
#install.packages("vegan")
library(vegan)

weight <- rowSums(swiss)/sum(swiss) #weight of each row(ob) in swiss

d <- dist(swiss) #Euclidean
wmds <- wcmdscale(d, w = weight) #default: k = 2
wmds_data <- as_tibble(wmds)
wmds
colnames(wmds_data) <- c("Dim.1", "Dim.2")

wmds <- swiss %>%
  dist() %>%          
  wcmdscale(w=weight) %>%
  as_tibble()
colnames(wmds) <- c("Dim.1", "Dim.2")

# Plot WMDS
# Plot1 with label and Plot2 without label
ggpubr::ggscatter(wmds_data, x = "Dim.1", y = "Dim.2", 
                  label = rownames(swiss),
                  xlab = "x",
                  ylab = "y",
                  title = "swiss dataset classical WMDS",
                  size = 2,
                  repel = TRUE)

ggpubr::ggscatter(wmds_data, x = "Dim.1", y = "Dim.2", 
                  xlab = "x",
                  ylab = "y",
                  title = "swiss dataset classical WMDS",
                  size = 1,
                  repel = TRUE)

# Plot WMDS
# Plot Clusters
wclust <- kmeans(wmds_data, 3)$cluster %>%
  as.factor()
wmds <- wmds_data[,1:2] %>%
  mutate(groups = wclust)
# Plot and color
ggpubr::ggscatter(wmds, x = "Dim.1", y = "Dim.2", 
                  color = "groups",
                  xlab = "x",
                  ylab = "y",
                  size = 1, 
                  ellipse = TRUE,
                  ellipse.type = "convex",
                  repel = TRUE)

#--------------------------------------------------------------
#wine wcmdscale

rowsum_wine = rowSums(wine[,2:14]) #We want to ignore the class!
sum_wine = sum(wine[,2:14])
weight <- rowsum_wine/sum_wine #weight of each row(ob) in swiss

wmds <- wine %>%
  dist() %>%          
  wcmdscale(w=weight) %>%
  as_tibble()
colnames(wmds) <- c("Dim.1", "Dim.2")

# Plot WMDS
# Plot with label
ggpubr::ggscatter(wmds, x = "Dim.1", y = "Dim.2", 
                  label = wine[,1],
                  xlab = "x",
                  ylab = "y",
                  shape = 2,
                  title = "Wine Dataset Classical WMDS",
                  size = 1,
                  repel = TRUE)


# Plot WMDS
# Plot with clusters
wclust <- kmeans(wmds, 3)$cluster %>%
  as.factor()
wmds <- wmds[,1:2] %>%
  mutate(groups = wclust)
class <- wine[,1]
wmds$wine <- class
ggpubr::ggscatter(wmds, x = "Dim.1", y = "Dim.2", 
                  label = "wine",
                  color = "groups",
                  #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                  xlab = "x",
                  ylab = "y",
                  size = 1, 
                  ellipse = TRUE,
                  #ellipse.type = "convex",
                  repel = TRUE)


#------------------------------------------------------------------------------
#smacofsym swiss
#install.packages("smacof")

library(smacof)

d <- dist(swiss)
smds <- smacofSym(d, ndim=2)
#summary(mds)
smds_data <- as_tibble(smds$conf)
colnames(smds_data) <- c("Dim.1","Dim.2")

# Plot MDS
# Plot1 with label & Plot2 without label
ggpubr::ggscatter(smds_data, x = "Dim.1", y = "Dim.2", 
                  label = rownames(swiss),
                  xlab = "x",
                  ylab = "y",
                  title = "swiss dataset smacofSym",
                  size = 2,
                  repel = TRUE)

ggpubr::ggscatter(smds_data, x = "Dim.1", y = "Dim.2", 
                  xlab = "x",
                  ylab = "y",
                  title = "swiss dataset smacofSym",
                  size = 1,
                  repel = TRUE)

# Plot SMDS
# Plot Clusters
sclust <- kmeans(smds_data, 3)$cluster %>%
  as.factor()

smds <- smds_data %>%
  mutate(groups = sclust)

# Plot and color
ggpubr::ggscatter(smds, x = "Dim.1", y = "Dim.2", 
                  color = "groups",
                  xlab = "x",
                  ylab = "y",
                  size = 1, 
                  ellipse = TRUE,
                  ellipse.type = "convex",
                  repel = TRUE)

#--------------------------------------------------------------
#wine smacofSys

d <- dist(wine[,2:14])
smds <- smacofSym(d, ndim=2)
smds_data <- as_tibble(smds$conf)
colnames(smds_data) <- c("Dim.1","Dim.2")




# Plot WMDS
# Plot with label
ggpubr::ggscatter(smds_data, x = "Dim.1", y = "Dim.2", 
                  label = wine[,1],
                  xlab = "x",
                  ylab = "y",
                  shape = 2,
                  title = "Wine Dataset Classical WMDS",
                  size = 1,
                  repel = TRUE)


# Plot SMDS
# Plot with clusters
sclust <- kmeans(smds_data, 3)$cluster %>%
  as.factor()
smds <- smds_data %>%
  mutate(groups = sclust)
class <- wine[,1]
smds$wine <- class
ggpubr::ggscatter(smds, x = "Dim.1", y = "Dim.2", 
                  label = "wine",
                  color = "groups",
                  #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                  xlab = "x",
                  ylab = "y",
                  size = 1, 
                  ellipse = TRUE,
                  #ellipse.type = "convex",
                  repel = TRUE)
