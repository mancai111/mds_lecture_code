library(MASS)
library(vegan)
library(permute)
library(lattice)
library(facteoextra)

######wine dataset#####################################################################

data<-read.table("C:/Users/16341/Desktop/wine.txt")
colnames(data)<-c("Class","Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Magnesium","Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins","Color_intensity","Hue","OD280/OD315","Proline")
data1<-data[,-1]
data1<-apply(data1, 2,as.numeric)
n<-nrow(data1)
p<-ncol(data1)

#1.1#isoMDS()--dimension=3
d<-dist(data1) #euclidean
MDS1<-isoMDS(d,k=3) 
x1<-MDS1$points[,1]
x2<-MDS1$points[,2]
x3<-MDS1$points[,3]
plot3d(x1,x2,x3)
###classes with different color
x11<-MDS1$points[which(data$Class==1),1]
x12<-MDS1$points[which(data$Class==1),2]
x13<-MDS1$points[which(data$Class==1),3]
x21<-MDS1$points[which(data$Class==2),1]
x22<-MDS1$points[which(data$Class==2),2]
x23<-MDS1$points[which(data$Class==2),3]
x31<-MDS1$points[which(data$Class==3),1]
x32<-MDS1$points[which(data$Class==3),2]
x33<-MDS1$points[which(data$Class==3),3]
plot3d(x11,x12,x13,col = "1",xlim = c(min(x1),max(x1)),ylim = c(min(x2),max(x2)),zlim = c(min(x3),max(x3)))
points3d(x21,x22,x23,col = "2")
points3d(x31,x32,x33,col = "3")

#1.2#isoMDS()--dimension=2
###Euclidean distance metrix
data1.dis<- dist(data1) #Euclidean
MDS2<-isoMDS(data1.dis,k=2)
x1<-MDS2$points[,1]
x2<-MDS2$points[,2]
x11<-MDS2$points[which(data$Class==1),1]
x12<-MDS2$points[which(data$Class==1),2]
x21<-MDS2$points[which(data$Class==2),1]
x22<-MDS2$points[which(data$Class==2),2]
x31<-MDS2$points[which(data$Class==3),1]
x32<-MDS2$points[which(data$Class==3),2]
####plot NMDS
plot(x11,x12,col = "1",xlim = c(min(x1),max(x1)),ylim = c(min(x2),max(x2)),sub="swiss-isoMDS-Euclidean")
points(x21,x22,col = "2")
points(x31,x32,col = "3")
####plot cluster
x00<-cbind(x1,x2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)

###Bray-Curtis dissimilartiy matrix
data1.dis<- vegdist(data1) #Bray-Curtis
MDS2<-isoMDS(data1.dis,k=2)
x1<-MDS2$points[,1]
x2<-MDS2$points[,2]
x11<-MDS2$points[which(data$Class==1),1]
x12<-MDS2$points[which(data$Class==1),2]
x21<-MDS2$points[which(data$Class==2),1]
x22<-MDS2$points[which(data$Class==2),2]
x31<-MDS2$points[which(data$Class==3),1]
x32<-MDS2$points[which(data$Class==3),2]
####plot NMDS
plot(x11,x12,col = "1",xlim = c(min(x1),max(x1)),ylim = c(min(x2),max(x2)),sub="swiss-isoMDS-Bray-Curtis")
points(x21,x22,col = "2")
points(x31,x32,col = "3")
####plot k-means cluster
x00<-cbind(x1,x2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)

#2#nomoMDS()-vegan package
###Euclidean
data1.dis<- vegdist(data1,method = euc)
data1.mds0 <- monoMDS(data1.dis)
a01<-which(data$Class==1)
a02<-which(data$Class==2)
a03<-which(data$Class==3)
NMDS1<-data1.mds0$points[,1]
NMDS2<-data1.mds0$points[,2]
####plot NMDS
plot(NMDS1[a01],NMDS2[a01],type = "p",col=1,xlim = c(min(NMDS1),max(NMDS1)),ylim = c(min(NMDS2),max(NMDS2)),sub = "wine-monoMDS-Euc")
points(NMDS1[a02],NMDS2[a02],type = "p",col=2)
points(NMDS1[a03],NMDS2[a03],type = "p",col=3)
####plot cluster
x00<-cbind(NMDS1,NMDS2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)

###Bray-Curtis
data1.dis<- vegdist(data1)
data1.mds0 <- monoMDS(data1.dis)
a01<-which(data$Class==1)
a02<-which(data$Class==2)
a03<-which(data$Class==3)
NMDS1<-data1.mds0$points[,1]
NMDS2<-data1.mds0$points[,2]
####plot NMDS
plot(NMDS1[a01],NMDS2[a01],type = "p",col=1,xlim = c(min(NMDS1),max(NMDS1)),ylim = c(min(NMDS2),max(NMDS2)),sub = "wine-monoMDS-bray")
points(NMDS1[a02],NMDS2[a02],type = "p",col=2)
points(NMDS1[a03],NMDS2[a03],type = "p",col=3)
####plot cluster
x00<-cbind(NMDS1,NMDS2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)

#3#metaMDS()--vegan package
###euclidean
d<- vegdist(data1,method = euc)
data1.mds<-metaMDS(d,trace = FALSE)
NMDS1<-data1.mds$points[,1]
NMDS2<-data1.mds$points[,2]
####plot NMDS
plot(NMDS1[a01],NMDS2[a01],type = "p",col=1,xlim = c(min(NMDS1),max(NMDS1)),ylim = c(min(NMDS2),max(NMDS2)),sub = "wine-metaMDS-euc")
points(NMDS1[a02],NMDS2[a02],type = "p",col=2)
points(NMDS1[a03],NMDS2[a03],type = "p",col=3)
####plot cluster
x00<-cbind(NMDS1,NMDS2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)

###Bray-Curtis
data1.dis<- vegdist(data1) #bray
data1.mds<-metaMDS(data1.dis,trace = FALSE)
NMDS1<-data1.mds$points[,1]
NMDS2<-data1.mds$points[,2]
####plot NMDS
plot(NMDS1[a01],NMDS2[a01],type = "p",col=1,xlim = c(min(NMDS1),max(NMDS1)),ylim = c(min(NMDS2),max(NMDS2)),sub = "wine-metaMDS-bray")
points(NMDS1[a02],NMDS2[a02],type = "p",col=2)
points(NMDS1[a03],NMDS2[a03],type = "p",col=3)
####plot cluster
x00<-cbind(NMDS1,NMDS2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)



#####swiss dataset###########################################################
swiss<-read.csv("C:/Users/16341/Desktop/swiss.csv",header = T)
data1<-apply(swiss[-1],2,as.numeric)
n<-nrow(data1)
p<-ncol(data1)


#1#isoMDS()--MASS package--dimension=2
###Euclidean distance metrix
d<-dist(data1) #Euclidean
MDS1<-isoMDS(d,k=2)
x1<-MDS1$points[,1]
x2<-MDS1$points[,2]
####plot NMDS
plot(x1,x2,pch="*",sub="swiss-isoMDS-Euc")
text(x1,x2,labels=swiss$Location,col="blue",cex = 0.6)
####plot cluster
x00<-cbind(x1,x2)
km.res = kmeans(x00, 2)
fviz_cluster(km.res, x00)

data1.dis<- vegdist(data1) #Bray-Curtis
MDS1<-isoMDS(data1.dis,k=2)
x1<-MDS1$points[,1]
x2<-MDS1$points[,2]
plot(x1,x2,pch="*",sub="swiss-isoMDS-Bray-Curtis")
text(x1,x2,labels=swiss$Location,col="blue",cex = 0.6)
x00<-cbind(x1,x2)
km.res = kmeans(x00, 2)
library(facteoextra)## Visualize clusters using factoextra
fviz_cluster(km.res, x00)


#2#nomoMDS()-vegan package
###Euclidean
d<-vegdist(data1,method = euc)
data1.mds0 <- monoMDS(da)
NMDS1<-data1.mds0$points[,1]
NMDS2<-data1.mds0$points[,2]
###plot NMDS
plot(data1.mds0, type = "t",xlim = c(-2,2),sub = "swiss-monoMDS-Euc")
text(data1.mds0,labels = swiss$Location,col="blue",cex=0.6)
###plot cluster
x00<-cbind(NMDS1,NMDS2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)

###Bray-Curtis
data1.dis<- vegdist(data1) #Bray-Curtis
data1.mds0 <- monoMDS(data1.dis)
NMDS1<-data1.mds0$points[,1]
NMDS2<-data1.mds0$points[,2]
####plot NMDS
plot(NMDS1,NMDS2,type = "p",sub = "swiss-monoMDS-Bray")
text(data1.mds0,labels = swiss$Location,col="blue",cex=0.6, xpd=TRUE)
####plot cluster
x00<-cbind(NMDS1,NMDS2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)

#3#metaMDS()--vegan package
###Euclidean
d<- vegdist(data1) #Bray-Curtis
data1.mds<-metaMDS(d)
NMDS1<-data1.mds$points[,1]
NMDS2<-data1.mds$points[,2]
####plot NMDS
plot(NMDS1,NMDS2,type = "p", sub="swiss-metaMDS-Euc")
text(data1.mds,labels = swiss$Location,col="blue",cex=0.6)
####plot cluster
x00<-cbind(NMDS1,NMDS2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)## Visualize clusters using factoextra

###Bray-Curtis
data1.dis<- vegdist(data1) #Bray-Curtis
data1.mds<-metaMDS(data1.dis)
NMDS1<-data1.mds$points[,1]
NMDS2<-data1.mds$points[,2]
####plot NMDS
plot(NMDS1,NMDS2,type = "p", sub="swiss-metaMDS-Bray")
text(data1.mds,labels = swiss$Location,col="blue",cex=0.6)
####plot cluster
x00<-cbind(NMDS1,NMDS2)
km.res = kmeans(x00, 3)
fviz_cluster(km.res, x00)## Visualize clusters using factoextra