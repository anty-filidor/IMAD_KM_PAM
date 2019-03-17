# Configuration of environment
rm(list=ls())
clc <- function() cat("\014 Screen has been succesfully wiped")
Sys.setenv(LANG = "en")
library(cluster)
library(clusterCrit)
library(ggplot2)
library(NbClust)
library(factoextra)
library(funtimes)
library(clusterSim)
clc()

#Wczytywanie danych
wine <- read.csv(file="wine.data.csv")
myClass <-as.matrix(wine[,1])
myData <-as.matrix(wine[,-1]) #usunięcie kolumny oznaczceniem klasy


diabetes <- read.csv(file="diabetes.data.csv")
myClass <-as.matrix(diabetes[,9])
myData <-as.matrix(diabetes[,-9]) #usunięcie kolumny oznaczceniem klasy


glass <- read.csv(file="glass.data.csv")
myClass <-as.matrix(glass[,10])
myData <-as.matrix(glass[,-10]) #usunięcie kolumny oznaczceniem klasy


stock <- read.csv(file="dow_jones_index.data.csv")
stock<-stock[,-1]
stock<-stock[,-2]
myClass<-as.matrix(stock[,1])
myData<-as.matrix(stock[,-1])

head(myData)
myData <- data.Normalization(myData,type="n1",normalization="column") #normalizacja danychtype="n1"
head(myData)

acc=matrix(nrow=30, ncol=5)
dimnames(acc)=list(c(1:30), c("Davies-Bouldin", "DUNN", "Silhouette", "Putiry", "number"))

#Algorytm PAM
for(division in 1:30)
{
  pam.res <- pam(myData, 30, metric="manhattan")
  pam.res$cluster
  fviz_cluster(pam.res, data = myData, geom = "point",stand = FALSE, ellipse.type = "norm")
  table(pam.res$cluster, myClass)
  criteria <- intCriteria(myData,pam.res$cluster,"all")
  acc[division, 1]<-criteria$davies_bouldin
  acc[division, 2]<-criteria$dunn
  acc[division, 3]<-criteria$silhouette
  pur <- purity(myClass, pam.res$cluster)
  acc[division, 4]<-pur$pur
  acc[division, 5]<-division
  print(division)
  print(acc[division, 4])
}


#Algorytm K-means
for(division in 1:30)
{
  division <- 6
  km.res <- kmeans(myData, division, nstart = 10)
  km.res$cluster
  fviz_cluster(km.res, data = myData, geom = "point",stand = FALSE, ellipse.type = "norm")
  table(km.res$cluster, myClass)
  criteria <- intCriteria(myData,km.res$cluster,"all")
  acc[division, 1]<-criteria$davies_bouldin
  acc[division, 2]<-criteria$dunn
  acc[division, 3]<-criteria$silhouette
  pur <- purity(myClass, km.res$cluster)
  acc[division, 4]<-pur$pur
  acc[division, 5]<-division
  print(division)
}


plot(acc[,5], acc[,1], type="o", main="Dow Jones Data index",  xlab="Number of clusters",
     ylab="Quality of clusterisation", col="red", ylim=c(0,3), tck=1)
points(acc[,5], acc[,2], col="blue")
lines(acc[,5], acc[,2], col="blue")
points(acc[,5], acc[,3], col="green")
lines(acc[,5], acc[,3], col="green")
points(acc[,5], acc[,4], col="brown")
lines(acc[,5], acc[,4], col="brown")
legend(20.2,2.9,legend=c("Davies-Bouldin", "DUNN", "Silhouette", "Putiry"),
       col=c("red","blue","green","brown"),lty=c(1,1,1,1), pch="o", ncol=1)






