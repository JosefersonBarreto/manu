setwd("C:\\Users\\Emanuela\\Documents\\PIBIC\\pibic 2022")

IDH1 = read.csv("IDH.csv",sep = ";")
IDH1

rownames(IDH1) <- IDH1$ï..Estados
View(IDH1)

dados <- IDH1[,2:5]
View(dados)
str(dados)
as.numeric(dados$IDH)
as.numeric(dados$IDH.R)
as.numeric(dados$IDH..L)
as.numeric(dados$IDH..E)


library(cluster)
library(vegan)
library(ecodist)
library(MASS) 


library(maps)
library(mapproj)
par(mar=c(1,1,1,1))
m <- map("world","Brazil", fill=T, col="grey95")
map.axes()

map.scale(ratio=T, cex=0.9)
abline(h=0, lty = 2)
map.scale()
map.grid(m, nx = 5, ny = 5, col="grey", font=1, cex=0.9, pretty = T)
colnames(IDH1[,2:5]) <- IDH1[-4]
IDH1[,2:5]=t(IDH1[,2:5])
dist1<-distance(IDH1[,2:5],"mahalanobis")
clust1<-hclust(dist1,"ward.D")
groups<- cutree(clust1, 5)
par(new=TRUE)

palette(brewer.pal(5,"Blues")
        plot.window(map,col= groups)

plot.default(IDH1$, cex=1,axes=F, ann=T)
text(IDH1, cex=0.5, col=groups)


##################################################################
library(ClustGeo)
# description of 5 municipalities in the map
head(map@IDH1) 
#outra forma 
# corta o dendrograma para obter a partição em 5 clusters
P5 <- cutree(clust1,5)
city_label <- as.vector(IDH1$ï..Estados)
names(P5) <- city_label

plot.default(map, border = "grey", col = P5, 
     main = "Partition P5 obtained with D0 only")
legend("topleft", legend = paste("cluster",1:5), 
       fill = 1:5, bty = "n", border = "white")

