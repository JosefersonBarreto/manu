###################################################
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

library(ClustGeo)

D.geo <- IDH1$D.geo
map <- IDH1$map
map("world","Brazil", fill=T, col="grey95")
map.axes()
colnames(IDH1[,2:5]) <- IDH1[-4]
IDH1[,2:5]=t(IDH1[,2:5])
dist1<-distance(IDH1[,2:5],"mahalanobis")
clust1<-hclust(dist1,"ward.D")
groups<- cutree(clust1, 5)
city_label <- as.vector(map$"ï..Estados")
names(groups) <- city_label

plot(map,border = "grey", col = groups, 
     main = "Partition P5 obtained with D0 only")
legend("topleft", legend = paste("cluster",1:5), 
       fill = 1:5, bty = "n", border = "white")

#############################################################
library(rgdal)
library(ggplot2)
library(RColorBrewer)
dados_z = scale(dados)
View(dados_z)
shapeUFs <- readOGR('C:\\Users\\Emanuela\\Desktop\\Brasil', 'UFEBRASIL')

# Simulando o resultado do agrupamento por k-means:
mediask <- data.frame(
  ï..Estados = shapeUFs@data$ï..Estados,
  cluster = sample(1:4, nrow(shapeUFs@data), replace = TRUE)
)
IDH1$ï..Estados
# Convertendo o shape para data.frame (necessário para usar geom_map):
mapaUFs <- fortify(dados, region = 'ï..Estados')

km <- kmeans(dados_z, centers = 5)
km
km, data = dados_z
ggplot(km, data = dados_z) +
  geom_map(
    map = mapaUFs,
    color = 'gray20',
    aes(map_id = IDH1,
        fill = as.factor(cluster)) 
  ) +
  expand_limits(  # porque o data.frame com os clusters não possui coordenadas
    x = mapaUFs$long,
    y = mapaUFs$lat
  ) +
  scale_fill_brewer(
    'Cluster',
    palette = 'Accent'
  ) +
  coord_map() +  # define uma projeção, para que o mapa mantenha as proporções
  theme_void()   # para não exibir grade, eixos, etc
###############################################################

