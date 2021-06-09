
#' Load the libraries
library(sp)
library(spData)
library(tmap)
library(rgdal)
library(fGarch)
library(spdep)
library(pheatmap)

set.seed(2019)
options( scipen=999)
columbus = rgdal::readOGR(verbose = F, system.file("shapes/columbus.shp", package="spData")[1])
sp::proj4string(columbus) = CRS("+init=epsg:4326")

#' Remove unnecesery columns
columbus@data[,1:20]=NULL

#' Create an  'ID' and 'var1' columns with values for each polygon of the dataset.
columbus$id = paste0("A", as.character(1:length(columbus))) 
columbus$var1 = abs(round( rsnorm(nrow(columbus), mean = 30, sd = 20, xi = 1.9) ,0))
head(columbus)
hist(columbus$var1, xlab="Τιμές var1", ylab="Συχνότητα", 
     col="orange", main="Ιστόγραμμα συχνοτήτων 'var1'")

#' Subset 
#' ===
#' 
#' Subset of data. Keep only the first 10 polygons:
n=10
pol = subset(columbus, columbus$id %in% paste0("A", 1:n) )

#' Calculate the centroid point for each polygon:
coords = coordinates(pol)

#' General visualization along with their centroids:
plot( pol, bg="lightblue", col="white", main="Πολύγωνα και κεντροιειδή σημεία" )
points(coords ,  col="red", pch=19, lwd=0.5)
text(coordinates(pol), labels=pol$id, col="blue", pos=3, cex=1 )
box()


#' A custom function for visualization of a thematic map with just 1 variable:
myThematicMap = function(invar, intitle){
  result = tm_shape(pol) +
    tm_fill(invar, palette="viridis", title=invar, legend.format=list(text.separator="-"), 
            style="quantile", n=4 ) +
    tm_borders(col="black",lwd=2, lty=1)+
    tm_text(invar, size=1, print.tiny=T)+
    tm_layout(title = intitle, 
              title.size =1.2, 
              legend.title.size=1.2, 
              legend.text.size=1.1,
              legend.position=c(0.7,0.6), 
              title.position = c(0.5,0.95)  )
  return(result)
}

#' Map of variable 'var1'
p = myThematicMap(invar="var1",intitle="Map of variable var1")
print(p)




#' Neighbours
#' ===
#' 
#' Calculate the neighbours with 4 different ways.
#' 
#' 1) Common border
nb_queen = poly2nb(pol, queen=TRUE, row.names=pol$id)
#' 2) Distance based
nb_9000 = dnearneigh(coords, d1=0, d2=0.9, row.names = pol$id)
#' 3) K=1 closest neigbour
nb_k1 = knn2nb( knearneigh(coords, k = 1), row.names = pol$id)
#' 4) K=3 closest neighbours
nb_k3 = knn2nb( knearneigh(coords, k = 3), row.names = pol$id)

#' A custom function for visualizing a neighbour structure
plotNeighbours = function(inpol, nei, stiliOnomaton="id", title="Kalimera"){
  plot(inpol, bg="lightblue", col="white", main=title, lwd=0.4)
  plot(nei, coordinates(inpol), add=T,  col="red", pch=19, lwd=0.5)
  text(coordinates(inpol), labels=inpol[[stiliOnomaton]], cex=0.8, col="blue" , pos=4, offset=0.5 )
  box()
}

#' Visualize the neighbours
par(mfrow = c(2,2), oma = c(0.2,0.3,0.3,0.1)  , mar = c(0,0,1,0.2)  )
plotNeighbours(pol, nb_9000, "id", "Απόσταση 9χλμ")
plotNeighbours(pol, nb_queen, "id", "Κοινά Όρια")
plotNeighbours(pol, nb_k1, "id", "K=1")
plotNeighbours(pol, nb_k3, "id", "K=3")

#' Neighbour Matrix
#' ===
#' 
#' Custom function for the conversion of the neighbour to a Matrix format.
#' 
#' 
#' * typos="**B**" --> Binary Matrix with 1 or 0
#' 
#' * typos="**W**" --> Standardised by row with percentages (%)
#' 
getPinaka = function(inGeitonia, typos="B"){
  mat_binary = nb2mat(inGeitonia, style=typos , zero.policy=T)
  colnames(mat_binary)=pol$id
  df_binary = as.data.frame(mat_binary)
  return(df_binary)
}

#' Summary of the neighbour structure based on common border (1):
summary(nb_queen)

#' Convert to matrix:
nb=getPinaka(nb_queen, typos="B")
dim(nb) # dimensions
print(nb)

#' Sum of columns:
colSums(nb)

#' Sum of rows. This is a symmetrical matrix. So 'row sums' is the same as 'column sums'.
#' 
rowSums(nb)

#' Average frequency of neighbouring polygons of the 4 different neighbouring structures:
mean(rowSums(nb))
mean(rowSums(getPinaka(nb_9000, typos="B")))
mean(rowSums(getPinaka(nb_k1, typos="B")))
mean(rowSums(getPinaka(nb_k3, typos="B")))


#' Standardization
#' ===
#' 
#' Neigbour Matrix (row standardised) with percentages (%):
nbw = getPinaka(nb_9000, typos="W")

#' Rounded to 2 decimals:
round(nbw,2)

#' Sum of weights by row. It should be 100% for each row:
rowSums(nbw)

#' Only 1 neighbour in each row, so it should be 100% in each row:
round(getPinaka(nb_k1, typos="W"), 2) 
#' Only 3 neighbouring polygons in each row, so it should be 33.3% in each row:
round(getPinaka(nb_k3, typos="W"), 2) 
round(getPinaka(nb_queen, typos="W"), 2)

#' Visualization of matrix with some colours:

myMatrix=as.matrix(  getPinaka(nb_queen, typos="W")  )
pheatmap::pheatmap(myMatrix, 
                   display_numbers = T, 
                   cluster_rows = F, 
                   cluster_cols = F, 
                   angle_col = 0)


#' Neighbouring values
#' ===
#' 
#' Neighbouring relationships
nb 
pol$var1 # variable value in each polygon
temp1 = nb * pol$var1
temp1

#' Number of neighbouring polygons:
pol$geitones =  apply(temp1, 2, function(x) sum(x>0) )

#' Sum of neighbouring values:
pol$geitonia_sum = round( apply(temp1, 2, sum),2)

#' Average of neighbouring values:
pol$geitonia_mean = round( apply(temp1, 2, mean),2)
pol@data

#' Which polygons have `var1` greater than the Average of the Study Area?
mean(pol$var1)
apantisi = pol$var1 > mean(pol$var1)
pol@data[apantisi,]

#' Moran Scaterplot
#' ===
#' 
#' 
par( mfrow=c(1,1), mar =c(4,4,2,0.1) )
plot(pol$var1, pol$geitonia_mean, pch=20,main="Moran Scaterplot",
     xlab="var1",ylab="Local Average of var1", cex=1.3,  col="blue")
abline(lm(pol$geitonia_mean~pol$var1), col="green4", lwd=1, lt=2 )
abline(v=mean(pol$var1), lt=2, col="red")
abline(h=mean(pol$geitonia_mean), lt=2,col="red")

text(19,mean(pol$geitonia_mean),"mean of Local Average for var1",  pos=3, offset=0.5  )
text(mean(pol$var1),6,"ΜΟ var1",srt=90, pos=2, offset=0.5)
text(pol$var1, pol$geitonia_mean, labels = pol$id, pos=4,offset=0.2)

text(10,20, "LΗ", col="red", cex=2)
text(70,5, "ΗL", col="red", cex=2)
text(10,5, "LL",  col="brown", cex=2)
text(70, 20, "HH",  col="brown", cex=2)
