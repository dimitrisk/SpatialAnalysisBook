
# Data
library(maptools)
library(rgdal)
library(spatstat)

data(lansing)
lansing

table(lansing$marks)
summary(lansing)

# SUbset of data. Select just one type of trees: "blackoak"
bo = unmark(subset(lansing, lansing$marks=="blackoak" ))
marks(bo)="blackoak"
table(bo$marks)
summary(bo)
plot(bo)

# frequency of observations
bo$n

# Split data to a list by type of tree
xorismena = split(lansing)
typeof(xorismena)
xorismena
plot(xorismena)
sapply(xorismena, function(x){ x$n })


# Export data to a CSV file (coordinates of trees)
coo = data.frame(x=bo$x, y=bo$y) # only the first 2 columns
write.csv(coo,"myTreesCoordinates.csv")

# Export data to a Shapefile  (coordinates + attributes of trees)
sh = SpatialPointsDataFrame(coords=coo, data=coo) 
writeOGR(sh, layer="", dsn="myShapefile.shp", driver="ESRI Shapefile")



# Quadrat Analysis with a 4x4 grid
Q = quadratcount(bo, nx = 4, ny = 4)
Q
summary(Q)

Q[1,] # 1st row

# Mean number of observations by cell of the Quadrat
mean(Q)


# Visualize the cells with the number of observations by cell
plot(bo, pch=16,cex=0.5, main="Trees by cell" )
plot(Q, add=T, col="red", cex = 2)

# Histogram of observations by cell
hist(Q, col="cyan2", nclass = 10, main="Histogram by cell" )

# Quadrat count by cell for all data (lansing dataset)
ola = quadratcount(split(lansing), 4, 4 )
ola$blackoak
ola$hickory
plot(ola, main="Frequency of observations")


# Intensity of observations
ent = intensity(Q)
ent

# Area of each cell
Q/ent

# Average frequency of observations (trees) by area
mean(ent)

# Histogram of intensity by cell
hist(ent, col="orange", breaks = 10, main="Histogram of intensity by cel\n( blackoak )" )

# Coloured cells by intensity of events
plot( intensity(Q, image=TRUE), main="Intensity of events")
plot(bo, pch=20,add=TRUE)



# Kernel Density Estimation 
library(spatstat)

K1 = density.ppp(bo, sigma=0.1)
plot(K1, main="KDE sigma=0.1")
plot(bo, add=TRUE)

K2 = density.ppp(bo, sigma=0.2)
plot(K2, main="KDE sigma=0.2")
plot(bo, add=TRUE)

K3 = density.ppp(bo, sigma=0.3)
plot(K3, main="KDE sigma=0.3")
plot(bo, add=TRUE)


# Selection of the 20th tree and visualize its locations under the 3 different KDE
i=20
mypoint = bo[i]
mypoint

mySigmas = c(0.1,0.2,0.3)
myK = list(K1,K2,K3)

#+ out.width=c('100%')
par(mfrow=c(1,3), mar=c(0,0,0,1.5) )
for(k in 1:3){
  plot( myK[[k]], main="" )
  points(mypoint, bg="green", col="black", pch=21, cex=1.5)
  title(sprintf("KDE\nsigma=%s",mySigmas[k]) ,line=-3  )
}
par(mfrow=c(1,1), mar=c(2,2,2,2))


#' Export the value for this point 
#' under the 3 different KDE:
K1[mypoint]
K2[mypoint]
K3[mypoint]

#' Export values of our point from the KDE list
sapply(myK, function(x){x[mypoint]})


#' Neighbour distances
#' ===
#' 
#' Distance from each point to each nearest tree: 
nearest = nndist(bo, k=1)
summary(nearest)
plot(sort(nearest))

#' Mean distance to nearest neighbour:
mean(nearest)

#' distance to 2nd and 3rd nearest neighbour: 
summary(nndist(bo, k=2))
summary(nndist(bo, k=3))
