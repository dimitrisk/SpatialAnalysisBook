# data
library(rgdal)
library(plotrix)
poleis = rgdal::readOGR("Data/poleis.shp", verbose = F, stringsAsFactors =F)
poleis

# Visualize data
plot(poleis, cex=1, col="red",  pch=20, main="Towns")
text(poleis, labels = poleis$onoma, pos = 2)
box()

# Visualize with variable point size
plot(poleis, cex=as.numeric(as.vector(poleis$pop))/100, col="orange",  pch=19, main="Towns by size")
text(poleis, labels = poleis$onoma, pos = 4 )
box()


# Empty data.frame with just the points coordinates
co = data.frame(coordinates(poleis))
names(co) = c("x","y")
print(co)

# Coordinates of Spatial Mean Center
center.mean = c( mean(co$x), mean(co$y) )

# Calculations for the weighted Spatial Center
co$pop = as.numeric(as.vector(poleis$pop))
co$Wx = co$pop * co$x
co$Wy = co$pop * co$y
print(co)

center.weighted = c(sum(co$Wx)/sum(co$pop), sum(co$Wy)/sum(co$pop))


# Spatial Median center 
center.median = c(median(co$x), median(co$y))

# Trimmed Spatial Center (remove 20% from each side)
center.trim_mean <- c( mean(co$x, trim = 0.2), mean(co$y, trim = 0.2) )

# Bring all results to a single data.frame
kentrika = data.frame(
  typos = c("Weighted", "Median", "Trimmed Mean", "Mean"),
  x = c( center.weighted[[2]], center.median[[2]], center.trim_mean[[2]], center.mean[[2]]), 
  y = c( center.weighted[[1]], center.median[[1]], center.trim_mean[[1]], center.mean[[1]]))
print(kentrika)



# Standard Distance
n=nrow(co)
standard_distance = sqrt( sum(((co$x-center.mean[1])^2+(co$y-center.mean[2])^2))/(( n))   )



# Visualize all cities with the 4 different centers and a circle with Standard Distance radius.
plot(poleis, cex=(co$pop)/100, col="orange",  pch=19, main="Towns")
text(poleis, labels = poleis$onoma )

points(kentrika$y, kentrika$x, pch=10, col=c("blue","green","orange","brown"), cex=2 )
text(kentrika$y, kentrika$x, labels =kentrika$typos, pos =4)

draw.circle(center.mean[1],center.mean[2],radius=standard_distance/2,border="red",lwd=2)



# Distance Calculations
# between 9 cities with 2 different methods

# Distance Matrix: method 1
library(raster)
dm = as.matrix(dist( cbind(co$x,co$y)  ))
colnames(dm) = poleis$onoma
rownames(dm) = poleis$onoma
dm

# Distance Matrix: method 2
library(fields)
dm2 = pointDistance(poleis, lonlat = FALSE)
colnames(dm2)=poleis$onoma
rownames(dm2)=poleis$onoma
dm2


# Remove the Diagonals of the Distance Matrix (self-distances):
library(sna)
dm = diag.remove(dm)
dm
dm2 = diag.remove(dm2)
dm2

# subset from the Distance Matrix
dm[,1] # 1st column
dm[5,] # 5th line


# Identify the closest couple of cities
elaxisti_Index = which(dm == min(dm, na.rm=TRUE), arr.ind = TRUE)
elaxisti_Index

# Distance of this closest couple of cities
dm[elaxisti_Index]


# Select the distance between two specific cities
ena = subset(poleis, poleis$onoma=="A")
duo = subset(poleis, poleis$onoma=="D")


# Distance between a city and the Spatial Mean point
ena = subset(poleis, poleis$onoma=="C")
pointDistance(ena, c(center.median[1],center.median[2]), lonlat = TRUE)


# Distance between all 9 cities and the Spatial Median point
apo = pointDistance(poleis, c(center.median[1],center.median[2]), lonlat = TRUE)
apo

# Identify the max/min value in this vector
which.max(apo)
which.min(apo)

# Get the min value from the vector.
# Not the position in the vector, but the value itself.
apo[which.min(apo)]

# Distances between: 9 cities and the 4 centers
co
co$Dmean = pointDistance( cbind(co$x,co$y), center.mean, lonlat = T )
co$Dmedian = pointDistance( cbind(co$x,co$y), center.median, lonlat = T )
co$Dtrimedmean = pointDistance( cbind(co$x,co$y), center.trim_mean, lonlat = T )
co$DweightedMean = pointDistance( cbind(co$x,co$y), center.weighted, lonlat = T )


# Which center is generaly closest to all 9 cities?
sum(co$Dmean)
sum(co$Dmedian)
sum(co$Dtrimedmean)
sum(co$DweightedMean)

# Column Sums of the Distance Matrix
co[c("Dmean","Dmedian", "Dtrimedmean", "DweightedMean")]
colSums(co[c("Dmean","Dmedian", "Dtrimedmean", "DweightedMean")])
