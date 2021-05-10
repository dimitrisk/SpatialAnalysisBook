

library(sp)
data(meuse)
data(meuse.grid)
 

library(gstat)
library(tripack) # voronoi.mosaic
library(RColorBrewer)
 

# points
coordinates(meuse) = ~x+y
proj4string(meuse) = CRS("+init=epsg:28992")
head(meuse)

# grid 
data(meuse.grid)
coordinates(meuse.grid) = ~x + y
gridded(meuse.grid) = TRUE 
proj4string(meuse.grid) = CRS("+init=epsg:28992")


# visualization
plot(meuse.grid, col="grey")
plot(geometry(meuse.grid), add=T , cex = 1.5, col = "brown"  )
plot(meuse, add=T,  pch=20)

# points subset (10 points)
test = meuse[1:10,] 

plot(meuse.grid, col="grey")
plot(geometry(meuse.grid), add=T , cex = 1.5, col = "brown"  )
plot(test, add=T,  pch=20)

# Randomness seed
set.seed(2020)

# Select 13 random points
indices = sample( 1:nrow(meuse), size=13, replace=F)
test2 = meuse[ indices ,]

plot(meuse.grid, col="grey")
plot(geometry(meuse.grid), add=T , cex = 1.5, col = "brown"  )
plot(test2, add=T,  pch=20)

# apply a function by category of trees
table(meuse$soil)
aggregate(meuse$cadmium , by=list(SOILGROUP = meuse$soil), FUN=sum)
aggregate(meuse$cadmium , by=list(SOILGROUP = meuse$soil), FUN=function(x){ sd(x)}  )

# Grid variables:
plot(meuse.grid["part.a"])
plot(meuse.grid["part.b"])
plot(meuse.grid["dist"])
plot(meuse.grid["soil"])
plot(meuse.grid["ffreq"])

#Points variables:
head(meuse)
summary(meuse@data)
hist(meuse$zinc)

# log(zinc)
spplot(meuse, "zinc", do.log = TRUE,
       key.space=list(x = 0.1, y = 0.95, corner = c(0, 1)),
       scales=list(draw = TRUE))

spplot(meuse, "zinc", do.log = TRUE,
       key.space=list(x=0.2,y=0.9,corner=c(0,1)),
       scales=list(draw = TRUE), cuts = 3,
       legendEntries = c("low", "intermediate", "high"))

# ffreq
levels(meuse.grid$ffreq) = c("frequent", "moderately frequent", "infrequent")
plot(meuse.grid["ffreq"], scale.size = lcm(5.8), scale.frac = 0.15)


# Thiessen (Voronoi) 
library(deldir)
cc = coordinates(meuse)
vtess = deldir(cc[, 1], cc[, 2])

plot(cc, type="n", asp=1)
points(cc, pch=20, col="red", cex=0.5)
plot(vtess, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)


# Delaunay Triangulation
plot(cc, type="n", asp=1)
plot(vtess, wlines="triang", wpoints="none",add=TRUE, lty=1)
points(cc, pch=20, col="blue", cex=0.5)







# I.D.W.
# Inverse Distance Weighting
PbIDW2 = idw(formula=lead~1,locations=meuse,newdata=meuse.grid,idp=2)
head(PbIDW2)

# Average of the column of the new values (prediction value) (*var1.pred*)
mean(PbIDW2$var1.pred)

PbIDW3 = idw(formula=lead~1,locations=meuse,newdata=meuse.grid,idp=3)
head(PbIDW3@data)

PbIDW4 = idw(formula=lead~1,locations=meuse,newdata=meuse.grid,idp=4)
head(PbIDW4@data)

PbIDW5 = idw(formula=lead~1,locations=meuse,newdata=meuse.grid,idp=5)
head(PbIDW5@data)


spplot(PbIDW2,"var1.pred") # (Power = 2)
spplot(PbIDW3,"var1.pred") # (Power = 3)
spplot(PbIDW4,"var1.pred") # (Power = 4)
spplot(PbIDW5,"var1.pred") # (Power = 5)

library(raster)
s = stack( raster(PbIDW2), raster(PbIDW3), raster(PbIDW4), raster(PbIDW5) )
names(s) = c('IDW2', 'IDW3', 'IDW4','IDW5' )
spplot(s)






# Evaluation of IDW models


# RMSE (Root Mean Square Error).
myRMSE = function(observed, predicted) {
  apotelesma = sqrt(mean((predicted - observed)^2, na.rm=TRUE))
  return(apotelesma)
}

# Evalation results
p = extract(s, meuse )
myerrors = data.frame( Observed = meuse$zinc,   p)

myRMSE(myerrors$Observed, myerrors$IDW2)
myRMSE(myerrors$Observed, myerrors$IDW3)
myRMSE(myerrors$Observed, myerrors$IDW4)
myRMSE(myerrors$Observed, myerrors$IDW5)

# clear RStudio plots so far.
# dev.off(dev.list()["RStudioGD"])
# dev.off()

plot(myerrors$IDW5, myerrors$Observed, 
     main="Σύγκριση",
     xlab="predicted (IDW5)", ylab="actual",  cex=0.8)
abline(a=0,b=1, col="red")

# Range of values
myrange = range(myerrors)

plot(myerrors$IDW5, myerrors$Observed, 
     xlim=c(myrange),ylim=c(myrange), main="Σύγκριση (ίδιο εύρος)",
     xlab="predicted (IDW5)", ylab="actual",  cex=0.8)
abline(a=0,b=1, col="red")

# Simulation
# 10 random points for evaluation
n=10
myselection = sample(1:length(meuse), n)
allOthers = (1:length(meuse))[- myselection]    
test = meuse[myselection, ]
train = meuse[allOthers, ]

plot( meuse.grid , col="grey", main="Random points for rvaluation")
points(test, col="blue")
points(train, col="red")

model = idw(formula=zinc~1, locations=train, newdata=meuse.grid, idp=5)
p = extract(  raster(model), test )
p

myerrors2 = data.frame( expected = test$zinc, predicted = p)
myerrors2$diff =  myerrors2$predicted - myerrors2$expected 
myerrors2

mean(myerrors2$diff)
myRMSE(myerrors2$expected, myerrors2$predicted)

# Multiple Simulations
# 4 repetitions of 10 random points of evaluation
set.seed(2020)
repetitions = 4
rmse = rep(NA, repetitions)
myerrors3 = list()
listOfModels = list()

n=10

for (k in 1:repetitions) {
  myselection = sample(1:length(meuse), n) # select random points
  allOthers = (1:length(meuse))[- myselection]    # the rest of the points
  test = meuse[myselection, ]
  train = meuse[allOthers, ]
  
  model = idw(formula=zinc~1,locations=train,newdata=meuse.grid,idp=5)
  p = extract(  raster(model), test )
  
  listOfModels[[k]] = model
  myerrors3[[k]] = data.frame( expected = test$zinc, predicted = p, diff= (test$zinc-p) )
  rmse[k] = myRMSE(test$zinc, p )
}

length(listOfModels)

# visualize the 1st model
plot( listOfModels[[1]] )

# Deviations of the 1st model.
myerrors3[[1]]
hist(myerrors3[[1]]$diff)
mean(myerrors3[[1]]$diff) #  Bias

plot(myerrors3[[1]]$expected, myerrors3[[1]]$predicted)
abline(a=0,b=1, col="red")

#' values of the RMSE
rmse
