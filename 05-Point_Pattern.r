# Data
library(maptools)
library(rgdal)
library(spatstat)

data(lansing)
lansing

# plot all together
myCols=c("red","green","blue","orange")
myNames=c("blackoak","hickory","misc","whiteoak") # blackoak, hickory, maple, misc, redoak, whiteoak

mydata=subset(lansing, lansing$marks %in% myNames, drop=T) 
plot(mydata, cols=myCols )

# plot separately
trees = split.ppp(mydata, un=T, drop=T) # split the ppp data into a list of ppp based on marks.

par(mfrow=c(2,2), mar=c(0,0,1,0))
for(i in 1:length(trees)){
  plot(trees[[i]], main=myNames[i], cols=myCols[i] )
}
par(mfrow=c(1,1), mar=c(2,2,2,2))


# Subseting data
table(lansing$marks)
summary(lansing)

# Select just the **redoak** type of tree:
ro = unmark(subset(lansing, lansing$marks=="redoak"))
marks(ro)="redoak"

summary(ro)

plot(ro)

# Create a  data.frame with just coordinates of trees:
coo = data.frame(x=ro$x, y=ro$y)

    
    
    
    
# Distance matrix
# for a single type of trees (redoak:  346 * 346 ):
library(raster)
dm = as.matrix(dist( cbind(ro$x,ro$y)  ))

dim(dm)

# Remove the diagonal from the distance matrix. Self-distances:
library(sna)
dm = diag.remove(dm)

# Select specific distances from distance matrix:
head(dm[,1]) # 1st column
head(dm[5,]) # 5th row

# Row means and Column means. 
# There should be the same here. This is a symmetrical matrix: 
head(rowMeans(dm, na.rm = T))
head(colMeans(dm, na.rm = T))

#' For each tree average-distance towards all other trees: 
coo$MeanDistance = rowMeans(dm, na.rm = T)
head(coo$MeanDistance)

#' Colour gradient method.
library(RColorBrewer)
color.gradient = function(x, colors=c("red","yellow","green"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
# Visualization
plot(coo[,1:2], col=color.gradient(coo$MeanDistance), pch=19, main="Mean Distance to all other trees" )


# Distance matrix between two point-sets (346 redoak χ  703 hickory):
duo=subset(lansing, marks=="hickory") # second group
duo$n # number of observations

library(raster)
dm2 = pointDistance(cbind(ro$x,ro$y), cbind(duo$x, duo$y), lonlat = FALSE)
dim(dm2)

length( rowMeans(dm2) ) # number of observations
length( colMeans(dm2) ) # number of observations

head( rowMeans(dm2) )
head( colMeans(dm2) )
    

    

# Distance to neighbour 
# For each point, what is the distance to its nearest event? 
coo$nearest = nndist(ro, k=1)
head(coo)
summary(coo$nearest)
mean(coo$nearest)

plot(coo$x, coo$y, col=color.gradient(coo$nearest), pch=19, main="Mean Distance to closest trees" )
 
    
    
    
# Histogram of distances to nearest event 
onomata =  as.character(unique(lansing$marks))

for (i in 1:length(onomata)){
  d = subset(lansing, lansing$marks==onomata[i])
  mydist = nndist(d, k=1)
  mesiapostasi = round(mean(mydist, na.rm=T),3)
  hist( mydist, xlim=c(0,0.25), ylim=c(0,300), breaks=seq(0, 0.25, by=0.01),xlab="Απόσταση",ylab="Συχνότητα", 
        main=sprintf("%s\nΜέση απόσταση: %s",onomata[i],mesiapostasi), col=rainbow(8)[i] )
}

# Average distance to nearest by tree type.
# using a single line of code   
by(lansing, INDICES=marks(lansing), FUN=function(x) { mean(nndist(x)) } )
    

# Distance functions

# ecdf
head(coo)
summary(coo$nearest)
plot(ecdf(coo$nearest), cex=0.5, pch=0, col="cyan3",lwd = 1,
     main="ECDF\ndistance to nearest neighbour")


# G function
G = Gest(ro)
plot(G , lwd = 3)

# G function (percentiles)
plot(G, . ~ theo, main="G function (percentiles)", 
     mgp=c(1.5,0.4,0), legendargs=list(bty="n", cex=0.8,x.intersp=0.4, y.intersp=0.8 ))



# K function
K =Kest(ro)
plot(K, main="K function", mgp=c(1.5,0.4,0),
     legendargs=list(bty="n", cex=0.8,x.intersp=0.4, y.intersp=0.8 ) )

# K function (percentiles)
par(mar=c(2.5,3,2,0.2))
plot(K, . ~ theo, main="K function (percentiles)",
     mgp=c(1.5,0.4,0),  legendargs=list(bty="n", cex=0.8,x.intersp=0.4, y.intersp=0.8 ))



# F function
Fe = Fest(ro)
plot(Fe, main="F function", mgp=c(1.5,0.4,0),   
     legendargs=list(bty="n", cex=0.8,x.intersp=0.4, y.intersp=0.8 ) )

# F function (percentiles)
plot(Fe, . ~ theo, main="F function (percentiles)", 
       mgp=c(1.5,0.4,0),legendargs=list(bty="n", cex=0.8,x.intersp=0.4, y.intersp=0.8 ))




# L function
myL = Lest(ro)
plot(myL, main="L function", mgp=c(1.5,0.4,0),  
     legendargs=list(bty="n", cex=0.8,x.intersp=0.4, y.intersp=0.8 ) )

# L function (percentiles)
plot(myL, . ~ theo, main="L function (percentiles)",  
     mgp=c(1.5,0.4,0),  legendargs=list(bty="n", cex=0.8,x.intersp=0.4, y.intersp=0.8 ))


# J function
myJ = Jest(ro)
plot(myJ, main="J function",mgp=c(1.5,0.4,0), 
     legendargs=list(bty="n", cex=0.8,x.intersp=0.4, y.intersp=0.8 ) )



# Functions average
# get values from G function:
data1 = subset(lansing, marks =="misc", drop = T )
myg = Gest(data1)
plot(myg)

head(myg$r) # distances (horizontal axes)
head(myg$km) # G(r) values (vertical axes)

myg$r[1:4] # the first 4 distances
myg$r[4] # the 4th distance

myg$km[1:5] # the first 5 G(r) values
myg$km[45] # the 45th G(r) value

mean(myg$km) # average value of G(r)
mean( Kest(data1)$iso ) # average value of K(r)
mean( Lest(data1)$iso ) # average value of L(r)
mean( Fest(data1)$km ) # average value of F(r)
mean( Jest(data1)$km ) # average value of J(r)
