# 09) Spatial Association

# Libraries
library(sp)
library(spdep)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(raster)
library(elsa)

# My custom function 
myMap = function(inPol, invar="A",legendTitle="Υπόμνημα", inTitle="Τίτλος"){
  newVarName = paste0("Rounded_",invar)
  inPol[[newVarName]] = round(inPol[[invar]],2)
  result=tm_shape(inPol) + 
    tm_fill(newVarName, palette="RdBu", style="pretty", midpoint=NA,
            legend.format=list(text.separator=""), title=legendTitle) +
    tm_text(newVarName, size=1.7, print.tiny=T)+
    tm_layout(main.title=inTitle, title.size =1.8, legend.title.size=1.5, 
              legend.text.size=1.2, legend.position=c(1,0.6),
              outer.margins=c(0,0,0,0.15),main.title.position="center") +
    tm_borders(alpha=0.9, col="black",lwd=2, lty=1)
  print(result)
}

# Create some data
n=5
A = matrix(0, nrow=n, ncol =n, byrow=FALSE)
A[c(1,7:8, 21:22)]=1
A[c(13:14,10)]=2
A[c(5,11)]=1
A[c(16,19, 25)]=3
#A[c(16,19,25)]=3
rA = raster(A)

B = matrix(0, nrow=n, ncol =n, byrow=FALSE)
B[c(1:10)]=2
B[c(13:15, 18:20)]=1
B[c(24:25)]=4
B[c(24:25)]=3
rB = raster(B)

r= raster::brick(rA,rB)
names(r)=c("A","B")
proj4string(r) = CRS("+init=epsg:4326")

pA=rasterToPolygons(r, dissolve=FALSE)

library(stringr) 
pA$id = paste0("P", str_pad(1:length(pA), 2, pad = "0") )
writeOGR(pA, "pA.geojson", layer="pA", driver="GeoJSON", overwrite=T)
#readOGR("pa.geojson")
#writeOGR(pA, "pA","pA",driver="ESRI Shapefile")


# Visualization of initial data
myMap(pA, "A", inTitle = "Values of variable 'A'")
myMap(pA, "B", inTitle = "Values of variable 'B'")


centers = coordinates(pA)

# Coordinates
library(ggplot2)
ggplot(pA, aes(x = long, y = lat, group = id)) + 
  geom_polygon(colour='cyan4', fill='white')  +  coord_equal()


# Define the neighbour
neighbours  = poly2nb(pA, queen = T)
neighbours

par( mfrow=c(1,1), mar =c(0,0,1,0) )
plot(pA, col='white', border='black', main="Γειτονία με βάση τα κοινά όρια", lwd=2)
plot(neighbours, centers, col='red', lwd=0.7, lty=5, add=TRUE, pch=21)
par( mfrow=c(1,1), mar =c(4,4,4,4) )




# Convert the neighbour structure to 
# weights structure
listw = nb2listw(neighbours) 
listw



# Global test of Morans' Ι
I1 = moran.test(pA$A,listw)
I2 = moran.test(pA$B,listw)
I1
I2

# Value of the test of Moran’s I
I1[["estimate"]][["Moran I statistic"]]

# ‘p’ value for the evaluation
# of statistical importance of the model.
I1[["p.value"]]

# As ‘p’ value has been calculated analyticaly and not by a Monte Carlo Simulation
# For better examination of the statistical iportance of the model, we conduct a Monte Carlo Simulation.
sim1 = moran.mc(pA$A, listw, nsim=599)
sim1


# Visualize the distribution of values
# of the Monte Carlo Simulation of Moran's I
plot(sim1, main="Κατανομή τιμών της Monte Carlo προσομοίωσης του Moran's I", 
     xlab="Values of variable 'Α'",
     ylab="Πυκνότητα",
     lty=2, col="red", lwd=2)

# Global test of Geary’s C
C1 = geary.test(pA$A,listw)
C2 = geary.test(pA$B,listw)

C1
C2

sim2 = geary.mc(pA$A, listw, nsim=599)
sim2

# Visualize the distribution of values of the Monte Carlo simulations of Geary's C
plot(sim2, main="Κατανομή τιμών της Monte Carlo προσομοίωσης του Geary's C", 
     xlab="Values of variable 'Α'",
     ylab="Πυκνότητα", lty=2, col="blue", lwd=2)



# Moran's scaterplot
moran.plot(pA$A, listw = nb2listw(neighbours, style = "W"), 
           main="Moran scaterplot",
           xlab="Values of variable 'Α'", pch=19,
           ylab="Values of variable 'Α' at spatial lag",
           col="red")

# Local Moran
# Positive values of Ii indicate that a polygon
# has other polygons with similar values in its neighbour.

# *  Ii: local moran statistic
# * E.Ii: Αναμενόμενη τιμή του: local moran statistic
# * Var.Ii: μεταβλητότητα της τιμής του: local moran statistic
# * Z.Ii: Τυπικές αποκλίσεις των τιμών του Ι (z values)
# * Pr(): p-value 

# Οπτικοποίηση τοπικού δείκτη Moran
local = localmoran(x = pA$A, listw = nb2listw(neighbours, style = "W"))
head(local)

# Attach results back to polygons
moran.map = cbind(pA, local)
head(moran.map)

myMap(moran.map, "Ii",inTitle = "Local Moran (values of Ι)")
myMap(moran.map, "Pr.z....E.Ii..",inTitle = "Local Moran (p values)")


# Map of local moran (categories)

# Create an empty vector
quadrant = rep(0,nrow(local))

# Center of the variable, around its Mean
m.VariableA = pA$A - mean(pA$A)     

# Center of local Moran, around the Mean
m.local = local[,1] - mean(local[,1])    

# Level of Significance
signif = 0.5

# Καταμερισμός πολυγώνων σε κατηγορίες
quadrant[m.VariableA >0 & m.local>0] = 4  # HH
quadrant[m.VariableA <0 & m.local<0] = 1  # LL     
quadrant[m.VariableA <0 & m.local>0] = 2  # LH
quadrant[m.VariableA >0 & m.local<0] = 3  # HL
quadrant[local[,5]>signif] = 0   

# Visualization of categories map
brks = c(0:4)
colors = c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")

par(mar=c(0,0,1,7),    xpd=TRUE  )
plot(pA ,col=colors[findInterval(quadrant,brks,all.inside=FALSE)], 
     main="Χάρτης κατηγοριών Moran")
legend("right", legend = c("Ασύμαντο","LL","LH","HL","HH"),
       inset=c(-0.18), cex=1.5,  fill=colors,bty="n")
text(centers, label=paste0(pA$id,"\n",pA$A), cex=1.5 )


# Loacal

# Loacal Moran’s I:
library(elsa)
lisa.i = lisa(pA, d1=0,d2=0.2,statistic='I', zcol="A")
summary(lisa.i$Ii)
myMap(lisa.i,"Ii", inTitle = "Τοπικός Δεικτης Moran")

# Loacal Geary’s C
lisa.c = lisa(pA, d1=0, d2=0.2, statistic='c', longlat=F, zcol="A")
summary(lisa.c$LocalGeray)
myMap(lisa.c,"LocalGeray", inTitle = "Τοπικός Δεικτης Geary")


# Global

# Global Moran's I
elsa::moran(r$A)
elsa::moran(r$B) 

# Global Geary’s C
elsa::geary(r$A)
elsa::geary(r$B)


# Getis-Ord
nb = dnearneigh(centers, 0, 0.3)
nb

# Binary neighbouring matrix
nb_lw = nb2listw(nb, style = 'B')
nb_lw

plot(pA)
plot(nb, centers, add=T, col="red")

local_g = localG(pA$A, nb_lw)
local_g = cbind(pA, as.matrix(local_g)) # pA or pA@data
names(local_g)[4] = "gstat"
head(local_g)

# Χάρτης Getis-Ord "G"
myMap(local_g,"gstat", inTitle = "Map of Getis Org G")