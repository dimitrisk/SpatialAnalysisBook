
# load libraries
library(rgdal)
library(maptools)
library(sp)
library(raster)
library(spatstat)
library(plotrix)


# load Shapefile with 9 cities
poleis = rgdal::readOGR("Data/poleis.shp", verbose = F, stringsAsFactors =F)
poleis
poleis@data # data.frame of the shapefile

# COnvert Strings to Numbers
poleis$income
poleis$pop
poleis$income = as.numeric(poleis$income)
poleis$pop = as.numeric(poleis$pop)

# Mean income of cities
x = mean(poleis$income)
x

# Change the CRS from WGS84 to the Greek System EGSA
crs(poleis)
poleis2 = spTransform(poleis, CRS("+init=epsg:2100") )
crs(poleis2)

# Visualize the new dataset
plot(poleis2)

# Visualiza cities with name label
plot(poleis2)
text(poleis2, label=poleis2$onoma, pos=2, col="red")

#' Visualiza cities with name label in diferent position 
#' Bottom (1), Left (2), Top (3), Right (4)
plot(poleis2)
text(poleis2, label=poleis2$onoma, pos=1, col="blue")

# Subset of data
poleis2@data
epilogi = subset(poleis2, income < 1000)
epilogi2 = subset(epilogi, pop > 500)
epilogi2@data

# Logical 'AND'
syndiasmos = subset(poleis2, income <= 1000 & pop > 500)
syndiasmos@data

# Logical 'OR'
syndiasmos2 = subset(poleis2, income <= 1000 | pop > 500)
syndiasmos2@data

# Spatial Point Pattern
# 
# Convert "towns data" to a "Spatial Point Pattern"
poleisPPP = as.ppp(poleis2)
poleisPPP
summary(poleisPPP)
plot(poleisPPP)

# Attributes of the Spatial Point Pattern
as.data.frame(poleisPPP)
marks(poleisPPP)
poleisPPP$marks$income

# subset from Spatial Point Pattern
epilogi = subset(poleisPPP, income<1000)
epilogi
epilogi$marks
plot( unmark(epilogi) )
plot( epilogi, main="Titlos Eikonas", use.marks=F )
