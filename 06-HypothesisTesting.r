# Libraries
library(maptools)
library(rgdal)
library(spatstat)

data(lansing) # Data
lansing

# plot all together
myCols=c("red","green","blue","orange", "purple","brown")
myNames=c("blackoak","hickory","maple","misc","redoak","whiteoak")
plot(lansing, cols=myCols )

# plot independently 
trees = split.ppp(lansing, un=T, drop=T)
par(mfrow=c(2,3), mar=c(2,2,1,0))
for(i in 1:length(trees)){
  plot(trees[[i]], main=myNames[i], cols=myCols[i], pch=i )
}
par(mfrow=c(1,1), mar=c(3,3,2,0))


# analyse by tree type
by(lansing, INDICES=marks(lansing), FUN=function(x) { mean(nndist(x)) } )
by(lansing, INDICES=marks(lansing), FUN=function(x) {  x$n } )

# select tree type: *misc*
subset(lansing, marks =="misc", drop = T ) 

# visualize type type misc without dropping other tree types:
plot( subset(lansing, marks=="misc") )

# visualize just tree type misc and drop other tree types:
plot( subset(lansing, marks=="misc", drop=T)  )

# visualize points of types:  `misc` and `blackoak`:
plot( subset(lansing, marks %in% c("misc","blackoak"), drop=T ) )

# Nearest neighbour
 
# Nearest neighbour by tree-type
dm1 = nndist(lansing, by=marks(lansing))
head(dm1)

# Distance of 5th observation (tree) to nearest neigbour, by tree-type.
dm1[5,]

# For each observation (tree), what is the distance to a tree of type `blackoak`?
head(dm1[,1])
hist(dm1[,1], main="Απόσταση προς blackoak")

# The minimum value of Nearest Neigbour Matrix between tree-types.
dm2 = minnndist(lansing, by=marks(lansing))
rownames(dm2) = names(dm2)
dm2

min(dm1[,1])
min(dm1[,3])
min(dm1[,3])

# Average value of functions

# Export all values from the G function
data1 = subset(lansing, marks =="misc", drop = T )
myg = Gest(data1)
plot(myg)

head(myg$r) # distances (horizontal axis)
head(myg$km) # values of G(r) (vertical axis)
mean(myg$km) # average value of G(r)

mean( Kest(data1)$iso ) # average value of K(r)
mean( Lest(data1)$iso ) # average value of L(r)
mean( Fest(data1)$km ) # average value of F(r)
mean( Jest(data1)$km ) # average value of J(r)


# Simulation

# Subset of data (only `maple` trees)
data = subset(lansing, marks =="maple", drop = T ) 
set.seed(1821)

# Simulation of 40 random G-functions (nearest neiboour) :
ENV_G = envelope(data, fun = Gest, nsim = 40)
plot(ENV_G)
head(ENV_G$r) # Distance (horisontal axis)
head(ENV_G$obs) #   (vertical axis)

# values of maple (observed):
head(ENV_G$obs)

# simulated values:
head(ENV_G$theo) # Theoretical
head(ENV_G$lo) # min
head(ENV_G$hi) # max

# average G(r) of simulation ranges:
mean(ENV_G$hi)
mean(ENV_G$lo)

# average G(r) values of observed maple values
mean(ENV_G$obs)

# Simulation of 45 random Κ functions (buffer) :
ENV_K = envelope(data, fun = Kest, nsim = 45)
plot(ENV_K)

# Simulation of 50 random F functions (empty space) :
ENV_F = envelope(data, fun = Fest, nsim = 50)
plot(ENV_F)
  
#' Simulation of 55  random L functions (transformation of Κ) :
ENV_L = envelope(data, fun = Lest, nsim = 55)
plot(ENV_L)

#' Simulation of 60 random  J functions (conversion of G and F) :
ENV_J = envelope(data, fun = Jest, nsim = 20)
plot(ENV_J, ylim=c(0,2))


# Multiple types
 
# G function between two different tree types
gc = Gcross(lansing, i = "blackoak", j = "maple") # blackoak, hickory, maple, misc, redoak, whiteoak 
plot(gc)

# Average value of G(r) values between two different tree types: from blackoak towards maple
mean(gc$km)

# Calculation of multiple type for functions: K, L, and J. 
# If we don't define `i` and `j`, then the two first tree-types are selected.
plot(Kcross(lansing))
plot(Lcross(lansing))
plot(Jcross(lansing))

# Simulation of 70  random G functions between 2 different tree types:
# From  blackoak towards maple.
egc = envelope(lansing, Gcross, nsim = 70, i = "blackoak", j = "maple")
plot(egc)

# Statistics for pairs of multiple types
ola = alltypes(lansing, "Gcross",  nsim = 25)
ola
plot(ola)
ola[1]$which
plot(ola[1]$fns[[3]]) # blackoak -> maple

