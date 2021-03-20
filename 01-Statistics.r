# vector 
x = c(8,13,6)
z = 1:10
y = seq(10, 100, 10)

# data.frame
df = data.frame(z,y)

# randomness seed
set.seed(1821)

# random numbers
sample(1:10, 2)

# embedded datasets
library(datasets)

# Load data
data(iris)
head(iris)
summary(iris)
head(iris[,1:4]) # just 4 first columns

# Measures of central tendency

mean(iris$Sepal.Length)
mean(iris$Sepal.Length, na.rm=T)  # remove the NA values
median(iris$Sepal.Length)
max(iris$Sepal.Length)
min(iris$Sepal.Length)
sd(iris$Sepal.Length)

# Random selection of `n` lines
set.seed(1821)
n=95
indexes = sample(nrow(iris), n)
data = iris[indexes,]  

# Column-wise computations
colMeans(data[,1:4])
colSums(data[,1:4])
sapply(data[,1:4], median)

# Quantiles
sapply(data[,1:4], quantile)
sapply(data[,1:4], quantile)[,4] # 4th column (Petal.Width)
sapply(data[,1:4], quantile)[3,] # 3rd row (50%)

#' Range of 4 variables:
sapply(data[,1:4], min) # Ελάχιστο
sapply(data[,1:4], max) # Μέγιστο
sapply(data[,1:4], range) # Ελάχιστο και Μέγιστο

myTemp = sapply(data[,1:4], range) # range
myTemp
myTemp[1,] # minimum (1st line)
abs(myTemp[1,] - myTemp[2,]) # absolute difference (min - max)

#' Variance of the values
sapply(data[,1:4], var)
var(data$Sepal.Length)


# Cumulative Frequency
#
# Cumulative Frequency of a single variable
cumsum(data$Petal.Width)

# 6th value of Cumulative Frequency of `Petal.Width`
cumsum(data$Petal.Width)[1:6]

# Empirical Cumulative Distribution Function
mySynartisi = ecdf(data$Sepal.Width)
plot(mySynartisi, main="Sepal Width", col="orange")

# Percentages for `Sepal.Width` up to `2.5`
apotelesma = mySynartisi(2.5)
apotelesma  # decimal [0,1]
apotelesma * 100 # [0,100]


# Group Statistics
library(dplyr)

# Filtering
virginica = filter(iris, Species == "virginica")
head(virginica)
mean(virginica$Sepal.Length)

sepalLength6 = filter(iris, Species == "virginica", Sepal.Length > 6)
head(sepalLength6)
tail(sepalLength6)

# Mean by group
apotelesma = data %>% group_by(Species) %>% summarise(MO = mean(Sepal.Length) )
apotelesma

# Mean by group with a new column `Plithos` (number of observations):
data %>% group_by(Species) %>% summarise(MO = mean(Sepal.Length), Plithos = n() )

#' Observations with greater value in `Sepal.Length` column
data %>% filter(Sepal.Length == max(Sepal.Length))

#' Observations with greater value in `Sepal.Length` by  (`Species`):
data %>% group_by(Species) %>% filter(Sepal.Length == max(Sepal.Length))
