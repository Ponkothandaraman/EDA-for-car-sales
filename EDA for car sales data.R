library(MASS)
library(dplyr)
library(plyr)
library(lattice)
library(ggplot2)
library(tidyverse)

df1 <- read.csv('Car_sales.csv')
df1
#Descriptive statistics
summary(df1)
str(df1)

#Checking null values
colSums(is.na(df1))

#removing null values
df1 <- na.omit(df1)
colSums(is.na(df1))
summary(df1)

#Changing the specific variables to categorical
df1$Manufacturer=as.factor(df1$Manufacturer)
df1$Vehicle_type=as.factor(df1$Vehicle_type)
str(df1)

#Histogram analysis
histogram(~Horsepower|Manufacturer,data=df1,col=c("black","red"),main="Manufacturer wise horsepower analysis")
histogram(~Fuel_efficiency|Vehicle_type,data=df1,col=c("red","black"),main="Fuel efficiency of vehicle type")
histogram(~Horsepower|Vehicle_type,data=df1,col=c("red","black"),main="Horsepower of vehicle type")
histogram(~Sales_in_thousands|Vehicle_type,data=df1,col=c("red","black"),main="Sales_in_thousands of vehicle type")

#Subsetting the vehicle type attribute to Car
df2<-subset(df1,Vehicle_type== 'Car' & Sales_in_thousands > 480,select=c(Vehicle_type,Sales_in_thousands))
df2
df3<-subset(df1,Vehicle_type== 'Car' & Sales_in_thousands < 200,select=c(Vehicle_type,Sales_in_thousands))
df3
histogram(~Sales_in_thousands,data=df3,col="darkmagenta")


#Boxplot analysis
boxplot(Price_in_thousands~Manufacturer,data=df1,col=c("yellow","green"))
boxplot(Price_in_thousands~Vehicle_type,data=df1,col=c("yellow","green"))


#Scatterplot analysis
ggplot(df1, aes(x=Price_in_thousands, y=Sales_in_thousands)) + geom_point()
ggplot(df1, aes(x=Fuel_efficiency, y=Sales_in_thousands)) + geom_point()
ggplot(df1, aes(x=Horsepower, y=Sales_in_thousands)) + geom_point()
ggplot(df1, aes(x=Wheelbase, y=Sales_in_thousands)) + geom_point()

