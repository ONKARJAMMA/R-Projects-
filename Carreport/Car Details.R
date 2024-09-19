Car_details_v3
View(Car_details_v3)
head(Car_details_v3)
colnames(Car_details_v3)
data_cars<-Car_details_v3
dim(data_cars)
names(data_cars)
colSums(data_cars$mileage)
rownames(data_cars)

summary(data_cars)


data_cars$year
data_cars$km_driven

mean(data_cars$selling_price)
mean(data_cars$mileage)
sort(data_cars$eng)
summary(data_cars)
max(data_cars$selling_price)
max(data_cars$mileage)
min(data_cars$mileage)
median(data_cars$mileage)
mode(data_cars$name)
sd(data_cars$mileage)

rownames(data_cars)[which.max((data_cars$mileage))]
rownames(data_cars)[which.min((data_cars$mileage))]

IQR(data_cars$mileage)

boxplot(data_cars$mileage)
barplot(data_cars$mileage,data_cars$eng,xlab = "Mileage",ylab = "Engine")
barplot(data_cars$seats,data_cars$mileage,xlab = "Mileage",ylab = "Engiene")

library(moments)
skewness(data_cars$year)
skewness(data_cars$mileage)
skewness(data_cars$eng)

kurtosis(data_cars$year)
kurtosis(data_cars$mileage)
kurtosis(data_cars$eng)


library(ggplot2)

qplot(data_cars$selling_price,geom = 'histogram',binwith=2)+xlab('YEAR')
qplot(data_cars$mileage,geom = 'histogram',binwith=2)+xlab('YEAR')
qplot(data_cars$km_driven,geom = 'histogram',binwith=2)+xlab('YEAR')
qplot(data_cars$seats,geom = 'histogram',binwith=2)+xlab('Milage')
qplot(data_cars$eng,geom = 'histogram',binwith=2)+xlab('Milage')


qplot(data_cars$mileage,data_cars$eng,data = data_cars,xlab = "Mileage In Kmpl",ylab = "Engine In CC")
qplot(data_cars$mileage,data_cars$eng,data = data_cars,xlab = "Mileage In Kmpl",ylab = "Engine In CC",size=data_cars$fuel)
qplot(data_cars$mileage,data_cars$eng,data = data_cars,xlab = "Mileage In Kmpl",ylab = "Engine In CC",size=data_cars$km_driven)
qplot(data_cars$mileage,data_cars$eng,data = data_cars,xlab = "Mileage In Kmpl",ylab = "Engine In CC",facets = vs~am)

qplot(1:10,rnorm(10),color=runif(10))
qplot(1:10,letters[1:10])


plot(data_cars$mileage,data_cars$eng ,data=data_cars,color=data_cars$fuel,geom='boxplot')
warning()


summary(data_cars$selling_price)


hist(data_cars$selling_price)
library(moments)
skewness(data_cars$selling_price)
kurtosis(data_cars$selling_price)

summary(data_cars$km_driven)


hist(data_cars$km_driven)
library(moments)
skewness(data_cars$km_driven)
kurtosis(data_cars$km_driven)

summary(data_cars$mileage)
hist(data_cars$mileage)
library(moments)
skewness(data_cars$mileage)
kurtosis(data_cars$mileage)

summary(data_cars$eng)
hist(data_cars$eng)
library(moments)
skewness(data_cars$eng)
kurtosis(data_cars$eng)

summary(data_cars$seats)
hist(data_cars$seats)
library(moments)
skewness(data_cars$seats)
kurtosis(data_cars$seats)

mean(data_cars$selling_price)
mean(data_cars$km_driven)
mean(data_cars$mileage)
mean(data_cars$eng)
mean(data_cars$seats)

median(data_cars$selling_price)
median(data_cars$km_driven)
median(data_cars$mileage)
median(data_cars$eng)
median(data_cars$seats)

min(data_cars$selling_price)
min(data_cars$km_driven)
min(data_cars$mileage)
min(data_cars$eng)
min(data_cars$seats)

max(data_cars$selling_price)
max(data_cars$km_driven)
max(data_cars$mileage)
max(data_cars$eng)
max(data_cars$seats)

sd(data_cars$selling_price)
sd(data_cars$km_driven)
sd(data_cars$mileage)
sd(data_cars$eng)
sd(data_cars$seats)

IQR(data_cars$selling_price)
IQR(data_cars$km_driven)
IQR(data_cars$mileage)
IQR(data_cars$eng)
IQR(data_cars$seats)


rownames(data_cars)[which.max((data_cars$selling_price))]
rownames(data_cars)[which.min((data_cars$selling_price))]

rownames(data_cars)[which.max((data_cars$km_driven))]
rownames(data_cars)[which.min((data_cars$km_driven))]

rownames(data_cars)[which.max((data_cars$mileage))]
rownames(data_cars)[which.min((data_cars$mileage))]

rownames(data_cars)[which.max((data_cars$eng))]
rownames(data_cars)[which.min((data_cars$eng))]

rownames(data_cars)[which.max((data_cars$seats))]
rownames(data_cars)[which.min((data_cars$seats))]










