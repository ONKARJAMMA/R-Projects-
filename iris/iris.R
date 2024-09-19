View(iris)
head(iris)
str(iris)

df <- iris[, 1:4]
boxplot(df)
pairs(df)
stars(df)

PL <- df$Petal.Length
barplot(PL)
hist(PL)
SP <- iris$Species
pie(table(SP))
boxplot(PL ~ SP)
summary(aov(PL ~ SP))

PW <- df$Petal.Width
plot(PL, PW, col = SP)
abline(lm(PW ~ PL))





Species <- iris$Species
# Comparing distributions of petal length of the three species by boxplot
boxplot(PL ~ Species)


counts <- table(iris$Species) # tabulate the frequencies
counts
pie(counts) # pie chart
barplot(counts) # bar plot



PW <- iris$Petal.Width # just lazy
plot(PW, PL) # scatterplot of pental width vs pental len
gth





summary(iris)
