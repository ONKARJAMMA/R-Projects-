# Load required packages
install.packages("e1071")
library(e1071)

# Define the dataset
diabetes <- read.csv("D:/New/diabetes/diabetes.csv")
View(diabetes)
dataset <-  read.csv("D:/New/diabetes/diabetes.csv")

# Preprocess Data: Replace 0 values with mean for each column
dataset$BloodPressure <- ifelse(dataset$BloodPressure == 0, mean(dataset$BloodPressure[dataset$BloodPressure != 0]), dataset$BloodPressure)
dataset$Glucose <- ifelse(dataset$Glucose == 0, mean(dataset$Glucose[dataset$Glucose != 0]), dataset$Glucose)
dataset$Insulin <- ifelse(dataset$Insulin == 0, mean(dataset$Insulin[dataset$Insulin != 0]), dataset$Insulin)
dataset$BMI <- ifelse(dataset$BMI == 0, mean(dataset$BMI[dataset$BMI != 0]), dataset$BMI)

head(dataset)
print(dataset)
# Calculate and Print Statistics for Each Column

# Glucosez
print("Statistics for Glucose:")
print("Mean:")
print(mean(dataset$Glucose))
print("Median:")


print(median(dataset$Glucose))



print("Mode:")
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
print(getmode(dataset$Glucose))
print("Skewness:")
print(skewness(dataset$Glucose))
print("Quartiles:")
print(quantile(dataset$Glucose, probs = c(0.25, 0.5, 0.75)))
print("Min:")
print(min(dataset$Glucose))
print("Max:")
print(max(dataset$Glucose))
print("Kurtosis:")
print(kurtosis(dataset$Glucose))
print("Summary Statistics:")
print(summary(dataset$Glucose))
hist(dataset$Glucose)
pie_chart <- ggplot(dataset, aes(x = "", fill = factor(Glucose > 120))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Glucose Levels (<=120 vs >120)") +
  scale_fill_manual(values = c("blue", "red"), labels = c("<= 120", "> 120"), name = "Glucose Level")
print(pie_chart)

ggplot(dataset, aes(x = dataset$Glucose, y = dataset$BMI)) +
  geom_point() +
  labs(x = "Glucose", y = "BMI", title = "Scatter Plot of Glucose vs BMI")

cor(dataset)





# BloodPressure
print("Statistics for BloodPressure:")
print("Mean:")
print(mean(dataset$BloodPressure))
print("Median:")
print(median(dataset$BloodPressure))
print("Mode:")
print(getmode(dataset$BloodPressure))
print("Skewness:")
print(skewness(dataset$BloodPressure))
print("Quartiles:")
print(quantile(dataset$BloodPressure, probs = c(0.25, 0.5, 0.75)))
print("Min:")
print(min(dataset$BloodPressure))
print("Max:")
print(max(dataset$BloodPressure))
print("Kurtosis:")
print(kurtosis(dataset$BloodPressure))
print("Summary Statistics:")
print(summary(dataset$BloodPressure))
hist(dataset$BloodPressure)

# Insulin
print("Statistics for Insulin:")
print("Mean:")
print(mean(dataset$Insulin))
print("Median:")
print(median(dataset$Insulin))
print("Mode:")
print(getmode(dataset$Insulin))
print("Skewness:")
print(skewness(dataset$Insulin))
print("Quartiles:")
print(quantile(dataset$Insulin, probs = c(0.25, 0.5, 0.75)))
print("Min:")
print(min(dataset$Insulin))
print("Max:")
print(max(dataset$Insulin))
print("Kurtosis:")
print(kurtosis(dataset$Insulin))
print("Summary Statistics:")
print(summary(dataset$Insulin))
hist(dataset$Insulin)

# BMI
print("Statistics for BMI:")
print("Mean:")
print(mean(dataset$BMI))
print("Median:")
print(median(dataset$BMI))
print("Mode:")
print(getmode(dataset$BMI))
print("Skewness:")
print(skewness(dataset$BMI))
print("Quartiles:")
print(quantile(dataset$BMI, probs = c(0.25, 0.5, 0.75)))
print("Min:")
print(min(dataset$BMI))
print("Max:")
print(max(dataset$BMI))
print("Kurtosis:")
print(kurtosis(dataset$BMI))
print("Summary Statistics:")
print(summary(dataset$BMI))
hist(dataset$BMI)




# Age
print("Statistics for Age:")
print("Mean:")
print(mean(dataset$Age))
print("Median:")
print(median(dataset$Age))
print("Mode:")
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
print(getmode(dataset$Age))
print("Skewness:")
print(skewness(dataset$Age))
print("Quartiles:")
print(quantile(dataset$Age, probs = c(0.25, 0.5, 0.75)))
print("Min:")
print(min(dataset$Age))
print("Max:")
print(max(dataset$Age))
print("Kurtosis:")
print(kurtosis(dataset$Age))
print("Summary Statistics:")
print(summary(dataset$Age))
hist(dataset$Age)

# Pie chart for Age
pie_chart_age <- ggplot(dataset, aes(x = "", fill = factor(Age > 40))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Age (<=40 vs >40)") +
  scale_fill_manual(values = c("blue", "red"), labels = c("<= 40", "> 40"), name = "Age Group")
print(pie_chart_age)

# Scatter plot for Age vs. BMI
ggplot(dataset, aes(x = Age, y = Insulin)) +
  geom_point() +
  labs(x = "Age", y = "Insulin", title = "Scatter Plot of Age vs Insulin")

