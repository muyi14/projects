install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")

library(tidyverse)
library(corrplot)
library(rcompanion)
library(ggplot2)
library(datarium)
library(qqplotr)


df <- read_excel("C:/Users/NJIRIBEAKO CLEOPATRA/Downloads/gdpdata.xlsx")
View(df)

#procedure two======Confirming the data sets
View(df)

#procedure three====Checking the data structure for data types
str(df) 

#EVALUATION THE THE STRUCTURE OF THE DATA SET BY Summarizing the data
summary(df)  

#Check for missing values
null_values <- colSums(is.na(df))
print(null_values) 

#Check for outliers
# Create a boxplot
boxplot(df$`GDP (US$)`,col="skyblue", main="Boxplot of GDP (US$)")
boxplot(df$`Current Health Expenditure(% of GDP)`,col="skyblue", main="Boxplot of Current Health Expenditure(% of GDP)")
boxplot(df$`Electricity Access(% of population)`,col="skyblue", main="Boxplot of Electricity Access(% of population)")
boxplot(df$`Life Expectancy(years)`,col="skyblue", main="Boxplot of Life Expectancy(years)")

#Preprocessing
head(df)
tail(df) 

#STATISTICAL ANALYSIS
#To compute the MEAN of the relevant columns
#we assign the appropriate value to the column of interest
mean(df$`GDP (US$)`)
mean(df$`Current Health Expenditure(% of GDP)`)
mean(df$`Electricity Access(% of population)`)
mean(df$`Life Expectancy(years)`)

#To compute the MEDIAN
median(df$`GDP (US$)`)
median(df$`Current Health Expenditure(% of GDP)`)
median(df$`Electricity Access(% of population)`)
median(df$`Life Expectancy(years)`)

#To compute the MODE
table(df$`GDP (US$)`)
which.max(table(df$`GDP (US$)`))
table(df$`Current Health Expenditure(% of GDP)`)
which.max(table(df$`Current Health Expenditure(% of GDP)`))
table(df$`Electricity Access(% of population)`)
which.max(table(df$`Electricity Access(% of population)`))
table(df$`Life Expectancy(years)`)
which.max(table(df$`Life Expectancy(years)`))

#To compute the STANDARD DEVIATION
std_dev_GDP <- sd(df$`GDP (US$)`)
# Print the result
print(std_dev_GDP)
std_dev_Health <- sd(df$`Current Health Expenditure(% of GDP)`)
# Print the result
print(std_dev_Health)
std_dev_Electricity <- sd(df$`Electricity Access(% of population)`)
# Print the result
print(std_dev_Electricity)
std_dev_Lifeexpectancy <- sd(df$`Life Expectancy(years)`)
# Print the result
print(std_dev_Lifeexpectancy)

#To calculate SKEWNESS & KURTOSIS
install.packages("e1071")
library(e1071)
skewness(df$`GDP (US$)`)
skewness(df$`Current Health Expenditure(% of GDP)`)
skewness(df$`Electricity Access(% of population)`)
skewness(df$`Life Expectancy(years)`)

# Create a histogram for the GDP column
hist(df$`GDP (US$)`, main = "Histogram of GDP", xlab = "GDP")
# Histogram showing GDP distribution
title(main = paste("Skewness: ", skewness(df$`GDP (US$)`)))
hist(df$`Current Health Expenditure(% of GDP)`, main = "Histogram of Health exp" , xlab = "Health")
title(main = paste("Skewness: ", skewness(df$`Current Health Expenditure(% of GDP)`)))
hist(df$`Electricity Access(% of population)`, main = "Histogram of Electricity access" , xlab = "Electricity")
title(main = paste("Skewness: ", skewness(df$`Electricity Access(% of population)`)))
hist(df$`Life Expectancy(years)`, main = "Histogram of Life expectancy" , xlab = "Life expectancy")
title(main = paste("Histogram: ", skewness(df$`Life Expectancy(years)`)))


#CORRELATION
# Calculate the correlation coefficient
correlation_coefficient <- cor(df$`GDP (US$)`, df$`Current Health Expenditure(% of GDP)`)
# Print the correlation coefficient
cat("Correlation Coefficient:", correlation_coefficient, "\n")

# Calculate the correlation coefficient
correlation_coefficient <- cor(df$`GDP (US$)`, df$`Electricity Access(% of population)`)
# Print the correlation coefficient
cat("Correlation Coefficient:", correlation_coefficient, "\n")

# Calculate the correlation coefficient
correlation_coefficient <- cor(df$`GDP (US$)`, df$`Life Expectancy(years)`)
# Print the correlation coefficient
cat("Correlation Coefficient:", correlation_coefficient, "\n")

# Calculate the correlation coefficient
correlation_coefficient <- cor(df$`Electricity Access(% of population)`, df$`Life Expectancy(years)`)
# Print the correlation coefficient
cat("Correlation Coefficient:", correlation_coefficient, "\n")

# Scatterplot
plot(df$`Electricity Access(% of population)`, df$`Life Expectancy(years)`, col = "blue", main = "Scatterplot", xlab = "Electricity Access(% of population)", ylab = "Life Expectancy(years)")
plot(df$`GDP (US$)`, df$`Current Health Expenditure(% of GDP)`, col = "green", main = "Scatterplot", xlab = "GDP (US$)", ylab = "Current Health Expenditure(% of GDP)")
plot(df$`GDP (US$)`, df$`Electricity Access(% of population)`, col = "red", main = "Scatterplot", xlab = "GDP (US$)", ylab = "Electricity Access(% of population)")
plot(df$`GDP (US$)`, df$`Life Expectancy(years)`, col = "purple", main = "Scatterplot", xlab = "GDP (US$)", ylab = "Life Expectancy(years)")


#HYPOTHESIS TESTING
# Create a data frame for Europe
europe_df <- df[df$Continent == "Europe", ]
# Create a data frame for Africa
africa_df <- df[df$Continent == "Africa", ]

#Assessing Normality of data
#Shapirowilk
variable_to_test <- df$`Life Expectancy(years)`
# Shapiro-Wilk test
shapiro_test_result <- shapiro.test(variable_to_test)
# Print the test result
print(shapiro_test_result)

variable_to_test <- df$`GDP (US$)`
# Shapiro-Wilk test
shapiro_test_result <- shapiro.test(variable_to_test)
# Print the test result
print(shapiro_test_result)

variable_to_test <- df$`Current Health Expenditure(% of GDP)`
# Shapiro-Wilk test
shapiro_test_result <- shapiro.test(variable_to_test)
# Print the test result
print(shapiro_test_result)

variable_to_test <- df$`Electricity Access(% of population)`
# Shapiro-Wilk test
shapiro_test_result <- shapiro.test(variable_to_test)
# Print the test result
print(shapiro_test_result)

#QQplot
# Assuming 'Variable' is the column you want to assess for normality
# Replace 'Variable' with the actual column name in your data frame
variable_to_assess <- df$Variable

# Create a Q-Q plot
variable_to_assess <- df$`GDP (US$)`
qqnorm(variable_to_assess)
qqline(variable_to_assess, col = 2)  
title("Q-Q Plot for Assessing Normality")

variable_to_assess <- df$`Current Health Expenditure(% of GDP)`
qqnorm(variable_to_assess)
qqline(variable_to_assess, col = 2)  
title("Q-Q Plot for Assessing Normality")
      
      variable_to_assess <- df$`Electricity Access(% of population)`
      qqnorm(variable_to_assess)
      qqline(variable_to_assess, col = 2)  
      title("Q-Q Plot for Assessing Normality")  
      
      variable_to_assess <- df$`Life Expectancy(years)`
      qqnorm(variable_to_assess)
      qqline(variable_to_assess, col = 2)  
      title("Q-Q Plot for Assessing Normality")
      
      #Transform data
      variable_to_transform <- df$`GDP (US$)`
      # Perform log transformation
      log_transformed_variable <- log(variable_to_transform)
      # Create a Q-Q plot for the log-transformed variable
      qqnorm(log_transformed_variable)
      qqline(log_transformed_variable, col = 2)
      
      variable_to_transform <- df$`Current Health Expenditure(% of GDP)`
      # Perform log transformation
      log_transformed_variable <- log(variable_to_transform)
      # Create a Q-Q plot for the log-transformed variable
      qqnorm(log_transformed_variable)
      qqline(log_transformed_variable, col = 2)
      title("Log-Transformed Current Health Exp (% of GDP)")
      
      variable_to_transform <- df$`Electricity Access(% of population)`
      # Perform log transformation
      log_transformed_variable <- log(variable_to_transform)
      # Create a Q-Q plot for the log-transformed variable
      qqnorm(log_transformed_variable)
      qqline(log_transformed_variable, col = 2)
      title("Log-Transformed Electricity Acess (% of pop)")
      
      variable_to_transform <- df$`Life Expectancy(years)`
      # Perform log transformation
      log_transformed_variable <- log(variable_to_transform)
      # Create a Q-Q plot for the log-transformed variable
      qqnorm(log_transformed_variable)
      qqline(log_transformed_variable, col = 2)
      title("Log-Transformed Life expectancy (years)")
      
    #1st Hypothesis
      H0: The distribution of GDP is identical for both populations
      H1: The distribution of GDP is not identical for both populations
      
      variable_to_compare <- df$`GDP (US$)`
      continent_indicator <- df$Continent
      
      # Create a list of variables for each continent
      groups <- split(variable_to_compare, continent_indicator)
      # Perform Kruskal-Wallis test
      kruskal_test_result <- kruskal.test(groups)
      # Print the test result
      print(kruskal_test_result)
      
      
      #2nd Hypothesis
      H0: Life expectancy is identical for both populations
      H1: Life expectancy is not identical for both populations
      
      variable_to_compare <- df$`Life Expectancy(years)`
      continent_indicator <- df$Continent
      
      # Create a list of variables for each continent
      groups <- split(variable_to_compare, continent_indicator)
      # Perform Kruskal-Wallis test
      kruskal_test_result <- kruskal.test(groups)
      # Print the test result
      print(kruskal_test_result)
      
      
      #REGRESSION
      df_numeric <- df[,c("GDP (US$)","Current Health Expenditure(% of GDP)","Electricity Access(% of population)","Life Expectancy(years)")]
      cor(df_numeric)
  
      # Install corrplot package
      if (!requireNamespace("corrplot", quietly = TRUE)) {
        install.packages("corrplot")
      }
      
      # Load the corrplot package
      library(corrplot)
      # Set the size of the plot
      par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
      correlation_matrix <- cor(df_numeric)
      # Create a larger correlation plot
      corrplot(correlation_matrix, method = "color", add = FALSE, tl.col = "black", tl.srt = 30)
      
      #forward model
      model_1 <- lm(`GDP (US$)` ~ `Current Health Expenditure(% of GDP)`, df_numeric)
      summary(model_1)
      
      model_2 <- lm(`Life Expectancy(years)` ~ `Current Health Expenditure(% of GDP)`, df_numeric)
      summary(model_2)
      
      model_3 <- lm(`Life Expectancy(years)` ~ `Electricity Access(% of population)`, df_numeric)
      summary(model_3)
    
model_4 <- lm(`GDP (US$)` ~ `Electricity Access(% of population)` + `Life Expectancy(years)`, data = df_numeric)
summary(model_4)

model_5 <- lm(`Life Expectancy(years)` ~ `Electricity Access(% of population)` + `Current Health Expenditure(% of GDP)`, data = df_numeric)
summary(model_5)

model_6 <- lm(`Life Expectancy(years)` ~ `Electricity Access(% of population)` + `Current Health Expenditure(% of GDP)` + `GDP (US$)`, data = df_numeric)
summary(model_6)


      


      
      
      
      
