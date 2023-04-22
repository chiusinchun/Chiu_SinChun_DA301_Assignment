## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages('tidyverse')
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(DataExplorer)

# Import the data set.
setwd("~/LSE/3rd Course/LSE_DA301_assignment_files")
turtle_sales <- read.csv('turtle_sales.csv',header = TRUE)

# Print the data frame.
View(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
turtle_sales2 <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
View(turtle_sales2)
as_tibble(turtle_sales2)


# View the descriptive statistics.
summary(turtle_sales2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Product,NA_Sales,data=turtle_sales2)
qplot(Product,EU_Sales,data=turtle_sales2)
qplot(Product,Global_Sales,data=turtle_sales2)

## 2b) Histograms
# Create histograms.
qplot(Product,data=turtle_sales2,geom='histogram')

## 2c) Boxplots
# Create boxplots.
turtle_sales2_B <- turtle_sales2
turtle_sales2_B$Product <- as.factor(turtle_sales2_B$Product)
qplot(Product,NA_Sales,data=turtle_sales2_B,geom='boxplot')
qplot(Product,EU_Sales,data=turtle_sales2_B,geom='boxplot')
qplot(Product,Global_Sales,data=turtle_sales2_B,geom='boxplot')
###############################################################################

# 3. Observations and insights

## Your observations and insights here ......




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(turtle_sales2)
head(turtle_sales2)
tail(turtle_sales2)
dim(turtle_sales2)
str(turtle_sales2)

# Check output: Determine the min, max, and mean values.
summary(turtle_sales2)

# View the descriptive statistics.
skim(turtle_sales2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
turtle_sales3 <- turtle_sales2 %>% group_by(Product)%>%
  summarise(sum_NA_Sales=sum(NA_Sales),
            sum_EU_Sales=sum(EU_Sales),
            sum_Global_Sales=sum(Global_Sales),
            .groups='drop')

# View the data frame.
View(turtle_sales3)

# Explore the data frame.
summary(turtle_sales3)
skim(turtle_sales3)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Product,sum_NA_Sales,data=turtle_sales3)
qplot(Product,sum_EU_Sales,data=turtle_sales3)
qplot(Product,sum_Global_Sales,data=turtle_sales3)

# Create histograms.
qplot(Product,data=turtle_sales3,geom = 'histogram')

# Create boxplots.
turtle_sales3_B <- turtle_sales3
turtle_sales3_B$Product <- as.factor(turtle_sales3_B$Product)
qplot(Product,sum_NA_Sales,data=turtle_sales3_B,geom='boxplot')
qplot(Product,sum_EU_Sales,data=turtle_sales3_B,geom='boxplot')
qplot(Product,sum_Global_Sales,data=turtle_sales3_B,geom='boxplot')

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(turtle_sales3$sum_NA_Sales,
       col='blue',
       xlab="z Value",
       ylab="NA Sales")
qqline(turtle_sales3$sum_NA_Sales,
       col='red',
       lwd=2)


qqnorm(turtle_sales3$sum_EU_Sales,
       col='blue',
       xlab="z Value",
       ylab="EU Sales")
qqline(turtle_sales3$sum_EU_Sales,
       col='red',
       lwd=2)


qqnorm(turtle_sales3$sum_Global_Sales,
       col='blue',
       xlab="z Value",
       ylab="Global Sales")
qqline(turtle_sales3$sum_Global_Sales,
       col='red',
       lwd=2)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(turtle_sales3$sum_NA_Sales)

shapiro.test(turtle_sales3$sum_EU_Sales)

shapiro.test(turtle_sales3$sum_Global_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(turtle_sales3$sum_NA_Sales)
kurtosis(turtle_sales3$sum_NA_Sales)

skewness(turtle_sales3$sum_EU_Sales)
kurtosis(turtle_sales3$sum_EU_Sales)

skewness(turtle_sales3$sum_Global_Sales)
kurtosis(turtle_sales3$sum_Global_Sales)

## 3d) Determine correlation
# Determine correlation.
cor(turtle_sales3$sum_NA_Sales,turtle_sales3$sum_EU_Sales)

cor(turtle_sales3$sum_EU_Sales,turtle_sales3$sum_Global_Sales)

cor(turtle_sales3$sum_NA_Sales,turtle_sales3$sum_Global_Sales)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.
turtle_sales4 <- turtle_sales3 %>% rename(North_American=sum_NA_Sales, 
                                          European=sum_EU_Sales,
                                          global=sum_Global_Sales)

turtle_sales4 <- gather(turtle_sales4,key = "variable",value = "value",-Product)  

turtle_sales4 <-  turtle_sales4 %>% rename(Region = variable, Sales = value)

View(turtle_sales4)

ggplot(data=turtle_sales4,
       mapping=aes(x=Product,y=Sales,color=Region))+
  geom_point(alpha=0.5,size=1.5)+
  geom_smooth(method='lm',se=FALSE,size=1.5)+
  scale_x_continuous(breaks = seq(0,10000,500),"Product")+
  scale_y_continuous(breaks = seq(0,70,10),"Sales")+
  scale_color_manual(values=c('red','blue','green'))

turtle_sales4_H <- subset(turtle_sales4,Sales>23)
View(turtle_sales4_H)

ggplot(turtle_sales4_H,
       aes(Product,Region,fill=Sales))+
  scale_x_continuous(breaks = seq(0,10000,100),"Product")+geom_tile()+
  scale_fill_gradient(low="white",high = "red")

###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(turtle_sales)
View(turtle_sales2)
View(turtle_sales3)
View(turtle_sales4)

# Determine a summary of the data frame.
summary(turtle_sales)
summary(turtle_sales2)
summary(turtle_sales3)
summary(turtle_sales4)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
install.packages('psych')
library(psych)
cor(turtle_sales3)
corPlot(turtle_sales3,cex=2)

NA_EU_lm <- lm(sum_NA_Sales~sum_EU_Sales,data = turtle_sales3)
summary(NA_EU_lm)


EU_Global_lm <- lm(sum_EU_Sales~sum_Global_Sales,data = turtle_sales3)
summary(EU_Global_lm)


NA_Global_lm <- lm(sum_NA_Sales~sum_Global_Sales,data = turtle_sales3)
summary(NA_Global_lm)



## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(NA_EU_lm)

plot(EU_Global_lm)

plot(NA_Global_lm)

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
m_model_gs=lm(sum_Global_Sales~sum_NA_Sales+sum_EU_Sales,
              data=turtle_sales3)

# Multiple linear regression model.
summary(m_model_gs)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
turtle_sales_pred <- data.frame(sum_NA_Sales = c(34.02,3.93,2.73,2.26,22.08),
                                sum_EU_Sales = c(23.80,1.56,0.65,0.97,0.52))
turtle_sales_pred

sum_Global_pred = predict(m_model_gs,newdata =turtle_sales_pred,
                          interval = 'confidence')

sum_Global_pred

turtle_sales3_check <- subset(turtle_sales3,
                              sum_NA_Sales %in% c(34.02,3.93,2.73,2.26,22.08))
turtle_sales3_check

###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




