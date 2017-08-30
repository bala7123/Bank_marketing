options(scipen = 999) # To disable scientific notation
library(ggplot2)
library(dplyr)
library(corrplot)
library(arules)
library(car)
library(mice)
library(recommenderlab)
library(proxy)
library(glmnet)
library(foreach)
library(ResourceSelection)
library(caret)
library(SDMTools)
library(RColorBrewer)
library(gmodels)
library("e1071")


## Data Loading

getwd()

bankm <- read.csv("C:\\Users\\Bala\\Documents\\Bank_Marketing\\bank-additional-full.csv", 
                  sep = ";", stringsAsFactors = FALSE)

## DATA PREPARATION

names(bankm)


# [1] "age"            "job"            "marital"        "education"     
# [5] "default"        "housing"        "loan"           "contact"       
# [9] "month"          "day_of_week"    "duration"       "campaign"      
# [13] "pdays"          "previous"       "poutcome"       "emp.var.rate"  
# [17] "cons.price.idx" "cons.conf.idx"  "euribor3m"      "nr.employed"   
# [21] "y"   


dim(bankm)

#[1] 41188    21

str(bankm)

# There is no separate train and test data available in this dataset. we have to split the dataset into train and test accordingly. 
# We would proceed with Data exploration and then create train and test before modeling

# Define the Numeric and Character variables

numeric_vars <- names(bankm[sapply(bankm, is.numeric)])

character_vars <- names(bankm[sapply(bankm, is.character)])


## Data Preparation

cor_matrix <- cor(bankm[,numeric_vars])

write.csv(cor_matrix, "C:\\Users\\Bala\\Documents\\Bank_Marketing\\Bank_cor_matrix.csv")

# The correlation matrix is available in the observations folder under Bank marketing

corr <- cor(bankm[numeric_vars])

corrplot(corr, method = "circle")


## Scatter Plot

barplot(table(bankm$y), main = "Subscribe a term deposit")

round(prop.table(table(bankm$y == "yes")) * 100)

# FALSE  TRUE 
# 89%    11% 

#we have very less data for "Yes" category which means we must slice the sample accordingly
# we must get "Yes" and "No" equally in both the samples

summary(bankm)

# On seeing the summary, we can say that 
# Duration & Campaign has outliers, 
# Pdays must be transformed 
# emp.var.rate & cons.conf.idx has negative values
# the numeric data needs to be scaled as we have different units of them

summary(bankm$duration)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0   102.0   180.0   258.3   319.0  4918.0 

# Duration value till 3rd quartile is 319 only but max is 4918 which is more than 15 times of 3rd quartile
# Hence we need to check for outliers in this feature

plot(bankm$duration)

plot(bankm$campaign ~ bankm$duration)

class(bankm$duration)

summary(bankm$campaign)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.568   3.000  56.000 

plot(bankm$campaign)

prop.table(table(bankm$pdays > 30))

#Out of the total data 96.5% of pdays value has same value 
#The remaining 3.5% are spread across different values 

#Hence some kind of transformation is required for pdays

prop.table(table(bankm$previous))

hist(bankm$previous)

# Even in previous, 86% of data is biased towards one value. the remaining 14% is split across different values

table(bankm$poutcome)

prop.table(table(bankm$poutcome))

summary(bankm$emp.var.rate)

table(bankm$emp.var.rate)

# Emp var rate feature is negative and must be validated during Feature Engineering. 

summary(bankm$cons.price.idx)

#This cons.price.idx is showing a normal distribution 

summary(bankm$cons.conf.idx)

# Cons.conf.idx is negative feature

plot(bankm$cons.conf.idx ~ bankm$cons.price.idx)

# As both cons conf and price are negative, we plotted both and found few cluster kind of 
# patterns that between the two. Hence there must exist some kind of colinearity between the two.
# We also noticed this in correlation matrix

summary(bankm$euribor3m)

# The data looks normally distributed

summary(bankm$nr.employed)

# The data looks normally distributed

#This data has already been investigated and studied hence we are jumping directly to FE.


#During EDA we noticed that there are a lot of "unknown" values in this dataset.Lets explore them

sapply(bankm, function(i) any(i %in% "unknown"))

# job   marital education   default   housing      loan 

#All these are missing values 
#so we have planned to first change them to NA and impute them

character_vars

# Job

bankm$job[bankm$job == "unknown"] <- rep(NA, 330)

table(bankm$job , useNA="ifany")

bankm$marital[bankm$marital == "unknown"] <- rep(NA, 80)

table(bankm$marital , useNA="ifany")

bankm$education[bankm$education == "unknown"] <- rep(NA, 1731)

table(bankm$education , useNA="ifany")

bankm$default[bankm$default == "unknown"] <- rep(NA, 8597)

table(bankm$default , useNA="ifany")

bankm$housing[bankm$housing == "unknown"] <- rep(NA, 990)

table(bankm$housing , useNA="ifany")

bankm$loan[bankm$loan == "unknown"] <- rep(NA, 990)

table(bankm$loan , useNA="ifany")

colnames(bankm[sapply(bankm, function(i) any(is.na(i)))])

#[1] "job"       "marital"   "education" "default"   "housing"  
#[6] "loan"

#To compute missing values, we can see that each of the features are dependent on other
#So we can reduce levels of certain feature to impute missing values easily

# we tried to impute the missing values with the help of adjucent columns but we could not
# Unfortunately none of the columns are dependant on the other to that specificity
# and the count of NAs are> 500 or so. So we go with the MICE package to impute them

str(bankm)

bankm$job <- as.factor(bankm$job)
bankm$marital <- as.factor(bankm$marital)
bankm$education <- as.factor(bankm$education)
bankm$default <- as.factor(bankm$default)
bankm$housing <- as.factor(bankm$housing)
bankm$loan <- as.factor(bankm$loan)

mice_bank <- mice(bankm, method = "cart")

mice_bank <- complete(mice_bank)

anyNA(mice_bank)

head(mice_bank)

bank_all <- mice_bank

# A very important observation is that there is a varaible called "Duration" that specifies the call duration with customer for a subscidary
# As the call duration is going to be zero for new customers, using this model is not correct

bankm <- bankm[ !names(bankm) %in% "duration"]