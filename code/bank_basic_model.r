
source(bank_data_prep_&_EDA.r)

#With the actual data let us proceed with a simple logistic model and evaluate the results

str(mice_bank)

mice_bank$contact <- as.factor(mice_bank$contact)
mice_bank$month <- as.factor(mice_bank$month)
mice_bank$day_of_week <- as.factor(mice_bank$day_of_week)
mice_bank$poutcome <- as.factor(mice_bank$poutcome)
 
str(mice_bank)

mice_bank$y <- ifelse(mice_bank$y == "yes", 1, 0)

table(mice_bank$y)

# Creating Train and Test data

set.seed(123)

index <- sample(1:nrow(mice_bank), size = 0.8 * nrow(mice_bank))

bank_train <- mice_bank[index,]

dim(bank_train)

bank_test <- mice_bank[-index,]

dim(bank_test)

table(bank_train$y)

# 0     1 
# 29215  3735

table(bank_test$y)

# 0    1 
# 7333  905 


############################## BASIC MODEL#################################


basic_model <- glm(bank_train$y ~. , data = bank_train, family = "binomial")

summary(basic_model)

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 23294  on 32949  degrees of freedom
Residual deviance: 13697  on 32905  degrees of freedom
AIC: 13787

Number of Fisher Scoring iterations: 11