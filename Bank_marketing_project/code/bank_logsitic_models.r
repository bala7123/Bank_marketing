
source(bank_data_prep_eda.r)
source(bank_feature_engineering.r)

# Basic model
# This model does not contain any Feature Engineering

bank_logistic <- glm(bank_train$y ~ age+job+contact+month+
                   day_of_week +campaign +pdays +previous+ 
                   cons.price.idx+cons.conf.idx+euribor3m, data = bank_train, 
                 family = "binomial")   

summary(bank_logistic)

#AIC: 18386

# Hosmer Lemeshow
# This test is performed on the model to decide the Goodness of Fit

hl <- hoslem.test(bank_logistic$y, fitted(bank_logistic), g = 10)

hl

# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  model$y, fitted(model)
# X-squared = 23.203, df = 8, p-value = 0.003113


# Confusion matrix
# A confusion matrix is created to find the Sensitivity and Specificity of the model
# by using the True Postive / Negative and False Positive / Negative Values

# First we must predict the values using the test data before proceeding with the confusion matrix

# The test dv must be binned to zeros and ones before predicting

test_class_var <- ifelse(test.class.var == "yes",1,0)

# Yes - Deposit is subscribed, No - Deposit not subscribed

lr.pred <- predict(bank_logistic, bank_test, type = "response")

lr.pred <- round(lr.pred)

# Confusion Matrix

bnk_cm1 <- SDMTools::confusion.matrix(bank_test$y, lr.pred_all, threshold = 0.5)

    obs
pred    0   1
   0 3593 367
   1   49 110

SDMTools::sensitivity(bnk_cm1)

#[1] 0.230608

SDMTools::specificity(bnk_cm1)

#[1] 0.9865459

SDMTools::accuracy(bank_test$y, lr.pred_all, threshold = 0.5)

# threshold       AUC omission.rate sensitivity specificity prop.correct     Kappa
#  0.5        0.6085769      0.769392    0.230608   0.9865459    0.8990046 0.3057109

SDMTools::auc(bank_test$y, lr.pred_all)

#[1] 0.7955244

SDMTools::omission(bnk_cm1)

#[1] 0.769392                     

# The Accuracy of the model is 89.90% 
# The sensitivity and specificity are calulated from the matrix above. 
# The matrix says FALSE POSITIVE is 49 (1.18%) which means, our model has predicted that these many people will take the subscidy but actually they did not. 
# That is one prediction which we must worry about. we will be discussing this in the observation



# Now with the Feature Engineered data, we will perform two models 
# One eliminating the multicollinearity variables and other keeping the multicollinearity variables
# We are doing this test to make sure that the removal of multicollinear variables has effect on the model

> names(bank_train)
 [1] "age"            "job"            "marital"        "education"     
 [5] "default"        "housing"        "loan"           "contact"       
 [9] "month"          "day_of_week"    "duration"       "campaign"      
[13] "pdays"          "previous"       "poutcome"       "emp.var.rate"  
[17] "cons.price.idx" "cons.conf.idx"  "euribor3m"      "nr.employed"   
[21] "y"              "age_intr"       "job_cat"        "campaign_bn"   
[25] "pdays_bn" 

# From the above list from 22 to 25 are new features created
# Duration feature will be removed as per the dataset

# The below model is created including variables identified as multicollinear

model1 <- glm(bank_train$y ~. , data = bank_train[-c(1,2,6,7,11:13)], family = "binomial")

summary(model1)

****

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0429  -0.3920  -0.3197  -0.2628   3.3840  

    Null deviance: 26173  on 37068  degrees of freedom
Residual deviance: 20423  on 37025  degrees of freedom
AIC: 20511

Number of Fisher Scoring iterations: 13

****

pred1 <- predict(model1, bank_test[-c(1,2,6,7,11:13)], type = "response")

pred1



# The below model is created omitting multicollinearity variables

names(bank_train)

model2 <- glm(bank_train$y ~. , data = bank_train[-c(1,2,6,7,11:13,15:16,20)], 
              family = "binomial")

summary(model2)

pred2 <- predict(model2, bank_test[-c(1,2,6,7,11:13,15:16,20)], type = "response")

pred2


# The comparison of both the features will be present under observation