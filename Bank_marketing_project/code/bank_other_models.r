
# Let us try a Support Vector Machine technique on this. 
# Here i have removed the multicollinearity features, and less significant features

source(bank_data_prep_eda.r)
source(bank_feature_engineering.r)



names(bank_train)

# Model

svm_model <- svm(bank_train$y ~., data = bank_train[-c(1,2,6,7,11:13,15:16,20)], 
                 kernel = "linear", cost = 10, scale = TRUE)

summary(svm_model)

# Predict the model

svm_pred <- predict(svm_model, bank_test[-c(1,2,6,7,11:13,15:16,20)])

svm_pred

# Confusion matrix

bnk_cm4 <- SDMTools::confusion.matrix(bank_test$y, svm_pred, threshold = 0.5)

bnk_cm4

#         obs
# pred    0   1
# 0     3637 354
# 1       50  78


# The above confusion matrix results were not an improvement of the Logistic model as the prediction that i will be taking the subscidy 
and actually i did not is 50 here and 46 in Logistic. 
# hence we can train the model using Cross validation and validate the results

# Training the svm model

library(caret)
library(kernlab)

# i did not provide numbers in train control as the train data is around 32k and for the same reason repeats are kept 2.

trctrl <- trainControl(method = "repeatedcv", repeats = 2)

svm_train <- train(bank_train$y ~., data = bank_train[-c(1,2,6,7,11:13,15:16,20)],
                   method = "svmRadial", trControl = trctrl)



## Decision Tree

library(C50)

DC_Model <- C50::C5.0(bank_train[,-c(1,2,6,7,11:12,14:15,19:20)], bank_train$y)

summary(DC_Model)

dc_pred <- predict(DC_Model,bank_test[,-c(1,2,6,7,11:12,14:15,19:20)])


CrossTable(bank_test$y, dc_pred, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, 
           dnn = c("Actual Default","Predict Default"))


#Total Observations in Table:  4119 


#                | Predict Default 
# Actual Default |         0 |         1 | Row Total | 
# ---------------|-----------|-----------|-----------|
#              0 |      3612 |        61 |      3673 | 
#                |     0.877 |     0.015 |           | 
# ---------------|-----------|-----------|-----------|
#              1 |       355 |        91 |       446 | 
#                |     0.086 |     0.022 |           | 
# ---------------|-----------|-----------|-----------|
#   Column Total |      3967 |       152 |      4119 | 
# ---------------|-----------|-----------|-----------|

# An improvement in C5 over C4.5 tree is the adaptive boosting technique in Decision Tree
# In this many trees are built and the trees vote on the best class

# Hence we are boosting the model to predict better

bank_boost <- C5.0(bank_train[,-c(1,2,6,7,11:12,14:15,19:20)], bank_train$y, trials = 10)

summary(bank_boost)

# Predict after boost

bank_boost_pr <- predict(bank_boost, bank_test[,-c(1,2,6,7,11:12,14:15,19:20)])

CrossTable(bank_test$y, bank_boost_pr, prop.chisq = FALSE, prop.r = FALSE, 
           prop.c = FALSE, dnn = c("Actual Default", "Predict Default"))

#                | Predict Default 
# Actual Default |         0 |         1 | Row Total | 
# ---------------|-----------|-----------|-----------|
#              0 |      3605 |        68 |      3673 | 
#                |     0.875 |     0.017 |           | 
# ---------------|-----------|-----------|-----------|
#              1 |       341 |       105 |       446 | 
#                |     0.083 |     0.025 |           | 
# ---------------|-----------|-----------|-----------|
#   Column Total |      3946 |       173 |      4119 | 
# ---------------|-----------|-----------|-----------|


# It is clear that the boosting does not show proper improvement here.
# Instead False Positive is 68 here whereas in the DTREE without boosting it is 61
# One reason for this may be that the training data is noisy
# So lets ignore boosting and take the actual DTREE model into consideration

