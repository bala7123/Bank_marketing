# Basic model

bank_logistic <- glm(bank_train$y ~ age+job+contact+month+
                   day_of_week +campaign +pdays +previous+ 
                   cons.price.idx+cons.conf.idx+euribor3m, data = bank_train, 
                 family = "binomial")   

summary(bank_logistic)

#AIC: 18386

# Hosmer Lemeshow

library(ResourceSelection)

hl <- hoslem.test(bank_logistic$y, fitted(bank_logistic), g = 10)

hl

table(bank_train$job)
table(bank_test$job)


#confusion matrix

lr.pred <- predict(bank_logistic, bank_test, type = "response")

lr.pred <- round(lr.pred)

install.packages("caret")
library(caret)

library(SDMTools)

test_class_var <- ifelse(test.class.var == "yes",1,0)

# Yes - Deposit is subscribed, No - Deposit not subscribed

table(as.factor(test_class_var))

names(bank_test)

test.class.var <- bank_test[,20]

table(test.class.var)

bnk <- confusionMatrix(data = lr.pred, reference = test.class.var, positive = "1")

bnk

Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 7256  680
         1   90  212
                                               
               Accuracy : 0.9065     
	    Sensitivity : 0.23767              
            Specificity : 0.98775