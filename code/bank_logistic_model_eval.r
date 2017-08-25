
# Let us look at the evaluation of the two logistic models


# Hosmer-Lemeshow Model1

hl1 <- hoslem.test(model1$y, fitted(model1), g = 10)

hl1

# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  model1$y, fitted(model1)
# X-squared = 21.122, df = 8, p-value = 0.006829

# Hosmer-Lemeshow Model2

hl2 <- hoslem.test(model2$y, fitted(model2), g = 10)

hl2

#	Hosmer and Lemeshow goodness of fit (GOF) test
#
#data:  model2$y, fitted(model2)
#X-squared = 46.032, df = 8, p-value = 0.0000002344




# Accuracy of Model1

SDMTools::accuracy(bank_test$y, pred1, threshold = 0.5)

# threshold       AUC omission.rate sensitivity specificity prop.correct     Kappa
# 0.5       0.5975898     0.7914798   0.2085202   0.9866594    0.9024035 0.2785988

# Prop.correct percent is 90.24%

# Accuracy of Model2

SDMTools::accuracy(bank_test$y, pred2, threshold = 0.5)

# threshold       AUC omission.rate sensitivity specificity prop.correct     Kappa
#  0.5      0.5823031       0.82287     0.17713   0.9874762    0.8997329 0.2407116

# Prop.correct percent is 89.97%




# Pseudo R^2

pR2(model1) # McFadden = 0.2196813   Model1

pR2(model2)  # McFadden = 0.2121587  Model2


# Nagelkerke R^2

NagelkerkeR2(model1)   # [1] 0.2837151   Model1

NagelkerkeR2(model2)   # [1] 0.27471     Model2



# ROCR curve model1

roc1 <- ROCR::prediction(pred1, bank_test$y)
  
roc_perf1 <- ROCR::performance(prediction.obj = roc1, measure = "tpr",
                               x.measure = "fpr")  
  
plot(roc_perf1)  

# ROCR curve model2

roc2 <- ROCR::prediction(pred2, bank_test$y)

roc_perf2 <- ROCR::performance(prediction.obj = roc2, measure = "tpr",
                               x.measure = "fpr")  

plot(roc_perf2) 




# Gini Coefficient: model1

SDMTools::auc(bank_test$y, pred1)

gini_mod1 = (2 * 0.7799223) -1

gini_mod1  # 55.98%


# Gini Coefficient: model2

SDMTools::auc(bank_test$y, pred2)

gini_mod2 = (2 * 0.774584) -1

gini_mod2  # 54.91%



# Log likelihood of the model

lrtest(model1)  # Model1

Model 1: bank_train$y ~ marital + education + default + contact + month + 
    day_of_week + previous + poutcome + emp.var.rate + cons.price.idx + 
    cons.conf.idx + euribor3m + nr.employed + age_intr + job_cat + 
    campaign_bn + pdays_bn
Model 2: bank_train$y ~ 1
  #Df LogLik  Df  Chisq            Pr(>Chisq)    
1  44 -10212                                     
2   1 -13086 -43 5749.7 < 0.00000000000000022 ***

lrtest(model2)  # Model2

Model 1: bank_train$y ~ marital + education + default + contact + month + 
    day_of_week + previous + cons.price.idx + cons.conf.idx + 
    euribor3m + age_intr + job_cat + campaign_bn + pdays_bn
Model 2: bank_train$y ~ 1
  #Df LogLik  Df  Chisq            Pr(>Chisq)    
1  40 -10310                                     
2   1 -13086 -39 5552.8 < 0.00000000000000022 ***



# Confusion matrix model1

bnk_cm2 <- SDMTools::confusion.matrix(bank_test$y, pred1, threshold = 0.5)

bnk_cm2

#          obs
# pred    0   1
# 0     3624 353
# 1       49  93

# Confusion matrix model2

bnk_cm3 <- SDMTools::confusion.matrix(bank_test$y, pred2, threshold = 0.5)

bnk_cm3

#         obs
# pred    0   1
# 0     3627 367
# 1       46  79
