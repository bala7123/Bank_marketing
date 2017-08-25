################################# Dimensionality Reduction ##################################

# Find correlated Feature

library(caret)

findCorrelation(cor_matrix, cutoff = .6, verbose = TRUE, names = TRUE)

# Correlation <=0.6 is "euribor3m"    "emp.var.rate"

# Boruta

install.packages("Boruta")
library(Boruta)
library(ranger)

set.seed(456)

boruta <- Boruta(y ~., data = bank_train, doTrace = 2)
# Rejected attributes housing, loan and edu_Cat

print(boruta)

plot(boruta)

# Lower Variance filter

x <- nearZeroVar(bank_train, saveMetrics = TRUE)

str(x, vec.len=2)

x[x[,"zeroVar"] > 0, ]

x[x[,"zeroVar"] + x[,"nzv"] > 0, ]

# Backward selection


step(bnk_model4, direction = "backward")

# Based on bnk_model4, Backward selection started from AIC=18456
# Best AIC=18453.19

# Now create a model with all variables and try backward selection

names(bank_train)

model_all <- glm(bank_train$y ~., data = bank_train[,-c(11,21:25)], family = "binomial")
summary(model_all)

step(model_all, direction = "backward")

# Start AIC : 20556.4
# Best AIC : 20540

# So we use the bnk_model4 as the final model with the lowest AIC as 18453.19

