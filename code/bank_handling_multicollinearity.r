
source(bank_data_prep_&_eda.r)

# While performing EDA, we noticed that there are highly correlated independant variables in this dataset.
# Hence there is a chance for multicolinearity to exist. 
# With the actual data let us proceed with a simple logistic model and evaluate the results

str(bank_all)

bank_all$contact <- as.factor(bank_all$contact)
bank_all$month <- as.factor(bank_all$month)
bank_all$day_of_week <- as.factor(bank_all$day_of_week)
bank_all$poutcome <- as.factor(bank_all$poutcome)
 
str(bank_all)

# The dependent variable must be dummy coded , then only the values can be passed to a model
# Yes = 1, No = 0

bank_all$y <- ifelse(bank_all$y == "yes", 1, 0)

table(bank_all$y)

# Creating Train and Test data

set.seed(123) 		# We set seed to 123 so that the sample does not change	 

index <- sample(1:nrow(bank_all), size = 0.8 * nrow(bank_all))

bank_train <- bank_all[index,]  # Creating train data

dim(bank_train)

bank_test <- bank_all[-index,]  # Creating test data

dim(bank_test)

# Even though we had split the data 80-20, we must make sure that both the variations of DV are available in both Train and Test
# Then only the model will fetch good result

table(bank_train$y)

# 0     1 
# 29215  3735

table(bank_test$y)

# 0    1 
# 7333  905 


################# BASIC LOGISTIC MODEL #####################


basic_model <- glm(bank_train$y ~. , data = bank_train, family = "binomial")

summary(basic_model)


Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 23294  on 32949  degrees of freedom
Residual deviance: 13697  on 32905  degrees of freedom
AIC: 13787

Number of Fisher Scoring iterations: 11


# Now with this model result, we need to validate if Multi-collinearity exists

# Check for multicollinearity 

car::vif(basic_model)

# GVIF Df GVIF^(1/(2*Df))
# age              2.199783  1        1.483166
# job              5.542986 10        1.089400
# marital          1.438493  2        1.095158
# education        3.147317  6        1.100259
# default          1.000001  1        1.000000
# housing          1.011056  1        1.005513
# loan             1.004529  1        1.002262
# contact          2.380812  1        1.542988
# month           66.531033  9        1.262639
# day_of_week      1.061987  4        1.007546
# campaign         1.043421  1        1.021480
# pdays           10.568552  1        3.250931
# previous         4.719660  1        2.172478
# poutcome        25.222941  2        2.241037
# emp.var.rate   146.236546  1       12.092830
# cons.price.idx  66.652345  1        8.164089
# cons.conf.idx    5.418909  1        2.327855
# euribor3m      143.602948  1       11.983445
# nr.employed    176.085352  1       13.269716

sqrt(car::vif(basic_model))

# Any variable VIF is > 2 then it exhibits collinearity

# Out of these, nr.employed variable shows VIF as 174. Hence lets remove that first 
# and compute Logistic model


mc_model1 <- glm(bank_train$y ~ age+job+marital+education+default+housing+loan+contact+month+
                   day_of_week +campaign +pdays +previous+ poutcome+ emp.var.rate+ 
                   cons.price.idx+cons.conf.idx+euribor3m, data = bank_train, 
                 family = "binomial") #AIC: 18245

summary(mc_model1)

car::vif(mc_model1)

# GVIF Df GVIF^(1/(2*Df))
# age             2.198878  1        1.482862
# job             5.539420 10        1.089364
# marital         1.439120  2        1.095278
# education       3.146356  6        1.100231
# default         1.000000  1        1.000000
# housing         1.010504  1        1.005238
# loan            1.004473  1        1.002234
# contact         2.176540  1        1.475310
# month          14.584123  9        1.160540
# day_of_week     1.054745  4        1.006685
# campaign        1.042798  1        1.021175
# pdays          10.548569  1        3.247856
# previous        4.715523  1        2.171525
# poutcome       25.119064  2        2.238726
# emp.var.rate   94.533697  1        9.722844
# cons.price.idx 12.194981  1        3.492131
# cons.conf.idx   2.735467  1        1.653925
# euribor3m      62.219606  1        7.887941

#If you notice, there are no 3 digit variances now. so lets remove emp-var-rate now 

mc_model2 <- glm(bank_train$y ~ age+job+marital+education+default+housing+loan+contact+month+
                   day_of_week +campaign +pdays +previous+ poutcome+ 
                   cons.price.idx+cons.conf.idx+euribor3m, data = bank_train, 
                 family = "binomial")   #AIC: 18371

summary(mc_model2)

car::vif(mc_model2)

# GVIF Df GVIF^(1/(2*Df))
# age             2.194047  1        1.481232
# job             5.537696 10        1.089348
# marital         1.437710  2        1.095009
# education       3.156715  6        1.100533
# default         1.000001  1        1.000000
# housing         1.010147  1        1.005061
# loan            1.004286  1        1.002141
# contact         1.891583  1        1.375348
# month           5.326374  9        1.097381
# day_of_week     1.048571  4        1.005946
# campaign        1.038639  1        1.019136
# pdays          10.576134  1        3.252097
# previous        4.723173  1        2.173286
# poutcome       25.101264  2        2.238329
# cons.price.idx  2.529192  1        1.590343
# cons.conf.idx   2.355030  1        1.534611
# euribor3m       2.713016  1        1.647124

# Again we can see the larget variance reduced from 94 to 25. Lets remove poutcome and rerun the model

mc_model3 <- glm(bank_train$y ~ age+job+marital+education+default+housing+loan+contact+month+
                   day_of_week +campaign +pdays +previous+ 
                   cons.price.idx+cons.conf.idx+euribor3m, data = bank_train, 
                 family = "binomial")   #AIC: 18371

summary(mc_model3)

car::vif(mc_model3)

# GVIF Df GVIF^(1/(2*Df))
# age            2.198107  1        1.482602
# job            5.532207 10        1.089294
# marital        1.437136  2        1.094900
# education      3.148325  6        1.100289
# default        1.000001  1        1.000000
# housing        1.009960  1        1.004968
# loan           1.004146  1        1.002071
# contact        1.897050  1        1.377334
# month          5.309217  9        1.097184
# day_of_week    1.048620  4        1.005952
# campaign       1.038603  1        1.019119
# pdays          1.754223  1        1.324471
# previous       1.983862  1        1.408497
# cons.price.idx 2.482592  1        1.575624
# cons.conf.idx  2.351706  1        1.533527
# euribor3m      2.698825  1        1.642810


# Removed Features :  nr.employed  emp.var.rate  poutcome









