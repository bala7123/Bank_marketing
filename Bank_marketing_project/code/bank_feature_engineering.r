
source(bank_data_prep_eda.r)

############################# FEATURE ENGINEERING #######################################

# Binning Age

bankm$age_intr <- ifelse(bank_all$age >= 17 & bank_all$age <= 30,1,
                            ifelse(bank_all$age >= 31 & bank_all$age <= 45,2,
                                   ifelse(bank_all$age >= 46 & bank_all$age <= 70,3,4)))


table(bankm$age_intr)

# Create new Feature job_cat with only 3 unique values instead of 12

table(bankm$job)

bankm$job_cat <- ifelse(bankm$job == "retired" | bankm$job == "unemployed" | 
                          bankm$job == "student","No_Job",
                        ifelse(bankm$job == "unknown","Unknown","Has_job"))


table(bankm$job_cat)

table(bankm$campaign)

# We can bin campaigns by 1 to 6, 7 to 17 and 18 to 27 and 28 to 56

bankm$campaign_bn <- ifelse(bankm$campaign >= 1 & bankm$campaign <= 6, 1,
            ifelse(bankm$campaign >=7 & bankm$campaign <= 17,2,
                   ifelse(bankm$campaign >= 18 & bankm$campaign <= 27, 3, 4)))

table(bankm$campaign_bn)

names(bankm)

table(bankm$pdays)

# We can bin this as client was previous contacted or not (999)

bankm$pdays_bn <- ifelse(bankm$pdays == "999", "0",1)

table(bankm$pdays_bn)

table(bankm$previous)

table(bankm$poutcome)

# Lets turn all these into factors 

bankm$age_intr <- as.factor(bankm$age_intr)
bankm$job_cat <- as.factor(bankm$job_cat)
bankm$campaign_bn <- as.factor(bankm$campaign_bn)
bankm$pdays_bn <- as.factor(bankm$pdays_bn)
