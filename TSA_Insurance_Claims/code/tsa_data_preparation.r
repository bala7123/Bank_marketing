library(lubridate)
library(nnet)
library(ggplot2)

## TSA Claims

claims <- read.csv("tsa_claims.csv", stringsAsFactors = F, na.strings = "")

View(claims)

dim(claims)

#[1] 204267     13

anyNA(claims)
# TRUE

# To view NA values

colnames(claims[,sapply(claims, function(x) any(is.na(x)))])

# Claim.Number Date.Received Incident.Date  Airport.Code  Airport.Name 
# FALSE          TRUE          TRUE          TRUE          TRUE 
# Airline.Name    Claim.Type    Claim.Site          Item  Claim.Amount 
# TRUE          TRUE          TRUE          TRUE          TRUE 
# Status  Close.Amount   Disposition 
# TRUE          TRUE          TRUE 

# List of numeric and character features

character_vars <- colnames(claims[,sapply(claims, is.character)])

character_vars

numeric_vars <- colnames(claims[,sapply(claims, is.numeric)])

numeric_vars

############################# DATA PREPARATION ###################################

# Mark the dates with NA as 01-01-9999

claims$Date.Received_new[is.na(claims$Date.Received_new)] <- rep("9999-01-01" , 139)


#Except claim number, every feature contains NA
# Lets analyze every feature 

table(claims$Incident.Date, useNA = "ifany")
table(is.na(claims$Incident.Date))

table(is.na(claims$Date.Received))

summary(claims$Date.Received)

str(claims)

# Dates data preparation

# THe date field is in character datatype. Lets change it to Date

# FIrst convert the date in char format to Posixit notation

dt_rcvd <- strptime(claims$Date.Received, format = "%d-%b-%y")

# Then use format to remove the timezone

claims$Date.Received_new <- as.Date(dt_rcvd, "%Y/%m/%d")

# As we created a new variables lets remove the old date variable

#claims$Date.Received <- NULL

anyNA(claims)

head(claims$Date.Received_new)

# Remove timestamp from Incident_Date feature 

head(claims$Incident.Date)


claims$Incident.Date_new <- mdy_hm(claims$Incident.Date)

dim(claims)

claims <- subset(claims, !is.na(claims$Incident.Date_new))

# A few dates were in this format - 17-MAR-0201 00:00

head(claims$Incident.Date_new)

claims$Incident.Date_new <- as.Date(claims$Incident.Date_new, "%Y-%m-%d")

head(claims$Incident.Date_new)

table(is.na(claims$Incident.Date_new))

#claims$Incident.Date <- NULL

table(claims$Airport.Code, useNA = "ifany")

# Remove dollar sign from amount

claims$Claim.Amount <- gsub('\\$', '', claims$Claim.Amount)

claims$Claim.Amount <- gsub(' ', '', claims$Claim.Amount)

head(claims$Claim.Amount)

claims$Close.Amount <- gsub('\\$', '', claims$Close.Amount)

claims$Close.Amount <- gsub(' ', '', claims$Close.Amount)

head(claims$Close.Amount)

claims$Claim.Amount <- as.numeric(as.factor(claims$Claim.Amount))

claims$Close.Amount <- as.numeric(as.factor(claims$Close.Amount))

table(is.na(claims$Claim.Amount))

table(is.na(claims$Close.Amount))

table(claims$Airport.Name)

unique(claims$Airport.Name)

table(claims$Status)

unique(claims$Status)

unique(claims$Disposition)

table(is.na(claims$Disposition))

str(claims)

table(is.na(claims$Status))

unique(claims$Item)


colnames(claims[,sapply(claims, function(x) any(is.na(x)))])

table(is.na(claims$Airport.Code))

table(is.na(claims$Airport.Name))

table(is.na(claims$Airline.Name))

table(is.na(claims$Claim.Type))

table(is.na(claims$Item))

table(is.na(claims$Status))

table(is.na(claims$Claim.Amount))

table(is.na(claims$Status))

subset(claims, is.na(claims$Status))  # THere are a few rows with unnecessary characters, remove them

# Claim.Number                  Airport.Code Airport.Name Airline.Name Claim.Type
#   97232  ==> 2.csv.new <==         <NA>         <NA>         <NA>       <NA>
#   145145 ==> 3.csv.new <==         <NA>         <NA>         <NA>       <NA>
#   186743              <BR>         <NA>         <NA>         <NA>       <NA>
#   186744 ==> 4.csv.new <==         <NA>         <NA>         <NA>       <NA>
#   195600 ==> 5.csv.new <==         <NA>         <NA>         <NA>       <NA>

claims <- subset(claims, !is.na(claims$Status))

dim(claims)

str(claims)

table(claims$Status)

unique(claims$Status)

# Correct claim status 

claims$Status <- ifelse(claims$Status == "Approve in Full", "Approved",
       ifelse(claims$Status == "Settle", "Settled",
              ifelse(claims$Status == "Deny","Denied",claims$Status)))

unique(claims$Status)

claims$Status <- ifelse(claims$Status == "-","Not Specified",claims$Status)

table(claims$Status %in% "Insufficient; one of the following items required: sum certain; statement of fact; signature; location of incident; and date.")

claims$Status <- ifelse(claims$Status %in% "Insufficient; one of the following items required: sum certain; statement of fact; signature; location of incident; and date.","Isufficient_info", claims$Status)

claims$Status <- ifelse(claims$Status == "Closed as a contractor claim","Closed",
                        ifelse(claims$Status == "Claim has been assigned for further investigation",
                               "Further_investigation",
                               ifelse(claims$Status == "Pending response from claimant","Pending",claims$Status)))

unique(claims$Disposition)

table(claims$Disposition, useNA = "ifany")

prop.table(table(is.na(claims$Disposition)))

str(claims)

sapply(claims, function(x) any(is.na(x)))


names(claims)

dim(claims)

anyNA(claims)

View(claims)

str(claims)

claims$Claim.Number <- NULL

table(claims$Status)

claims <- subset(claims, !(claims$Status == "Pending" | claims$Status == "Canceled"))

dim(claims)

table(claims$Disposition)

#claims$Status <- NULL

str(claims)

# Create a year variable

#install.packages("lubridate")
library(lubridate)

claims$claim_year <- year(as.Date(claims$Date.Received_new, "%Y-%m-%d"))

str(claims)


claims$Airline.Name 

claims$Airline.Name <- gsub("\\;","",claims$Airline.Name) 

claims$Airport.Name <- gsub("\\;","", claims$Airport.Name)

head(claims$Airport.Name, n = 20)

head(claims$Airline.Name, n = 20)

plot(claims$claim_year)

# Removing rows with year > 2015 (Incorrect data)

claims <- subset(claims, !claims$claim_year > 2015)

summary(claims)


sapply(claims[!names(claims) %in% "Disposition"], function(x) any(is.na(x)))


anyNA(claims)

table(is.na(claims$Airport.Name))

claims$Airline.Name[is.na(claims$Airline.Name)] <- rep("Other",30342 )

claims$Airport.Code[is.na(claims$Airport.Code)] <- rep("Other", 6189)

claims$Airport.Name[is.na(claims$Airport.Name)] <- rep("Other", 6189)

unique(claims$Claim.Type)

claims$Claim.Type <- ifelse(claims$Claim.Type == "-" | is.na(claims$Claim.Type),"Other", claims$Claim.Type)

unique(claims$Claim.Site)

claims$Claim.Site <- ifelse(claims$Claim.Site == "-" | is.na(claims$Claim.Site), "Other", claims$Claim.Site)

unique(claims$Claim.Site)

unique(claims$Item)

table(is.na(claims$Item))

claims$Item[is.na(claims$Item)] <- rep("Other", 3483)

table(is.na(claims$Close.Amount))

claims$Claim.Amount[is.na(claims$Claim.Amount)] <- rep(0, 1948)

claims$Close.Amount[is.na(claims$Close.Amount)] <- rep(0, 47705)

unique(claims$Disposition)

prop.table(table(claims$Disposition, useNA = "ifany"))

table(claims$Status)

claims$Date.Received <- NULL

claims$Incident.Date <- NULL

names(claims)

head(claims)

unique(claims$Item)

# Item feature has ~4000 distinct items claimed. We must create bins with similar items to reduce the count

claims$Item <- ifelse(grepl("clothing", claims$Item, ignore.case = TRUE),"Clothing", claims$Item)

claims$Item <- ifelse(grepl("cosmetic", claims$Item, ignore.case = TRUE),"Beauty", claims$Item)

claims$Item <- ifelse(grepl("Audio|Computer|Laptop|PDA", claims$Item, ignore.case = TRUE),"Computer_peripheral", claims$Item)

claims$Item <- ifelse(grepl("Luggage|Bags|suitcase|dress|Fabric|Purses|Watches|Cameras", claims$Item, 
                            ignore.case = TRUE),"Luggage", claims$Item)
claims$Item <- ifelse(grepl("Jewelery|Money|ring|cash|currency", claims$Item, ignore.case = TRUE),"Previous", claims$Item)

claims$Item <- ifelse(grepl("Parts|Bike|Car|Tools|Video|accessories|Electronics|Electrical|TV|Players", 
                            claims$Item, ignore.case = TRUE),"Electronics", claims$Item)
claims$Item <- ifelse(grepl("Home|Items|Books|Kitchen|crafting|Dishes|Lamps|Paintings|Locks|Grills|Furniture|Outdoor|Mirrors", claims$Item, ignore.case = TRUE),"Home_use", claims$Item)

claims$Item <- ifelse(grepl("Medical|Health|Medicine|Hospital|Eyeglasses", claims$Item, 
                            ignore.case = TRUE), "Medical", claims$Item)

claims$Item <- ifelse(grepl("Other|-", claims$Item, ignore.case = TRUE), "Others", claims$Item)

claims$Item <- ifelse(grepl("Toys", claims$Item, ignore.case = TRUE), "Toys", claims$Item)

claims$Item <- ifelse(grepl("Machines|Equipment|Office|Typewriters", claims$Item, ignore.case = TRUE), "Machines", claims$Item)

claims$Item <- ifelse(grepl("Other|-|Instruments|Wigs|Material|Steel|Supplies|Tool", claims$Item, ignore.case = TRUE), "Others", claims$Item)

claims$Item <- ifelse(grepl("Phone", claims$Item, ignore.case = TRUE), "Phones", claims$Item)

claims$Item <- ifelse(grepl("Play|Sports|Sport|Game|Games", claims$Item, ignore.case = TRUE), "Games", claims$Item)

claims$Item <- ifelse(grepl("Binoculars", claims$Item, ignore.case = TRUE), "Binoculars", claims$Item)

claims$Item <- ifelse(grepl("Food|Drink|Drinks|Beer|Alcohol", claims$Item, ignore.case = TRUE), "Consumables", claims$Item)


head(claims)

##############################################################################

# EDA

# Plot the number of claims made per year and the outcome

str(claims)

claims$claim_year <- as.factor(claims$claim_year)

hist(claims$Claim.Amount)

hist(claims$Close.Amount)

barplot(table(claims$Status,claims$claim_year), xlab = "Number of Years")

write.csv(claims,"claims_formatted.csv")

# EDA Done in tableau and present in Observations folder

######## Multinomial logistic regression

str(claims)
anyNA(claims)

set.seed(123)

index = sample(1:nrow(claims), size = 0.7 * nrow(claims))

test <- claims[-index,]

dim(test)

train <- claims[index,]

anyNA(train)
anyNA(test)

train[character_vars] <- lapply(train[character_vars], as.factor)

test[character_vars] <- lapply(test[character_vars], as.factor)

names(train)
names(test)

test$Disposition <- NULL


# Multinomial logistic regression 


claim_res <- nnet(train$Disposition ~. , train[-c(1,2,8)], family="multinomial",
                  size = 100000,MaxNWts =100000)

