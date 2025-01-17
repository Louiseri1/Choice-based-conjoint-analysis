

library(tidyverse)
library(psych) # for describe() function
library(mlogit)

# Load the data 
#setwd("C:/Users/Kenne/OneDrive/Documents/MBM433_Course-Approval")
alt_data <- read.csv("alt_data.csv")
sets_data <- read.csv("sets_data.csv")
response_data <- readRDS("response_data.csv")


## ---------------------------------------------- ##
############### Exploring the data ################# 
## ---------------------------------------------- ##


### ----------------------- ###
###         alt_data        ### 
### ----------------------- ###
alt_data <- read.csv("alt_data.csv")
ncol(alt_data) # number of columns(variables) 
nrow(alt_data) # number of rows (observations)
colnames(alt_data) # names of columns(variables)
head(alt_data) # prints out the 6 first rows

# Splitting the dataset into multiple columns for each variable:
alt_data <- alt_data %>% 
  separate("product.color.brand.price.battery_life", 
           into =c("product", "color", "brand", "price", "battery_life"))
head(alt_data)
sum(is.na(alt_data))  # Checking for missing values
colnames(alt_data) # new names of columns (variables)
summary(alt_data)

# Checking the length to the variables: 
lenf = function(x) length(table(x)) 
# Above: Function for length for each variable in a dataframe
length_dis_alt_data = sapply(alt_data,lenf) # Shows the length for each variable
length_dis_alt_data

# Checking the class for each variables: 
sapply(alt_data, class)
# Converting color and brand to factors:
alt_data$brand = as.factor(alt_data$brand)
alt_data$color = as.factor(alt_data$color)
# Converting price, battery life, product to numerical: 
alt_data$price = as.numeric(alt_data$price)
alt_data$battery_life = as.numeric(alt_data$battery_life)
alt_data$product = as.numeric(alt_data$product)
sapply(alt_data, class)
summary(alt_data)

# Function for distribution of variables in a dataframe:
distribution = function(d) table(d) 
# Function for proportion of variables in a dataframe:
proportion = function(p) prop.table(table(p)) 

# Checking distribution and proportion (%) for all variables 
# except product in alt_data: 
dis_alt_data = sapply(alt_data[,-1], distribution) 
dis_alt_data 
prop_alt_data = sapply(alt_data[,-1], proportion)  
prop_alt_data

# Obtaining many descriptive statistics for all variables in the alt dataset:
describe(alt_data)


### ----------------------- ###
###       sets_data         ### 
### ----------------------- ###
sets_data <- read.csv("sets_data.csv")
ncol(sets_data) # number of columns(variables) 
nrow(sets_data) # number of rows (observations)
colnames(sets_data) # names of columns(variables)
head(sets_data, 10) # prints out the 10 first rows

# Splitting the dataset into multiple columns for each variable:
sets_data <- sets_data %>% 
  separate("setID.productID1.productID2.productID3", 
           into = c("setID", "productID1", "productID2", "productID3"))
head(sets_data)
sum(is.na(sets_data)) # Checking for missing values
colnames(sets_data) # new names of columns (variables)
summary(sets_data)

# Checking length of each variable
length_sets_data = sapply(sets_data,lenf) # Shows the length for each variable
length_sets_data
# Checking class of each variable
sapply(sets_data, class)
# Converting setID, productID1, productID2 and productID3 to numerical variables:
sets_data$setID = as.numeric(sets_data$setID)
sets_data$productID1 = as.numeric(sets_data$productID1)
sets_data$productID2 = as.numeric(sets_data$productID2)
sets_data$productID3 = as.numeric(sets_data$productID3)
sapply(sets_data, class)

# Obtaining many descriptive statistics for all variables in the sets dataset:
describe(sets_data)


### ----------------------- ###
###     response_data       ### 
### ----------------------- ###

# response_data: 
response_data <- readRDS("response_data.RDS")
dim(response_data)[1] # number of rows
dim(response_data)[2] # number of columns
names(response_data)  # names of columns(variables)
head(response_data) # prints out the 10 first rows
sum(is.na(response_data))  # Checking for missing values
summary(response_data)

# Checking length of each variable
length_response_data = sapply(response_data,lenf) # Shows the length for each variable
length_response_data
# Checking class of each variable
sapply(response_data, class)
# Converting choice, gender, region to factors: 
response_data$choice = as.factor(response_data$choice)
response_data$gender = as.factor(response_data$gender)
response_data$region = as.factor(response_data$region)
# Converting age to numeric:
response_data$age = as.numeric(response_data$age)
sapply(response_data_new, class)

# Checking distribution and proportion on response_data_new for variables
# choice, age, gender, region
dis_response_data = sapply(response_data_new[,-c(1:6,8)], distribution) 
dis_response_data
prop_response_data = sapply(response_data_new[,-c(1:6,8)], proportion)  
prop_response_data

# Obtaining many descriptive statistics for all variables in the 
# response dataset:
describe(response_data)


# ------------------------------#
###### Merging the datasets #####
# ------------------------------#
head(alt_data, 3)
head(sets_data, 3)
head(response_data, 3)


## First alternative product 
response_wide <- merge(response_data, alt_data, 
                       by.x = "prodID1", by.y = "product")
head(response_wide, 3) # prints the three first rows
colnames(response_wide)[11:14] <- c("color_1", "brand_1", 
                                    "price_1", "batterylife_1")
# Above: Renaming 11th to 14th columns

## Second alternative product
response_wide <- merge(response_wide, alt_data, 
                       by.x = "prodID2", by.y = "product")
head(response_wide, 3) # prints the three first rows
colnames(response_wide)[15:18] <- c("color_2", "brand_2", 
                                    "price_2", "batterylife_2")
# Above: Renaming 15th to 18th columns

## Third alternative product 
response_wide <- merge(response_wide, alt_data, 
                       by.x = "prodID3", by.y = "product")
head(response_wide, 3) # prints the three first rows
colnames(response_wide)[19:22] <- c("color_3", "brand_3", 
                                    "price_3", "batterylife_3")
# Above: Renaming 19th to 22nd columns

# Printing out the 10 first rows in our final dataset:
head(response_wide,10)
dim(response_wide)

# Checking for duplicated rows in the merged dataset
duplicated_rows <- sum(duplicated(response_wide))
duplicated_rows # 0 duplicated rows in the dataset


# ----------------------------------- #
#       2C - Model Validation         #
# ----------------------------------- # 
set.seed(123)
# Splitting the data into estimation and validation (75% estimation)
data_split <- initial_split(response_wide)
estimation <- training(data_split)
validation <- testing(data_split)

length(unique(response_wide$respID)) # 500

# converting the original estimation data frame into mlogit data format
estimation_dat <- mlogit.data(estimation,
                              choice = "choice", 
                              shape = "wide",
                              varying = 11:22,
                              id.var = "respID",
                              sep = "_")

# converting the original validation data frame into mlogit data format
validation_dat <- mlogit.data(validation,
                              choice = "choice", 
                              shape = "wide",
                              varying = 11:22,
                              id.var = "respID",
                              sep = "_")

### Model1
mlogit_mod1 <- mlogit(choice ~  as.factor(color) + as.factor(brand) + 
                                as.factor(price) + as.factor(batterylife) | 
                                age + gender, 
                                data = estimation_dat)
exp(coef(mlogit_mod1))  # computing odds ratio  

# estimating the predicted probabilities
# pred <- predict(response_train1, newdata = validation_dat, type = "response")
validation[,c("pred1.prob.1", "pred1.prob.2", "pred1.prob.3")] <- predict(mlogit_mod1, newdata = validation_dat, type = "response")
head(validation, 6) # printing the first six rows
# printing the first six rows for the added columns:
head(validation[, paste0("pred1.prob.", c(1, 2, 3))], 6) # alt 1 
head(validation[, c(23:25)], 6) # alt 2 
# finding the column index that contains the maximum probability
which.max(validation[1, c(23:25)]) 
choice_list <- c(1,2,3) # list of choices
# Adding predicted choice as a new column pred.choice
validation$pred.choice <- apply(validation[, c(23:25)],
                                MARGIN = 1,
                                FUN = function(x) choice_list[which.max(x)])

head(validation[, 23:26]) # print first six rows of added columns

# Creating classification table 
class_tab1 <- table(validation$pred.choice, validation$choice)
class_tab1 

# Computing the accuracy rate and saving it in a variable accuracyRate1
accuracyRate1 <- mean(validation$pred.choice == validation$choice)
accuracyRate1 # 0.47

summary(mlogit_mod1) # Pseudo-R Square from summary function
aic1 = AIC(mlogit_mod1) # Computing AIC
aic1 # 9458.584
summary(mlogit_mod1) # Likelihood ratio (LR) test from summary function


### Model 2 
# Creating a new dataframe validation2 where we have removed 
# the last four column from validation
validation2 <- validation[, 1:(ncol(validation) -4)]
# Creating a new mlogit model where we treat price and betterylife as numeric
mlogit_mod2 <- mlogit(choice ~  as.factor(color) + as.factor(brand) + 
                        price + batterylife | 
                        age + gender, 
                      data = estimation_dat)
exp(coef(mlogit_mod2))  # Computing odds ratio  

# estimating the predicted probabilities
validation2[,c("pred2.prob.1", "pred2.prob.2", "pred2.prob.3")] <- predict(mlogit_mod2, newdata = validation_dat, type = "response")
head(validation2, 6) # printing the first six rows
# printing the first six rows for the added columns
head(validation2[, paste0("pred2.prob.", c(1, 2, 3))], 6) # alt 1 
head(validation2[, c(23:25)], 6) # alt 2 
# finding the column index that contains the maximum probability
which.max(validation2[1, c(23:25)])
choice_list <- c(1,2,3) # list of choices
# predicted choice is saved in a new column pred.choice
validation2$pred2.choice <- apply(validation2[, c(23:25)],
                                  MARGIN = 1,
                                  FUN = function(x) choice_list[which.max(x)])
head(validation2[, 23:26]) # print first six rows of selected columns

# saving the classification table in a object named class_tab
class_tab2 <- table(validation2$pred2.choice, validation2$choice)
class_tab2 # printing the results

# computing the accuracy rate and saving it in a variable accuracyRate
accuracyRate2 <- mean(validation2$pred2.choice == validation2$choice)
accuracyRate2 # 0.47, so no reductioin in accuracy rate
summary(mlogit_mod2) # Pseudo-R Square and Likelihood ratio (LR) test from summary function
aic2 = AIC(mlogit_mod2) # Computing AIC
aic2

# Likelihood ratio test, mlogit_mod2 vs mlogit_mod1
lrtest(mlogit_mod2, mlogit_mod1)
# Here, mlogit_mod1 have more parameters, since the model mlogit_mod2 has more 
# attributes that are treated as a factor (nominal variable) compared to 
# mlogit_mod2. Thus, mlogit_mod2 is nested in mlogit_mod1. 
# The results shows that the Chi-squared value is -0.1419 which is 
# not significant at 5% level (p-value = 0.9976), meaning that we 
# cannot reject the null hypothesis or the full model is not better than 
# the restricted one. 
# As such, we should use the more parsimonious model, which is the model
# mlogit_mod2. 


### Model 3
# Creating a new dataframe validation3 where we have removed 
# the last four column from validation2
validation3 <- validation2[, 1:(ncol(validation) -4)]
# Creating a new mlogit model where we include region
mlogit_mod3 <- mlogit(choice ~  as.factor(color) + as.factor(brand) + 
                        price + batterylife | 
                        age + gender + region, 
                        data = estimation_dat)
exp(coef(mlogit_mod3))  # Computing odds ratio  

# estimating the predicted probabilities
validation3[,c("pred3.prob.1", "pred3.prob.2", "pred3.prob.3")] <- predict(mlogit_mod3, newdata = validation_dat, type = "response")
head(validation3, 6) # printing the first six rows
# printing the first six rows for the added columns
head(validation3[, paste0("pred3.prob.", c(1, 2, 3))], 6) # alt 1 
head(validation3[, c(22:25)], 6) # alt 3 
# finding the column index that contains the maximum probability
which.max(validation3[1, c(22:25)])
choice_list <- c(1,2,3) # list of choices
# predicted choice is saved in a new column pred.choice
validation3$pred3.choice <- apply(validation3[, c(22:25)],
                                  MARGIN = 1,
                                  FUN = function(x) choice_list[which.max(x)])
head(validation3[, 22:26]) # print first six rows of selected columns

# saving the classification table in a object named class_tab
class_tab3 <- table(validation3$pred3.choice, validation3$choice)
class_tab3  # printing the results

# computing the accuracy rate and saving it in a variable accuracyRate3
accuracyRate3 <- mean(validation3$pred3.choice == validation3$choice)
accuracyRate3 # 0.2373333 --> so we get a reduction in accuracy rate by 
# including region

summary(mlogit_mod3) # Pseudo-R Square and Likelihood ratio (LR) test from summary function
aic3 = AIC(mlogit_mod3) # Computing AIC
aic3

# Likelihood ratio test, mlogit_mod2 vs mlogit_mod3
lrtest(mlogit_mod2, mlogit_mod3)
# Here, mlogit_mod2 is nested in mlogit_mod3
# The results shows that the Chi-squared value is 23.557 which is 
# not significant at 5% level (p-value = 0.2623), meaning that we 
# cannot reject the null hypothesis or the full model with region is not 
# better than the restricted one. 
# As such, we should use the model mlogit_mod2. 


### Model 4
# Creating a new dataframe validation4 where we have removed 
# the last four column from validation3:
validation4 <- validation3[, 1:(ncol(validation) -4)]
# Creating a new mlogit model where we include non-linear term for 
# the variable batterylife (also tried including for price but that gave a error message: system is computationally singular)
mlogit_mod4 <- mlogit(choice ~  as.factor(color) + as.factor(brand) + 
                                price + batterylife + I(batterylife^2) | 
                                age + gender, 
                                data = estimation_dat)
exp(coef(mlogit_mod4))  # Computing odds ratio  

# estimating the predicted probabilities
validation4[,c("pred4.prob.1", "pred4.prob.2", "pred4.prob.3")] <- predict(mlogit_mod4, newdata = validation_dat, type = "response")
head(validation4, 6) # printing the first six rows
# printing the first six rows for the added columns
head(validation4[, paste0("pred4.prob.", c(1, 2, 3))], 6) # alt 1 
head(validation4[, c(22:25)], 6) # alt 3 
# finding the column index that contains the maximum probability
which.max(validation4[1, c(22:25)])
choice_list <- c(1,2,3) # list of choices
# predicted choice is saved in a new column pred.choice
validation4$pred4.choice <- apply(validation4[, c(22:25)],
                                  MARGIN = 1,
                                  FUN = function(x) choice_list[which.max(x)])
head(validation4[, 22:26]) # print first six rows of selected columns

# saving the classification table in a object named class_tab
class_tab4 <- table(validation4$pred4.choice, validation4$choice)
class_tab4 # printing the results

# computing the accuracy rate and saving it in a variable accuracyRate3
accuracyRate4 <- mean(validation4$pred4.choice == validation4$choice)
accuracyRate4 

summary(mlogit_mod4) # Pseudo-R Square and Likelihood ratio (LR) test from summary function
aic4 = AIC(mlogit_mod4) # Computing AIC
aic4

# Likelihood ratio test, mlogit_mod2 vs mlogit_mod3
lrtest(mlogit_mod2, mlogit_mod4)
# Here, mlogit_mod2 is nested in mlogit_mod4
# The results shows that the Chi-squared value is 0.0294 which is 
# not significant at 5% level (p-value = 0.8639), meaning that we 
# cannot reject the null hypothesis or the full model with region is not 
# better than the restricted one. 
# As such, we should use the model mlogit_mod2 were we are assuming that 
# batterylevel affect the total utility in a linear way 


# ----------------------------------- #
#      2D - Model Estimation          #
# ----------------------------------- # 

# converting response_wide to a long dataframe format which mlogit understands
# and saving it in a new data frame called estimation_data: 
estimation_data <- mlogit.data(response_wide,
                               choice = "choice", # the choice column
                               shape = "wide",  # the shape of the original data
                               varying = 11:22, # the position of the alternative-specific variables
                               id.var = "respID", # respondent ID
                               sep = "_") # separtion of variable and alternative names
head(estimation_data)

# For our model, we choose that the effects of the attributes 
# color and price should be nominal, and the effects of price and batterylevel
# should be linear/numeric. 
# Reusing the best model mlogit_mod2 from Model Validation part
mlogit_mod2 <- mlogit(choice ~  as.factor(color) + as.factor(brand) + 
                                price + batterylife | 
                                age + gender, 
                                data = estimation_dat)
exp(coef(mlogit_mod2))  # Computing odds ratio  
# All predictors have a high p-value except the predictor price, 
# which have a p-value that is practically equal to zero. Thus, price
# can be viewed as a promising predictor. 
summary(mlogit_mod2)
AIC(mlogit_mod2)


# ----------------------------------- #
#    2E - RESULT INTERPRETATIONS      #
# ----------------------------------- # 

# parameter estimates
parEst <- coef(mlogit_mod2)
parEst


## utility scores for 3 color levels
utility_color <- c(0, parEst[3:4])
plot(x = utility_color, 
     type = "b", 
     lwd = "2", 
     col = "red", 
     xlab = "Color", 
     ylab = "Utility", 
     ylim = c(-0.5, 1.0), 
     xaxt = "n", 
     main = "Color Smart Watch ") 
axis(1, at = 1:3, labels = c("Black", "Lilack", "White")) 
graphics.off()

## utility scores for 3 brand levels
utility_brand <- c(0, parEst[5:6])
# create a scatter plot
plot(x = utility_brand, 
     type = "b", 
     lwd = "2", 
     col = "red", 
     xlab = "Color", 
     ylab = "Utility", 
     ylim = c(-0.5, 0.5), 
     xaxt = "n", 
     main = "Color Smart Watch ") 
axis(1, at = 1:3, labels = c("Apple", "Samsung", "Fitbit")) 
graphics.off()


######## Not 100% sure about the two plots below since we have numerical values 
# instead of factor variables, but these do make sense for me
## utility scores for 4 price levels (numeric - continuous)
# utility_price <- c(0, parEst[7:9]) if it was stored as factor
price_list <- c(990, 2390, 3190, 6190)
utility_price <- parEst[7]*price_list
plot(x = utility_price, 
     type = "b", 
     lwd = "2", 
     col = "red", 
     xlab = "Price NOK", 
     ylab = "Utility", 
     ylim = c(-0.5, 0.5), 
     xaxt = "n", 
     main = "Purchase Price (NOK)") 
axis(1, at = 1:4, labels = c("990", "2390", "3190", "6190")) 
graphics.off()

## utility scores for 4 battery levels (numeric - continuous)
# utility_betterylevel <- c(0, parEst[10:12]) if it was stored as factor
batterylevel_list <- c(10, 18, 40, 80)
utility_batterylevel <- parEst[8]*(batterylevel_list)
plot(x = utility_batterylevel, 
     type = "b", 
     lwd = "2", 
     col = "red", 
     xlab = "Hours", 
     ylab = "Utility", 
     ylim = c(-0.1, 0.1), 
     xaxt = "n", 
     main = "Battery level (Hours)") 
axis(1, at = 1:4, labels = c("10", "18", "40", "80")) 
graphics.off()


#### Unsure about the two plots below since we have individual-specific variables
## utility scores for 3 individual-specific variable gender on two levels (male and famle)
utility_gender <- c(0, parEst[11:12])
# create a scatter plot
plot(x = utility_gender, 
     type = "b", 
     lwd = "2", 
     col = "red", 
     xlab = "gender", 
     ylab = "Utility", 
     ylim = c(-0.5, 0.5), 
     xaxt = "n", 
     main = "Gender") 
axis(1, at = 1:3, labels = c("genderind1", "genderind2", "genderind3")) 
graphics.off()

## utility scores for 3 individual-specific variable age as numeric (continuous) 
range(estimation_dat$age)
utility_age <- c(0, parEst[9:10])*(60-18)
# create a scatter plot
plot(x = utility_age, 
     type = "b", 
     lwd = "2", 
     col = "red", 
     xlab = "age", 
     ylab = "Utility", 
     ylim = c(-0.5, 0.5), 
     xaxt = "n", 
     main = "Age") 
axis(1, at = 1:3, labels = c("ageind1", "ageind2", "ageind3")) 
graphics.off()



#### Relative importance for numeric attributes ####

# Create an empty data frame "importTable"
print(parEst)

importTable <- data.frame(Attribute = c("color", "brand", "price", "batterylife", "age", "gender",  "Total"),
                          Range = c(0, 0, 0, 0, 0, 0, 0)) 
importTable

part_worths_color <- c(0, parEst[3:4])
range_color <- abs(max(part_worths_color) - min(part_worths_color))
importTable$Range[importTable$Attribute == "color"] <- range_color 

part_worths_brand <- c(0, parEst[5:6])
range_brand <- abs(max(part_worths_brand) - min(part_worths_brand))
importTable$Range[importTable$Attribute == "brand"] <- range_brand 

#part_worths_price <- c(0, parEst[7])
range_price <- abs(parEst[7]*(6190-990)) # range of price
importTable$Range[importTable$Attribute == "price"] <- range_price

range_batterylife <- abs(parEst[8]*(80-10)) # range of batterylife
importTable$Range[importTable$Attribute == "batterylife"] <- range_batterylife

# Not 100% sure of age
#range_age <- abs(parEst[9:10]*(60-18))
#part_worths_age <- c(0, parEst[9:10])
part_worths_age <- c(0, parEst[9:10])*(60-18)
range_age <- abs(max(part_worths_age) - min(part_worths_age))
importTable$Range[importTable$Attribute == "age"] <- range_age

# Not 100% sure of gender
#range_gender <- abs(parEst[11:12]) # range of gender
part_worths_gender <- c(0, parEst[11:12])
range_gender <- abs(max(part_worths_gender) - min(part_worths_gender))
importTable$Range[importTable$Attribute == "gender"] <- range_gender

# Total ranges
importTable$Range[importTable$Attribute == "Total"] <- range_color + 
  range_brand + range_price + range_batterylife + range_age + range_gender
importTable

# Relative Importance
importTable$Relative_Importance <- (importTable$Range/(range_color + range_brand + range_price + range_batterylife + range_age + range_gender))*100
importTable
importTable_desc <- importTable[order(importTable$Relative_Importance, decreasing = TRUE), ]
importTable_desc

# Bar plot of Relative Importance
barplot(height = importTable_desc$Relative_Importance[1:6], 
        horiz = TRUE, 
        col = "blue", 
        names.arg = c("price", "gender", "color", "age", "brand", "batlf"), 
        main = "Relative importance by attribute")







