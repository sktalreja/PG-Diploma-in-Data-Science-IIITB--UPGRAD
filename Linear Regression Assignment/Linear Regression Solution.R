#--------------------------------------Linear Regression - Assignment-----------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------

#Business Understanding
#A Chinese automobile company "Geely Auto" wants to enter into US market and they have contacted an automobile consulting company
#to understand the factors on which the pricing of cars depends in American Market. 
#Essentially, the company wants to know:
# .	Which variables are significant in predicting the price of a car
# .	How well those variables describe the price of a car


#Loading the required packages
library(MASS)
library(car)
library(tidyverse)

#Loading the file "cars_df_Assignment" into "cars_df" dataframe
cars_df <- read.csv("CarPrice_Assignment.csv", header = TRUE, na.strings= c("",NA)) #Converting blank cell to NA while loading the data

#Total rows in data frame
nrow(cars_df) #205 rows are threre

View(cars_df)
str(cars_df) #Structure of cars_df dataframe
#25 variables are there : 11 categorical & 14 Numeric

#------------------------------------Data Cleaning & Preparation-----------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------

#Checking the NA values in dataframe
sum(is.na(cars_df)) #No NA values there in dataframe

#Checking the blank values 
sapply(cars_df, function(x) length(which(x == ""))) #No blank value

#Checking the duplicate car id and row
sum(duplicated(cars_df)) #No duplicate row
sum(duplicated(cars_df$car_ID)) #No duplicate car

#Removing "car_ID" column from dataframe as it is not useful for model. 
cars_df <- cars_df[-1]

#Column - symboling
#-------------------
typeof(cars_df$symboling)
summary(cars_df$symboling)


#Column - CarName
#----------------
#Creating columns "car_company" & "car_model" from "CarName" as it contains the car name & model and store the dataframe in cars_df1
cars_df1 <- (separate(data = cars_df, col = CarName, into = c("car_company", "car_model"), sep = ' ')) 
cars_df1 <- cars_df1[ ,-3] #Removing car_model column

cars_df1$car_company <- str_to_lower(cars_df1$car_company) #Lowering the case of "car_company" column 

unique(factor(cars_df1$car_company)) #Checking the unique companies

#In car_company column in some rows "mazda" is misspelt as "maxda", "porsche" as "porcshce", "toyota" as "toyouta", 
#"volkswagen" as "vokswagen" & "vw". 

#Changing all these company names to valid names
cars_df1$car_company <- ifelse(cars_df1$car_company == "maxda", "mazda", ifelse(cars_df1$car_company == "porcshce", "porsche",
                       ifelse(cars_df1$car_company == "toyouta", "toyota", ifelse(cars_df1$car_company %in% c("vokswagen", "vw"),
                       "volkswagen", cars_df1$car_company))))

summary(factor(cars_df1$car_company))

#Converting "car_company" into dummy variable
dummy_1 <- data.frame(model.matrix(~car_company, data = cars_df1))
View(dummy_1)

#Removing first column 
dummy_1 <- dummy_1[-1]

#Combining the dummy variable with "cars_df1" dataframe and storing in cars_df1
cars_df1 <- cbind(cars_df1[ ,-2], dummy_1)


#Column - fueltype
#-----------------
str(cars_df1$fueltype)
summary(factor(cars_df1$fueltype))

#Let's create dummy variable for this variable
levels(cars_df1$fueltype) <- c(1,0) #0 stands for "gas" & 1 for "diesel" fuel type
cars_df1$fueltype <- as.numeric(levels(cars_df1$fueltype))[cars_df1$fueltype]
summary(factor(cars_df1$fueltype))


#Column - aspiration
#-------------------
summary(factor(cars_df1$aspiration))

#Let's create dummy variable for this variable
levels(cars_df1$aspiration) <- c(1,0) #0 stands for "turbo" & 1 for "std" aspiration
cars_df1$aspiration <- as.numeric(levels(cars_df1$aspiration))[cars_df1$aspiration]
summary(factor(cars_df1$aspiration))


#Column - doornumber
#-------------------
summary(factor(cars_df1$doornumber))

#Converting categorical variable "doornumber" into numeric variable ("two" as 2, "four" as 4)
cars_df1$doornumber <- as.numeric(ifelse(cars_df1$doornumber == "two", 2, ifelse(cars_df1$doornumber == "four", 4, NA)))
summary(factor(cars_df1$doornumber))

#Column - carbody
#--------------------
summary(factor(cars_df1$carbody))

#converting "carbody" into dummy variable
dummy_2 <- data.frame(model.matrix(~carbody, data = cars_df1))
dummy_2 <- dummy_2[-1]

#Combining the dummy variable with "cars_df1" dataframe and storing in cars_df1
cars_df1 <- cbind(cars_df1[ ,-5], dummy_2)


#Column - drivewheel
#----------------------
summary(factor(cars_df1$drivewheel))

#converting "drivewheel" into dummy variable
dummy_3 <- data.frame(model.matrix(~drivewheel, data = cars_df1))
dummy_3 <- dummy_3[-1]

#Combining the dummy variable with "cars_df1" dataframe and storing in cars_df1
cars_df1 <- cbind(cars_df1[ ,-5], dummy_3)


#Column - enginelocation  
#------------------------
summary(factor(cars_df1$enginelocation))

#Let's create dummy variable for this variable
levels(cars_df1$enginelocation) <- c(1,0) #0 stands for "rear" & 1 for "front" enginelocation
cars_df1$enginelocation <- as.numeric(levels(cars_df1$enginelocation))[cars_df1$enginelocation]
summary(factor(cars_df1$aspiration))


#Column - wheelbase  
#--------------------
typeof(cars_df1$wheelbase)
summary(cars_df1$wheelbase)
quantile(cars_df1$wheelbase, probs = seq(0,1,0.01)) #Checking the outliers
#There is no such outliers present


#Column - carlength
#-------------------
typeof(cars_df1$carlength)
summary(cars_df1$carlength)
quantile(cars_df1$carlength, probs = seq(0,1,0.01)) #Checking the outliers
#There is no such outliers present


#Column - carwidth
#-------------------
typeof(cars_df1$carwidth)
summary(cars_df1$carwidth)
quantile(cars_df1$carwidth, probs = seq(0,1,0.01)) #Checking the outliers
#There is no such outliers present


#Column - carheight
#-------------------
typeof(cars_df1$carheight)
summary(cars_df1$carheight)
quantile(cars_df1$carheight, probs = seq(0,1,0.01)) #Checking the outliers
#There is no such outliers present


#Column - curbweight
#-------------------
typeof(cars_df1$curbweight)
summary(cars_df1$curbweight)
quantile(cars_df1$curbweight, probs = seq(0,1,0.01)) #Checking the outliers
#There is no such outliers present


#Column - enginetype  
#------------------------
summary(factor(cars_df1$enginetype))

#converting "enginetype" into dummy variable
dummy_4 <- data.frame(model.matrix(~enginetype, data = cars_df1))
dummy_4 <- dummy_4[-1]

#Combining the dummy variable with "cars_df1" dataframe and storing in cars_df1
cars_df1 <- cbind(cars_df1[ ,-11], dummy_4)


#Column - cylindernumber
#-------------------
summary(factor(cars_df1$cylindernumber))

#Converting categorical variable "doornumber" into numeric variable("two" as 2, "three" as 3, "four" as 4,
#"five" as 5, "six" as 6, "eight" as 8 and "twelve" as 12)
cars_df1$cylindernumber <- as.numeric(ifelse(cars_df1$cylindernumber == "two", 2, ifelse(cars_df1$cylindernumber == "three", 3, 
                                      ifelse(cars_df1$cylindernumber == "four", 4, ifelse(cars_df1$cylindernumber == "five", 5, 
                                      ifelse(cars_df1$cylindernumber == "six", 6, ifelse(cars_df1$cylindernumber == "eight", 8,
                                      12)))))))
summary(factor(cars_df1$cylindernumber))


#Column - enginesize
#-------------------
typeof(cars_df1$enginesize)
summary(cars_df1$enginesize)
quantile(cars_df1$enginesize, probs = seq(0,1,0.01)) #Checking the outliers
#There is no such outliers present


#Column - fuelsystem  
#------------------------
summary(factor(cars_df1$fuelsystem))

#converting "fuelsystem" into dummy variable
dummy_5 <- data.frame(model.matrix(~fuelsystem, data = cars_df1))
dummy_5 <- dummy_5[-1]

#Combining the dummy variable with "cars_df1" dataframe and storing in cars_df1
cars_df1 <- cbind(cars_df1[ ,-13], dummy_5)


#Column - boreratio
#---------------------
typeof(cars_df1$boreratio)
summary(cars_df1$boreratio)
quantile(cars_df1$boreratio, probs = seq(0,1,0.01)) #Checking the outliers
#There is no such outliers present


#Column - stroke
#---------------------
typeof(cars_df1$stroke)
summary(cars_df1$stroke)
quantile(cars_df1$stroke, probs = seq(0,1,0.01)) #Checking the outliers
#There is no such outliers present


#Column - compressionratio
#--------------------------
typeof(cars_df1$compressionratio)
summary(cars_df1$compressionratio)
quantile(cars_df1$compressionratio, probs = seq(0,1,0.01)) #Checking the outliers

#There are some cars in dataframe in which compression ratio is as large as 21 & greater, generally diesel cars have such large ratio. 
#Let's check if the cars having such high compression ratio are diesel cars or not
unique(cars_df$fueltype[cars_df$compressionratio >= 21]) #So, these are diesel cars we can not ignore these compression ratio


#Column - horsepower
#---------------------
typeof(cars_df1$horsepower)
summary(cars_df1$horsepower)
quantile(cars_df1$horsepower, probs = seq(0,1,0.01)) #Checking the outliers
#It has one outlier of 288, let's check which car is this 
cars_df$CarName[cars_df$horsepower == 288] #porsche cayenne is a SUV and it has high horsepower, we can't ignore


#Column - peakrpm
#---------------------
typeof(cars_df1$peakrpm)
summary(cars_df1$peakrpm)
quantile(cars_df1$peakrpm, probs = seq(0,1,0.01)) #Checking the outliers
#There is not such outliers present


#Column - citympg
#---------------------
typeof(cars_df1$citympg)
summary(cars_df1$citympg)
quantile(cars_df1$citympg, probs = seq(0,1,0.01)) #Checking the outliers
#There is not such outliers present


#Column - highwaympg
#---------------------
typeof(cars_df1$highwaympg)
summary(cars_df1$highwaympg)
quantile(cars_df1$highwaympg, probs = seq(0,1,0.01)) #Checking the outliers
#There is not such outliers present

View(cars_df1) #See the dataframe
str(cars_df1) #There is no caategorical variable present


#----------------------------------------Model building & Evaluation--------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------

#set the seed to 100
set.seed(100)

#Creating random indices for train and test dataset
indices <- sample(1:nrow(cars_df1), 0.7*nrow(cars_df1))

#Create training data sets and taking 70% of the data for training
train_cars <- cars_df1[indices, ]

#Similarly store the rest of the observation into an object "test_cars".
test_cars <- cars_df1[-indices, ]

#Build 1st model "model_1" multilinear model
model_1 <- lm(price~., data = train_cars)
summary(model_1) #Adjusted R-squared : 0.9691 


#We have 59 variables excluding price to see, using "stepAIC" to reduce the independent variables
#Using Step AIC Method
step_cars <- stepAIC(model_1, direction = "both")
step_cars

#Storing the equation of step_cars into model_2
model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + curbweight + enginesize + boreratio + stroke + 
                horsepower + peakrpm + citympg + car_companybmw + car_companybuick + 
                car_companydodge + car_companyjaguar + car_companymazda + 
                car_companymitsubishi + car_companynissan + car_companypeugeot + 
                car_companyplymouth + car_companyporsche + car_companyrenault + 
                car_companysaab + car_companysubaru + car_companytoyota + 
                car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
                enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, 
              data = train_cars)

summary(model_2) #Adjusted R-squared:  0.9731 


# Multicollinearity checking using VIF
# Assumption : If the VIF is above 2, remove the variables as they are statistically insignificant
#Removing variables one by one with VIF > 2 and p >0.05
#-------------

vif(model_2) 

#Remove horsepower variable with 36.642101 VIF value & 0.173700 p value and store rest model in model_3

model_3 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + curbweight + enginesize + boreratio + stroke + 
                peakrpm + citympg + car_companybmw + car_companybuick + 
                car_companydodge + car_companyjaguar + car_companymazda + 
                car_companymitsubishi + car_companynissan + car_companypeugeot + 
                car_companyplymouth + car_companyporsche + car_companyrenault + 
                car_companysaab + car_companysubaru + car_companytoyota + 
                car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
                enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, 
              data = train_cars)

vif(model_3)
summary(model_3) #Adjusted R-squared:  0.9729 

#Remove carlength variable with 13.856159 VIF value & 0.116862 p value and store rest model in model_4

model_4 <- lm(formula = price ~ aspiration + enginelocation +
                carwidth + curbweight + enginesize + boreratio + stroke + 
                peakrpm + citympg + car_companybmw + car_companybuick + 
                car_companydodge + car_companyjaguar + car_companymazda + 
                car_companymitsubishi + car_companynissan + car_companypeugeot + 
                car_companyplymouth + car_companyporsche + car_companyrenault + 
                car_companysaab + car_companysubaru + car_companytoyota + 
                car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
                enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, 
              data = train_cars)

vif(model_4)
summary(model_4) #Adjusted R-squared:  0.9725 

#Remove enginetypeohc variable with 5.828816 VIF value & 0.072981 p value and store rest model in model_5

model_5 <- lm(formula = price ~ aspiration + enginelocation +
                carwidth + curbweight + enginesize + boreratio + stroke + 
                peakrpm + citympg + car_companybmw + car_companybuick + 
                car_companydodge + car_companyjaguar + car_companymazda + 
                car_companymitsubishi + car_companynissan + car_companypeugeot + 
                car_companyplymouth + car_companyporsche + car_companyrenault + 
                car_companysaab + car_companysubaru + car_companytoyota + 
                car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +
                enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, 
              data = train_cars)
vif(model_5)
summary(model_5) #Adjusted R-squared:  0.9719 

#Remove boreratio variable with 4.172390 VIF value & 0.165597 p value and store rest model in model_6

model_6 <- lm(formula = price ~ aspiration + enginelocation +
                carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + car_companybmw + car_companybuick + 
                car_companydodge + car_companyjaguar + car_companymazda + 
                car_companymitsubishi + car_companynissan + car_companypeugeot + 
                car_companyplymouth + car_companyporsche + car_companyrenault + 
                car_companysaab + car_companysubaru + car_companytoyota + 
                car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +
                enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi, 
              data = train_cars)

vif(model_6)
summary(model_6) #Adjusted R-squared:  0.9717

#Remove fuelsystemspdi variable with 2.688525 VIF value & 0.191648 p value and store rest model in model_7

model_7 <- lm(formula = price ~ aspiration + enginelocation +
                carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + car_companybmw + car_companybuick + 
                car_companydodge + car_companyjaguar + car_companymazda + 
                car_companymitsubishi + car_companynissan + car_companypeugeot + 
                car_companyplymouth + car_companyporsche + car_companyrenault + 
                car_companysaab + car_companysubaru + car_companytoyota + 
                car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +
                enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
              data = train_cars)

vif(model_7)
summary(model_7) #Adjusted R-squared:  0.9715

#Now model has no variable with high VIF(>2) and insignificant(p>0.05), removing other insignifican variable one by one
#Removing car_companysaab with p value 0.160857 and storing model in model_8

model_8 <- lm(formula = price ~ aspiration + enginelocation +
                carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + car_companybmw + car_companybuick + 
                car_companydodge + car_companyjaguar + car_companymazda + 
                car_companymitsubishi + car_companynissan + car_companypeugeot + 
                car_companyplymouth + car_companyporsche + car_companyrenault + 
                car_companysubaru + car_companytoyota + 
                car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +
                enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
              data = train_cars)
vif(model_8)
summary(model_8) #Adjusted R-squared:  0.9712

#Removing car_companypeugeot with p value 0.060313 and storing model in model_9

model_9 <- lm(formula = price ~ aspiration + enginelocation +
                carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + car_companybmw + car_companybuick + 
                car_companydodge + car_companyjaguar + car_companymazda + 
                car_companymitsubishi + car_companynissan +
                car_companyplymouth + car_companyporsche + car_companyrenault + 
                car_companysubaru + car_companytoyota + 
                car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd +
                enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
              data = train_cars)

vif(model_9)
summary(model_9) #Adjusted R-squared:  0.9706

#There is no insignificant variable present now, removing variables with high VIF value one by one and checking if Adjusted R-squared value is affecting or not

#Removing curbweight with VIF 20.995940 and checking Adjusted R-squared. Storing model in model_10

model_10 <- lm(formula = price ~ aspiration + enginelocation +
                 carwidth + enginesize + stroke + 
                 peakrpm + citympg + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota + 
                 car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + drivewheelrwd +
                 enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
               data = train_cars)

vif(model_10)
summary(model_10) #Adjusted R-squared:  0.9692 (Not much affected)

#Removing citympg which became insignificant(p value = 0.108216) and check Adjusted R-squared. Store model in model_11

model_11 <- lm(formula = price ~ aspiration + enginelocation +
                 carwidth + enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota + 
                 car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + drivewheelrwd +
                 enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
               data = train_cars)

vif(model_11)
summary(model_11) #Adjusted R-squared:  0.9688(Not much affected)

#Removing carbodysedan with VIF = 12.839651 and check Adjusted R-squared. Store model in model_12

model_12 <- lm(formula = price ~ aspiration + enginelocation +
                 carwidth + enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota + 
                 car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + drivewheelrwd +
                 enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
               data = train_cars)

vif(model_12)
summary(model_12) #Adjusted R-squared:  0.9671(Not much affected)

#Removing carbodywagon which became insignificant(p value = 0.854709)  and check Adjusted R-squared. Store model in model_13

model_13 <- lm(formula = price ~ aspiration + enginelocation +
                 carwidth + enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota + 
                 car_companyvolkswagen + carbodyhardtop + carbodyhatchback + 
                 drivewheelrwd +
                 enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
               data = train_cars)

vif(model_13)
summary(model_13) #Adjusted R-squared:  0.9673(Increased)

#Removing carbodyhardtop which became insignificant(p value = 0.137458)  and check Adjusted R-squared. Store model in model_14

model_14 <- lm(formula = price ~ aspiration + enginelocation +
                carwidth + enginesize + stroke + 
                peakrpm + car_companybmw + car_companybuick + 
                car_companydodge + car_companyjaguar + car_companymazda + 
                car_companymitsubishi + car_companynissan +
                car_companyplymouth + car_companyporsche + car_companyrenault + 
                car_companysubaru + car_companytoyota + 
                car_companyvolkswagen + carbodyhatchback + 
                drivewheelrwd +
                enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
              data = train_cars)

vif(model_14)
summary(model_14) #Adjusted R-squared:  0.967(Not much affected)

#Removing carbodyhatchback which became insignificant(p value = 0.144555)  and check Adjusted R-squared. Store model in model_15

model_15 <- lm(formula = price ~ aspiration + enginelocation +
                 carwidth + enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota + 
                 car_companyvolkswagen + 
                 drivewheelrwd +
                 enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
               data = train_cars)

vif(model_15)
summary(model_15) #Adjusted R-squared:  0.9667(Not much affected)

#Removing car_companyvolkswagen which became insignificant(p value = 0.076850)  and check Adjusted R-squared. Store model in model_16

model_16 <- lm(formula = price ~ aspiration + enginelocation +
                 carwidth + enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota +
                 drivewheelrwd +
                 enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
               data = train_cars)

vif(model_16)
summary(model_16) #Adjusted R-squared:  0.9661(Not much affected)

#Removing enginesize which has high VIF value = 8.605385 and check Adjusted R-squared. Store model in model_17

model_17 <- lm(formula = price ~ aspiration + enginelocation +
                 carwidth + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota +
                 drivewheelrwd +
                 enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
               data = train_cars)

vif(model_17)
summary(model_17) #Adjusted R-squared:  0.9165(Value is significantly reduced, ignoring this model, using model_16)

#Checking the VIF & summary of model_16 again 
vif(model_16)
summary(model_16) 

#Removing enginelocation which has high VIF value = 4.975804 and check Adjusted R-squared. Store model in model_18

model_18 <- lm(formula = price ~ aspiration +
                 carwidth + enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota +
                 drivewheelrwd +
                 enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
               data = train_cars)

vif(model_18)
summary(model_18) #Adjusted R-squared: 0.9618 (Not much affected)

#Removing carwidth which has high VIF value = 3.919438 and check Adjusted R-squared. Store model in model_19

model_19 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota +
                 drivewheelrwd +
                 enginetyperotor + fuelsystem2bbl + fuelsystemmpfi, 
               data = train_cars)

vif(model_19)
summary(model_19) #Adjusted R-squared: 0.9588(Not much affected)

#Removing fuelsystem2bbl with VIF value = 3.490256 and check Adjusted R-squared. Store model in model_20

model_20 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + car_companyrenault + 
                 car_companysubaru + car_companytoyota +
                 drivewheelrwd +
                 enginetyperotor + fuelsystemmpfi, 
               data = train_cars)
vif(model_20)
summary(model_20) #Adjusted R-squared: 0.9573 (Not much affected)

#Removing car_companyrenault which became insignificant with pvalue(0.065935) and check Adjusted R-squared. Store model in model_21

model_21 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + 
                 car_companysubaru + car_companytoyota +
                 drivewheelrwd +
                 enginetyperotor + fuelsystemmpfi, 
               data = train_cars)

vif(model_21)
summary(model_21) #Adjusted R-squared: 0.9564 (Not much affected)

#Removing fuelsystemmpfi which became insignificant(p value = 0.055765) and check Adjusted R-squared. Store model in model_22

model_22 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + 
                 car_companysubaru + car_companytoyota +
                 drivewheelrwd +
                 enginetyperotor,
               data = train_cars)

vif(model_22)
summary(model_22) #Adjusted R-squared: 0.9555 (Not much affected)

#Removing drivewheelrwd with VIF value = 2.613995 and check Adjusted R-squared. Store model in model_23

model_23 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + car_companymazda + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + 
                 car_companysubaru + car_companytoyota +
                 enginetyperotor,
               data = train_cars)
vif(model_23)
summary(model_23) #Adjusted R-squared: 0.9529 (Not much affected)


#Now there is no insignificant variable and have low VIF. Removing some of them in order of their insignificance
#Removing car_companymazda with p value = 0.016497 and check Adjusted R-squared. Store model in model_24

model_24 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + 
                 car_companymitsubishi + car_companynissan +
                 car_companyplymouth + car_companyporsche + 
                 car_companysubaru + car_companytoyota +
                 enginetyperotor,
               data = train_cars)

vif(model_24)
summary(model_24) #Adjusted R-squared: 0.951(Not much affected)

#Removing car_companynissan with p value = 0.002622 and check Adjusted R-squared. Store model in model_25

model_25 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + 
                 car_companymitsubishi +
                 car_companyplymouth + car_companyporsche + 
                 car_companysubaru + car_companytoyota +
                 enginetyperotor,
               data = train_cars)

vif(model_25)
summary(model_25) #Adjusted R-squared: 0.9478 (Not much affected)

#Removing car_companytoyota with p value = 0.003694 and check Adjusted R-squared. Store model in model_26

model_26 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companydodge + car_companyjaguar + 
                 car_companymitsubishi +
                 car_companyplymouth + car_companyporsche + 
                 car_companysubaru +
                 enginetyperotor,
               data = train_cars)

vif(model_26)
summary(model_26) #Adjusted R-squared: 0.9447(Not much affected)

#Removing car_companydodge with p value = 0.005012 and check Adjusted R-squared. Store model in model_27

model_27 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companyjaguar + 
                 car_companymitsubishi +
                 car_companyplymouth + car_companyporsche + 
                 car_companysubaru +
                 enginetyperotor,
               data = train_cars)

vif(model_27)
summary(model_27) #Adjusted R-squared: 0.9416(Not much affected)

#Removing car_companyplymouth with p value = 0.01062 and check Adjusted R-squared. Store model in model_28

model_28 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companyjaguar + 
                 car_companymitsubishi +
                 car_companyporsche + 
                 car_companysubaru +
                 enginetyperotor,
               data = train_cars)

vif(model_28)
summary(model_28) #Adjusted R-squared: 0.9391(Not much affected)

#Removing car_companymitsubishi with p value = 0.00277 and check Adjusted R-squared. Store model in model_29

model_29 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 peakrpm + car_companybmw + car_companybuick + 
                 car_companyjaguar + 
                 car_companyporsche + 
                 car_companysubaru +
                 enginetyperotor,
               data = train_cars)

vif(model_29)
summary(model_29) #Adjusted R-squared: 0.9353(Not much affected)

#Removing peakrpm with p value = 0.000211 and check Adjusted R-squared. Store model in model_30

model_30 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 car_companybmw + car_companybuick + 
                 car_companyjaguar + 
                 car_companyporsche + 
                 car_companysubaru +
                 enginetyperotor,
               data = train_cars)
vif(model_30)
summary(model_30) #Adjusted R-squared: 0.9287(Not much affected)

#Removing car_companyjaguar with p value = 0.0000133 and check Adjusted R-squared. Store model in model_31

model_31 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 car_companybmw + car_companybuick +
                 car_companyporsche + 
                 car_companysubaru +
                 enginetyperotor,
               data = train_cars)

vif(model_31)
summary(model_31) #Adjusted R-squared: 0.9183 (Not much affected)

#Removing car_companysubaru with p value = 0.000396 and check Adjusted R-squared. Store model in model_32

model_32 <- lm(formula = price ~ aspiration +
                 enginesize + stroke + 
                 car_companybmw + car_companybuick +
                 car_companyporsche +
                 enginetyperotor,
               data = train_cars)

vif(model_32)
summary(model_32) #Adjusted R-squared: 0.9109 (Not much affected)

#Removing stroke with p value = 0.00021 and check Adjusted R-squared. Store model in model_33

model_33 <- lm(formula = price ~ aspiration +
                 enginesize +
                 car_companybmw + car_companybuick +
                 car_companyporsche +
                 enginetyperotor,
               data = train_cars)

vif(model_33)
summary(model_33) #Adjusted R-squared: 0.9021 (Not much affected)

##This will be our final model(model_34)


#Model Evaluation
#------------------

#Let's predict the prices of our testing(test_cars) dataset
Predict_1 <- predict(model_33, test_cars[ , !names(test_cars) %in% c("price")])
test_cars$testprice <- Predict_1
View(test_cars)

#Checking the correlation
r1 <- cor(test_cars$price, test_cars$testprice)
r1  # 0.9045818 (Highly Correlated)

#R squared value
r1sqrd <- r1^2
r1sqrd # 0.8182682 (Good rsquare value)

#Solution 
#-----------
#model_34 is the final model with Adjusted R-squared value = 0.9021, made and evaluated and got the correlation of 0.90 & 
#Rsquare value of 0.82. 

#As per the final model, we can say that the car prices in American market are dependent on aspirations, engine size, 
#if the cars is of company of bmw, buick & porsche and enginetyperotor. 