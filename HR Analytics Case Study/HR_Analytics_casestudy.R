#Load the package
#----------------
library(tidyverse)
library(cowplot)
library(GGally)
library(car)
library(MASS)
library(e1071)
library(caret)
library(caTools)
library(corrplot)
library(ROCR)

#Business understanding and problem statement
#---------------------------------------------
# A large company named XYZ having around 4000 employees and aroung 15% of the employees leave the company and need to replaced with the
# talent pool available in the job  market. 
# The management has contracted an HR analytics firm to understand what factors they should focus on, in order to curb attrition.
# In other words, they want to know what changes they should make to their workplace, in order to get most of their employees to stay. 
# Also, they want to know which of these variables is most important and needs to be addressed right away.

# So we will be modelling the probability of attrition using a logistic regression. 

#Data understanding and Prepartion 
#----------------------------------

#Loading the data
general <- read.csv("general_data.csv", header = TRUE)
employee <- read.csv("employee_survey_data.csv", header = TRUE)
manager <- read.csv("manager_survey_data.csv", header = TRUE)
in_time <- read.csv("in_time.csv", header = TRUE)
out_time <- read.csv("out_time.csv", header = TRUE)

#Understanding and Preparing general data 
#----------------------------------------
summary(general)
#There are some columns which has same values for all observations, removing these.
general$EmployeeCount <- NULL
general$Over18 <- NULL
general$StandardHours <- NULL

str(general)

#Create Ranges for the continuous variables as per the data dictionary
general$Education <- ifelse(general$Education == 1, 'Below College', 
                              ifelse(general$Education == 2, 'College',
                                     ifelse(general$Education == 3, 'Bachelor',
                                            ifelse(general$Education == 4,'Master', 'Doctor'))))


summary(general$Age)

#Range for Age - Below 30 (Youth), 30 to 40 (Mid-Age), Above 40(Seniors)  
general$Age <- sapply(general$Age, function(x){
  if(x < 30 ){
    return("Youth")
  } else if(x >= 30 && x < 40){
    return("Mid-Age")
  } else if(x >= 40){
    return("Senior")
  }
})

summary(general$DistanceFromHome)
# Range for Distancefrom Home - 
# Within 2 Kms - Proximity
# 2 to 7 Kms - Near
# 7 to 15 Kms - Far
# More than 15 kms - VeryFar

general$DistanceFromHome <- sapply(general$DistanceFromHome, function(x){
  if(x<= 2)  {
    return("Proximity") 
  } else if(x >2 & x <=7){
    return("Near")
  } else if(x > 7 & x <=15){
    return("Far")
  } else if(x >=15){
    return("Very Far")
  }
})

#The following columns are Categorical, convert these to factors
cols <- c("Attrition", "BusinessTravel", "Department", "EducationField", "Gender", "JobRole",
          "MaritalStatus", "DistanceFromHome", "Age", "StockOptionLevel", "JobLevel", "Education")

#Convert all the character cols to factors
general[, cols] <- data.frame(apply(general[cols], 2, as.factor))

#Checking NA in general survey data
sum(is.na(general)) #28 NA 
sapply(general, function(x) sum(is.na(x))) #NumCompaniesWorked and TotalWorkingYears contains NA

#Get the mode of NumCompaniesWorked worked and replace NA with this mode value
fillncw <- names(sort(-table(general$NumCompaniesWorked)))[1]
fillncw # Mode "1"

general$NumCompaniesWorked <- ifelse(is.na(general$NumCompaniesWorked), fillncw, 
                                          general$NumCompaniesWorked)

#Get the mode of TotalWorkingYears worked and replace NA with this mode value
filltwy <- names(sort(-table(general$TotalWorkingYears)))[1]
filltwy # Mode "10"

general$TotalWorkingYears <- ifelse(is.na(general$TotalWorkingYears), filltwy, 
                                     general$TotalWorkingYears)

sum(is.na(general)) #No NA present

# Understanding and preparing employee survey data
#-------------------------------------------------
str(employee)

#Missing value treatment
sum(is.na(employee)) #Total 83 NA values are there 
sapply(employee, function(x) sum(is.na(x))) #EnvironmentSatisfaction, JobSatisfaction and WorkLifeBalance have NAs

# Get the mode of EnvironmentSatisfaction and replace "NA"s with mode value
fillsat <- names(sort(-table(employee$EnvironmentSatisfaction)))[1]
fillsat #Mode - 3

employee$EnvironmentSatisfaction <- ifelse(is.na(employee$EnvironmentSatisfaction), fillsat, 
                                           employee$EnvironmentSatisfaction)

#Get the mode of JobSatisfaction and replace "NA"s with mode value
fillJobSat <- names(sort(-table(employee$JobSatisfaction)))[1]
fillJobSat #Mode - 4
employee$JobSatisfaction <- ifelse(is.na(employee$JobSatisfaction), fillJobSat, 
                                   employee$JobSatisfaction)

#Get the mode of WorkLifeBalance and replace "NA"s with mode value
fillWorkLife <- names(sort(-table(employee$WorkLifeBalance)))[1]
fillWorkLife #Mode - 3
employee$WorkLifeBalance <- ifelse(is.na(employee$WorkLifeBalance), fillWorkLife,
                                   employee$WorkLifeBalance)

#Creating categorical variables as per data dictionary

employee$EnvironmentSatisfaction <- ifelse(employee$EnvironmentSatisfaction == 1, 'Low', 
                                            ifelse(employee$EnvironmentSatisfaction == 2, 'Medium', 
                                                   ifelse(employee$EnvironmentSatisfaction == 3, 'High', 'Very High')))

employee$JobSatisfaction <- ifelse(employee$JobSatisfaction == 1, 'Low',
                                    ifelse(employee$JobSatisfaction == 2, 'Medium',
                                           ifelse(employee$JobSatisfaction == 3, 'High', 'Very High')))

employee$WorkLifeBalance <- ifelse(employee$WorkLifeBalance == 1, 'Bad', 
                                    ifelse(employee$WorkLifeBalance == 2, 'Good', 
                                           ifelse(employee$WorkLifeBalance == 3, 'Better', 'Best')))

#Check the structure of employee survey data
str(employee)

#The following columns are Categorical, convert these to factors.
cols <- c("WorkLifeBalance", "JobSatisfaction", "EnvironmentSatisfaction")
#Convert all the character cols to factors
employee[, cols] <- data.frame(apply(employee[cols], 2, as.factor))

#Understanding and preparing manager survey data
#-----------------------------------------------
str(manager)
sum(is.na(manager)) #No NA values present

#Creating categorical variables as per data dictionary
manager$JobInvolvement <- ifelse(manager$JobInvolvement == 1, 'Low', 
                                   ifelse(manager$JobInvolvement == 2, 'Medium',
                                          ifelse(manager$JobInvolvement == 3, 'High', 'Very High')))


manager$PerformanceRating <- ifelse(manager$PerformanceRating == 1, 'Low', 
                                      ifelse(manager$PerformanceRating == 2, 'Good', 
                                             ifelse(manager$PerformanceRating == 3, 'Excellent', 'Outstanding')))

#Convert the columns to factors
manager$JobInvolvement <- as.factor(manager$JobInvolvement)
manager$PerformanceRating <- as.factor(manager$PerformanceRating)
str(manager)

#Data preparation and understanding of in_time and out_time dataframes
#---------------------------------------------------------------------

#Changing the column name of in_time & out_time
names(in_time)[1] <- "EmployeeID"
names(out_time)[1] <- "EmployeeID"

#Calculating the number of seconds have elapsed since "01-01-1970" in  in_time and out_time dataframe
in_time[-1] <- sapply(in_time[-1], function(x) as.POSIXct(x, "%d-%m-%Y %H:%M"))
in_time <- as.data.frame(in_time)
str(in_time)

out_time[-1] <- sapply(out_time[-1], function(x) as.POSIXct(x, "%d-%m-%Y %H:%M"))
out_time <- as.data.frame(out_time)
str(out_time)

#Total missing values in in_time and out_time
sum(is.na(in_time)) #109080
sum(is.na(out_time)) #109080

#These NA values are for same EmpIDs across in time and out time.
length(which(is.na(in_time)) & which(is.na(out_time)))

# Removing columns with all NAs
in_time <- in_time[, which(colSums(is.na(in_time)) < nrow(in_time))] 
out_time <- out_time[, which(colSums(is.na(out_time)) < nrow(out_time))]

#Storing EmpID as seperate Dataframe
empID <- data.frame(in_time[1])

#Difference in out_time and in_time
diff_time <- out_time[-1] - in_time[-1]

#Create an attribute the count of leaves for each Employee
diff_time$Leaves <- apply(diff_time, 1, function(x){
  sum(is.na(x))
})

#Calculate Average working hours
diff_time$AverageWorking <- apply(diff_time[-250], 1, mean, na.rm = T)

diff_time$AverageWorking <- diff_time$AverageWorking/(60*60)

#Arrange in DataFrame
work_data <- cbind(empID, diff_time$AverageWorking, diff_time$Leaves)
colnames(work_data) <- c("EmployeeID", "AverageHours", "Leaves")
str(work_data)

work_data$AverageHours <- round(work_data$AverageHours, digits = 2)

#Check the correlation between Leaves and Average Hours Worked
cor(work_data$AverageHours, work_data$Leaves)
#  --0.2503671 : There is negative correlation but not a strong one.

# Create a column for ExtraWorkingHours 
work_data$ExtraHours <- ifelse(work_data$AverageHours > 8,1,0)

# We have work_data with 4 columns now.

#Merging of dataframes
#--------------------
str(general) #4410 obs. of  21 variables
str(manager) #4410 obs. of  3 variables
str(employee) #4410 obs. of  4 variables
str(work_data) #4410 obs. of  4 variables

# Collate the data together in one single file
length(unique(general$EmployeeID)) #4410
length(unique(manager$EmployeeID)) #4410
length(unique(employee$EmployeeID)) #4410
length(unique(work_data$EmployeeID)) #4410

setdiff(general$EmployeeID, manager$EmployeeID) #Identical EmployeeID
setdiff(manager$EmployeeID, employee$EmployeeID) #Identical EmployeeID
setdiff(employee$EmployeeID, work_data$EmployeeID) #Identical EmployeeID

# Merge all the files
hrdata <- merge(general, manager, by = "EmployeeID", all = F)
hrdata <- merge(hrdata, employee, by = "EmployeeID", all = F)
hrdata <- merge(hrdata, work_data, by = "EmployeeID", all = F)

View(hrdata)

#Understanding the structure
str(hrdata) #4410 obs. of  29 variables

#Change ExtraHours to Categorical and NumCompaniesWorked & TotalWorkingYears to numerical
hrdata$ExtraHours <- as.factor(hrdata$ExtraHours)
hrdata$NumCompaniesWorked <- as.numeric(hrdata$NumCompaniesWorked)
hrdata$TotalWorkingYears <- as.numeric(hrdata$TotalWorkingYears)

#Data Preparation and exploratory data analysis
#----------------------------------------------

# Barcharts for categorical features with stacked attrition information
bar_generic_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                           legend.position="none")


# Check below later - position = fill is more visual
plot_grid(ggplot(hrdata, aes(x=Age, fill=Attrition))+ geom_bar(position = "fill"),
          ggplot(hrdata, aes(x=BusinessTravel, fill=Attrition)) + geom_bar(position = "fill")  + bar_generic_theme,
          ggplot(hrdata, aes(x=Department, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          ggplot(hrdata, aes(x=DistanceFromHome, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          ggplot(hrdata, aes(x=Education, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          ggplot(hrdata, aes(x=EducationField, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          align = "h")


plot_grid(ggplot(hrdata, aes(x=Gender, fill=Attrition))+ geom_bar(position = "fill"),
          ggplot(hrdata, aes(x=JobRole, fill=Attrition)) + geom_bar(position = "fill")  + bar_generic_theme,
          ggplot(hrdata, aes(x=MaritalStatus, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          ggplot(hrdata, aes(x=StockOptionLevel, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          ggplot(hrdata, aes(x=JobInvolvement, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          ggplot(hrdata, aes(x=JobLevel, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          align = "h")

plot_grid(ggplot(hrdata, aes(x=PerformanceRating, fill=Attrition))+ geom_bar(position = "fill"),
          ggplot(hrdata, aes(x=EnvironmentSatisfaction, fill=Attrition)) + geom_bar(position = "fill")  + bar_generic_theme,
          ggplot(hrdata, aes(x=JobSatisfaction, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          ggplot(hrdata, aes(x=WorkLifeBalance, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          ggplot(hrdata, aes(x=ExtraHours, fill = Attrition)) + geom_bar(position = "fill") + bar_generic_theme,
          align = "h")

# Reveals Attrition is more for 
# Age(Youth - 18 to 30 years), BusinessTravel(Travel_Frequently), Department(HumanResources),
# EducationField(HumanResources), JobRole(Research_Director), MaritalStatus(Single), 
# JobInvolvement(Low), EnvironmentSatisfaction(Low), JobSatisfaction(Low), WorkLifeBalance(Bad),
# ExtraHours

#Attrition moderately depends on Education (more attritions for "College")

###### Histograms and Box-plots for numerical and continuous variables
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

############ Outlier treatment for Numerical data #############
treatOutlier <- function(column){
  qnt <- quantile(column, probs=c(.25, .75), na.rm = T)
  caps <- quantile(column, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(column, na.rm = T)
  column[column < (qnt[1] - H)] <- caps[1]
  column[column > (qnt[2] + H)] <- caps[2]
  return (column)
}

#Monthly Income
plot_grid(ggplot(hrdata, aes(MonthlyIncome))+ geom_histogram(binwidth =10000),
          ggplot(hrdata, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
summary(hrdata$MonthlyIncome)

quantile(hrdata$MonthlyIncome, seq(0,1, 0.01))
# As there are outliers, fixing the values max
#Consideration : The outliers income which we got are exaggerated 

hrdata$MonthlyIncome <- treatOutlier(hrdata$MonthlyIncome)
boxplot(hrdata$MonthlyIncome)

#NumCompaniesWorked
plot_grid(ggplot(hrdata, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 1),
          ggplot(hrdata, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) #OUtliers are there
#Consideration : The ouliers NumCompaniesWorked are exaggerated

hrdata$NumCompaniesWorked <- treatOutlier(hrdata$NumCompaniesWorked)

#PercentSalaryHike
plot_grid(ggplot(hrdata, aes(PercentSalaryHike))+ geom_histogram(binwidth = 1),
          ggplot(hrdata, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# No outliers

#TotalWorkingYears
plot_grid(ggplot(hrdata, aes(TotalWorkingYears))+ geom_histogram(binwidth = 2),
          ggplot(hrdata, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) #Outliers are there

#Consideration : The ouliers TotalWorkingYears are exaggerated
#Treat outliers
hrdata$TotalWorkingYears <- treatOutlier(hrdata$TotalWorkingYears)

#TrainingTimesLastYear
plot_grid(ggplot(hrdata, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 0.5),
          ggplot(hrdata, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) #Outliers are there

#Consideration : The ouliers TrainingTimesLastYear are exaggerated
#Treat outliers
hrdata$TrainingTimesLastYear <- treatOutlier(hrdata$TrainingTimesLastYear)

#YearsAtCompany
plot_grid(ggplot(hrdata, aes(YearsAtCompany))+ geom_histogram(binwidth = 1),
          ggplot(hrdata, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Consideration : The ouliers YearsAtCompany are exaggerated
#Treat Outliers
hrdata$YearsAtCompany <- treatOutlier(hrdata$YearsAtCompany)

#YearsSinceLastPromotion
plot_grid(ggplot(hrdata, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1),
          ggplot(hrdata, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Consideration : The ouliers YearsSinceLastPromotion are exaggerated
#Treat Outliers
hrdata$YearsSinceLastPromotion <- treatOutlier(hrdata$YearsSinceLastPromotion)

#YearsWithCurrManager
plot_grid(ggplot(hrdata, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 1),
          ggplot(hrdata, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Consideration : The ouliers YearsWithCurrManager are exaggerated
#Treat Outliers
hrdata$YearsWithCurrManager <- treatOutlier(hrdata$YearsWithCurrManager)

#AverageHours
plot_grid(ggplot(hrdata, aes(AverageHours))+ geom_histogram(binwidth = 0.5),
          ggplot(hrdata, aes(x="",y=AverageHours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Consideration : The ouliers AverageHours are exaggerated
#Treat Outliers
hrdata$AverageHours <- treatOutlier(hrdata$AverageHours)

#Leaves
plot_grid(ggplot(hrdata, aes(Leaves))+ geom_histogram(binwidth = 1.5),
          ggplot(hrdata, aes(x="",y=Leaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#No outliers

#Checking correlation amoung numerical variables
M <- hrdata[, c("Leaves", "AverageHours", "YearsWithCurrManager", "YearsSinceLastPromotion",
                "YearsAtCompany", "TrainingTimesLastYear", "TotalWorkingYears", "PercentSalaryHike",
                "NumCompaniesWorked", "MonthlyIncome")]
corrplot(cor(M), method = "circle")

#Strong correlation exists in following variables:
# 1. Years with current manager and Years since last promotion
# 2. Years with current manager and Years at company
# 3. Total working years and Years at company

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

#Data Preparation & Feature standardisation

# Bringing the Attrition variable in the correct format
hrdata$Attrition <- ifelse(hrdata$Attrition =="Yes", 1, 0)

#Missing values
sapply(hrdata, function(x) sum(is.na(x))) # No missing values
str(hrdata)

#Normalising continuous features
hrdata$MonthlyIncome <-scale(hrdata$MonthlyIncome)
hrdata$NumCompaniesWorked <- scale(hrdata$NumCompaniesWorked)
hrdata$PercentSalaryHike <- scale(hrdata$PercentSalaryHike)
hrdata$TotalWorkingYears <- scale(hrdata$TotalWorkingYears)
hrdata$TrainingTimesLastYear <- scale(hrdata$TrainingTimesLastYear)
hrdata$YearsAtCompany <- scale(hrdata$YearsAtCompany)
hrdata$YearsSinceLastPromotion <- scale(hrdata$YearsSinceLastPromotion)
hrdata$YearsWithCurrManager <- scale(hrdata$YearsWithCurrManager)
hrdata$AverageHours <- scale(hrdata$AverageHours)
hrdata$Leaves <- scale(hrdata$Leaves)

# Checking attrition rate of prospect employees
round(prop.table(table(hrdata$Attrition)), 4) #About 16.16 % Attrition rate

# Creating dataframes of categorical and numerical features
hrdata_cat <- hrdata[ , sapply(hrdata, is.factor)]
hrdata_num <- hrdata[ , sapply(hrdata, is.numeric)]
hrdata_num <- hrdata_num[-1] #Removing employeed id varible

#Creating dummy variables of factor attributes
dummies <- data.frame(sapply(hrdata_cat, 
                             function(x) data.frame(model.matrix(~x-1, data = hrdata_cat)) [ ,-1]))

#Final Dataset
hrdata_final <- cbind(hrdata_num, dummies)
View(hrdata_final)

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#Model Building and Evulation
#----------------------------

#splitting the data between train and test
set.seed(100)
indices = sample(1:nrow(hrdata_final), 0.7*nrow(hrdata_final))

train = hrdata_final[indices,]
test = hrdata_final[-(indices),]

#Model Building
#--------------
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC: 2093.3, Null deviance: 2747.7, Residual deviance: 1971.3

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

## Removing multicollinearity through VIF check
vif(model_2)

#Removing variables with VIF values > 2 and have high Pr value > 0.5
#There is no such variable which satisfy above condition

#Removing variables based on insignificance

#Removing JobInvolvement.xVery.High as have high Pr-value
model_3 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + Age.xSenior + 
                 Age.xYouth + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + JobInvolvement.xLow + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")

summary(model_3)
vif(model_3)

#Removing JobInvolvement.xLow as it has highest p-value among all variables
model_4 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + Age.xSenior + 
                 Age.xYouth + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")
summary(model_4)
vif(model_4)

#Removing Age.xSenior
model_5 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager +  
                 Age.xYouth + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")

summary(model_5)
vif(model_5)

#Removing Education.xDoctor
model_6 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager +  
                 Age.xYouth + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")
summary(model_6)
vif(model_6)

#Removing JobLevel.x5
model_7 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager +  
                 Age.xYouth + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")
summary(model_7)
vif(model_7)


#Removing JobRole.xResearch.Director
model_8 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager +  
                 Age.xYouth + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xManufacturing.Director +  
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")
summary(model_8)
vif(model_8) #AIC - 2057.9

# There is no insignificant variable present, removing variables with high VIF value and checking AIC, it should not increase considerably

#Removing EducationField.xLife.Sciences
model_9 <-  glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")

summary(model_9) #AIC - 2080.5
vif(model_9)

#Removing BusinessTravel.xTravel_Frequently
model_10 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")

summary(model_10) #AIC - 2127.5
vif(model_10)

#Removing WorkLifeBalance.xBetter 
model_11 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")

summary(model_11)  #AIC: 2156.3
vif(model_11)

#Removing MaritalStatus.xSingle
model_12 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried +
                  StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                  WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")

summary(model_12)
vif(model_12) #AIC: 2211.9

#No such variables present with high VIF value >2, removing insignificant variables

#Removing WorkLifeBalance.xBest
model_13 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried +
                  StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xGood + ExtraHours, data = train, family = "binomial")

summary(model_13) #AIC : 2210.1
vif(model_13)

#Removing WorkLifeBalance.xGood 
model_14 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried +
                  StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_14) #AIC : 2208.3
vif(model_14)

#Removing EducationField.xMarketing
model_15 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried +
                  StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_15) #AIC : 2208
vif(model_15)

#Removing EducationField.xOther
model_16 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  EducationField.xMedical + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried +
                  StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_16) #AIC : 2208.1
vif(model_16)

#Removing EducationField.xMedical
model_17 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried +
                  StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_17) #AIC: 2207.7
vif(model_17)

#Removing StockOptionLevel.x1 
model_18 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_18) #AIC: 2209.3
vif(model_18)

#Removing EducationField.xTechnical.Degree
model_19 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xMarried +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_19) #AIC: 2211.3
vif(model_19)

#Removing JobRole.xSales.Executive 
model_20 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director +  
                  MaritalStatus.xMarried +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_20) #AIC: 2212.6
vif(model_20)

#Removing BusinessTravel.xTravel_Rarely as it has considerably high Pr value as compared to other variables
model_21 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + 
                  JobRole.xManufacturing.Director +  
                  MaritalStatus.xMarried +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_21) #AIC: 2219.9
vif(model_21)

#Removing EnvironmentSatisfaction.xVery.High as it has considerably high Pr value as compared to other variables
model_22 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + 
                  JobRole.xManufacturing.Director +  
                  MaritalStatus.xMarried +
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_22) #AIC: 2228.5
vif(model_22)

#Removing TrainingTimesLastYear as it has considerably high Pr value as compared to other variables
model_23 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + 
                  JobRole.xManufacturing.Director +  
                  MaritalStatus.xMarried +
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_23) #AIC: 2237.1
vif(model_23)

#Removing JobSatisfaction.xLow as it has considerably high Pr value as compared to other variables
model_24 <- glm(Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  Age.xYouth + 
                  JobRole.xManufacturing.Director +  
                  MaritalStatus.xMarried +
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + 
                  ExtraHours, data = train, family = "binomial")

summary(model_24) #AIC: 2245.4
vif(model_24)

##############################################################
#All the variables have low p values (significant).
#There are 10 significant variables

final_model <- model_24

#Model Evaluation
#----------------
#Predict probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

summary(test_pred)
# Assigned probabilities are in the range from 0.001 to 0.899

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#Accuracy - 85.48%
#Sensitivity - 23.67%
#Specificity - 96.95%

#Taking cutoff of 40%
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#Accuracy - 84.73%
#Sensitivity - 37.68%
#Specificity - 93.45%

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#Plotting accuracy, sensitivity and sprecificity for all the probabilites
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff

# Let's choose a cutoff value of 0.1616 for final model
test_cutoff_attrition <- factor(ifelse(test_pred >=0.1616, "Yes", "No"))
table(test_cutoff_attrition)

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1] 

sens <- conf_final$byClass[1] 

spec <- conf_final$byClass[2]

acc #Accuracy - 0.6855631 

sens #Sensitivity - 0.6908213 

spec #Specificity - 0.6845878
View(test)

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition == 'Yes',1,0)
table(test_cutoff_attrition)
test_actual_attrition <- ifelse(test_actual_attrition == 'Yes',1,0)
table(test_actual_attrition)

#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
plot(performance_measures_test, col = "red", main = "ROC curve")

library(Epi)
ROC(form = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
      YearsSinceLastPromotion + YearsWithCurrManager +  
      Age.xYouth + 
      JobRole.xManufacturing.Director +  
      MaritalStatus.xMarried +
      EnvironmentSatisfaction.xLow + 
      JobSatisfaction.xVery.High + 
      ExtraHours, data= test, plot = "ROC")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.3754091

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile

# If we choose this model, sort all the Employees according to Probability
# and contact top 30% in this sorted list, we will be able to catch 69.56% of the Employees
# that are going to leave the organization.

write.csv(Attrition_decile, "fiel.csv")

Attrition_decile$Gain
random_model <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
Attrition_decile <- cbind(Attrition_decile, random_model)

ggplot(Attrition_decile, aes(x= Attrition_decile$bucket)) + 
  geom_line(aes(y = Attrition_decile$random_model, color = "Attrition_decile$perfect_model")) +
  geom_point(data = Attrition_decile, aes(x= Attrition_decile$bucket, y=Attrition_decile$random_model)) +
  geom_line(aes(y = Attrition_decile$Gain, color = "Attrition_decile$Gain"), show.legend = F) +
  geom_point(data = Attrition_decile, aes(x= Attrition_decile$bucket, y=Attrition_decile$Gain))
  


ggplot(Attrition_decile, aes(x= Attrition_decile$bucket, y = Attrition_decile$Cumlift)) + geom_line()


#Result from the final model
# Variables which are most important for attrition are :
# 1. NumCompaniesWorked - Number of companies worked
# 2. TotalWorkingYears - Total working years of experience
# 3. YearsSinceLastPromotion - Years since last promotion
# 4. Age - Age of the employee
# 5. Job Role - Role of the job
# 6. Marital Status - Marital Status of the employee
# 7. EnvironmentSatisfaction- Environment satisfaction of the employee
# 8. Job Satisfaction - Job Satisfaction of the employee
# 9. ExtraHours - Extra Hours 
# 10 YearsWithCurrManager - Years with current manager 


#################### Plot the different factors and their relative coefficients ########

factors <- c("JobRole.xManufacturingDirector", "JobSatisfaction.xVeryHigh", "TotalWorkingYears",
"YearsWithCurrManager", "MaritalStatus.xMarried", "NumCompaniesWorked", 
"YearsSinceLastPromotion", "Age.xYouth", "EnvironmentSatisfaction.xLow",
"ExtraHours")

coeff <- c(-0.82, -0.79, -0.59, -0.41, -0.39, 0.38, 0.48, 0.71, 0.95, 1.64)

final <- as.data.frame(cbind(factors, coeff))

str(final)


ggplot(final, aes(y = reorder(coeff), x = factors, fill = factors)) +
  geom_bar(stat = "identity", show.legend = F)+ 
  scale_fill_manual("legend", values = c("JobRole.xManufacturingDirector" = "green",
                                         "JobSatisfaction.xVeryHigh" = "green",
                                         "TotalWorkingYears" = "green",
                                         "YearsWithCurrManager" = "green",
                                         "MaritalStatus.xMarried" = "green",
                                         "NumCompaniesWorked" = "red",
                                         "YearsSinceLastPromotion" = "red",
                                         "Age.xYouth" = "red",
                                         "EnvironmentSatisfaction.xLow" = "red",
                                         "ExtraHours" = "red")) +
  coord_flip() +
  ylab("Coefficients") + xlab("Factors")+
  theme(axis.title.x = element_text(colour="black", size=30), axis.text.x = element_text(colour="blue", size = 22), axis.title.y = element_text(colour="black", size=30, angle = 90), axis.text.y  = element_text(colour="blue", size = 26), plot.title = element_text(colour="red", size=30))

#################