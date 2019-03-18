
##-------------------------------------------------------------------------------------------------------------------------------------
##--------------------------------------- BFSI Capstone Project-----------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

# Loading required packages
library(tidyverse)
library(caTools)
library(MASS)
library(caret)
library(dummies)
library(randomForest)
library(ROSE)
#------------------------
## Business Understanding
#------------------------

# CredX is a leading credit card provider that gets thousands of credit card applicants every year. 
# But in the past few years, it has experienced an increase in credit loss. The CEO believes that 
# the best strategy to mitigate credit risk is to 'acquire the right customers'

# In this project, we will help CredX identify the right customers using predictive models. 
# Using past data of the bank's applicants, we will determine the factors affecting credit risk, 
# create strategies to mitigate the acquisition risk and assess the financial benefit of your project

#--------------------------------
## Data Loading and Understanding
#--------------------------------

# There are two data sets in this project - demographic and credit bureau data. 
# Both files contain a performance tag which represents whether the applicant has gone 90 days past due or worse in the past 12-months (i.e. defaulted) 
# after getting a credit card.

# Demographic/application data: This is obtained from the information provided by the applicants at the time of credit card application. 
# It contains customer-level information on age, gender, income, marital status, etc.

# Loading the Demographic Data
demographic_data <- read.csv("Demographic data.csv", header = TRUE, na.strings = c("NA", ""))
str(demographic_data) #71295 Rows, 12 columns

head(sample(demographic_data)) #First few rows
tail(demographic_data) #Last few rows

data.frame(prop.table(table(demographic_data$Performance.Tag))) %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste((as.character(round((Freq * 100),2))), "%", sep = " ")) , vjust = -0.5, size = 3) +
  theme_bw() +
  xlab("Performance Tag") + ylab("Total Percentage") + labs(fill = "Performance Tag", title = "Performance Tag v/s Total Percentage")

prop.table(table(demographic_data$Performance.Tag)) #4.22 % Total Default Rate

# Data is imbalanced as we can see that only 4.22% default rate there 

# Credit bureau: This is taken from the credit bureau and contains variables such as 'number of times 30 DPD or worse in last 3/6/12 months', 
# 'outstanding balance', 'number of trades', etc.

# Loading the Credit Beureau Data
credit_data <- read.csv("Credit Bureau data.csv", header = TRUE, na.strings = c("NA", ""))
str(credit_data) #71295 Rows, 19 Columns

head(credit_data) #First few Rows
tail(credit_data) #Last few rows

data.frame(prop.table(table(credit_data$Performance.Tag))) %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste((as.character(round((Freq * 100),2))), "%", sep = " ")) , vjust = -0.5, size = 3) +
  theme_bw() +
  xlab("Performance Tag") + ylab("Total Percentage") + labs(fill = "Performance Tag", title = "Performance Tag v/s Total Percentage")

prop.table(table(credit_data$Performance.Tag)) #4.22 % Total Default Rate - Imbalanced data

#-------------------
## Data Preparation
#-------------------

## >>>> Columns Names <<<<<

# Checking the column names of both the dataframes
names(demographic_data)
names(credit_data)

# Column names of both the dataframes are not appropriate, replacing "." with "_". 
names(demographic_data) <- str_replace_all(names(demographic_data), "\\.","_")
names(demographic_data)  # Column Names are appropriate now

names(credit_data) <- str_replace_all(names(credit_data), "\\.","_")
names(credit_data) # Column Names are appropriate now

## >>>> Duplicate Application ID <<<<<

# Checking the duplicate Application ID in both the dataframes
sum(duplicated(demographic_data$Application_ID)) # 3 Duplicate value
sum(duplicated(credit_data$Application_ID)) # 3 Duplicate value

# Removing rows having duplicate Application ID
demographic_data <- demographic_data[-which(duplicated(demographic_data$Application_ID)), ]
sum(duplicated(demographic_data$Application_ID)) #No Duplicate Ids now

credit_data <- credit_data[-which(duplicated(credit_data$Application_ID)), ]
sum(duplicated(credit_data$Application_ID)) #No Duplicate Ids now

sapply(demographic_data, function(x) sum(is.na(x)))

## >>>> Merging of dataframes <<<<<

# Joining the Demographic and Credit dataframes and creating a new dataframe "Master_df". 

#Performance Tag column of both credit and demographic data is identical
identical(demographic_data$Performance_Tag, credit_data$Performance_Tag) 

# Merging the demographic and credit data in one dataframe
master_df <- merge(demographic_data[,1:(ncol(demographic_data) - 1)], credit_data, by = "Application_ID")
str(master_df)

## >>>> Checking and removing NA Values <<<<<

# Checking and removing NA Values in master_df data
sum(is.na(master_df)) #3180 total NA values
sapply(master_df, function(x) sum(is.na(x))) #Checking NA in specific columns
summary(master_df)

# Gender has only 2 NA values, removing that rows
master_df <- master_df[-which(is.na(master_df$Gender)), ]
summary(master_df)

# Marital Status has only 6 NA values, removing that rows
master_df <- master_df[-which(is.na(master_df$Marital_Status__at_the_time_of_application_)), ]
summary(master_df)

# Number of Dependents has only 3 NA values, removing that rows
master_df <- master_df[-which(is.na(master_df$No_of_dependents)), ]
summary(master_df)

# Profession has only 14 NA values, removing that rows
master_df <- master_df[-which(is.na(master_df$Profession)), ]
summary(master_df)

# Type of Residence has only 8 NA values, removing that rows
master_df <- master_df[-which(is.na(master_df$Type_of_residence)), ]
summary(master_df)

# No_of_trades_opened_in_last_6_months has only 1 NA value, removing that rows
master_df <- master_df[-which(is.na(master_df$No_of_trades_opened_in_last_6_months)), ]
summary(master_df)

# It looks that Presence_of_open_home_loan, Outstanding_Balance have altogether missing NA values. 
# These are the cases in which there is no-hit in the credit bureau
View(master_df[which(is.na(master_df$Outstanding_Balance)), ]) # "Average CC Utilisation", "Presence of home loan" and "Outstanding Value"
                                                               # have all NA values (271) altogether, which is 271/nrow(master_df) * 100 = 0.38 %. 
                                                               # We can remove these rows. 

master_df <- master_df[-which(is.na(master_df$Outstanding_Balance)), ]
summary(master_df)

# Avgas_CC_Utilization_in_last_12_months has 785 NA values and we will be keeping this NA values.
# Education has 119 NA values, keeping that rows

## >>>> Checking Invalid data  <<<<<

summary(master_df)

# Age column has negative values, checking the rows in which Age is less than 18 as the applicants age should not be less than 18 
nrow(master_df[master_df$Age < 18,]) #Total 62 rows, removing such rows from the dataframe

master_df <- master_df[-which(master_df$Age < 18), ]
summary(master_df)

# We can see that the "Income" variable contains negative value
quantile(master_df$Income,seq(0,1,0.001))

nrow(master_df[master_df$Income < 0,]) #Total 80 rows

# Replacing Negative values in Income with 0.2% quantile value  = 3.000
master_df$Income[which(master_df$Income < 3.000)] <- 3.000

## >>>> Outlier Treatment for continous variables  <<<<<

summary(master_df)

# Checking Income column

# Plotting boxplot for the "Income" variable 
ggplot(data = master_df, aes(x = "", y = Income)) + 
  geom_boxplot(alpha=0.4, fill = "blue") +
  coord_flip() +
  labs(title = "Boxplot for Income Column") +
  theme_bw() #Normalized data - As mean is in centre

# Plotting Histogram to see if the Income Column is normalized
ggplot(data = master_df, aes(x = Income)) + 
  geom_histogram(alpha=0.4, fill = "blue", bins = 10) +
  labs(title = "Histogram for Income Column") +
  theme_bw() #Normalized data

quantile(master_df$Income, seq(0,1,0.002)) # There is no such outliers present

# Checking Outstanding Balance column

# Plotting boxplot for the "Outstanding Balance" variable 
ggplot(data = master_df, aes(x = "", y = Outstanding_Balance)) +
  geom_boxplot(alpha=0.4, fill = "blue") +
  coord_flip() +
  labs(title = "Boxplot for Outstanding Balance Column") +
  theme_bw() # It looks that Outstanding Balance is not normalised

# Plotting Histogram to see if the Outstanding Balance Column is normalized
ggplot(data = master_df, aes(x = Outstanding_Balance)) + 
  geom_histogram(alpha=0.4, fill = "blue", bins = 10) +
  labs(title = "Histogram for Outstanding Balance Column") +
  theme_bw() #Outstanding Balance is not normalised 

quantile(master_df$Outstanding_Balance, seq(0,1,0.001)) 

# Capping at 99.9% the percentile (4795079)
master_df$Outstanding_Balance[which(master_df$Outstanding_Balance > 4795079)] <- 4795079
summary(master_df$Outstanding_Balance)

# Checking Average CC Utilization column

# Plotting boxplot for the "Average CC Utilization" variable 
ggplot(data = master_df, aes(x = "", y = Avgas_CC_Utilization_in_last_12_months)) +
  geom_boxplot(alpha=0.4, fill = "blue") +
  coord_flip() +
  labs(title = "Boxplot for Average CC Utilization Column") +
  theme_bw() # It looks that Average CC Utilization is not normalised

# Plotting Histogram to see if the Average CC Utilization Column is normalized
ggplot(data = master_df, aes(x = Avgas_CC_Utilization_in_last_12_months)) + 
  geom_histogram(alpha=0.4, fill = "blue", bins = 10) +
  labs(title = "Histogram for Average CC Utilization Column") +
  theme_bw() #Average CC Utilization is not normalised 

quantile(master_df$Avgas_CC_Utilization_in_last_12_months, seq(0,1,0.001), na.rm = TRUE)

## >>>> Seperating Rejected applications from master data  <<<<<
summary(master_df) # Total 1423 rows have NA values in Performance_Tag, these are rejected applications

rejected_df <- master_df[which(is.na(master_df$Performance_Tag)), ]
summary(rejected_df)

# Removing rejected applications from master dataframe
master_df <- master_df[-which(is.na(master_df$Performance_Tag)), ]
summary(master_df)

dim(master_df) #69501 rows and 29 columns

#-------------------
## Data Exploration
#-------------------

# Copying the dataframe "master_df" into another dataframe "master_df_e"

master_df_e <- master_df
summary(master_df_e)

#-->>>>> Age
summary(master_df_e$Age)
ggplot(data = master_df, aes(x = Age)) + 
  geom_histogram(alpha=0.4, fill = "blue", bins = 10) +
  labs(title = "Histogram for Age Column") +
  theme_bw() #Age is normalised 

quantile(master_df_e$Age, seq(0,1,0.02))

# Binnning the Age variable in 18,30,40,50 and 65 and storing in new binning_age column
master_df_e$binning_age <- as.factor(cut(master_df_e$Age, include.lowest = TRUE, breaks = c(18, 30, 40, 50, 65)))
summary(master_df_e$binning_age)

# Writing a function "plot_response" to plot the default with individual columns
plot_response <- function(cat_var, var_name) {
  a <- aggregate(Performance_Tag ~ cat_var, master_df_e, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  colnames(agg_response) <- c(var_name, "default_rate","No.of_Prospect")
  agg_response[, 2] <- paste(as.character(round(agg_response[, 2], 2) * 100), "%", sep = " ")
  ggplot(agg_response, aes(agg_response[, 1], count, label = default_rate , fill = agg_response[, 1])) + 
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_text(size = 3, vjust = -0.5) + xlab(var_name) + ylab("Count") + theme_bw() +
    labs(fill = var_name, title = paste("Default rate for", var_name,  "categories")) +
    guides(fill = FALSE)
}

plot_response(master_df_e$binning_age, "Age")

# It looks that the Age does not have any impact on default as it is same 4% for all category

#-->>>>> Gender
summary(master_df_e$Gender)

plot_response(master_df_e$Gender, "Gender")

# It looks that the Gender does not have any impact on default as it is same 4% for all category

#-->>>>> Marital status
summary(master_df_e$Marital_Status__at_the_time_of_application_)

plot_response(master_df_e$Marital_Status__at_the_time_of_application_, "Marital Status")

# It looks that the Marital Status does not have any impact on default as it is same 4% for all category

#-->>>>> Number of Dependents
summary(master_df_e$No_of_dependents)
master_df_e$No_of_dependents <- as.factor(master_df_e$No_of_dependents) #Making it as factor

plot_response(master_df_e$No_of_dependents, "Number of Dependents")

# It looks that the Number of Dependents does not have any impact on default as it is same 4% for category

#-->>>>> Income
summary(master_df_e$Income)

ggplot(data = master_df, aes(x = Income)) + 
  geom_histogram(alpha=0.4, fill = "blue", bins = 5) +
  labs(title = "Histogram for Income Column") +
  theme_bw() #Income column is normalised 

quantile(master_df_e$Income, seq(0,1,0.01))

# Binnning the Income variable in 3,7,12,17,22,27,33,38,43,50,60  and storing in new binning_income column
master_df_e$binning_Income <- as.factor(cut(master_df_e$Income, include.lowest = TRUE, breaks = c(3,7,12,17,22,27,33,38,43,50,60)))
summary(master_df_e$binning_Income)

plot_response(master_df_e$binning_Income, "Income Slab")

# It looks that the Income does not have any impact on default as it is near about 4% for all category

#-->>>>> Education
summary(master_df_e$Education)

plot_response(master_df_e$Education, "Education")

# It looks that the Education does not have any impact on default as it is same 4% and 7% for others category

#-->>>>> Profession
summary(master_df_e$Profession)

plot_response(master_df_e$Profession, "Profession")

# It looks that the Profession does not have any impact on default as it is near about 4% for all category

#-->>>>> Type of Residence
summary(master_df_e$Type_of_residence)

plot_response(master_df_e$Type_of_residence, "Type of Residence")

# It looks that the Type of Residence does not have any impact on default as it is near about 4% to 5% for all category

#-->>>>> Number of Months in current residence
summary(master_df_e$No_of_months_in_current_residence)
ggplot(data = master_df, aes(x = No_of_months_in_current_residence)) + 
  geom_histogram(alpha=0.4, fill = "blue", bins = 5) +
  labs(title = "Histogram for Number of Months in current residence Column") +
  theme_bw() #Number of Months in current residence Column is not normalised

quantile(master_df_e$No_of_months_in_current_residence, seq(0,1,0.01))

# Binnning the Number of Months in current residence variable in 6,12,25,50,61,75,90,105,115,126 and storing in new binning_nof_months_in_cr column
master_df_e$binning_nof_months_in_cr <- as.factor(cut(master_df_e$No_of_months_in_current_residence, include.lowest = TRUE, breaks = c(6,12,25,50,61,75,90,105,115,126)))
summary(master_df_e$binning_nof_months_in_cr)

plot_response(master_df_e$binning_nof_months_in_cr, "Number of months in current residence")

# It looks that the Number of months in current residence does not have any impact on default as it is near about 4% to 5% for all category

#-->>>>> Number of Months in current company
summary(master_df_e$No_of_months_in_current_company)

ggplot(data = master_df, aes(x = No_of_months_in_current_company)) +
  geom_histogram(alpha=0.4, fill = "blue", bins = 5) +
  labs(title = "Histogram for Number of Months in current company Column") +
  theme_bw() #Number of Months in current company Column is normalised

quantile(master_df_e$No_of_months_in_current_company, seq(0,1,0.01))

# Binnning the Number of Months in current company variable in 3,7,17,24,34,43,51,60,133 and storing in new binning_nof_months_in_cur column
master_df_e$binning_nof_months_in_cur <- as.factor(cut(master_df_e$No_of_months_in_current_company, include.lowest = TRUE, breaks = c(3,7,17,24,34,43,51,60,133)))
summary(master_df_e$binning_nof_months_in_cur)

plot_response(master_df_e$binning_nof_months_in_cur, "Number of months in current company")

# It looks that the Number of months in current company does not have any impact on default as it is near about 4% to 5% for all category

#-->>>>> No_of_times_90_DPD_or_worse_in_last_6_months

summary(factor(master_df_e$No_of_times_90_DPD_or_worse_in_last_6_months))

# Making it as factor 
master_df_e$No_of_times_90_DPD_or_worse_in_last_6_months <- as.factor(master_df_e$No_of_times_90_DPD_or_worse_in_last_6_months)

plot_response(master_df_e$No_of_times_90_DPD_or_worse_in_last_6_months, "No of times 90 DPD or worse in last 6 months")

# No of times 90 DPD or worse in last 6 months column plays critical role in default of customer as the people who have not paid dues since 90 days for more than 1 time in last 6 months
# have considerably high default rate

#-->>>>> No_of_times_60_DPD_or_worse_in_last_6_months

summary(factor(master_df_e$No_of_times_60_DPD_or_worse_in_last_6_months))

# Making it as factor 
master_df_e$No_of_times_60_DPD_or_worse_in_last_6_months <- as.factor(master_df_e$No_of_times_60_DPD_or_worse_in_last_6_months)

plot_response(master_df_e$No_of_times_60_DPD_or_worse_in_last_6_months, "No of times 60 DPD or worse in last 6 months")

# No of times 60 DPD or worse in last 6 months plays critical role in default of customer as the people who have not paid dues since 60 days for more than 1 time in last 6 months
# have considerably high default rate

#-->>>>> No_of_times_30_DPD_or_worse_in_last_6_months

summary(factor(master_df_e$No_of_times_30_DPD_or_worse_in_last_6_months))

# Making it as factor 
master_df_e$No_of_times_30_DPD_or_worse_in_last_6_months <- as.factor(master_df_e$No_of_times_30_DPD_or_worse_in_last_6_months)

plot_response(master_df_e$No_of_times_30_DPD_or_worse_in_last_6_months, "No of times 30 DPD or worse in last 6 months")

# No of times 30 DPD or worse in last 6 months plays critical role in default of customer as the people who have not paid dues since 30 days for more than 1 time in last 6 months
# have considerably high default rate

#-->>>>> No_of_times_90_DPD_or_worse_in_last_12_months
summary(factor(master_df_e$No_of_times_90_DPD_or_worse_in_last_12_months))

# Making it as factor 
master_df_e$No_of_times_90_DPD_or_worse_in_last_12_months <- as.factor(master_df_e$No_of_times_90_DPD_or_worse_in_last_12_months)

plot_response(master_df_e$No_of_times_90_DPD_or_worse_in_last_12_months, "No of times 90 DPD or worse in last 12 months")

# No of times 90 DPD or worse in last 12 months plays critical role in default of customer as the people who have not paid dues since 90 days for more than 1 time in last 12 months
# have considerably high default rate

#-->>>>> No_of_times_60_DPD_or_worse_in_last_12_months

summary(factor(master_df_e$No_of_times_60_DPD_or_worse_in_last_12_months))

# Making it as factor 
master_df_e$No_of_times_60_DPD_or_worse_in_last_12_months <- as.factor(master_df_e$No_of_times_60_DPD_or_worse_in_last_12_months)

plot_response(master_df_e$No_of_times_60_DPD_or_worse_in_last_12_months, "No of times 60 DPD or worse in last 12 months")

# No of times 60 DPD or worse in last 12 months plays critical role in default of customer as the people who have not paid dues since 60 days for more than 1 time in last 12 months
# have considerably high default rate

#-->>>>> No_of_times_30_DPD_or_worse_in_last_12_months

summary(factor(master_df_e$No_of_times_30_DPD_or_worse_in_last_12_months))

# Making it as factor 
master_df_e$No_of_times_30_DPD_or_worse_in_last_12_months <- as.factor(master_df_e$No_of_times_30_DPD_or_worse_in_last_12_months)

plot_response(master_df_e$No_of_times_30_DPD_or_worse_in_last_12_months, "No of times 30 DPD or worse in last 12 months")

# No of times 30 DPD or worse in last 12 months plays critical role in default of customer as the people who have not paid dues since 30 days for more than 1 time in last 12 months
# have considerably high default rate

#-->>>>> Avgas_CC_Utilization_in_last_12_months

summary(master_df_e$Avgas_CC_Utilization_in_last_12_months)

ggplot(data = master_df, aes(x = Avgas_CC_Utilization_in_last_12_months)) +
  geom_histogram(alpha=0.4, fill = "blue", bins = 5) +
  labs(title = "Histogram for Average CC utilization in last 12 months Column") +
  theme_bw() #Right Skewed for Average CC Utilization in last 12 months

quantile(master_df_e$Avgas_CC_Utilization_in_last_12_months, seq(0,1,0.01), na.rm = T)

# Binnning the Avgas_CC_Utilization_in_last_12_months variable in 0,6,8,11,15,25,45,65,84,113 and storing in new binning_Avgas_CC_Utilization column
master_df_e$binning_Avgas_CC_Utilization <- as.factor(cut(master_df_e$Avgas_CC_Utilization_in_last_12_months, include.lowest = TRUE, breaks = c(0,6,8,11,15,25,45,65,84,113)))
summary(master_df_e$binning_Avgas_CC_Utilization)

plot_response(master_df_e$binning_Avgas_CC_Utilization, "Average utilization of credit card by customer")

# It looks that Average utilization of credit card by customer play important role in default of customer

#-->>>>> No_of_trades_opened_in_last_6_months

summary(factor(master_df_e$No_of_trades_opened_in_last_6_months))

# Making it as factor 
master_df_e$No_of_trades_opened_in_last_6_months <- as.factor(master_df_e$No_of_trades_opened_in_last_6_months)

plot_response(master_df_e$No_of_trades_opened_in_last_6_months, "No of trades opened in last 6 months")

# It looks that the Number of times the customer has done the trades in last 6 months plays important role in default of customer

#-->>>>> No_of_trades_opened_in_last_12_months

summary(factor(master_df_e$No_of_trades_opened_in_last_12_months))

# Making it as factor 
master_df_e$No_of_trades_opened_in_last_12_months <- as.factor(master_df_e$No_of_trades_opened_in_last_12_months)

plot_response(master_df_e$No_of_trades_opened_in_last_12_months, "No of trades opened in last 12 months")

# Number of times the customer has done the trades in last 12 months plays important role in default of customer

#-->>>>> No_of_PL_trades_opened_in_last_6_months

summary(factor(master_df_e$No_of_PL_trades_opened_in_last_6_months))

# Making it as factor 
master_df_e$No_of_PL_trades_opened_in_last_6_months <- as.factor(master_df_e$No_of_PL_trades_opened_in_last_6_months)

plot_response(master_df_e$No_of_PL_trades_opened_in_last_6_months, "No of PL trades opened in last 6 months")

# It looks that "No of PL trades in last 6 month of customer" plays important role in default of customers

#-->>>>> No_of_PL_trades_opened_in_last_12_months

summary(factor(master_df_e$No_of_PL_trades_opened_in_last_12_months))

# Making it as factor 
master_df_e$No_of_PL_trades_opened_in_last_12_months <- as.factor(master_df_e$No_of_PL_trades_opened_in_last_12_months)

plot_response(master_df_e$No_of_PL_trades_opened_in_last_12_months, "No of PL trades opened in last 12 months")

# It looks that "No of PL trades in last 12 month  of customer" plays important role in default of customers

#-->>>>> No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_

summary(factor(master_df_e$No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_))

# Making it as factor 
master_df_e$No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_ <- as.factor(master_df_e$No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_)

plot_response(master_df_e$No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_, "No of Inquiries in last 6 months excluding home auto loans")

# It looks that "No of Inquiries in last 6 months excluding home auto loans" plays much important role in default of customers

#-->>>>> No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_

summary(factor(master_df_e$No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_))

# Making it as factor 
master_df_e$No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ <- as.factor(master_df_e$No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_)

plot_response(master_df_e$No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_, "No of Inquiries in last 12 months excluding home auto loans")

# It looks that "No of Inquiries in last 12 months excluding home auto loans" plays much important role in default of customers

#-->>>>> Presence_of_open_home_loan

summary(factor(master_df_e$Presence_of_open_home_loan))

# Making it as factor 
master_df_e$Presence_of_open_home_loan <- as.factor(master_df_e$Presence_of_open_home_loan) #Making it as factor

plot_response(master_df_e$Presence_of_open_home_loan, "Presence of Open Home Loan")

# It looks that "Presence of Open Home Loan" do not play much important role in default of customers

#-->>>>> Outstanding Balance

summary(master_df_e$Outstanding_Balance)

ggplot(data = master_df, aes(x = Outstanding_Balance)) +
  geom_histogram(alpha=0.4, fill = "blue", bins = 5) +
  labs(title = "Histogram for Outstanding Balance Column") +
  theme_bw() #Right Skewed

quantile(master_df_e$Outstanding_Balance, seq(0,1,0.01))

# Binnning the Outstanding Balance variable in 0, 6849, 25577, 208434, 386886, 459636, 585448, 650357, 774255, 836260, 972385, 1143229,
# 1357262, 2926078, 2960934, 2990121, 3283108, 3651033, 4795079 and storing in new binning_outs_balance column
master_df_e$binning_outs_balance <- as.factor(cut(master_df_e$Outstanding_Balance, include.lowest = TRUE, breaks = c(0, 6849, 25577, 208434, 386886, 459636, 585448, 650357, 774255, 836260, 972385, 1143229,
                                                                                                                     1357262, 2926078, 2960934, 2990121, 3283108, 3651033, 4795079)))
summary(master_df_e$binning_outs_balance)

plot_response(master_df_e$binning_outs_balance, "Outstanding Balance")

# It looks that "Outstanding Balance" plays important role in default of customers

#-->>>>> Total_No_of_Trades

summary(factor(master_df_e$Total_No_of_Trades))

# Making it as factor 
master_df_e$Total_No_of_Trades <- as.factor(master_df_e$Total_No_of_Trades)

plot_response(master_df_e$Total_No_of_Trades, "Total No of Trades")

# It looks that "Total No of Trades" plays important role in default of customers

#-->>>>> Presence of open auto loan

summary(factor(master_df_e$Presence_of_open_auto_loan))

# Making it as factor 
master_df_e$Presence_of_open_auto_loan <- as.factor(master_df_e$Presence_of_open_auto_loan)

plot_response(factor(master_df_e$Presence_of_open_auto_loan), "Presence of open auto loan")

# It looks that "Presence of Auto Loan" do not play much important role in default of customers


### Important variables as per exploratory data analysis are : 
# 1. No_of_times_90_DPD_or_worse_in_last_6_months
# 2. No_of_times_60_DPD_or_worse_in_last_6_months
# 3. No_of_times_30_DPD_or_worse_in_last_6_months
# 4. No_of_times_90_DPD_or_worse_in_last_12_months
# 5. No_of_times_60_DPD_or_worse_in_last_12_months
# 6. No_of_times_30_DPD_or_worse_in_last_12_months
# 7. No_of_trades_opened_in_last_6_months
# 8. No_of_trades_opened_in_last_12_months
# 9. Total_No_of_Trades
# 10. Outstanding_Balance
# 11. Average utilization of credit card by customer in the last 12 months
# 12. Number of times the customer has opened the trades in last 12 months
# 13. Number of PL trades opened in last 6 months
# 14. Number of Inquiries in last 6 months excluding home auto loan
# 15. Number of Inquiries in last 12 months excluding home auto loan

# There is no demographic details important for defualt as per exploration

#------------------------------------
## WOE Analysis and Information Value
#------------------------------------

# "WOE and Information Values" are widely used statistical technique to screen variables in the credit risk modelling projects. 
# They help to explore data and screen variables. We are going to use this technique to identify important variables and also verify
# the important variables which we have identified from exploratory data analysis. 

# We will be using Information Package to calculate Information value. This package expects 1 as good customer in output variable. 
# But, in our dataframe 1 siginifies bad customer, so we will be changing that. 

library(Information)

summary(master_df)

master_df$Presence_of_open_home_loan <- as.factor(master_df$Presence_of_open_home_loan) #Making it as factor
master_df$Presence_of_open_auto_loan <- as.factor(master_df$Presence_of_open_auto_loan) #Making it as factor

# Replacing 0 with 1 and vice versa in Peformance Tag column as our focus is to choose right customers
master_df$Performance_Tag <- ifelse(master_df$Performance_Tag == 1,0,1)
prop.table(table(master_df$Performance_Tag)) # About 96 % applicants are good customers

# Copying the dataframe into master_df_WI 
master_df_WI <- master_df
summary(master_df_WI)

# Calculating Information Value and WOE, not using Application ID
master_df_WI <- create_infotables(data = master_df_WI[-1], y = "Performance_Tag", parallel = TRUE)

# Passing Information Value of variables into new dataframe IV_Value
IV_Value = data.frame(master_df_WI$Summary)
IV_Value

#If the IV value is following ,then varible is :
#Less than 0.02, "not useful"
#   0.02 to 0.1, "weak"
#    0.1 to 0.3, "medium" 
#    0.3 to 0.5, "strong"
#         > 0.5, "suspicious"

for (i in 1:nrow(IV_Value)) {
  ifelse(IV_Value$IV[i] < 0.02, IV_Value$importance[i] <- "Not Useful", 
  ifelse(IV_Value$IV[i] >= 0.02 & IV_Value$IV[i] < 0.1, IV_Value$importance[i] <- "Weak",
  ifelse(IV_Value$IV[i] >= 0.1  & IV_Value$IV[i] < 0.3, IV_Value$importance[i] <- "Medium",
  ifelse(IV_Value$IV[i] >= 0.3  & IV_Value$IV[i] < 0.5, IV_Value$importance[i] <- "Strong", 
         IV_Value$importance[i] <- "Suspicious"))))
}

IV_Value #Looking at the information values and variables 

# We will be using "Strong" and "Medium" importance variable for model building 

library(RColorBrewer)
colourCount = length(unique(IV_Value$Variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))


ggplot(IV_Value, aes(x = reorder(Variable, -IV), y = IV, fill = getPalette(colourCount))) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(IV,3)), vjust = -0.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  labs(x = "Columns", y = "Information Value") +
  ggtitle("Information Value of columns") +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red", size = 1) +
  theme(legend.position = "none")

## Total 15 relevant columns 

# Based on chart and dataframe, we can see that following variables are relavant for model building in decreasing order of Information Value

#                                                      Variable       IV         Importance
#                          Avgas_CC_Utilization_in_last_12_months 3.118158e-01     Strong
#                           No_of_trades_opened_in_last_12_months 2.992422e-01     Medium
#                        No_of_PL_trades_opened_in_last_12_months 2.976330e-01     Medium
# No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ 2.965392e-01     Medium
#                                             Outstanding_Balance 2.469674e-01     Medium
#                    No_of_times_30_DPD_or_worse_in_last_6_months 2.420549e-01     Medium
#                                              Total_No_of_Trades 2.378859e-01     Medium
#                         No_of_PL_trades_opened_in_last_6_months 2.203559e-01     Medium
#                   No_of_times_90_DPD_or_worse_in_last_12_months 2.142245e-01     Medium
#                    No_of_times_60_DPD_or_worse_in_last_6_months 2.062044e-01     Medium
#  No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_ 2.052807e-01     Medium
#                   No_of_times_30_DPD_or_worse_in_last_12_months 1.987550e-01     Medium
#                            No_of_trades_opened_in_last_6_months 1.864486e-01     Medium
#                   No_of_times_60_DPD_or_worse_in_last_12_months 1.858931e-01     Medium
#                    No_of_times_90_DPD_or_worse_in_last_6_months 1.603274e-01     Medium

# Plotting WOE Scores of relavant variables
plot_infotables(master_df_WI, c("Avgas_CC_Utilization_in_last_12_months", 
                                "No_of_trades_opened_in_last_12_months",
                                "No_of_PL_trades_opened_in_last_12_months",
                                "No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_"), same_scales = FALSE)

plot_infotables(master_df_WI, c("Outstanding_Balance", 
                                "No_of_times_30_DPD_or_worse_in_last_6_months",
                                "Total_No_of_Trades",
                                "No_of_PL_trades_opened_in_last_6_months"), same_scales = FALSE)

plot_infotables(master_df_WI, c("No_of_times_90_DPD_or_worse_in_last_12_months", 
                                "No_of_times_60_DPD_or_worse_in_last_6_months",
                                "No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_",
                                "No_of_times_30_DPD_or_worse_in_last_12_months"), same_scales = FALSE)

plot_infotables(master_df_WI, c("No_of_times_60_DPD_or_worse_in_last_12_months", 
                                "No_of_trades_opened_in_last_6_months",
                                "No_of_times_90_DPD_or_worse_in_last_6_months"), same_scales = FALSE)

# Storing Important variables in "imp_col" dataframe

imp_col <- IV_Value$Variable[which(IV_Value$importance %in% c("Strong", "Medium"))]

# Removing columns which are not important from the dataframe "master_df" and storing important columns in master_df_woe 
str(master_df)

master_df_woe  <- master_df[ ,c(1,which(colnames(master_df) %in% imp_col),29)]
str(master_df_woe) # Total 17 Columns 

# We will be replacing the actual values with WOE values in master_df_woe dataframe

# We are using scorecard package 
library(scorecard)

str(master_df_woe)

bins <- woebin(master_df_woe[-1], y = "Performance_Tag") # Creating the bins and ignoring Application ID

master_df_woe <- woebin_ply(master_df_woe, bins)
View(master_df_woe)

#----------------------------------------------------
## Model Building and Evaluation with unbalanced data 
#----------------------------------------------------

## >>>>>> Model using both credit and demographic data with WOE values

## With Logistic Regression :
#----------------------------

# Splitting the dataframe into train and test dataframe
set.seed(149)
master_df_woe$Performance_Tag <- as.factor(ifelse(master_df_woe$Performance_Tag == 1, "yes", "no"))

split_indices <- sample.split(master_df_woe$Performance_Tag, SplitRatio = 0.70) #Taking 70% of the data for training

train_woe <- master_df_woe[split_indices, -1] #Not using Application ID 
test_woe <- master_df_woe[!split_indices, -1] 

nrow(train_woe) / nrow(master_df_woe)

## -- >>>> Model Building 

# Using Logistic Regression Algorithm to build the model
model_1 <- glm(Performance_Tag ~., family = "binomial", data = train_woe)
summary(model_1)

# Using stepwise algorithm for removing insignificant variables
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2)

# stepAIC has removed some insignificant variables
# Checking the vif values and Pr values to remove insignificant variables
vif(model_2)

# Removing "No_of_times_60_DPD_or_worse_in_last_12_months_woe" as it is not significant
model_3 <- glm(formula = Performance_Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                  No_of_times_30_DPD_or_worse_in_last_12_months_woe + 
                 Avgas_CC_Utilization_in_last_12_months_woe + No_of_trades_opened_in_last_12_months_woe + 
                 No_of_Inquiries_in_last_12_months__excluding_home___auto_loans__woe, 
               family = "binomial", data = train_woe)


summary(model_3)
vif(model_3)

# Removing "No_of_times_30_DPD_or_worse_in_last_12_months_woe" as it is not much significant
model_4 <-  glm(formula = Performance_Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                  Avgas_CC_Utilization_in_last_12_months_woe + No_of_trades_opened_in_last_12_months_woe + 
                  No_of_Inquiries_in_last_12_months__excluding_home___auto_loans__woe, 
                family = "binomial", data = train_woe)

summary(model_4)
vif(model_4)

# All the variables are significant, storing in final model
final_model <- model_4

summary(final_model)

# Important Varables 
varImp(final_model)

#No_of_times_30_DPD_or_worse_in_last_6_months_woe                    7.956472
#Avgas_CC_Utilization_in_last_12_months_woe                          7.844766
#No_of_trades_opened_in_last_12_months_woe                           3.181634
#No_of_Inquiries_in_last_12_months__excluding_home___auto_loans__woe 4.275368

## -- >>>> Model Evaluation

# Predicting probabilities of response for the test data
prediction_logit <- predict(final_model, newdata = test_woe[,-1], type = "response")
summary(prediction_logit)

# Finding the optimal cutoff
perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(prediction_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_woe$Performance_Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.9128 to 0.9854 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(0.9128,0.9854,length=100)
OUT = matrix(0,100,3)

for(i in 1:100) {
  OUT[i,] = perform_fn(s[i])
} 

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("right",col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

print(OUT)

# Cutoff 
cutoff_lr <- s[which(abs(OUT[,1] - OUT[,2]) < 0.05099)]
cutoff_lr # 95.10% to 95.33% 

cutoff_lr <- cutoff_lr[2] #Taking 2nd one
cutoff_lr 

# Let's choose a cutoff value of 95.24% for final model
predicted_response <- factor(ifelse(prediction_logit >= cutoff_lr, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test_woe$Performance_Tag, positive = "yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc  #65.49%
sens #65.71%
spec #60.61%

# Plotting ROC curve
roc.curve(test_woe$Performance_Tag, predicted_response, plotit = TRUE)

# We can see that the area under the curve is 63.2% and we will try to improve further

# Loading the library
library(ROCR)

# KS Statistics - Test Data
test_logit_ub_PT <- ifelse(predicted_response == 'yes',1,0)
table(test_logit_ub_PT)
test_actual_logit_ub_PT <- ifelse(test_woe$Performance_Tag == 'yes',1,0)
table(test_actual_logit_ub_PT)

# on testing  data
pred_obj_lr_ub <- prediction(test_logit_ub_PT, test_actual_logit_ub_PT)
prf_msr_tst_lr_ub <- performance(pred_obj_lr_ub, "tpr", "fpr")
ks_table_lr_ub <- attr(prf_msr_tst_lr_ub, "y.values")[[1]] - 
  (attr(prf_msr_tst_lr_ub, "x.values")[[1]])

max(ks_table_lr_ub) #0.2632 - Not Bad

# Now we will make model using Random Forest algorithm. 

## With Random Forest :
#----------------------

# For Random Forest, we will not be using WOE values. 

str(master_df) # We will be using only significant variables as per information value. 

# Checking NA values 
lapply(master_df, function(x) sum(is.na(x))) # Avgas_CC_Utilization_in_last_12_months utilization has 745 NA and Education has 117 NA values

table(master_df$Performance_Tag)

# We will be using MICE package to impute the missing values 
library(mice)

master_df_impute <- mice(master_df[-1], m = 1, maxit = 50, meth = 'cart', seed = 501, parallel = TRUE)
master_df_1 <- complete(master_df_impute)
 
sum(is.na(master_df_1))  # No NA values now 
str(master_df_1)

# Splitting the dataframe into train and test dataframe
set.seed(19088)

# We will be changing the output variable, Non-Defaulter will be "yes" and vice-versa
master_df_1$Performance_Tag <- as.factor(ifelse(master_df_1$Performance_Tag == 1, "yes", "no"))
table(master_df_1$Performance_Tag)

# Taking only top Important Vriables for model building
master_df_2 <- master_df_1[ ,c(which(colnames(master_df_1) %in% imp_col),28)]

## Checking correlation among numerical variables
str(master_df_2)

numeric_var <- sapply(na.omit(master_df_2[,-c(16,17)]), is.numeric)
corr_matrix <- cor(na.omit(master_df_2[-c(16,17)])[ ,numeric_var])

corr_matrix <- round(corr_matrix, 1)

set.seed(197)
library(ggcorrplot)
ggcorrplot(corr_matrix, hc.order = TRUE, outline.col = "white", type = "lower", lab = TRUE)

split_indices_rf <- sample.split(master_df_2$Performance_Tag, SplitRatio = 0.70) #Taking 70% of the data for training

train_rf <- master_df_2[split_indices_rf, ]

str(train_rf)
test_rf  <- master_df_2[!split_indices_rf, ]

nrow(train_rf) / nrow(master_df_2) #70%

## -- >>>> Model Building 
library(mlbench)

# Prepare resampling method

control <- trainControl(method="repeatedcv", number = 5,repeats = 3, classProbs=TRUE, summaryFunction=twoClassSummary)

set.seed(88)

table(train_rf$Performance_Tag)

model_rf <- train(Performance_Tag~., data=train_rf, method="rf", metric="ROC", trControl=control, type = "Classification",
                  allowParallel=TRUE)

# Display the results
print(model_rf)

# Variable Importance
varImp(model_rf)

# Following Variables are important
#Outstanding_Balance                                             100.000
#Avgas_CC_Utilization_in_last_12_months                           81.174
#No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_  40.702
#Total_No_of_Trades                                               37.445
#No_of_trades_opened_in_last_12_months                            32.134
#No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_   27.631
#No_of_PL_trades_opened_in_last_12_months                         21.748
#No_of_trades_opened_in_last_6_months                             21.179
#No_of_PL_trades_opened_in_last_6_months                          15.228
#No_of_times_30_DPD_or_worse_in_last_12_months                    12.290
#No_of_times_90_DPD_or_worse_in_last_12_months                    11.000
#No_of_times_60_DPD_or_worse_in_last_12_months                    10.270
#No_of_times_30_DPD_or_worse_in_last_6_months                      6.733
#No_of_times_60_DPD_or_worse_in_last_6_months                      2.321
#No_of_times_90_DPD_or_worse_in_last_6_months                      0.000


## -- >>>> Model Evaluation

dim(test_rf)

# Predicting probabilities of response for the test data
prediction_rf <- predict(model_rf, newdata = test_rf[,-16], type = "prob")
summary(prediction_rf)

# Finding the optimal cutoff
perform_fn_rf_1 <- function(cutoff) 
{
  predicted_response <- factor(ifelse(prediction_rf[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance_Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out_r <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out_r) <- c("sensitivity", "specificity", "accuracy")
  return(out_r)
}

# Creating cutoff values from 0.656 to 0.99 for plotting and initiallizing a matrix of 100 X 4.
s_r = seq(0.374,0.99,length=100)
out_r = matrix(0,100,3)

for(i in 1:100) {
  out_r[i,] = perform_fn_rf_1(s_r[i])
} 

print(out_r)

# plotting cutoffs 
plot(s_r, out_r[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_r,out_r[,2],col="darkgreen",lwd=2)
lines(s_r,out_r[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Cutoff 
cutoff_rf <- s_r[which(abs(out_r[,1] - out_r[,2]) < 0.05642)]
cutoff_rf 

cutoff_rf <- cutoff_rf[1] # Taking first 
cutoff_rf # 0.9798788

# Let's choose a cutoff value of 95.86 % 
predicted_response_rf <- factor(ifelse(prediction_rf[, 2] >= cutoff_rf, "yes", "no"))
conf_final_rf <- confusionMatrix(predicted_response_rf, test_rf$Performance_Tag, positive = "yes")
acc_rf  <- conf_final_rf$overall[1]
sens_rf <- conf_final_rf$byClass[1]
spec_rf <- conf_final_rf$byClass[2]

acc_rf  #65.44%
sens_rf #65.68%
spec_rf #60.00%

# Plotting ROC curve
roc.curve(test_rf$Performance_Tag, predicted_response_rf, plotit = TRUE)
# 62.9 % Area under curve

#--------------------------------------------------
## Model Building and Evaluation with balanced data 
#--------------------------------------------------

# Now we will be balancing the data using ROSE(Random Over-Sampling Examples) package. 
# The ROSE package provides functions to deal with binary classification problems in the 
# presence of imbalanced classes. Artificial balanced samples are generated according 
# to a smoothed bootstrap approach and allow for aiding both the phases of estimation 
# and accuracy evaluation of a binary classifier in the presence of a rare class.

library(ROSE)

# Balancing the train data for logistic regression model 
str(train_woe)
table(train_woe$Performance_Tag)

train_woe_blncd <- ROSE(Performance_Tag ~., data = train_woe, seed = 9111)$data
prop.table(table(train_woe_blncd$Performance_Tag)) #Data is balanced now 

# Balancing the train data for random forest model 
str(train_rf)
table(train_rf$Performance_Tag)

train_rf_blncd <- ROSE(Performance_Tag ~., data = train_rf, seed = 28 , p = 0.5)$data

prop.table(table(train_rf_blncd$Performance_Tag)) #Data is balanced now 

## With Logistic Regression :
#----------------------------

# We are already having train and test data, now we will build the model

## -- >>>> Model Building 

# Using Logistic Regression Algorithm to build the model
model_b_1 <- glm(Performance_Tag ~., family = "binomial", data = train_woe_blncd)
summary(model_b_1)

# Using stepwise algorithm for removing insignificant variables
model_b_2 <- stepAIC(model_b_1, direction = "both")
summary(model_b_2)

# stepAIC has removed some insignificant variables

# Checking the vif values and Pr values to remove insignificant variables
vif(model_b_2)

# Removing "No_of_times_60_DPD_or_worse_in_last_6_months_woe" as it is not that significant

model_b_3 <- glm(Performance_Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                   No_of_times_30_DPD_or_worse_in_last_12_months_woe + Avgas_CC_Utilization_in_last_12_months_woe + 
                   No_of_trades_opened_in_last_12_months_woe + No_of_PL_trades_opened_in_last_6_months_woe + 
                   No_of_PL_trades_opened_in_last_12_months_woe + No_of_Inquiries_in_last_6_months__excluding_home___auto_loans__woe + 
                   No_of_Inquiries_in_last_12_months__excluding_home___auto_loans__woe + 
                   Outstanding_Balance_woe + Total_No_of_Trades_woe, family = "binomial", 
                 data = train_woe_blncd)
summary(model_b_3)
vif(model_b_3)

# Removing "Total_No_of_Trades_woe" as it is not that significant

model_b_4 <- glm(Performance_Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                   No_of_times_30_DPD_or_worse_in_last_12_months_woe + Avgas_CC_Utilization_in_last_12_months_woe + 
                   No_of_trades_opened_in_last_12_months_woe + No_of_PL_trades_opened_in_last_6_months_woe + 
                   No_of_PL_trades_opened_in_last_12_months_woe + No_of_Inquiries_in_last_6_months__excluding_home___auto_loans__woe + 
                   No_of_Inquiries_in_last_12_months__excluding_home___auto_loans__woe + 
                   Outstanding_Balance_woe, family = "binomial", 
                 data = train_woe_blncd)

summary(model_b_4)
vif(model_b_4)

# All the variables are significant, storing in final model
final_model_b <- model_b_4
summary(final_model_b)

# Variable Importance
varImp(final_model_b)

#No_of_times_30_DPD_or_worse_in_last_6_months_woe                     6.887617
#No_of_times_90_DPD_or_worse_in_last_12_months_woe                    3.631556
#No_of_times_30_DPD_or_worse_in_last_12_months_woe                    7.264261
#Avgas_CC_Utilization_in_last_12_months_woe                          18.160860
#No_of_trades_opened_in_last_12_months_woe                            5.175931
#No_of_PL_trades_opened_in_last_6_months_woe                          3.532983
#No_of_PL_trades_opened_in_last_12_months_woe                         4.174721
#No_of_Inquiries_in_last_6_months__excluding_home___auto_loans__woe   3.691165
#No_of_Inquiries_in_last_12_months__excluding_home___auto_loans__woe  7.702044
#Outstanding_Balance_woe                                              4.368381

## -- >>>> Model Evaluation
str(test_woe)

# Predicting probabilities of response for the test data
prediction_logit_b <- predict(final_model_b, newdata = test_woe[,-1], type = "response")
summary(prediction_logit_b)

# Finding the optimal cutoff
perform_fn_b <- function(cutoff)
{
  predicted_response <- factor(ifelse(prediction_logit_b >= cutoff, "no", "yes"))
  conf <- confusionMatrix(predicted_response, test_woe$Performance_Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.2675 to 0.6848 for plotting and initiallizing a matrix of 100 X 4.
s_b = seq(0.2675,0.6848,length=100)
OUT_b = matrix(0,100,3)

for(i in 1:100) {
  OUT_b[i,] = perform_fn_b(s_b[i])
}

# plotting cutoffs 
plot(s_b, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_b,OUT[,2],col="darkgreen",lwd=2)
lines(s_b,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

print(OUT_b)

# Cutoff 
cutoff_lr_b <- s_b[which(abs(OUT_b[,1] - OUT_b[,2]) < 0.0491)]
cutoff_lr_b #0.5288 to 0.5414

cutoff_lr_b <- cutoff_lr_b[4] #Taking last one 
cutoff_lr_b

# Let's choose a cutoff value of 54.14% for final model
predicted_response_b <- factor(ifelse(prediction_logit_b >= cutoff_lr_b, "no", "yes"))
conf_final_b <- confusionMatrix(predicted_response_b, test_woe$Performance_Tag, positive = "yes")
acc_b  <- conf_final_b$overall[1]
sens_b <- conf_final_b$byClass[1]
spec_b <- conf_final_b$byClass[2]

acc_b  #65.19 %
sens_b #65.40 %
spec_b #60.49 %

# Slight improvement in Accuracy

# Plotting ROC curve
roc.curve(test_woe$Performance_Tag, predicted_response_b, plotit = TRUE)
# 63.0 % 

# KS Statistics - Test Data
test_lr_b_PT <- ifelse(predicted_response_b == 'yes',1,0)
table(test_lr_b_PT)
test_actual_lr_b_PT <- ifelse(test_woe$Performance_Tag == 'yes',1,0)
table(test_actual_lr_b_PT)

# on testing  data
pred_obj_lr_b <- prediction(test_lr_b_PT, test_actual_lr_b_PT)
prf_msr_tst_lr_b <- performance(pred_obj_lr_b, "tpr", "fpr")
ks_table_lr_b <- attr(prf_msr_tst_lr_b, "y.values")[[1]] - 
  (attr(prf_msr_tst_lr_b, "x.values")[[1]])

max(ks_table_lr_b) #0.2590 - Not Bad


# Now we will make model using Random Forest algorithm. 

## With Random Forest :
#----------------------

# We are already having train and test data, now we will build the model
# For Random Forest, we will not be using WOE values. 

## -- >>>> Model Building 
str(train_rf_blncd)

control <- trainControl(method="repeatedcv", number = 5, repeats = 3, classProbs=TRUE, summaryFunction=twoClassSummary)

set.seed(141)

model_rf_b <- train(Performance_Tag~., data=train_rf_blncd, method="rf", metric="ROC", trControl=control)
 
print(model_rf_b)

# Important Variables
varImp(model_rf_b)
plot(varImp(model_rf_b))

#                                                                Overall
#No_of_times_90_DPD_or_worse_in_last_6_months                    100.000
#No_of_times_30_DPD_or_worse_in_last_6_months                     94.816
#No_of_times_60_DPD_or_worse_in_last_6_months                     87.721
#No_of_times_90_DPD_or_worse_in_last_12_months                    79.624
#No_of_times_30_DPD_or_worse_in_last_12_months                    64.313
#No_of_times_60_DPD_or_worse_in_last_12_months                    58.742
#Avgas_CC_Utilization_in_last_12_months                           44.049
#No_of_PL_trades_opened_in_last_12_months                         30.807
#No_of_PL_trades_opened_in_last_6_months                          19.011
#No_of_trades_opened_in_last_12_months                            11.590
#No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_   7.339
#No_of_trades_opened_in_last_6_months                              4.825
#No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_    3.896
#Total_No_of_Trades                                                2.879
#Outstanding_Balance                                               0.000

## -- >>>> Model Evaluation

# Predicting probabilities of response for the test data
prediction_rf_b <- predict(model_rf_b, newdata = test_rf[,-16], type = "prob")
summary(prediction_rf_b)

# Finding the optimal cutoff
perform_fn_rf_b <- function(cutoff) 
{
  predicted_response <- factor(ifelse(prediction_rf_b[, 1] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance_Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out_r <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out_r) <- c("sensitivity", "specificity", "accuracy")
  return(out_r)
}

# Creating cutoff values from 0.1822 to 1.0000 for plotting and initiallizing a matrix of 100 X 4
s_r_b = seq(0.182,0.999,length=100)
out_r_b = matrix(0,100,3)

for(i in 1:100) {
  out_r_b[i,] = perform_fn_rf_b(s_r_b[i])
}

print(out_r_b)

# Plotting the cutoffs 
plot(s_r_b, out_r_b[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_r_b,out_r_b[,2],col="darkgreen",lwd=2)
lines(s_r_b,out_r_b[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Cutoff 
cutoff_rf_b <- s_r_b[which(abs(out_r_b[,1] - out_r_b[,2]) < 0.080452)]
cutoff_rf_b # 0.7761818 to 0.8669596

cutoff_rf_b <- cutoff_rf_b[1] #Taking 1st one
cutoff_rf_b #0.7761818

# Let's choose a cutoff value of 0.7761818
predicted_response_rf_b <- factor(ifelse(prediction_rf_b[, 1] >= cutoff_rf_b, "yes", "no"))
conf_final_rf_b <- confusionMatrix(predicted_response_rf_b, test_rf$Performance_Tag, positive = "yes")
acc_rf_b  <- conf_final_rf_b$overall[1]
sens_rf_b <- conf_final_rf_b$byClass[1]
spec_rf_b <- conf_final_rf_b$byClass[2]

acc_rf_b  #67.%
sens_rf_b #67.94%
spec_rf_b #60.49%

# Plotting ROC curve
roc.curve(test_rf$Performance_Tag, predicted_response_rf_b, plotit = TRUE)
# 64.2%

# Evaluating the model on the entire Master Data 
#----------------------------------------------

master_pred_prob <- predict(model_rf_b, master_df_2[, -16], type = "prob")
summary(master_pred_prob) #Summarise the probability

master_response_rf <- as.factor(ifelse(master_pred_prob[,1] >= cutoff_rf_b, "yes", "no"))
confus_mtrx_rf_mstr <- confusionMatrix(master_response_rf, master_df_2$Performance_Tag, positive = "yes")

acc_rf_mstr <- confus_mtrx_rf_mstr$overall[1]
sens_rf_mstr <- confus_mtrx_rf_mstr$byClass[1]
spec_rf_mstr <- confus_mtrx_rf_mstr$byClass[2]

acc_rf_mstr  #67.51%
sens_rf_mstr #67.95%
spec_rf_mstr #57.42%

# Plotting ROC curve
roc.curve(master_df_2$Performance_Tag, master_response_rf, plotit = TRUE)
# 62.7%

#--------------------------------------------
# Application Scorecard on the master dataset
#--------------------------------------------

# Creating a new dataframe contains the "Application ID", "Actual Response", "Predicted Response", "Predicted Probability", "Score"

master_df_2$Application_ID <- master_df$Application_ID #Adding Application Id in master_df_2 df

scorecard_df <- master_df_2[ ,c(17,16)] #Storing Application ID and Performance Tag
str(scorecard_df) 

scorecard_df$prd_prob_good <- master_pred_prob$yes
scorecard_df$prd_prob_bad <- master_pred_prob$no

scorecard_df$log_odds <- log(scorecard_df$prd_prob_good/scorecard_df$prd_prob_bad) #Log odds of Good to Bad

P2D  <- 20 #Points to double which is 20
Base <- 400 #Base Score
Odds <- 10 #Odds 

#Calculating Factor & Offset
Factor=P2D/log(2)

Offset=Base-(Factor*log(Odds))

scorecard_df$score <- Offset + (Factor*scorecard_df$log_odds)

# Rounding to the near integer
scorecard_df$score <- round(scorecard_df$score,0)
summary(scorecard_df$score)

# Checking if there is any infinite values in the Score column
scorecard_df[which(scorecard_df$score=="Inf"|scorecard_df$score=="-Inf"),]
nrow(scorecard_df[which(scorecard_df$score=="Inf"|scorecard_df$score=="-Inf"),])#No such rows

# Giving maximum score to applicants having log_odds are infinite 
#scorecard_df$score[which(scorecard_df$score=="Inf"|scorecard_df$score=="-Inf")] <- max(scorecard_df$score[which(scorecard_df$score < Inf)])

# Sorting the "scorecard_df" with score in descreasing order 
scorecard_df <- arrange(scorecard_df, desc(score))
head(scorecard_df) #Arranged successfully

scorecard_df$Pred_Performance_Tag <- as.factor(ifelse(scorecard_df$prd_prob_good >= cutoff_rf_b, "yes", "no"))

round(cutoff_rf_b, 2) #0.78

# Finding the Score value after which customer is good 
score_cutoff <- scorecard_df$score[which(scorecard_df$prd_prob_good == round(cutoff_rf_b, 2))][1]
score_cutoff #370 >>>> It is the cutoff, so customer with equal to or more than can be considered as good customer

ggplot(scorecard_df , aes(score,prd_prob_good)) +
  geom_line() +  ggtitle("Accepted applicants -Score v/s probability of good") +
  geom_hline(yintercept=round(cutoff_rf_b, 2), col="green") +
  geom_vline(xintercept= score_cutoff, col = "blue") +
  xlab("Score") + ylab("Predicted Probanility Good") +
  theme_bw()

# Percentage of customers default as per cutoff value 370
length(which(scorecard_df$score < 370)) / nrow(scorecard_df) #33.11%

# Score distribution
ggplot(scorecard_df,aes(score))+
  geom_histogram(bins = 10, alpha=0.4, fill = "blue") +
  geom_vline(xintercept= score_cutoff,col="red") +
  theme_bw()

# Exact percentage of people who are expected to default correctly identified
length(which(scorecard_df$score < 370 & scorecard_df$Performance_Tag == "no")) /length(which(scorecard_df$Performance_Tag == "no")) #57.42

# Exact percentage of people who are not expected to default correctly identified
length(which(scorecard_df$score >= 370 & scorecard_df$Performance_Tag == "yes")) /length(which(scorecard_df$Performance_Tag == "yes")) #67.


#-------------------------------------------------
# Application Scorecard on the Rejected Population
#-------------------------------------------------

# Calculating the scores of rejected population to compare the scores of the rejected population with the approved candidates

str(rejected_df)

# Data Preparation
#----------------

# Most of the data preparation is already done erlier for rejected population, we will check missing values
rejected_df_1 <- rejected_df #Saving a copy

# Removing performance tag from the dataframe as it has NA for all 
rejected_df_1$Performance_Tag <- NULL

# Checking NA values
sapply(rejected_df_1, function(x) sum(is.na(x)))

# Education has 1 and Avgas_CC_Utilization_in_last_12_months has 35 missing values. 
# We will predict the missing values using mice package

rejected_df_1_impute <- mice(rejected_df_1[-1], m = 5, maxit = 50, meth = 'cart', seed = 550, parallel = TRUE)
rejected_df_1 <- complete(rejected_df_1_impute)

sum(is.na(rejected_df_1)) # No NA values

# Using only those columns which are relevant as per information value
rejected_df_1 <- rejected_df_1[ ,c(1, which(colnames(rejected_df_1) %in% imp_col))]
str(rejected_df_1)

rejected_df_1$Application_ID <- rejected_df$Application_ID # Adding Application ID

# Scorecard building on the rejected population
#---------------------------------------------

# Predicting the probabaility of default 
predict_reject_prob <- predict(model_rf_b, rejected_df_1[, -1],type = "prob")
summary(predict_reject_prob)

rejected_df_1$prd_prob_good <- predict_reject_prob$yes #Adding Good Probability
rejected_df_1$prd_prob_bad  <- predict_reject_prob$no  #Adding Bad Probability

rejected_df_1$log_odds <- log(rejected_df_1$prd_prob_good/rejected_df_1$prd_prob_bad) #Log odds of Good to Bad

rejected_df_1$score <- Offset + (Factor*rejected_df_1$log_odds)

# Rounding to the near integer
rejected_df_1$score <- round(rejected_df_1$score,0)
summary(rejected_df_1$score)

# Checking if there is any infinite values in the Score column
rejected_df_1[which(rejected_df_1$score=="Inf"|rejected_df_1$score=="-Inf"),] #No such rows

# Sorting the "rejected_df_1" with score in decreasing order 
rejected_df_1 <- arrange(rejected_df_1, desc(score))
head(rejected_df_1) #Arranged successfully

# New column to decide should the customer be approved for credit or not based on score
rejected_df_1$approved <- as.factor(ifelse(rejected_df_1$score < 370, "no", "yes"))

# Checking the percentage of correctly identified "bad customers" 
prcntg_corrctly_bad_idntfd <- length(which(rejected_df_1$approved == "no"))/nrow(rejected_df_1)
prcntg_corrctly_bad_idntfd  #99.85 % 

# So model has predicted 99.85% correct rejected population.

ggplot(rejected_df_1 , aes(score,prd_prob_good))+
  geom_line() +  ggtitle("Rejected applicants - Score v/s probability of good")+
  geom_vline(xintercept= 370,col="red") +
  theme_bw() +
  ylab("Predicted probability of good") +
  xlab("Score") #Plot shows that most of the rejected candidates are correctly identified as "bad"

#--------------------
# Financial benefits
#--------------------

# Creating Confusion matrix based on scorecard from final model

# Total applicants who have defaulted and model has predicted as likely to default
length(which(scorecard_df$score < 370 & scorecard_df$Performance_Tag == "no")) #1687

# Total applicants who have defaulted and model has predicted as not likely to default
length(which(scorecard_df$score >= 370 & scorecard_df$Performance_Tag == "no")) #1251

# Total applicants who have not defaulted and model has predicted as not likely to default
length(which(scorecard_df$score >= 370 & scorecard_df$Performance_Tag == "yes")) #45235

# Total applicants who have defaulted and model has predicted as likely to default
length(which(scorecard_df$score < 370 & scorecard_df$Performance_Tag == "yes")) #21328

# So we have the confusion matrix as below : 
#-------------------------------------------------------------------------------------------#
#Confusion Matrix	|                             |           Actual Output                   # 
#-------------------------------------------------------------------------------------------#
#                                               | Good Customer ("yes")| Bad Customer ("no")# 
#-------------------------------------------------------------------------------------------#
#Predicted Output	|      Good Customer ("yes")	|     45235	           |       1251         #
#                 |      Bad Customer ("no")	  |     21328	           |       1687         #
#-------------------------------------------------------------------------------------------#

#The given confusion matrix was made on the merge dataset without rejected population, 
#as we want to know how much gain was achieved using the model for the applicants 
#who were provided the credit card without using model. 

# First We will calculate Credit and Revenue Loss

# Credit Loss : It is the loss organization have to bear when the customers default from the loan. 
# Currently the credit loss in 4.22 % (Percentage of defaulter)

# Calculating the credit loss based on model prediction which model rejected some bad customers
1687/(45235 + 21328 + 1251 + 1687) *100 # Total 2.42% 

# So total credit loss saved
4.22 - 2.42 
# 1.8%

# Revenue Los : Revenue loss occurred to the organization when the good customers are wrongly 
# identified as likely to default or bad customers by model

21328/(45235 + 21328) *100
# 32 %

# Profit Calculation - With model v/s No Model 

# We are considering an average profit of ??? 2500 from each good customers or non-defaulters 
# and average loss of ??? 50,000 from each bad customers or defaulters. 

# Net Profit without model 
(45235 + 21328) * 2500 - (1251 + 1687) * 50000
# 19507500

# Profit using model will be total profit due to each true positive and each true negative 
# minus loss from each false positive and each false negative prediction.

# Net Profit with model 
(45235* 2500) + (1687 * 50000)-(21328 * 2500)-(1251 * 50000)
# 81567500

# Net Financial gain with using the model 
81567500 - 19507500
# 62060000

# Percentage financial gain
round(62060000/19507500 * 100, 2)
# 318.13 %

# So we can see that based on the assumptions we have, using financial model we are able to increase the financial gain to more than 300%

#-------------------------------------
# Model Building for Demographic Data 
#-------------------------------------

# We will be building the model on demographic data to know the predictive power of the application data. 

# Data preparation is already done for demographic data, we will use woe value and build the model
str(master_df_1)
str(demographic_data)

# # Storing demographic data in demographic_data_1
demographic_data_1 <- master_df_1[ ,colnames(master_df_1) %in% names(demographic_data)]
sum(is.na(demographic_data_1)) # No NA values 
str(demographic_data_1)
summary(demographic_data_1)

prop.table(table(demographic_data_1$Performance_Tag)) # About 4% default rate, we will balance the train data

# Now calculating WOE values for the dataframe and same we will use for Logistic Regression.
bins_1 <- woebin(demographic_data_1, y = "Performance_Tag", positive = "yes") # Creating the bins

demographic_df_woe <- woebin_ply(demographic_data_1, bins_1)
View(demographic_df_woe)

# Splitting the dataframe into train and test dataframe
set.seed(235)

split_indcs_dg <- sample.split(demographic_df_woe$Performance_Tag, SplitRatio = 0.70) #Taking 70% of the data for training

train_demographic <- demographic_df_woe[split_indcs_dg, ]
test_demographic <- demographic_df_woe[!split_indcs_dg, ]

nrow(train_demographic) / nrow(demographic_df_woe) #70%

# Balancing the train data using ROSE 
train_demographic_blncd <- ROSE(Performance_Tag ~., data = train_demographic, seed = 9136)$data
prop.table(table(train_demographic_blncd$Performance_Tag)) #Data is balanced now 

## Logistic Regression
#---------------------

# Model Building 
model_d_1 <- glm(Performance_Tag ~., family = "binomial", data = train_demographic_blncd)
summary(model_d_1)

# Using stepwise algorithm for removing insignificant variables
model_d_2 <- stepAIC(model_d_1, direction = "both")
summary(model_d_2)
vif(model_d_2)

# Removing "Type_of_residence_woe" from the model as it is not siginificant 
model_d_3 <- glm(Performance_Tag ~ Age_woe + Marital_Status__at_the_time_of_application__woe + 
                   No_of_dependents_woe + Income_woe + Education_woe + Profession_woe + 
                   No_of_months_in_current_residence_woe + 
                   No_of_months_in_current_company_woe, family = "binomial", 
                 data = train_demographic_blncd)

summary(model_d_3)
vif(model_d_3)

# Removing "Marital_Status__at_the_time_of_application__woe" from the model as it is not that siginificant 
model_d_4 <- glm(Performance_Tag ~ Age_woe +
                   No_of_dependents_woe + Income_woe + Education_woe + Profession_woe + 
                   No_of_months_in_current_residence_woe + 
                   No_of_months_in_current_company_woe, family = "binomial", 
                 data = train_demographic_blncd)

summary(model_d_4)
vif(model_d_4)

# Removing "Education_woe" from the model as it is not that siginificant 
model_d_5 <- glm(Performance_Tag ~ Age_woe +
                   No_of_dependents_woe + Income_woe + Profession_woe + 
                   No_of_months_in_current_residence_woe + 
                   No_of_months_in_current_company_woe, family = "binomial", 
                 data = train_demographic_blncd)

summary(model_d_5)
vif(model_d_5)

# All variables are significant now

# So we have final model 
final_d_model <- model_d_5

## -- >>>> Model Evaluation
str(test_demographic)

# Predicting probabilities of response for the test data
prediction_logit_d <- predict(final_d_model, newdata = test_demographic[,-1], type = "response")
summary(prediction_logit_d)

# Finding the optimal cutoff
perform_fn_d <- function(cutoff) 
{
  predicted_response <- factor(ifelse(prediction_logit_d >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_demographic$Performance_Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.2945 to 0.7263 for plotting and initiallizing a matrix of 1000 X 4.
s_d = seq(0.2945,0.7263,length=100)
OUT_d = matrix(0,100,3)

for(i in 1:100) {
  OUT_d[i,] = perform_fn_d(s_d[i])
} 

# plotting cutoffs 
plot(s_d, OUT_d[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_d,OUT_d[,2],col="darkgreen",lwd=2)
lines(s_d,OUT_d[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

print(OUT_d)

# Cutoff 
cutoff_d <- s_d[which(abs(OUT_d[,1] - OUT_d[,2]) < 0.02238)]
cutoff_d #0.49077 to 0.49513

cutoff_d <- cutoff_d[1] #Taking 1st one 
cutoff_d

# Let's choose a cutoff value of 49.07% for fimal model
predicted_response_d <- factor(ifelse(prediction_logit_d >= cutoff_d, "yes", "no"))
conf_final_d <- confusionMatrix(predicted_response_d, test_demographic$Performance_Tag, positive = "yes")
acc_d  <- conf_final_d$overall[1]
sens_d <- conf_final_d$byClass[1]
spec_d <- conf_final_d$byClass[2]

acc_d  #42.89%
sens_d #42.98%
spec_d #40.74% 

# Plotting ROC curve
roc.curve(test_demographic$Performance_Tag, predicted_response_d, plotit = TRUE)
# 58.4 %

# Very Bad Model

# We can say that the demographics data is not only enough to identify if the customer is going to default or not. 
# Credit data plays important role in this. 

#----------------------------------
#-------------------- Conclusion
#----------------------------------

# Random Forest performs better than other models so it is considered as best model. 

# Probability Cutoff = 0.78
# Evaluation metrics on test data
# Accuracy = 67%
# Sensitivity = 67.94%
# Specificity =  60.49%

# Significant variables:
#No_of_times_90_DPD_or_worse_in_last_6_months
#No_of_times_30_DPD_or_worse_in_last_6_months
#No_of_times_60_DPD_or_worse_in_last_6_months
#No_of_times_90_DPD_or_worse_in_last_12_months
#No_of_times_30_DPD_or_worse_in_last_12_months
#No_of_times_60_DPD_or_worse_in_last_12_months
#Avgas_CC_Utilization_in_last_12_months	
#No_of_PL_trades_opened_in_last_12_months	
#No_of_PL_trades_opened_in_last_6_months	
#No_of_trades_opened_in_last_12_months	
#No_of_Inquiries_in_last_12_months__excluding_home___auto_loans	
#No_of_trades_opened_in_last_6_month
#No_of_Inquiries_in_last_6_months__excluding_home___auto_loans
#Total_No_of_Trades
#Outstanding_Balance

#Application scorecard is built on Random Forest model
# Cutoff score = 370

# Based on assumption
# Financial gain (%) = 318.13%
# Revenue loss = 32%
#Credit loss saved = 1.8%

# --------------------------------------------------END------------------------------------------------------------------------------

