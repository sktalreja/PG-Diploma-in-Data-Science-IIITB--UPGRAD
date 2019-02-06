##-------------------------------------------Assignment - Acquisition Analytics------------------------------------------------------------
##-----------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------
# Loading all the relavant libraries
#-----------------------------------
library(tidyverse)
library(mice) #mice package for imputing missing values
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
#------------------------
# Business Understanding
#------------------------
# Portugese Banks had conducted a telemarketing campaign for a term-deposit product. Through the campaign, they had collected the data about
# the prospects' demographics, other financial products they have purchased in the past (loans, deposits etc.), the number of times hey were 
# called etc. They also recorded the response data, i.e. whether the person had subscribed to the term-deposit product, which is the target variable.

# The marketing team of the bank wants to launch yet another telemarketing campaign for the same product. You, an analyst at the bank, want to
# answer the following questions using the past data:
# Which prospects are more likely to buy the product (i.e. to respond )?
# Which attributes determine the propensity to buy a term-deposit?  
# Once you predict the likelihood of response, how many prospects should you target for telemarketing?
# By how much can you reduce the marketing cost using the model, and how many prospects will you acquire?

#-------------------
# Data Understanding
#-------------------
# Loading the Marketing data

bank_data <- read.csv("bank_marketing.csv", header = TRUE, na.strings = c("", "NA"))

str(bank_data) #41188 Rows, 21 Columns

head(sample(bank_data), 10) #Looking at first few rows

prop.table(table(bank_data$response)) %>% barplot(col = "darkred", xlab = "Response", ylab = "Percentage of response",
                                                  ylim = c(0, 1), beside = TRUE, main = "Response Percentage")
prop.table(table(bank_data$response)) #11.26% Total positive Response

#---------------------------------
# Data Preparation and Exploration
#---------------------------------

# Data Preparation
#-----------------

# Checking the NA values in data
sum(is.na(bank_data)) #No NA Values

# Checking the duplicate rows
sum(duplicated(bank_data)) #12 Duplicate rows

# Removing the duplicate rows
bank_data <- unique(bank_data)
nrow(bank_data) #41176 rows

# Checking if the column names are appropriate
colnames(bank_data)
colnames(bank_data) <- gsub("\\.", "_", colnames(bank_data)) #Replacing "." with "_" in column names
colnames(bank_data)

# Summary of the data
summary(bank_data)

# Outlier Treatment 
#--
# For Outlier Treatment, we will first identify the outliers and replace outliers with NA values and then impute the values by 
# doing prediction using mice package

# Checking the numerical variables only
#Age
boxplot(bank_data$age, col = "green", main = "Age Boxplot") #Clearly we can see that the outliers are present
quantile(bank_data$age, seq(0, 1, 0.01)) #We can see that 99 percentile is 71 and 100 percentile is 98
bank_data[(which(bank_data$age>71)),]$age <- NA #Replacing the outliers with NA values

#Duration
boxplot(bank_data$duration, col = "green", main = "Duration Boxplot") #Clearly we can see that the outliers are present
quantile(bank_data$duration, seq(0, 1, 0.01)) #We can see that 99 percentile is 1271.25 and 100 percentile is 4918.00
bank_data[(which(bank_data$duration>1271.25)),]$duration <- NA #Replacing the outliers with NA values

#Campaign
boxplot(bank_data$campaign, col = "green", main = "Campaign Boxplot") #Clearly we can see that the outliers are present
quantile(bank_data$campaign, seq(0, 1, 0.01)) #We can see that 99 percentile is 14 and 100 percentile is 56
bank_data[(which(bank_data$campaign>14)),]$campaign <- NA #Replacing the outliers with NA values

# Calling mice function to impute missing values for outlier treatment
bank_impute <- mice(bank_data, m = 5, maxit = 50, meth = 'pmm', seed = 1236)
complete_data <- complete(bank_impute)

# Let's check the quantiles of above variables again after imputation
quantile(complete_data$age, seq(0, 1, 0.01))
quantile(complete_data$duration, seq(0, 1, 0.01))
quantile(complete_data$campaign, seq(0, 1, 0.01)) #Data Looks fine 

#From now we will be using complete_data for further analysis

# Data Exploration
#-----------------
str(complete_data)

# Changing the response variable to numbers, i.e. "yes" to 1 and "no" to 0
complete_data$response <- ifelse(complete_data$response == "yes", 1, 0)
summary(complete_data$response)

# Exploring the columns one by one 

##======>>>>>>Client Related Data

#--->>>>>> Age

# Checking the distribution of Age column
qplot(age, data = complete_data, binwidth = 3) #Histogram shows that most of the people are in the age of 30 to 55 
quantile(complete_data$age, seq(0,1,0.02))

# Binnning the Age variable in 16, 20, 30, 40, 50, 60, 70, 80 and storing in new binning_age column
complete_data$binning_age <- as.factor(cut(complete_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Calculating the total response and response rate in each age category
agg_age <- merge(aggregate(response ~ binning_age, complete_data, mean),aggregate(response~binning_age, complete_data, sum),by = "binning_age")

# Adding No.of_prospect
count <- data.frame(table(complete_data$binning_age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# Changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))
agg_age

# Plotting the response rate of each age bucket in the plot
ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#Barplot clearly reflect that age plays critical role in response and response rate of age between 16 and 20, 60 to 80 is very high.

#--->>>>>> Job Category

str(complete_data$job) #str of job column
levels(complete_data$job)

# Plotting bar graph for job column

# Writing a function "plot_response" to do the same task for each variable
plot_response <- function(cat_var, var_name) {
  a <- aggregate(response~cat_var, complete_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + 
      geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
          geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  }

plot_response(complete_data$job, "job") #It shows that the "Retired" and "Student" has large response rate than rest of the category
#We will check the age range of "Student" and "Retired" person

qplot(job, age, data = filter(complete_data, job %in% c("student", "retired")), geom = "boxplot")
# Box Plot shows the most of the student are in the range of 17 to 30, while retired are in the range of 55 to 65. 
# So this add our age point that people having low and high age have high response rate.

#--->>>>>> Marital Status

summary(complete_data$marital) #summary of job column

# Summary of Marital Status shows that about 80 people have unknown marital status
# Replacing "Unknown" level to "married", as most of the people are "married"

levels(complete_data$marital)[4] <- "married"

# Plotting the response rate of marital status
plot_response(complete_data$marital,"marital") 
#Plot shows that the Marital Status does not seem to be a good predictors of response

#--->>>>>> Education
summary(complete_data$education)

plot_response(complete_data$education, "education") #Plot shows the "University Degree" and "Unknown" education people have high response rate

# There are many levels of education, reducing it to few, i.e. "Primary_Education", "Secondary_Education", "Tertiary_Education"
levels(complete_data$education) [c(1:3,5)] <- "Primary_Education"
levels(complete_data$education)[2] <- "Secondary_Education"
levels(complete_data$education)[4]<- "Tertiary_Education"

# Again plotting the response plot
plot_response(complete_data$education,"Education_levels") 
#plot shows that the education does not seem to be a good predictors of response

#--->>>>>> Default
summary(complete_data$default) #Large number of people have not defaulted the loan
plot_response(complete_data$default, "default")

complete_data <- complete_data[, -5] #Dropping the column as we do not have good distribution of default status
ncol(complete_data)

#--->>>>> Housing
summary(complete_data$housing)
plot_response(complete_data$housing, "Housing")
#plot shows that the Housing Loan does not seem to be a good predictors of response

#--->>>>> Loan
summary(complete_data$loan)
plot_response(complete_data$loan, "Loan")
#plot shows that the Personal Loan does not seem to be a good predictors of response


##======>>>>>>Campaign Related Data

#--->>>>> Contact
summary(complete_data$contact)
plot_response(complete_data$contact, "Contact")
#plot shows that the people who were contacted through Cellular phone have high response rate

#--->>>>> Month
summary(complete_data$month)
plot_response(complete_data$month, "Month")
#plot shows that the people who were contacted in the month of Sep, Oct, Dec and March seems to have a very high response rate
#It is may be because these are the months in which people do investment to save the taxes

#--->>>>> Week
summary(complete_data$day_of_week)
plot_response(complete_data$day_of_week, "Days of Week")

#Days of week doesn't seem to have any significant impact on response rate 

#--->>>>> Duration 
# It is a numeric variable
qplot(duration, data = complete_data, binwidth = 50) #Checking the distribution and we can see that most of the calls have low duration
summary(complete_data$duration) #Summary of duration column

# Average duration 
complete_data$response_1 <- as.factor(complete_data$response)
Avg_duration <- aggregate(duration~response_1,complete_data,mean)

# Positive Response have average duration of about 498 seconds where as Negative Response have average of about 214 seconds
# It reflects that if the peopl are conacted more, there is a high possibility that they will respond

complete_data <- complete_data[,-22] #Removing response column

#--->>>>> Campaign
# It is a numeric variable
qplot(campaign, data = complete_data, binwidth = 1) #Checking the distribution and we can see that most of the calls have low duration
summary(complete_data$campaign) #Most of the people are contacted from 1 to 4 times

#--->>>>> Pdays 
summary(complete_data$pdays)

# Converting this variable to factor type
complete_data$pdays <- as.factor(complete_data$pdays)
summary(complete_data$pdays) #There are many levels, reducing it to few

# Reducing the levels of this variable to 3
levels(complete_data$pdays) [1:10] <- "Contacted_in_first_10_days"
levels(complete_data$pdays) [2:17] <- "Contacted_after_10_days"
levels(complete_data$pdays) [3] <- "First_time_contacted"

# Plotting the response rate for each levels of pdays
plot_response(complete_data$pdays, "Pday")
# It shows that if the customer is frequently contacted then there is a large possibility of response

#--->>>>> Previous 
summary(complete_data$previous)

#Converting this variable to factor
complete_data$previous <- as.factor(complete_data$previous)
summary(complete_data$previous)

# Making the buckets 
levels(complete_data$previous)[1]   <-"Never contacted"
levels(complete_data$previous)[2:4] <- "Less_than_3_times"
levels(complete_data$previous)[3:6] <- "More than_3_times"
summary(complete_data$previous)

plot_response(complete_data$previous,"Previous_contacts") 
#Plot shows that if the customer is contacted number of times then there is a high possibility that he will respond

#--->>>>> Poutcome
summary(complete_data$poutcome)
plot_response(complete_data$poutcome,"Previous_Outcome")
# Plot shows that if the previously customer was contacted and he or she responded, then there is a high response rate in next campaign


##======>>>>>>Social and Economic Context attributes

#--->>>>> Employment Variation Rate - quarterly indicator(numeric)
summary(complete_data$emp_var_rate)
# Ranges from -3.4 to 1.4

# Distribution of Employment Variation Rate for Responses
qplot(emp_var_rate, data = complete_data, fill = factor(response), facets = .~response, geom = "density") + 
  xlab("Employment Variation Rate") + labs(fill = "Response") + ggtitle("Distribution of Employment Variation Rate for Responses") +
  theme_bw()

#--->>>>> Consumer Price Index - monthly indicator(numeric)
summary(complete_data$cons_price_idx)
# Ranges from 92.20 to 94.77

# Distribution of Consumer Price Index
qplot(cons_price_idx, data = complete_data, fill = factor(response), facets = .~response, geom = "density") + 
  xlab("Consumer Price Index") + labs(fill = "Response") + ggtitle("Distribution of Consumer Price Index for Responses") +
  theme_bw()

#--->>>>> Consumer Confidence Index - Monthly indicator(numeric)
summary(complete_data$cons_conf_idx)
# Ranges from -50.8 to -26.9

# Distribution of Consumer Confidence Index
qplot(cons_conf_idx, data = complete_data, fill = factor(response), facets = .~response, geom = "density") + 
  xlab("Consumer Confidence Index") + labs(fill = "Response") + ggtitle("Distribution of Consumer Confidence Index for Responses") +
  theme_bw()

#--->>>>> Euribor 3 month Rate - daily indicator(numeric)
summary(complete_data$euribor3m)
# Ranges from 0.634 to 5.045

# Distribution of Euribor 3 month Rate 
qplot(euribor3m, data = complete_data, fill = factor(response), facets = .~response, geom = "density") + 
  xlab("Euribor 3 month Rate") + labs(fill = "Response") + ggtitle("Distribution of Euribor 3 month Rate for Responses") +
  theme_bw()

#--->>>>> Number of Employees - quarterly indicator(numeric)
summary(complete_data$nr_employed)
# Ranges from 4964 to 5228

# Distribution of Number of Employees - quarterly indicator(numeric)
qplot(nr_employed, data = complete_data, fill = factor(response), facets = .~response, binwidth = 5) + 
  xlab("Number of Employees") + labs(fill = "Response") + ggtitle("Distribution of Number of Employees for Responses") +
  theme_bw()

#------------------------------
# Model Building and Evaluation
#------------------------------

#Removing the "duration" column from the dataframe, as we know that when duration is more, there is a chance of positive response. 
#However, as the duration increases, it increase the cost of telemarketing which is a concern for the organization. 

full_data <- complete_data[-10]
str(full_data)

# Removing binning_age variable
full_data <- full_data[-20]
str(full_data)

# Creating dummy variables
full_data$response <- as.integer(full_data$response)

full_d_data <- dummy.data.frame(full_data)
full_d_data$response <- as.factor(ifelse(full_d_data$response == 1, "yes", "no"))

# Splitting into train and test dataset
set.seed(1020)
split_indices <- sample.split(full_d_data$response, SplitRatio = 0.70) #Taking 70% of the data for training
train <- full_d_data[split_indices, ]
test <- full_d_data[!split_indices, ]

nrow(train) / nrow(full_d_data)
nrow(test) / nrow(full_d_data)

##-->> Model Building

# Using Logistic Regression algorithm to build the model

model_1 <- glm(response~., family = "binomial", data = train)
summary(model_1)

# Using stepwise algorithm for removing insignificant variables
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2)

# stepAIC has removed some variables and only the following ones remain
# Checking the vif values and Pr values to remove insignificant variables
vif(model_2)

#Removing "euribor3m" from the model as it has high VIF value and it is not significant
model_3 <- glm(response ~ jobretired + jobstudent + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                 day_of_weekthu + day_of_weektue + campaign + pdaysContacted_in_first_10_days + 
                 pdaysContacted_after_10_days + previousLess_than_3_times + 
                 poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                 nr_employed + `jobblue-collar` + educationprofessional.course, 
                 family = "binomial", data = train)
summary(model_3)
vif(model_3)

#Removing "previousLess_than_3_times" from the model as it has high VIF value and it is not significant
model_4 <- glm(response ~ jobretired + jobstudent + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                 day_of_weekthu + day_of_weektue + campaign + pdaysContacted_in_first_10_days + 
                 pdaysContacted_after_10_days + 
                 poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                 nr_employed + `jobblue-collar` + educationprofessional.course, 
               family = "binomial", data = train)
summary(model_4)
vif(model_4)

#Removing "educationprofessional.course" from the model as it is not significant
model_5 <- glm(response ~ jobretired + jobstudent + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                 day_of_weekthu + day_of_weektue + campaign + pdaysContacted_in_first_10_days + 
                 pdaysContacted_after_10_days + 
                 poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                 nr_employed + `jobblue-collar`,
               family = "binomial", data = train)
summary(model_5)
vif(model_5)

#Removing "`jobblue-collar`" from the model as it is not significant
model_6 <- glm(response ~ jobretired + jobstudent + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                 day_of_weekthu + day_of_weektue + campaign + pdaysContacted_in_first_10_days + 
                 pdaysContacted_after_10_days + 
                 poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                 nr_employed,
               family = "binomial", data = train)
summary(model_6)
vif(model_6)

#Removing "day_of_weektue" from the model as it is not that significant
model_7 <- glm(response ~ jobretired + jobstudent + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                 day_of_weekthu + campaign + pdaysContacted_in_first_10_days + 
                 pdaysContacted_after_10_days + 
                 poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                 nr_employed,
               family = "binomial", data = train)
summary(model_7)
vif(model_7)

#Removing "day_of_weekthu" from the model as it is not significant
model_8 <- glm(response ~ jobretired + jobstudent + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10_days + 
                 pdaysContacted_after_10_days + 
                 poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                 nr_employed,
               family = "binomial", data = train)
summary(model_8)
vif(model_8)

#Removing "day_of_weekfri" from the model as it is not significant
model_9 <- glm(response ~ jobretired + jobstudent + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10_days + 
                 pdaysContacted_after_10_days + 
                 poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                 nr_employed,
                 family = "binomial", data = train)
summary(model_9)
vif(model_9)

#Removing "monthoct" from the model as it is not that significant
model_10 <- glm(response ~ jobretired + jobstudent + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10_days + 
                 pdaysContacted_after_10_days + 
                 poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                 nr_employed,
               family = "binomial", data = train)
summary(model_10)
vif(model_10)

#Removing "jobstudent" from the model as it is not that significant
model_11 <- glm(response ~ jobretired + educationPrimary_Education + 
                  contactcellular + monthapr + monthjul + monthjun + monthmar + 
                  monthmay + monthnov + day_of_weekmon + 
                  campaign + pdaysContacted_in_first_10_days + 
                  pdaysContacted_after_10_days + 
                  poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                  nr_employed,
                family = "binomial", data = train)
summary(model_11)
vif(model_11)

#Removing "jobretired" from the model as it is not that significant
model_12 <- glm(response ~ educationPrimary_Education + 
                  contactcellular + monthapr + monthjul + monthjun + monthmar + 
                  monthmay + monthnov + day_of_weekmon + 
                  campaign + pdaysContacted_in_first_10_days + 
                  pdaysContacted_after_10_days + 
                  poutcomefailure + emp_var_rate + cons_price_idx + cons_conf_idx + 
                  nr_employed,
                family = "binomial", data = train)
summary(model_12)
vif(model_12)


#Removing "emp_var_rate" from the model as it has very large VIF value
model_13 <- glm(response ~ educationPrimary_Education + 
                  contactcellular + monthapr + monthjul + monthjun + monthmar + 
                  monthmay + monthnov + day_of_weekmon + 
                  campaign + pdaysContacted_in_first_10_days + 
                  pdaysContacted_after_10_days + 
                  poutcomefailure + cons_price_idx + cons_conf_idx + 
                  nr_employed, family = "binomial", data = train)
summary(model_13)
vif(model_13)

#Removing "monthapr" from the model as it is insignificant
model_14 <- glm(response ~ educationPrimary_Education + 
                  contactcellular + monthjul + monthjun + monthmar + 
                  monthmay + monthnov + day_of_weekmon + 
                  campaign + pdaysContacted_in_first_10_days + 
                  pdaysContacted_after_10_days + 
                  poutcomefailure + cons_price_idx + cons_conf_idx + 
                  nr_employed, family = "binomial", data = train)
summary(model_14)
vif(model_14)

#Removing "cons_conf_idx" from the model as it is insignificant
model_15 <- glm(response ~ educationPrimary_Education + 
                  contactcellular + monthjul + monthjun + monthmar + 
                  monthmay + monthnov + day_of_weekmon + 
                  campaign + pdaysContacted_in_first_10_days + 
                  pdaysContacted_after_10_days + 
                  poutcomefailure + cons_price_idx + 
                  nr_employed, family = "binomial", data = train)
summary(model_15)
vif(model_15)

#Removing "monthjul" from the model as it is not that significant
model_16 <- glm(response ~ educationPrimary_Education + 
                  contactcellular + monthjun + monthmar + 
                  monthmay + monthnov + day_of_weekmon + 
                  campaign + pdaysContacted_in_first_10_days + 
                  pdaysContacted_after_10_days + 
                  poutcomefailure + cons_price_idx + 
                  nr_employed, family = "binomial", data = train)
summary(model_16)
vif(model_16)

#Removing "cons_price_idx" from the model as it is not that significant
model_17 <- glm(response ~ educationPrimary_Education + 
                  contactcellular + monthjun + monthmar + 
                  monthmay + monthnov + day_of_weekmon + 
                  campaign + pdaysContacted_in_first_10_days + 
                  pdaysContacted_after_10_days + 
                  poutcomefailure + 
                  nr_employed, family = "binomial", data = train)
summary(model_17)
vif(model_17)

#Removing "monthjun" from the model as it is not that significant
model_18 <- glm(response ~ educationPrimary_Education + 
                  contactcellular + monthmar + 
                  monthmay + monthnov + day_of_weekmon + 
                  campaign + pdaysContacted_in_first_10_days + 
                  pdaysContacted_after_10_days + 
                  poutcomefailure + 
                  nr_employed, family = "binomial", data = train)
summary(model_18)
vif(model_18)

#Removing "educationPrimary_Education" from the model as it is not that significant
model_19 <- glm(response ~contactcellular + monthmar + 
                  monthmay + monthnov + day_of_weekmon + 
                  campaign + pdaysContacted_in_first_10_days + 
                  pdaysContacted_after_10_days + poutcomefailure +
                  nr_employed, family = "binomial", data = train)
summary(model_19)
vif(model_19)

# model_19 is the final model as all insignificant variables have been removed
final_model <- model_19

##-->> Model Evaluation

# Predicting probabilities of response for the test data
prediction_logit <- predict(final_model, newdata = test[,-60], type = "response")
summary(prediction_logit)

# Evaluating the model by using the probability cutoff as 50%
predicted_response <- factor(ifelse(prediction_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf # Accuracy - 90.18%, Sensitivity - 24.2% (Quite low), Specificity - 98.55%

# As we can see that the Sensitivity is quite low, so we need to find the optimal cutoff to have balanced Accuracy, Sensitivity & Specificity

# Finding the optimal cutoff
perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(prediction_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)
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
s[9] # 0.08919192
#Taking the cutoff as 0.08919192, at this value we have balanced sensitivity - 67.31%, Specificity - 77.74% and Accuracy - 76.57%

# Let's choose a cutoff value of 8.91 % for fimal model
predicted_response <- factor(ifelse(prediction_logit >= 0.0891, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc # 76.57%
sens # 67.31%
spec # 77.77%

#-----------------------
#Top x% target analysis
#----------------------
colnames(full_d_data)

# Finding the probaility of response for each prospect
full_d_data$prob <- predict(final_model, newdata = full_d_data[,-60], type = "response")
summary(full_d_data$prob)

# Making a new data frame which contains the Actual Response, Predicted Response, Predicted Probability of Response, Duration of Call in Seconds
# and cost of each call

final_df <- dplyr::select(full_d_data, A_Response = response, Prob = prob)
final_df$P_Response <- factor(ifelse(final_df$Prob >= 0.0891, "yes", "no")) #Adding the Predicted Response column based on cutoff
final_df$D_call <- complete_data$duration #Adding Duration of Call
final_df <- mutate(final_df, C_Call = 0.033*(D_call) + 0.8) #Adding Cost of each call based on logic provided by business

# Addin Prospect_ID column in final_df
final_df <- mutate(final_df, Prospect_ID = seq(1, nrow(final_df), by = 1))

# Copying the dataframe for finding top X% prospects
final_df_1 <- final_df

# Sorting final_df in descending order of probability oe response
final_df_1 <- arrange(final_df_1, desc(Prob))

head(final_df) #top 6 propect 
tail(final_df) #last 6 prospect

#Creating Lift and Gain chart

lift <- function(labels , predicted_prob, groups=10, duration, cost_of_call) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob, duration, cost_of_call))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE),
                                     avg_duration=mean(duration),
                                     total_duration_per_decile=sum(duration),
                                     avg_costofcall=mean(cost_of_call),
                                     total_cost_per_decile=sum(cost_of_call))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Creating a table of cumulative gain and lift 
predicted_rsponse <- as.factor(ifelse(final_df_1$P_Response=="yes",1,0))
LG = lift(predicted_rsponse, final_df_1$Prob, groups = 10, final_df_1$D_call, final_df_1$C_Call)
View(LG)

sum(LG$total_duration_per_decile)==sum(final_df_1$D_call)
sum(LG$total_cost_per_decile)==sum(final_df_1$C_Call)

# Finding the total responders(Response == "yes")
length(which(final_df_1$A_Response == "yes")) #4639 total response 

# Business objective is to achieve 80% of total responders
0.8 * length(which(final_df_1$A_Response == "yes")) #Approx 3712 people 

#as 5th decile seem to cumulatively have 3744 responders,sum/average cost of calls for 5 deciles gives the 
#total/average cost to target top 80% of responders

sum((LG$total_cost_per_decile)[1:5]) #191572
round(mean((LG$avg_costofcall)[1:5]),2) #9.31
round(mean((LG$avg_duration)[1:5]),2) #275.73 seconds

# Thus the average duration of calls by targetting customers from the first 5 deciles is 275.73 secs with an average cost of Rs:9.31/-

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

# The Cumulative Lift of 3.6 for top one decile,
# means that when selecting 10% of the records based on the model, 
# one can expect 3.6 times the total number of targets (events) found by randomly 
# selecting 10%-of-records without a model.

#-----------
#Conclusion
#-----------

# After sucessfully building the model and sorting the data based on decreasing order of probability of response, we came to know that
# by targeting top 50% (5th decile) prospects, we can reach top 80% of the total responders with minimum cost. 
# 