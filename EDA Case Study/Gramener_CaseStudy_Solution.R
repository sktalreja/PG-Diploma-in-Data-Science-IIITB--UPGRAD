#-----------------------------------------EDA Case study----------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------
#Loading the packages
library(tidyverse)
library(ggplot2)
library(lubridate)

#Loading the file loan.csv into loan dataframe. 
loan_df <- read.csv("loan.csv", stringsAsFactors = F, na.strings= c("",NA), header = TRUE) #Converting blank cell to NA while loading the data

#Total rows in data frame
nrow(loan_df) #39717 rows are present in dataframe

#Structure of the dataframe
str(loan_df)

#----------------------------------------- Business Understanding --------------------------------------

# There are 3 types of attributes in the data set
## Customer's Demographic Information: 
# emp_title 
# emp_length
# home_ownership
# annual_inc
# verification_status
# addr_state
# zip_code
# title
# purpose
# desc
# url

## Loan Characteristics Information

# loan amount
# funded amount
# funded amount invested
# interest rate
# loan status
# loan grade
# loan sub-grade
# dti
# loan issue date
# loan term
# installment
# revol_util

## Credit information: Customer Behaviour variables 

#  delinq_2yrs
#  earliest_cr_line
#  inq_last_6mths
#  open_acc
#  pub_rec
#  revol_bal
#  total_acc
#  out_prncp 
#  out_prncp_inv
# total_pymnt"             
# total_pymnt_inv
# total_rec_prncp
# total_rec_int 
# total_rec_late_fee 
# recoveries             
# collection_recovery_fee
# last_pymnt_d
# last_pymnt_amnt
# next_pymnt_d
# last_credit_pull_d
# application_type       
###

# Business Objective
# The company wants to understand the driving factors behind the loan default. 
# If one is able to identify these risky loan applicants, then such loans can be reduced thereby cutting down the amount of credit loss. 
# Identification of such applicants using EDA is the aim of this case study. 
#######################
 
# Mapping business problem with the dataset.
# The variables related to the customer behaviour data can not be collected at the time of application. Thus analysing these variable could not solve 
# our business problem. As per the business problem, it wants to understand the driving factors behind the loan default at the time of application stage.

# So, we are using the following type of attributes for analysis
# 1. Attributes pertaining to Customers demographic and its characteristics
# 2. Attributes pertaining to the loan characteristics 


customer_behaviour_var<- c( 
  "delinq_2yrs",
  "earliest_cr_line",
  "inq_last_6mths",
  "open_acc",
  "pub_rec",
  "revol_bal",
  "total_acc",
  "out_prncp",
  "out_prncp_inv",
  "total_pymnt",
  "total_pymnt_inv",
  "total_rec_prncp",
  "total_rec_int",
  "total_rec_late_fee",
  "recoveries",
  "collection_recovery_fee",
  "last_pymnt_d",
  "last_pymnt_amnt",
  "next_pymnt_d",
  "last_credit_pull_d",
  "application_type")

#Remove customer behaviour variables
loan_df <- loan_df[, !colnames(loan_df) %in% customer_behaviour_var]
ncol(loan_df)

# The attributes "acc_now_delinq","chargeoff_within_12_mths","delinq_amnt", "pub_rec_bankruptcies","collections_12_mths_ex_med" & "tax_liens" 
# contain large number of zero observations.
# Attributes as "policy_code", "initial_list_status" and "pymnt_plan" are having same value for all the observations. 
# These are not meaningful features for the analysis
# Removing these not required variables for the analysis.

not_required_var <- c("member_id","id","acc_now_delinq","chargeoff_within_12_mths","pymnt_plan","initial_list_status","delinq_amnt","pub_rec_bankruptcies", "tax_liens","collections_12_mths_ex_med","policy_code",
                   "url","desc","emp_title","zip_code","addr_state","title")

loan_df <- loan_df[, !colnames(loan_df) %in% not_required_var]

loan_df %>%
  summarise_all(funs(sum(is.na(.))))

sum(is.na(loan_df)) # 2207381 NA values


#-----------------------------------------Data Cleaning & preparation--------------------------------------

#Checking the duplicate id and row
sum(duplicated(loan_df$id)) #No duplicate Id or row 

#Checking the blank values 
sapply(loan_df, function(x) length(which(x == ""))) #No blank value

#Checking the NA values  
sum(is.na(loan_df)) #2207381 NA values are present in dataframe

#Checking the NA values in individual columns 
sapply(loan_df, function(x) sum(is.na(x)))

#Removing the column where more than 60% of the values are NA
loan_df <- select(loan_df, -mths_since_last_delinq, -mths_since_last_record, -mths_since_last_major_derog, -(annual_inc_joint:verification_status_joint),
                  -(tot_coll_amt:bc_util), -(mo_sin_old_il_acct:percent_bc_gt_75), -(tot_hi_cred_lim:total_il_high_credit_limit))

ncol(loan_df)
sum(is.na(loan_df)) #50 NA values there
sapply(loan_df, function(x) sum(is.na(x))) #50 NA Values there in revol_util


#---------------------------------------Univariate and Bivariate analysis-----------------------------------

#Making a new column d_status which will have the two values, defaulted & not defaulted. 
#Defaulted - When loan status is Charged Off
#Not defaulted - When loan status is Fully Paid or Current
loan_df$d_status <- as.factor(ifelse(loan_df$loan_status == "Charged Off", "Defaulted", "Not defaulted"))

#Checking what percentage of loans are defaulted 
prop.table(table(loan_df$d_status)) #About 14% of the loans are defaulted. 


#Plotting the barplot for the percentage of loans that are defaulted with default status. 
plot1 <- ggplot(loan_df, aes(x = d_status, fill = d_status)) +
  geom_bar() +
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(prop.table(..count..) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.9), 
            size = 4, vjust = -0.9) +
  labs(x  = "Default Status", y = "Number of loans", fill = "Default Status") +
  ggtitle("Number of loans v/s Default status") # Bar chart clearly shows that the 14% of the loans are defaulted. 

plot1 # See the plot 


#Creating a segmented variable loan_amnt_range of loan_amnt variable where we will make the ranges of 5000. 
loan_df$loan_amnt_range <- factor(ifelse(loan_df$loan_amnt<=5000, "0-5000", ifelse(loan_df$loan_amnt > 5000 & loan_df$loan_amnt <= 10000
                            ,"5000-10000", ifelse(loan_df$loan_amnt > 10000 & loan_df$loan_amnt <= 15000, "10000-15000",
                            ifelse(loan_df$loan_amnt > 15000 & loan_df$loan_amnt <= 20000, "15000-20000", ifelse(loan_df$loan_amnt > 20000 
                            & loan_df$loan_amnt <= 25000, "20000-25000", ifelse(loan_df$loan_amnt > 25000 & loan_df$loan_amnt <= 30000, "25000-30000",
                            ifelse(loan_df$loan_amnt > 30000, "30000-35000", NA) )))))), 
                            levels = c("0-5000","5000-10000","10000-15000","15000-20000", "20000-25000",  "25000-30000","30000-35000" ))

table(loan_df$loan_amnt_range)

#Plotting a bar chart of loan amount range vs Percentage of loan(Default Status) to identify whether there is any influence of Loan amount range 
#on loan default. 
plot2 <- loan_df %>%
count(loan_amnt_range, d_status) %>%
  group_by(loan_amnt_range) %>%
  mutate(pct = n/sum(n)) %>%
ggplot(aes(x = loan_amnt_range, y = pct, fill = d_status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5)) +
 labs(x = "Loan Amount Range", y = "Percentage of loan", fill = "Default Status") +
  ggtitle("Percentge of Loan vs Loan amount range")

plot2 #See the plot 
# Above Bar chart clearly shows that the loan amount is affecting the loan default rate. With the increase in loan amount, default rate is
# increasing. About 22% of the loans are defaulting for the loans ranging from 30000 to 35000 as compared to the 13 % loans below 15000. 

prop.table(table(loan_df$loan_amnt_range, loan_df$d_status), 1) # Loan from 15000 - 35000 have high rate of default. 

#Checking the correlation in loan amount & funded amount and loan amount & funded_amnt_inv. 
cor.test(loan_df$loan_amnt, loan_df$funded_amnt) #Have correlation of arount 98%, that means it is sufficient to do the analysis with loan amount and default status. 
cor.test(loan_df$loan_amnt, loan_df$funded_amnt_inv) #Have correlation of arount 94%, that means it is sufficient to do the analysis with loan amount and default status. 


#Analysis on Term column 
#Plotting a bar chart of term vs default percentage of default status to identify if there is an influence of term in loan defaults. 
plot3 <- loan_df %>%
  count(term, d_status) %>%
  group_by(term) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = term, y = pct, fill = d_status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5)) +
  labs(x = "Loan Term", y = "Percentage of loan", fill = "Default Status") +
  ggtitle("Percentage of loan vs Loan term")

plot3 # See the plot 
#Above bar chart shows that loan term is clearly influencing the loan default. For loan term 60 months, default rate is 23% and for
#loan term 36 months, default rate is 11%.
prop.table(table(loan_df$term, loan_df$d_status), 1)


#Analysis on interest rate 
#Checking if the interest rate is influencing the default rate. To do that we will create a segmented column int_rate_group. 
#Converting the column into numeric type. 
loan_df$int_rate <- as.numeric(sub("%","0",loan_df$int_rate))

#It will be categorised as :
#0 to 10% - Low 
#10 to 18% - Medium
#More than 18% - High
loan_df$int_rate_group <- factor(ifelse(loan_df$int_rate <= 10, "Low", ifelse(loan_df$int_rate <= 18, "Medium", "High"))
                                 , levels = c("Low", "Medium", "High"))
unique(loan_df$int_rate_group)

prop.table(table(loan_df$int_rate_group, loan_df$d_status), 1)
#Plotting the stacked bar chart to check if the interest rates are influencing the default rate. 

plot4 <-loan_df %>%
  count(int_rate_group, d_status) %>%
  group_by(int_rate_group) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = int_rate_group, y = pct, fill = d_status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5)) +
  labs(x = "Grade", y = "Percentage of loan", fill = "Default Status") +
  ggtitle("Percentage of loan amount vs Interest Rate")

plot4 # See the plot
#Above chart clearly shows that the with the increase in interest rate default rate is substantially increasing. 


#Grade column analysis
#Checking if the grade is influencing the default rate as interest rate is dependent on the grade. For e.g. interest rate is 
#getting increased from A to G.

plot5 <-loan_df %>%
  count(grade, d_status) %>%
  group_by(grade) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = grade, y = pct, fill = d_status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5)) +
  labs(x = "Grade", y = "Percentage of loan", fill = "Default Status") +
  ggtitle("Percentage of loan amount vs Grade")

plot5 # See the plots 
prop.table(table(loan_df$grade, loan_df$d_status), 1)

#Above chart clearly shows that with the increase(i.e from A to G) in Grade, default rate is increasing.
#For grade G default rate is 32% where as for grade A default rate is 6%.


#Now going further down and checking for the specific subgrades where default rate is very high.
plot6 <-loan_df %>%
  count(sub_grade, d_status) %>%
  group_by(sub_grade) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = sub_grade, y = pct, fill = d_status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5)) +
  labs(x = "Sub Grade", y = "Percentage of loan", fill = "Default Status") +
  ggtitle("Percentage of loan amount vs Sub Grade")

plot6 #See the plot

# Above bar chart clearly shows that about 46% of the loans are getting defaulted for sub grade F5 and 40% for G3. Also, Default rate 
# is increasing with the increase in grade from A1 to G5. 


#Checking if the home ownership has any influence on loan default rate. 
prop.table(table(loan_df$home_ownership, loan_df$d_status), 1) # There is not much change in default rate with respect to the home ownership.


#Checking if the verification status is affecting default status or not. 
unique(factor(loan_df$verification_status)) #Making "Source Verified" & "Verified" verification status as "Verified" only.
loan_df$verification_status <- ifelse(loan_df$verification_status == "Source Verified", "Verified", loan_df$verification_status)

prop.table(table(loan_df$verification_status, loan_df$d_status), 1)
#Clearly shows that Verified sources have high default rate of 15% as compared to 13% of Non verified


#dti column analysis
#Checking if the dti(Debt to Income ratio) affecting the default of loan. Debt to income ratio plays key role in the disbursement of loan amount.
#As it tells us that what percentage of debt a person is having currently w.r.t to his income. 

#Deriving a new segmented column dti_level from dti column based on different ranges defined below. 
#0 to 15 - Tier 1
#15 to 20 - Tier 2
#More than 20 - Tier 3

loan_df$dti_level <- factor(ifelse(loan_df$dti < 15, "Tier 1", ifelse(loan_df$dti >= 15 & loan_df$dti < 20, "Tier 2", "Tier 3"))
                            , levels = c("Tier 1","Tier 2", "Tier 3" ))
unique(loan_df$dti_level) 

prop.table(table(loan_df$dti_level, loan_df$d_status), 1)
plot7 <- loan_df %>%
  count(dti_level, d_status) %>%
  group_by(dti_level) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = dti_level, y = pct, fill = d_status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5)) +
  labs(x = "Debt to income ratio", y = "Percentage of loan", fill = "Default Status") +
  ggtitle("Percentage of loan amount vs Debt to income ratio")

plot7 #See the plot 
#Above chart clearly shows that the loan default is dependent on Debt to income ratio, i.e. with the increase in Debt to income ratio
#person is more likely to default.

#Analysis on revol_util
#------------------------------------
#Checking if the revolving utilization is affecting the default ratio. 
#Revolving Utilization - It is important parameter for taking the decision related to loan disbursement to the customer.
#It measures how much of your credit limits are in use on each of your credit accounts and expresses that calculation as a percentage.

#converting the column to numeric datatype
loan_df$revol_util <- as.integer(as.vector(gsub("%", "", loan_df$revol_util)))
sum(is.na(loan_df$revol_util)) #50 NA values are present.

#converting NA values with Median values(Considering that every person has credit account)
loan_df <- loan_df %>%
  mutate(revol_util = if_else(is.na(revol_util), median(revol_util, na.rm = TRUE), revol_util))
sum(is.na(loan_df$revol_util)) #No NA values present.

#Changing the column name of revol_util to revol_util(%)
colnames(loan_df) [17] <- c('revol_util(%)')
ncol(loan_df)

#Deriving new column revol_util_category where categories are :
#Excellent - 0 to 10
#Good - 10 to 30 
#Fair - 30 to 50
#Poor - 50 to 75
#Very poor - Greater than 75

loan_df$revol_util_category <- factor(ifelse(loan_df$`revol_util(%)` <= 10, "Excellent", ifelse(loan_df$`revol_util(%)` <= 30, "Good", 
                                  ifelse(loan_df$`revol_util(%)` <= 50, "Fair", ifelse(loan_df$`revol_util(%)` <= 75, "Poor", "Very Poor")))), levels =
                                    c("Excellent","Good", "Fair","Poor", "Very Poor"))
                                                                                                                    
unique(loan_df$revol_util_category)

#Checking if the Loan purpose has any influence on loan default rate. 
prop.table(table(loan_df$revol_util_category, loan_df$d_status), 1)

plot8 <- loan_df %>%
  count(revol_util_category, d_status) %>%
  group_by(revol_util_category) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = revol_util_category, y = pct, fill = d_status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5)) +
  labs(x = "Loan Purpose", y = "Percentage of loan", fill = "Default Status") +
  ggtitle("Percentage of loan amount vs Revolving utilization category")

plot8 #See the plot
#Above chart clearly shows that with fall in category, default rate is increasing. It means Fair, Poor and Very Poor category have high default rate as compared to
#Excellent and Good category.


#Yearwise Distribution of loans
# Identifying Loans disbursal and default rate status based on year wise
loan_df <- loan_df %>%
  mutate(loan_issue_year=year(parse_date_time(x=loan_df$issue_d, orders = c("%b-%y"))), 
loan_issue_month=month(parse_date_time(x=loan_df$issue_d, orders = c("%b-%y"))))

#Yearwise Loan distribution by Loan purpose

plot9 <- ggplot(loan_df, aes(x= factor(purpose), fill = d_status)) + 
     geom_bar(position = "dodge") + 
     facet_grid(loan_issue_year~.) + 
     ggtitle("Year-wise Loan Distribution By Loan Purpose") + 
     labs(y="Sum of Loan Amount", x="Loan Purpose") + 
     theme(axis.title.x = element_text(colour="red", size=22), axis.text.x = element_text(colour="blue"), axis.title.y = element_text(colour="red", size=22, angle = 90), axis.text.y  = element_text(colour="blue"), plot.title = element_text(colour="red", size=30))

plot9 # See the plot
#2007- Company had focus on Loan product types as Credit Card, debt consolidation and Others.
#2008-2011 – Many other loan products were introduced, with major focus on Credit Card, Debt Consolidation, House improvement and Others

#Yearwise Defaulted Loan amount by Loan purpose

plot10 <- ggplot(subset(loan_df, d_status=="Defaulted"), aes(x=reorder(factor(purpose),-loan_amnt), fill = loan_amnt)) +
      facet_grid(loan_issue_year~.) +
      geom_bar(fill="red") +
      ggtitle("Year-wise Defaulted Loan Amount By Loan Purpose")+ 
      labs(y="Sum of Loan Amount", x="Loan Purpose")+
      theme(axis.title.x = element_text(colour="red", size=22), axis.text.x = element_text(colour="blue"), axis.title.y = element_text(colour="red", size=22, angle = 90), axis.text.y  = element_text(colour="blue"), plot.title = element_text(colour="red", size=30))

plot10 # See the plot
# 2007 to 2011 - Across the years, loan defaults are increasing for the below loan types Credit Card, Small business, Home improvement, Others, Major purchase, Car.


#Analysis on purpose column
#Column: purpose: Categorical variable :Character type

plot11 <- ggplot(loan_df, aes(x=purpose, fill= purpose))+
  geom_bar() +
  ggtitle("Loan Purpose")+ 
  geom_text(aes(y = (..count..), 
                label = scales::percent((..count..)/sum(..count..))),
            stat = 'count', 
            vjust = -0.4,
            size = 4) +
  ylab("Count in percentage")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #rotate x-labels vertically

plot11
#Observations
#46.9% applicants have taken loan for debt consolidation
#12.9% applicants have taken loan through credit cards and 10.01% applicants have taken loan for miscelleneous purposes

#Checking if the Loan purpose has any influence on loan default rate. 
prop.table(table(loan_df$purpose, loan_df$d_status), 1)

plot12 <- loan_df %>%
  count(purpose, d_status) %>%
  group_by(purpose) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = purpose, y = pct, fill = d_status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5)) +
  labs(x = "Loan Purpose", y = "Percentage of loan", fill = "Default Status") +
  ggtitle("Percentage of loan amount vs Loan purpose")

plot12 #See the plot

#Above bar chart clearly shows that the "Small Business" is the critical area and about 26% of the loans are getting defaulted as compared to
#10-18% in other areas. Where as, for Renewable Energy & Educational 18% and 17% of the loans are getting defaulted.

# Analysing the loan status for Top 4 products on the basis of Loan purpose distribution and most default rates, we get the following
# Debt Consolidation
# Credit Card loans
# Other loans
# Home improvement
# Identifying the key attributes leading to loan defaults in these top 4 product types.

# Create subset for the top 4 product types
loan_debt_consolidation_set <- subset(loan_df, loan_df$purpose == "debt_consolidation")
loan_home_improvement_set <- subset(loan_df, loan_df$purpose == "home_improvement")
loan_credit_card_set <- subset(loan_df, loan_df$purpose == "credit_card")
loan_other_set <- subset(loan_df, loan_df$purpose == "other")


plot13 <- ggplot(loan_debt_consolidation_set, aes(fill = d_status)) +
  geom_bar(mapping = aes(x = loan_amnt_range), position = "fill", color = "grey") +
  geom_bar(mapping = aes(x = int_rate_group), position = "fill", color = "light blue") +
  geom_bar(mapping = aes(x = grade), position = "fill", color = "yellow") + 
  geom_bar(mapping = aes(x = revol_util_category), position = "fill", color = "red") +
  geom_bar(mapping = aes(x = dti_level), position = "fill", color = "blue") + 
  geom_bar(mapping = aes(x = term), position = "fill", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#669933", "#FFCC66")) +
  xlab("Contributing Attributes") +
  ylab("Loan Default rate for Debt Consolidation") +
  coord_flip() + 
  theme(axis.title.x = element_text(colour="red", size=22), axis.text.x = element_text(colour="blue"), axis.title.y = element_text(colour="red", size=22, angle = 90), axis.text.y  = element_text(colour="blue"), plot.title = element_text(colour="red", size=30))


plot13 # See plot
#Key Driving Factors for Loan Default for Debt consolidation in descending order
#Grade – F & G
#Interest rate -  High(> 18%)
#Loan Term  - 60 Months
#Loan Amount – 30K to 35K
#Revolving Utilization – Very poor

plot14 <- ggplot(loan_other_set, aes(fill = d_status)) +
  geom_bar(mapping = aes(x = loan_amnt_range), position = "fill", color = "grey") +
  geom_bar(mapping = aes(x = int_rate_group), position = "fill", color = "light blue") +
  geom_bar(mapping = aes(x = grade), position = "fill", color = "yellow") + 
  geom_bar(mapping = aes(x = revol_util_category), position = "fill", color = "red") +
  geom_bar(mapping = aes(x = dti_level), position = "fill", color = "blue") + 
  geom_bar(mapping = aes(x = term), position = "fill", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel1") +
  xlab("Contributing Attributes") +
  ylab("Loan Default rate for Others") +
  coord_flip() + 
  theme(axis.title.x = element_text(colour="red", size=22), axis.text.x = element_text(colour="blue"), axis.title.y = element_text(colour="red", size=22, angle = 90), axis.text.y  = element_text(colour="blue"), plot.title = element_text(colour="red", size=30))


plot14 # See the plot
#Key Driving Factors for Loan Default for Others in descending order
#Grade – G & E
#Loan Term  - 60 Months
#Interest rate -  High(> 18%)
#Loan Amount – 30K to 35K
#Revolving Utilization – Very poor


plot15<- ggplot(loan_home_improvement_set, aes(fill = d_status)) +
  geom_bar(mapping = aes(x = loan_amnt_range), position = "fill", color = "grey") +
  geom_bar(mapping = aes(x = int_rate_group), position = "fill", color = "light blue") +
  geom_bar(mapping = aes(x = grade), position = "fill", color = "yellow") + 
  geom_bar(mapping = aes(x = revol_util_category), position = "fill", color = "red") +
  geom_bar(mapping = aes(x = dti_level), position = "fill", color = "blue") + 
  geom_bar(mapping = aes(x = term), position = "fill", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Pastel2") +
  xlab("Contributing Attributes") +
  ylab("Loan Default rate for Home Improvement") +
  coord_flip() + 
  theme(axis.title.x = element_text(colour="red", size=22), axis.text.x = element_text(colour="blue"), axis.title.y = element_text(colour="red", size=22, angle = 90), axis.text.y  = element_text(colour="blue"), plot.title = element_text(colour="red", size=30))


plot15
#Key Driving Factors for Loan Default for Home improvement in descending order
#Grade – F & E
#Loan Term  - 60 Months
#Loan Amount – 20K to 25K
#Revolving Utilization – Very poor
#Interest rate -  High(> 18%)


plot16 <- ggplot(loan_credit_card_set, aes(fill = d_status)) +
  geom_bar(mapping = aes(x = loan_amnt_range), position = "fill", color = "grey") +
  geom_bar(mapping = aes(x = int_rate_group), position = "fill", color = "light blue") +
  geom_bar(mapping = aes(x = grade), position = "fill", color = "yellow") + 
  geom_bar(mapping = aes(x = revol_util_category), position = "fill", color = "red") +
  geom_bar(mapping = aes(x = dti_level), position = "fill", color = "blue") + 
  geom_bar(mapping = aes(x = term), position = "fill", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#669933", "#FFCC66")) +
  xlab("Contributing Attributes") +
  ylab("Loan Default rate for Credit Card") +
  coord_flip() + 
  theme(axis.title.x = element_text(colour="red", size=22), axis.text.x = element_text(colour="blue"), axis.title.y = element_text(colour="red", size=22, angle = 90), axis.text.y  = element_text(colour="blue"), plot.title = element_text(colour="red", size=30))

plot16
#Key Driving Factors for Loan Default for Credit Card in descending order
#Loan Term  - 60 Months
#Grade – F & E
#Loan Amount – 30K to 35K
#Interest rate -  High(> 18%)
#Revolving Utilization – Very poor

