#Loading the packages
library(stringr)
library(tidyr)
library(lubridate)
library(chron)
library(ggplot2)

# Loading datasets in R
Uber <- read.csv("Uber Request Data.csv", header = TRUE, stringsAsFactors = FALSE)

str(Uber)

View(Uber)

# Data Cleaning & Preparation

# Looking for duplicate Request Id.

sum(duplicated(Uber$Request.id)) # No Duplicate Request ID in dataframe

# Checking the unique pick up points in dataframe. 
unique(Uber$Pickup.point) #"Airport" and "City" are two unique points only 

#Cheking the unique values in Status column 
unique(Uber$Status) #"Trip Completed", "Cancelled" and "No Cars Available" are three status exist

#-------------------------------Missing Values--------------------------------------------------------------------------------------

# Finding the total NA values in Dataframe
sum(is.na(Uber)) # 6564 NA values are available

# Checking the blank values in dataframe
sapply(Uber, function(x) length(which(x == ""))) # No blank values are present in dataframe

#-------------------------------NA Values in individual columns---------------------------------------------------------------------

# Finding the NA value in individual columns in dataframe
sapply(Uber, function(x) sum(is.na(x))) # 2650 values are present in Driver.id column and 3914 values are present in Drop.timestamp

# Checking the NA in Driver.id column when Status was "Trip Completed". 
sum(which(is.na(Uber$Driver.id) & Uber$Status == "Trip Completed")) # No such NA values are there in Driver.Id when Trip was completed.
# That means driver Id is NA only when Trip was either cancelled or No Cars was available

# Checking the NA values in Drop.timestamp when Status was "Trip Completed".
sum(which(is.na(Uber$Drop.timestamp) & Uber$Status == "Trip Completed")) # No such NA values are there in Drop.timestamp when Trip was completed.
# That means Drop Timestamp is NA only when Trip was either cancelled or No Cars was available

#---------------------------------Data and Time formatting in Request & Drop timestamp columns------------------------------------

# Converting the timestamps in proper format using parse_date_time function from lubridate package. 

Uber$Request.timestamp <- parse_date_time(Uber$Request.timestamp, c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S")) 

Uber$Drop.timestamp <- parse_date_time(Uber$Drop.timestamp, c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S")) 
typeof(Uber$Request.timestamp) 
typeof(Uber$Drop.timestamp) # Both columns are now converted into date type

# Splitting the Day and time of request datime column

Request_Datime <- do.call(rbind, strsplit(as.character(Uber$Request.timestamp), split = " ")) %>%
as.data.frame(stringsAsFactors = FALSE)
names(Request_Datime) <- c("Request_Day", "Request_Time")

Uber <- cbind(Uber, Request_Datime) #Combining both the dataframes i.e. Uber & Request_Datime

#Changing the format of Request_Day to date
Uber$Request_Day <- as.Date(Uber$Request_Day, format = "%Y-%m-%d")
typeof(Uber$Request_Day)

#Changing the time format of Request_Time column to time usisng chron package
Uber$Request_Time <- chron(times = Uber$Request_Time)
typeof(Uber$Request_Time)

#Extracting the Request hour from Request time column and stroring in new column Request_Hour
Uber$Request_Hour <- as.numeric(substr(as.character(Uber$Request_Time),1,2))

#-------------------------------------Plotting & Analysis---------------------------------------------------

#1st Plot: Plotting the hourly request plot to identify the most suitable time slots buckets.   
Plot_1st <- ggplot(data = Uber, mapping = aes(x = factor(Request_Hour), fill = factor(Status))) +
  geom_bar(position = "stack") +
  xlab("Request Hour") +
  ylab("Number of cars requested") +
  labs(fill = "Status") +
  ggtitle("Number of cars requested w.r.t Request Hour")

Plot_1st #See the plot
# Base on Plot2 we will create an additional Time_slot variable and divide the time into 4 slots as mentioned below:
# 23 to 04 hours - Night
# 05 to 09 hours - Morning Rush
# 10 to 16 hours - Normal hours 
# 17 to 22 hours - Evening Rush

# Creating the additional column time_slot in dataframe 
time_slots <- c("Night", "Morning Rush", "Normal hours", "Evening Rush")

Uber$Time_slot <- as.factor(ifelse(Uber$Request_Hour <= 4,time_slots[1],
                            ifelse(Uber$Request_Hour <= 9,time_slots[2],ifelse(Uber$Request_Hour <= 16, time_slots[3],
                            ifelse(Uber$Request_Hour <= 22,time_slots[4],time_slots[1])))))

unique(Uber$Time_slot)
 
#2nd Plot: Plotting the bar chart having timeslots is on x axis and number of requests on y axis to know time interval
# in which large number of requests are getting cancelled or no cabs available
Plot_2nd <- ggplot(data = Uber, mapping = aes(x = factor(Time_slot), fill = factor(Pickup.point))) +
   geom_bar(position = "stack") +
   xlab("Time Slots") +
   ylab("Number of cabs requested") +
   labs(fill = "Pickup.point") +facet_grid(~Status) +
   geom_text(stat = 'count', aes(label = ..count.. , vjust = -1)) +
   ggtitle("Number of cars requested on different time slots and status")
 
Plot_2nd #See the plot
# Above bar chart clearly shows that the large number of cabs are getting cancelled in Morning Rush and 
# large number of cabs are not available in Evening Rush. 

#Making the subsets of Uber data frame for time slots "Morning Rush" and "Evening Rush" as these are two critical slots where 
#problem persists

Umorning_rush <- subset(Uber, Time_slot == "Morning Rush")
nrow(Umorning_rush)
unique(factor(Umorning_rush$Time_slot))

Uevening_rush <- subset(Uber, Time_slot == "Evening Rush")
nrow(Uevening_rush)
unique(factor(Uevening_rush$Time_slot))

#3rd Plot: Plotting the bar chart to identify the pick up point from where large number of cabs are getting cancelled or 
#no cars available for morning time slot.
Plot_3rd <- ggplot(data = Umorning_rush, mapping = aes(x = factor(Pickup.point), fill = factor(Status))) +
  geom_bar(position = "stack") +
  xlab("Pick up points") +
  ylab("Number of cabs requested") +
  labs(fill = "Status") +
  facet_wrap(~Status) +
  geom_text(stat = 'count', aes(label = ..count.., vjust = 1)) +
  ggtitle("Number of cars requested on different pick up point - Morning Rush Hours")

Plot_3rd #See the plot
#Clearly the above bar chart shows that in Morning Rush time slot, the large number of cabs are getting cancelled from "City".
#Also many cars are not available in city. 


#4th Plot: Plotting the bar chart to identify the pick up point from where large number of cabs are getting cancelled or 
#no cars available for Evening Rush time slot.
Plot_4th <- ggplot(data = Uevening_rush, mapping = aes(x = factor(Pickup.point), fill = factor(Status))) +
  geom_bar(position = "stack") +
  xlab("Pick up points") +
  ylab("Number of cabs requested") +
  labs(fill = "Status") +
  facet_wrap(~Status) +
  geom_text(stat = 'count', aes(label = ..count.., vjust = 1)) +
  ggtitle("Number of cars requested on different pick up point - Evening Rush Hours")

Plot_4th #See the plot
#Clearly, the above bar chart shows that large number of cars not available from Airport in Evening Rush hours. 

#Demand vs Supply
#5th Plot: Plotting the Bar charts to identify the percentage of requests with status cancelled or No cars available for 
#Evening and Morning Rush time slots 

Plot_5th <- ggplot(subset(Uber, Time_slot %in% c("Morning Rush", "Evening Rush")),
                   aes(x = factor(Pickup.point), fill = factor(Status), y = length(Request.id))) +
  geom_bar(position = "fill", stat = "identity", width = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Time_slot) +
  xlab("Pick up point") +
  ylab("Percentage of requests") +
  labs(fill = "Status") +
  ggtitle("Percentage of requests and Status in different time slots")

Plot_5th #See the plot

# Above chart provide the below inputs for Morning and Evening Rush hours;
# Evening Rush Hours(Pick up point - Airport): About 70% of the requests have status "No Cars Available" and about 5% of the requests have status "Cancelled"
# Morning Rush Hours(Pick up point - City): About 50% of the request have status "Cancelled" and about 20% of the requests have status "No Cars Available"

#-----------------------------------------------Demand & Supply calculations for Evening Rush & Morning Rush slots -------------------------------------

#1. Morning Rush 

#Total number of requests made
T_mrush_request <- length(Umorning_rush$Request.id)
T_mrush_request #2103 total requests made

#Number of requests cancelled or cars unavailable 
T_mrush_unavailable <- length(which(Umorning_rush$Status != "Trip Completed"))
T_mrush_unavailable # 1249 requests were either cancelled or no cars available

#Finding the percentage of requests cancelled or no cars available 
T_mrush_un_prcnt <- T_mrush_unavailable/T_mrush_request * 100
T_mrush_un_prcnt # About 59% cabs were getting cancelled or no cars available 

#Finding the number of requests made from pick up point "city"
T_mrush_rcity <- length(which(Umorning_rush$Pickup.point == "City"))
T_mrush_rcity #1677 requests were made during from city

#Finding the number of requests cancelled from city
T_mrush_ccity <- length(which(Umorning_rush$Pickup.point == "City" & Umorning_rush$Status == "Cancelled"))
T_mrush_ccity #820 requests were cancelled from city 

#Finding the number of requests in which No cars were available 
T_mrush_nacity <- length(which(Umorning_rush$Pickup.point == "City" & Umorning_rush$Status == "No Cars Available"))
T_mrush_nacity #385 requests were there in which no cars were available

#Finding the percentage of requests cancelled from city 
T_mrush_ccprcnt <- T_mrush_ccity/T_mrush_rcity * 100
T_mrush_ccprcnt #About 49% of the requests were cancelled from city 

##Finding the percentage of requests in which no cars available from city 
T_mrush_cnaprcnt <- T_mrush_nacity/T_mrush_rcity *100
T_mrush_cnaprcnt #About 23% of the requests were there in which No cars were available from city

#2. Evening Rush 
#Total number of requests made
T_erush_request <- length(Uevening_rush$Request.id)
T_erush_request #2646 total requests made

#Number of requests cancelled or cars unavailable 
T_erush_unavailable <- length(which(Uevening_rush$Status != "Trip Completed"))
T_erush_unavailable # 1708 requests were either cancelled or no cars available

#Finding the percentage of requests cancelled or no cars available 

T_erush_un_prcnt <- T_erush_unavailable/T_erush_request * 100
T_erush_un_prcnt # About 64% cabs were getting cancelled or no cars available 

#Finding the number of requests made from pick up point "Airport"
T_erush_rairport <- length(which(Uevening_rush$Pickup.point == "Airport"))
T_erush_rairport #1983 requests were made during from city

#Finding the number of requests in which no cars were available from Airport
T_erush_naairport <- length(which(Uevening_rush$Pickup.point == "Airport" & Uevening_rush$Status == "No Cars Available"))
T_erush_naairport #1421 requests were there in which no cars werer available from airport

##Finding the percentage of requests in which no cars available from city 
T_erush_anaprcnt <- T_erush_naairport/T_erush_rairport *100
T_erush_anaprcnt #About 70% of the requests were there in which No cars were available from airport
