#------------------------
##Attaching the packages
#------------------------
library(tidyverse)
library(lubridate)
library(graphics)
library(forecast)
library(tseries)

cols <- c("black", "blue")
labels <- c("Raw", "Smoothed")

#-----------------------
##Business Understanding
#-----------------------

#"Global Mart" is an online store super giant having worldwide operations.
#It takes orders and delivers across the globe and deals with all the major product categories - consumer, corporate & home office
#We need to finalise the plan for the next 6 months that would help to manage the revenue and inventory accordingly.
#We need to find the 2 most profitable and consistent buckets of 21 buckets from 7 different market segments and in 3 major categories. 
#After finding the 2 buckets we need to forecast the sales and quantity for next 6 months

#----------------------------------
#Data understanding and Prepartion 
#----------------------------------

#Loading the Data
#----------------
superstore <- read.csv("Global Superstore.csv", header = TRUE, stringsAsFactors = FALSE)

str(superstore) #Dataset contains 51290 rows and 24 columns (7 numerics & 17 character) 
summary(superstore)

#Data Preparation
#----------------

#Checking the NA values in dataframe
sapply(superstore, function(x) sum(is.na(x))) #Postal.Code column has NA values but not replacing with any value as it will not be required for summarization

#Checking the blank values in dataframe
sapply(superstore, function(x) sum(which(x == ""))) #No Blank values in columns

#Checking duplicate rows
sum(duplicated(superstore)) #No duplicate rows in dataframe

#Removing the columns which are not relevant for analysis and storing in new dataframe "superstore1"
superstore1 <- superstore[c("Order.Date", "Segment", "Market", "Sales", "Quantity", "Profit")]
View(superstore1)
str(superstore1)

#Order.Date - Converting "Order.Date" column to date column and calculating the number of months have passed from first month, storing in new column
superstore1$Order.Date <- dmy(superstore1$Order.Date)
str(superstore1)
startdate <- min(superstore1$Order.Date) #Getting the start date of orders
startdate ##"2011-01-01"

#Creating a new column "Month" to store the difference in number of momnths from first month
superstore1$Month <- sapply(superstore1$Order.Date, 
                                  function(x) length(seq(from= min(superstore1$Order.Date), to=x, by='month')))

#Removing the column "Order.date as it is not relevant now after calculating the "Month" column 
superstore1$Order.Date <- NULL

#Finding 21 segments based on "Segment" & "Market" column in "superstore1" dataframe
segments <- superstore1 %>%
  group_by(Segment, Market) %>%
  summarise(COV = sd(Profit)/mean(Profit)) %>% #COV = Coefficient of variation 
  arrange(COV) %>%
  data.frame()

##Taking the subsets of "superstore1" dataframe based on "segments"
segment1  <- subset(superstore1, Segment == segments$Segment[1] &  Market == segments$Market[1])
segment2  <- subset(superstore1, Segment == segments$Segment[2] &  Market == segments$Market[2])
segment3  <- subset(superstore1, Segment == segments$Segment[3] &  Market == segments$Market[3])
segment4  <- subset(superstore1, Segment == segments$Segment[4] &  Market == segments$Market[4])
segment5  <- subset(superstore1, Segment == segments$Segment[5] &  Market == segments$Market[5])
segment6  <- subset(superstore1, Segment == segments$Segment[6] &  Market == segments$Market[6])
segment7  <- subset(superstore1, Segment == segments$Segment[7] &  Market == segments$Market[7])
segment8  <- subset(superstore1, Segment == segments$Segment[8] &  Market == segments$Market[8])
segment9  <- subset(superstore1, Segment == segments$Segment[9] &  Market == segments$Market[9])
segment10 <- subset(superstore1, Segment == segments$Segment[10] & Market == segments$Market[10])
segment11 <- subset(superstore1, Segment == segments$Segment[11] & Market == segments$Market[11])
segment12 <- subset(superstore1, Segment == segments$Segment[12] & Market == segments$Market[12])
segment13 <- subset(superstore1, Segment == segments$Segment[13] & Market == segments$Market[13])
segment14 <- subset(superstore1, Segment == segments$Segment[14] &  Market == segments$Market[14])
segment15 <- subset(superstore1, Segment == segments$Segment[15] & Market == segments$Market[15])
segment16 <- subset(superstore1, Segment == segments$Segment[16] & Market == segments$Market[16])
segment17 <- subset(superstore1, Segment == segments$Segment[17] & Market == segments$Market[17])
segment18 <- subset(superstore1, Segment == segments$Segment[18] & Market == segments$Market[18])
segment19 <- subset(superstore1, Segment == segments$Segment[19] & Market == segments$Market[19])
segment20 <- subset(superstore1, Segment == segments$Segment[20] & Market == segments$Market[20])
segment21 <- subset(superstore1, Segment == segments$Segment[21] & Market == segments$Market[21])

### Aggregating "Sales", "Quantity" & "Profit" columns of above 21 segments based on "Month" column
segment1_a <- segment1 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>% 
  arrange(Month) %>%
  data.frame()

segment2_a <- segment2 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>% 
  arrange(Month) %>%
  data.frame()

segment3_a <- segment3 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment4_a <- segment4 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment5_a <- segment5 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment6_a <- segment6 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment7_a <- segment7 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment8_a <- segment8 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment9_a <- segment9 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment10_a <- segment10 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment11_a <- segment11 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment12_a <- segment12 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment13_a <- segment13 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment14_a <- segment14 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment15_a <- segment15 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment16_a <- segment16 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment17_a <- segment17 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment18_a <- segment18 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment19_a <- segment19 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment20_a <- segment20 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

segment21_a <- segment21 %>%
  group_by(Month) %>%
  summarise(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit)) %>%
  arrange(Month) %>%
  data.frame()

#----------------
## Data Analysis
#----------------
sgmt_cov <- data.frame(matrix(ncol = 5, nrow = 21))
names(sgmt_cov) <- c("segment", "cov", "total_profit", "average_profit", "SegmentName")
i <- 1
for (i in 1:21) {
  sgmt_cov[i,1] <- i
} #Initialising the segments values

#Storing the coefficient of variation of segments and total profit in cov and total_profit columns of sgmt_cov data frame respectively
#Coefficient of variation
sgmt_cov[1,2]  <- sd(segment1_a$Profit)/mean(segment1_a$Profit)
sgmt_cov[2,2]  <- sd(segment2_a$Profit)/mean(segment2_a$Profit)
sgmt_cov[3,2]  <- sd(segment3_a$Profit)/mean(segment3_a$Profit)
sgmt_cov[4,2]  <- sd(segment4_a$Profit)/mean(segment4_a$Profit)
sgmt_cov[5,2]  <- sd(segment5_a$Profit)/mean(segment5_a$Profit)
sgmt_cov[6,2]  <- sd(segment6_a$Profit)/mean(segment6_a$Profit)
sgmt_cov[7,2]  <- sd(segment7_a$Profit)/mean(segment7_a$Profit)
sgmt_cov[8,2]  <- sd(segment8_a$Profit)/mean(segment8_a$Profit)
sgmt_cov[9,2]  <- sd(segment9_a$Profit)/mean(segment9_a$Profit)
sgmt_cov[10,2] <- sd(segment10_a$Profit)/mean(segment10_a$Profit)
sgmt_cov[11,2] <- sd(segment11_a$Profit)/mean(segment11_a$Profit)
sgmt_cov[12,2] <- sd(segment12_a$Profit)/mean(segment12_a$Profit)
sgmt_cov[13,2] <- sd(segment13_a$Profit)/mean(segment13_a$Profit)
sgmt_cov[14,2] <- sd(segment14_a$Profit)/mean(segment14_a$Profit)
sgmt_cov[15,2] <- sd(segment15_a$Profit)/mean(segment15_a$Profit)
sgmt_cov[16,2] <- sd(segment16_a$Profit)/mean(segment16_a$Profit)
sgmt_cov[17,2] <- sd(segment17_a$Profit)/mean(segment17_a$Profit)
sgmt_cov[18,2] <- sd(segment18_a$Profit)/mean(segment18_a$Profit)
sgmt_cov[19,2] <- sd(segment19_a$Profit)/mean(segment19_a$Profit)
sgmt_cov[20,2] <- sd(segment20_a$Profit)/mean(segment20_a$Profit)
sgmt_cov[21,2] <- sd(segment21_a$Profit)/mean(segment21_a$Profit)

#Total Profit
sgmt_cov[1,3]  <- sum(segment1_a$Profit)
sgmt_cov[2,3]  <- sum(segment2_a$Profit)
sgmt_cov[3,3]  <- sum(segment3_a$Profit)
sgmt_cov[4,3]  <- sum(segment4_a$Profit)
sgmt_cov[5,3]  <- sum(segment5_a$Profit)
sgmt_cov[6,3]  <- sum(segment6_a$Profit)
sgmt_cov[7,3]  <- sum(segment7_a$Profit)
sgmt_cov[8,3]  <- sum(segment8_a$Profit)
sgmt_cov[9,3]  <- sum(segment9_a$Profit)
sgmt_cov[10,3] <- sum(segment10_a$Profit)
sgmt_cov[11,3] <- sum(segment11_a$Profit)
sgmt_cov[12,3] <- sum(segment12_a$Profit)
sgmt_cov[13,3] <- sum(segment13_a$Profit)
sgmt_cov[14,3] <- sum(segment14_a$Profit)
sgmt_cov[15,3] <- sum(segment15_a$Profit)
sgmt_cov[16,3] <- sum(segment16_a$Profit)
sgmt_cov[17,3] <- sum(segment17_a$Profit)
sgmt_cov[18,3] <- sum(segment18_a$Profit)
sgmt_cov[19,3] <- sum(segment19_a$Profit)
sgmt_cov[20,3] <- sum(segment20_a$Profit)
sgmt_cov[21,3] <- sum(segment21_a$Profit)

# Average Profit
sgmt_cov[1,4]  <- mean(segment1_a$Profit)
sgmt_cov[2,4]  <- mean(segment2_a$Profit)
sgmt_cov[3,4]  <- mean(segment3_a$Profit)
sgmt_cov[4,4]  <- mean(segment4_a$Profit)
sgmt_cov[5,4]  <- mean(segment5_a$Profit)
sgmt_cov[6,4]  <- mean(segment6_a$Profit)
sgmt_cov[7,4]  <- mean(segment7_a$Profit)
sgmt_cov[8,4]  <- mean(segment8_a$Profit)
sgmt_cov[9,4]  <- mean(segment9_a$Profit)
sgmt_cov[10,4] <- mean(segment10_a$Profit)
sgmt_cov[11,4] <- mean(segment11_a$Profit)
sgmt_cov[12,4] <- mean(segment12_a$Profit)
sgmt_cov[13,4] <- mean(segment13_a$Profit)
sgmt_cov[14,4] <- mean(segment14_a$Profit)
sgmt_cov[15,4] <- mean(segment15_a$Profit)
sgmt_cov[16,4] <- mean(segment16_a$Profit)
sgmt_cov[17,4] <- mean(segment17_a$Profit)
sgmt_cov[18,4] <- mean(segment18_a$Profit)
sgmt_cov[19,4] <- mean(segment19_a$Profit)
sgmt_cov[20,4] <- mean(segment20_a$Profit)
sgmt_cov[21,4] <- mean(segment21_a$Profit)

#Segment Names
sgmt_cov[, 5] <- c("US_Consumer", "US_Corporate", "US_HomeOffice",
                           "APAC_Consumer", "APAC_Corporate", "APAC_HomeOffice",
                           "EU_Consumer", "EU_Corporate", "EU_HomeOffice",
                           "EMEA_Consumer", "EMEA_Corporate", "EMEA_HomeOffice",
                           "Africa_Consumer", "Africa_Corporate", "Africa_HomeOffice",
                           "LATAM_Consumer", "LATAM_Corporate", "LATAM_HomeOffice",
                           "Canada_Consumer", "Canada_Corporate", "Canada_HomeOffice")

sgmt_cov[, c(2,3,4)] <- sapply(sgmt_cov[, c(2,3,4)], function(x) round(x,2))


#Ordering the sgmt_cov based on "cov" values
sgmt_cov1 <- sgmt_cov
sgmt_cov1$segment <- factor(sgmt_cov1$segment, levels = sgmt_cov1[order(sgmt_cov1$cov), "segment"])

arrange(sgmt_cov1, cov, total_profit, average_profit) #Checking the two best segments in terms of COV, Total Profit and Average Profit

##Plotting the coefficient of variation 
sgmt_cov1 %>%
  ggplot(aes(x = segment, y = cov)) + 
  geom_col(aes(fill = segment)) + guides(fill = FALSE) +
  labs(x = "Segment", y = "Coefficient of Variation", title = "Coefficient of Variation of 21 different segments")

##Above chart clearly shows that the coefficient of variation(COV) of 7th and 4th segments have low values and are consistent profitable segments.
head(segment7_a)
head(segment4_a)

#checking the "segment" and "market" of these two segments
head(segment7, 1) # Segment - "Consumer" and Market - "EU"
head(segment4, 1) # Segment - "Consumer" and Market - "APAC"

##Storing segment7_a and segment4_a time series in consumer_EU and Segment_B for further analysis and model building
consumer_EU <- segment7_a
consumer_APAC <- segment4_a

#-----------------------------
#Model Building and Evaluation
#-----------------------------

#Making the time series model for "consumer_EU" and "consumer_APAC" segments to predict the sales and quantity for next 6 months

#Let's create the first time series for all rows 
#------------------------------------------------

#Sales
consumer_EU_Sales_tts <- ts(consumer_EU[2], frequency = 12)
consumer_APAC_Sales_tts <- ts(consumer_APAC[2], frequency = 12)

#Quantity
consumer_EU_Quantity_tts <- ts(consumer_EU[3], frequency = 12)
consumer_APAC_Quantity_tts <- ts(consumer_APAC[3], frequency = 12)

#Profit
consumer_EU_Profit_tts <- ts(consumer_EU[4], frequency = 12)
consumer_APAC_Profit_tts <- ts(consumer_APAC[4], frequency = 12)

#Plotting the above 6 time series
plot(consumer_EU_Sales_tts, main = "EU & Consumer Sales")
plot(consumer_APAC_Sales_tts, main = "APAC & Consumer Sales")
plot(consumer_EU_Quantity_tts, main = "EU & Consumer Quantity")
plot(consumer_APAC_Quantity_tts, main = "APAC & Consumer Quantity")
plot(consumer_EU_Profit_tts, main = "EU & Consumer Profit")
plot(consumer_APAC_Profit_tts, main = "APAC & Consumer Profit")

#Let's create the model on first 42 rows as remaining 6 rows will be used for evaluation
#---------------------------------------------------------------------------------------
consumer_EU_p <- consumer_EU[1:42, ]
consumer_APAC_p <- consumer_APAC[1:42, ]

#Sales
consumer_EU_Sales_pts <- ts(consumer_EU_p[2], frequency = 12)
consumer_APAC_Sales_pts <- ts(consumer_APAC_p[2], frequency = 12)

#Quantity
consumer_EU_Quantity_pts <- ts(consumer_EU_p[3], frequency = 12)
consumer_APAC_Quantity_pts <- ts(consumer_APAC_p[3], frequency = 12)

#Profit
consumer_EU_Profit_pts <- ts(consumer_EU_p[4], frequency = 12)
consumer_APAC_Profit_pts <- ts(consumer_APAC_p[4], frequency = 12)

#Plotting the above 6 time series
plot(consumer_EU_Sales_pts, main = "EU & Consumer Sales")
plot(consumer_APAC_Sales_pts, main = "APAC & Consumer Sales")
plot(consumer_EU_Quantity_pts, main = "EU & Consumer Quantity")
plot(consumer_APAC_Quantity_pts, main = "APAC & Consumer Quantity")
plot(consumer_EU_Profit_pts, main = "EU & Consumer Profit")
plot(consumer_APAC_Profit_pts, main = "APAC & Consumer Profit")

#Above chart clearly shows that the smoothing will be required for all the time series
#Smoothing the series - Moving Average Smoothing for all six time series 

#consumer_EU_Sales_pts
#--
w <-1
consumer_EU_Sales_pts_sm <- stats::filter(consumer_EU_Sales_pts, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- consumer_EU_Sales_pts_sm[w+2] - consumer_EU_Sales_pts_sm[w+1]
for (i in seq(w,1,-1)) {
  consumer_EU_Sales_pts_sm[i] <- consumer_EU_Sales_pts_sm[i+1] - diff
}

#Smoothing right end of the time series
n <- length(consumer_EU_Sales_pts)
diff <- consumer_EU_Sales_pts_sm[n-w] - consumer_EU_Sales_pts_sm[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer_EU_Sales_pts_sm[i] <- consumer_EU_Sales_pts_sm[i-1] + diff
}

#Convert the time series to a dataframe
consumer_EU_Sales_pts_sm_df <- as.data.frame(cbind(consumer_EU_p$Month, as.vector(consumer_EU_Sales_pts_sm)))
colnames(consumer_EU_Sales_pts_sm_df) <- c('Month', 'Sales')
lines(consumer_EU_Sales_pts_sm, col = "blue")

legend("bottomright", labels, col=cols, lwd=2)

#consumer_APAC_Sales_pts
#--
w <-1
consumer_APAC_Sales_pts_sm <- stats::filter(consumer_APAC_Sales_pts, 
                                          filter=rep(1/(2*w+1),(2*w+1)), 
                                          method='convolution', sides=2)

#Smoothing left end of the time series
diff <- consumer_APAC_Sales_pts_sm[w+2] - consumer_APAC_Sales_pts_sm[w+1]
for (i in seq(w,1,-1)) {
  consumer_APAC_Sales_pts_sm[i] <- consumer_APAC_Sales_pts_sm[i+1] - diff
}

#Smoothing right end of the time series
n <- length(consumer_APAC_Sales_pts)
diff <- consumer_APAC_Sales_pts_sm[n-w] - consumer_APAC_Sales_pts_sm[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer_APAC_Sales_pts_sm[i] <- consumer_APAC_Sales_pts_sm[i-1] + diff
}

#Convert the time series to a dataframe
consumer_APAC_Sales_pts_sm_df <- as.data.frame(cbind(consumer_APAC_p$Month, as.vector(consumer_APAC_Sales_pts_sm)))
colnames(consumer_APAC_Sales_pts_sm_df) <- c('Month', 'Sales')

lines(consumer_APAC_Sales_pts_sm, col = "blue")

legend("bottomright", labels, col=cols, lwd=2)

#######################
#consumer_EU_Quantity_pts
#--
w <-1
consumer_EU_Quantity_pts_sm <- stats::filter(consumer_EU_Quantity_pts, 
                                            filter=rep(1/(2*w+1),(2*w+1)), 
                                            method='convolution', sides=2)

#Smoothing left end of the time series
diff <- consumer_EU_Quantity_pts_sm[w+2] - consumer_EU_Quantity_pts_sm[w+1]
for (i in seq(w,1,-1)) {
  consumer_EU_Quantity_pts_sm[i] <- consumer_EU_Quantity_pts_sm[i+1] - diff
}

#Smoothing right end of the time series
n <- length(consumer_EU_Quantity_pts)
diff <- consumer_EU_Quantity_pts_sm[n-w] - consumer_EU_Quantity_pts_sm[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer_EU_Quantity_pts_sm[i] <- consumer_EU_Quantity_pts_sm[i-1] + diff
}

#Convert the time series to a dataframe
consumer_EU_Quantity_pts_sm_df <- as.data.frame(cbind(consumer_EU_p$Month, as.vector(consumer_EU_Quantity_pts_sm)))
colnames(consumer_EU_Quantity_pts_sm_df) <- c('Month', 'Quantity')


lines(consumer_EU_Quantity_pts_sm, col = "blue")
legend("bottomright", labels, col=cols, lwd=2)

###################
#consumer_APAC_Quantity_pts
#--
w <-1
consumer_APAC_Quantity_pts_sm <- stats::filter(consumer_APAC_Quantity_pts, 
                                             filter=rep(1/(2*w+1),(2*w+1)), 
                                             method='convolution', sides=2)

#Smoothing left end of the time series
diff <- consumer_APAC_Quantity_pts_sm[w+2] - consumer_APAC_Quantity_pts_sm[w+1]
for (i in seq(w,1,-1)) {
  consumer_APAC_Quantity_pts_sm[i] <- consumer_APAC_Quantity_pts_sm[i+1] - diff
}

#Smoothing right end of the time series
n <- length(consumer_APAC_Quantity_pts)
diff <- consumer_APAC_Quantity_pts_sm[n-w] - consumer_APAC_Quantity_pts_sm[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer_APAC_Quantity_pts_sm[i] <- consumer_APAC_Quantity_pts_sm[i-1] + diff
}

#Convert the time series to a dataframe
consumer_APAC_Quantity_pts_sm_df <- as.data.frame(cbind(consumer_APAC_p$Month, as.vector(consumer_APAC_Quantity_pts_sm)))
colnames(consumer_APAC_Quantity_pts_sm_df) <- c('Month', 'Quantity')

lines(consumer_APAC_Quantity_pts_sm, col = "blue")
legend("bottomright", labels, col=cols, lwd=2)

##########################
#consumer_EU_Profit_pts
#--
w <-1
consumer_EU_Profit_pts_sm <- stats::filter(consumer_EU_Profit_pts, 
                                               filter=rep(1/(2*w+1),(2*w+1)), 
                                               method='convolution', sides=2)

#Smoothing left end of the time series
diff <- consumer_EU_Profit_pts_sm[w+2] - consumer_EU_Profit_pts_sm[w+1]
for (i in seq(w,1,-1)) {
  consumer_EU_Profit_pts_sm[i] <- consumer_EU_Profit_pts_sm[i+1] - diff
}

#Smoothing right end of the time series
n <- length(consumer_EU_Profit_pts)
diff <- consumer_EU_Profit_pts_sm[n-w] - consumer_EU_Profit_pts_sm[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer_EU_Profit_pts_sm[i] <- consumer_EU_Profit_pts_sm[i-1] + diff
}

#Convert the time series to a dataframe
consumer_EU_Profit_pts_sm_df <- as.data.frame(cbind(consumer_EU_p$Month, as.vector(consumer_EU_Profit_pts_sm)))
colnames(consumer_EU_Profit_pts_sm_df) <- c('Month', 'Profit')

lines(consumer_EU_Profit_pts_sm, col = "blue")
legend("bottomright", labels, col=cols, lwd=2)

##########################
#consumer_APAC_Profit_pts
#--
w <-1
consumer_APAC_Profit_pts_sm <- stats::filter(consumer_APAC_Profit_pts, 
                                           filter=rep(1/(2*w+1),(2*w+1)), 
                                           method='convolution', sides=2)

#Smoothing left end of the time series
diff <- consumer_APAC_Profit_pts_sm[w+2] - consumer_APAC_Profit_pts_sm[w+1]
for (i in seq(w,1,-1)) {
  consumer_APAC_Profit_pts_sm[i] <- consumer_APAC_Profit_pts_sm[i+1] - diff
}

#Smoothing right end of the time series
n <- length(consumer_APAC_Profit_pts)
diff <- consumer_APAC_Profit_pts_sm[n-w] - consumer_APAC_Profit_pts_sm[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer_APAC_Profit_pts_sm[i] <- consumer_APAC_Profit_pts_sm[i-1] + diff
}

#Convert the time series to a dataframe
consumer_APAC_Profit_pts_sm_df <- as.data.frame(cbind(consumer_APAC_p$Month, as.vector(consumer_APAC_Profit_pts_sm)))
colnames(consumer_APAC_Profit_pts_sm_df) <- c('Month', 'Profit')

####################################

##Global Prediction - Foecast of Sales & Quantity for Consumer of APAC and EU region
##----------------------------------------------------------------------------------

#Consumer_EU_Sales
#-----------------
plot(consumer_EU_Sales_pts, main = "Consumer Sales in EU - Smoothened time Series")
lines(consumer_EU_Sales_pts_sm, col="blue", lwd=2)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lf_consumer_EU_Sales_pts_sm <- lm(Sales ~ poly(sin(Month), 1)
            * poly(sin(Month), 2), data=consumer_EU_Sales_pts_sm_df) ### We are guessing it will be sinusiodal waves

gp_consumer_EU_Sales <- predict(lf_consumer_EU_Sales_pts_sm, Month=consumer_EU_p$Month)
summary(gp_consumer_EU_Sales)

lines(consumer_EU_p$Month, gp_consumer_EU_Sales, col='red', lwd=2)
labels <- c("Global Prediction")
cols <- c("Red")
legend("bottomright", labels, col=cols, lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

lp_consumer_EU_Sales <- consumer_EU_Sales_pts_sm - gp_consumer_EU_Sales
plot(lp_consumer_EU_Sales, col='green', type = "l", lwd = 2)
legend("bottomright", c("Local Prediction"), col= c("Green"), lwd=2)

acf(lp_consumer_EU_Sales, main = "ACF for EU_Consumer Sales") #After 0.3 lag it is statinary
acf(lp_consumer_EU_Sales, type="partial", main = "PACF for EU_Consumer Sales") #After 0.1 lag it is statinary
armafit_consumer_EU_Sales <- auto.arima(lp_consumer_EU_Sales)

tsdiag(armafit_consumer_EU_Sales)
armafit_consumer_EU_Sales ##No Autoregressive behaviour in residual time series 


#We'll check if the residual series is white noise

## We will reconfirm and do some test
resi_eu_sales <- lp_consumer_EU_Sales-fitted(armafit_consumer_EU_Sales)

adf.test(resi_eu_sales,alternative = "stationary")
kpss.test(resi_eu_sales)

## Both KPSS and adf says that it is white noise 

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata1 <- consumer_EU[43:48, 1:2]
timevals_out1 <- outdata1$Month

gp_consumer_EU_Sales_out <- predict(lf_consumer_EU_Sales_pts_sm,data.frame(Month =timevals_out1))
gp_consumer_EU_Sales_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec1 <- accuracy(gp_consumer_EU_Sales_out,outdata1[,2])[5]
MAPE_class_dec1 #40.43501

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred1 <- c(ts(gp_consumer_EU_Sales),ts(gp_consumer_EU_Sales_out))
plot(consumer_EU_Sales_tts , col = "black", main = "Original and Forecasted Values")
lines(class_dec_pred1, col = "magenta")



#ts.plot(consumer_EU_Sales_tts, gp_consumer_EU_Sales_out)
Next_6_month <- c(49,50,51,52,53,54)
#Prediction for next 6 months of Sales of consumer segment of EU region
consumer_eu_pred_6_mnth_sales <- predict(lf_consumer_EU_Sales_pts_sm,data.frame(Month = Next_6_month))
consumer_eu_pred_6_mnth_sales

forec1 <- predict(armafit_consumer_EU_Sales, n.ahead = 6)
val <- consumer_eu_pred_6_mnth_sales + forec1$pred

ts.plot(consumer_EU_Sales_pts, val)
lines(consumer_EU_Sales_pts_sm, col = "blue")
lines(val, col = "magenta", lwd = 2)
legend("bottomright", c("Forecasted Sales"), col= c("Magenta"), lwd=2)


#Consumer_APAC_Sales
#-----------------
plot(consumer_APAC_Sales_pts, main = "Consumer Sales in APAC - Smoothened time Series")
lines(consumer_APAC_Sales_pts_sm, col="blue", lwd=2)
legend("bottomright", c("Raw", "Smoothed"), col= c("Black", "Blue"), lwd=2)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lf_consumer_APAC_Sales_pts_sm <- lm(Sales ~ poly(sin(Month), 2)
                                  * poly(sin(Month), 2), data=consumer_APAC_Sales_pts_sm_df) ### We are guessing it will be sinusiodal waves

gp_consumer_APAC_Sales <- predict(lf_consumer_APAC_Sales_pts_sm, Month=consumer_APAC_p$Month)
summary(gp_consumer_APAC_Sales)

plot(consumer_APAC_Sales_pts, main = "Consumer Sales in APAC - Smoothened time Series")
lines(consumer_APAC_Sales_pts_sm, col="blue", lwd=2)
lines(consumer_APAC_p$Month, gp_consumer_APAC_Sales, col='red', lwd=2)
legend("bottomright", c("Global prediction"), col= c("red"), lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

lp_consumer_APAC_Sales <- consumer_APAC_Sales_pts_sm - gp_consumer_APAC_Sales
plot(lp_consumer_APAC_Sales, col='green', type = "l", lwd =2)
legend("bottomright", c("Local prediction"), col= c("green"), lwd=2)
acf(lp_consumer_APAC_Sales, main = "ACF for APAC_Consumer Sales") 
acf(lp_consumer_APAC_Sales, type="partial", main = "PACF for APAC_Consumer Sales") 

armafit_consumer_APAC_Sales <- auto.arima(ts(lp_consumer_APAC_Sales))

tsdiag(armafit_consumer_APAC_Sales)
armafit_consumer_APAC_Sales ##No Autoregressive behaviour in residual time series 

#We'll check if the residual series is white noise

## We will reconfirm and do some test
resi_apac_sales <- lp_consumer_APAC_Sales - fitted(armafit_consumer_APAC_Sales)

adf.test(resi_apac_sales,alternative = "stationary")
kpss.test(resi_apac_sales)

## Both KPSS and adf says that it is white noise 

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata2 <- consumer_APAC[43:48, 1:2]
timevals_out2 <- outdata2$Month

gp_consumer_APAC_Sales_out <- predict(lf_consumer_APAC_Sales_pts_sm,data.frame(Month =timevals_out2))
gp_consumer_APAC_Sales_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec2 <- accuracy(gp_consumer_APAC_Sales_out,outdata2[,2])[5]
MAPE_class_dec2 #[1] 39.94147

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred2 <- c(ts(gp_consumer_APAC_Sales),ts(gp_consumer_APAC_Sales_out))
plot(consumer_APAC_Sales_tts , col = "black", main = "Orignal & Forecasted Values")
lines(class_dec_pred2, col = "magenta")

#Prediction for next 6 months of Sales of consumer segment of APAC region
consumer_apac_pred_6_mnth_sales <- predict(lf_consumer_APAC_Sales_pts_sm,data.frame(Month = Next_6_month))
consumer_apac_pred_6_mnth_sales


###############################################
#Consumer_EU_Quantity
#-----------------
plot(consumer_EU_Quantity_pts, main = "Consumer Quantity in EU - Smoothened time Series")
lines(consumer_EU_Quantity_pts_sm, col="blue", lwd=2)
#legend("bottomright", c("Raw", "Smoothed"), col= c("Black", "Blue"), lwd=2)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lf_consumer_EU_Quantity_pts_sm <- lm(Quantity ~ poly(sin(Month), 1)
                                  * poly(sin(Month), 2), data=consumer_EU_Quantity_pts_sm_df) ### We are guessing it will be sinusiodal waves

gp_consumer_EU_Quantity <- predict(lf_consumer_EU_Quantity_pts_sm, Month=consumer_EU_p$Month)
summary(gp_consumer_EU_Quantity)

lines(consumer_EU_p$Month, gp_consumer_EU_Quantity, col='red', lwd=2)
legend("bottomright", c("Global Prediction"), col= c("Red"), lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

lp_consumer_EU_Quantity <- consumer_EU_Quantity_pts_sm - gp_consumer_EU_Quantity
plot(lp_consumer_EU_Quantity, col='green', type = "l", lwd = 2)
legend("bottomright", c("Local Prediction"), col= c("Green"), lwd=2)
acf(lp_consumer_EU_Quantity, main = "ACF plot for EU_Consumer Quantity") 
acf(lp_consumer_EU_Quantity, type="partial", main = "PACF plot for EU_Consumer Quantity")
armafit_consumer_EU_Quantity <- auto.arima(lp_consumer_EU_Quantity)

tsdiag(armafit_consumer_EU_Quantity)
armafit_consumer_EU_Quantity ##No Autoregressive behaviour in residual time series 

#We'll check if the residual series is white noise

## We will reconfirm and do some test
resi_eu_quantity <- lp_consumer_EU_Quantity-fitted(armafit_consumer_EU_Quantity)

adf.test(resi_eu_quantity, alternative = "stationary")
kpss.test(resi_eu_quantity)

## Both KPSS and adf says that it is white noise 

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata3 <- consumer_EU[43:48, c(1,3)]
timevals_out3 <- outdata3$Month

gp_consumer_EU_Quantity_out <- predict(lf_consumer_EU_Quantity_pts_sm, data.frame(Month =timevals_out3))
gp_consumer_EU_Quantity_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec3 <- accuracy(gp_consumer_EU_Quantity_out,outdata3[,2])[5]
MAPE_class_dec3 #43.25895

#Let's also plot the predictions along with original values, to get a visual feel of the fit

class_dec_pred3 <- c(ts(gp_consumer_EU_Quantity), ts(gp_consumer_EU_Quantity_out))
plot(consumer_EU_Quantity_tts , col = "black", main = "Original & Forecasted values")
lines(class_dec_pred3, col = "magenta")


#Prediction for next 6 months of Quantity of consumer segment of EU region
consumer_eu_pred_6_mnth_quantity <- predict(lf_consumer_EU_Quantity_pts_sm, data.frame(Month = Next_6_month))
consumer_eu_pred_6_mnth_quantity



#Consumer_APAC_Quantity
#------------------------
plot(consumer_APAC_Quantity_pts, main = "Consumer Quantity in APAC - Smoothened time Series")
lines(consumer_APAC_Quantity_pts_sm, col="blue", lwd=2)
#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lf_consumer_APAC_Quantity_pts_sm <- lm(Quantity ~ poly(sin(Month), 1)
                                  * poly(sin(Month), 2), data=consumer_APAC_Quantity_pts_sm_df) ### We are guessing it will be sinusiodal waves

gp_consumer_APAC_Quantity <- predict(lf_consumer_APAC_Quantity_pts_sm, Month=consumer_EU_p$Month)
summary(gp_consumer_APAC_Quantity)

lines(consumer_APAC_p$Month, gp_consumer_APAC_Quantity, col='red', lwd=2)
#legend("bottomright", c("Global Prediction"), col= c("Red"), lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

lp_consumer_APAC_Quantity <- consumer_APAC_Quantity_pts_sm - gp_consumer_APAC_Quantity
plot(lp_consumer_APAC_Quantity, col='green', type = "l", lwd = 2)
legend("bottomright", c("Local Prediction"), col= c("Green"), lwd=2)

acf(lp_consumer_APAC_Quantity, main = "ACF plot for Consumer Quantity - APAC")
acf(lp_consumer_APAC_Quantity, type="partial", main = "PACF plot for Consumer Quantity - APAC")
armafit_consumer_APAC_Quantity <- auto.arima(lp_consumer_APAC_Quantity)

tsdiag(armafit_consumer_APAC_Quantity)
armafit_consumer_APAC_Quantity ## Autoregressive of order 1 behaviour in residual time series 

#We'll check if the residual series is white noise

## We will reconfirm and do some test
resi_apac_quantity <- lp_consumer_APAC_Quantity-fitted(armafit_consumer_APAC_Quantity)

adf.test(resi_apac_quantity,alternative = "stationary")
kpss.test(resi_apac_quantity)

## Both KPSS and adf says that it is white noise 

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata4 <- consumer_APAC[43:48, c(1,3)]
timevals_out4 <- outdata4$Month

gp_consumer_APAC_Quantity_out <- predict(lf_consumer_APAC_Quantity_pts_sm, data.frame(Month =timevals_out4))
gp_consumer_APAC_Quantity_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec4 <- accuracy(gp_consumer_APAC_Quantity_out,outdata4[,2])[5]
MAPE_class_dec4 #46.02607

#Let's also plot the predictions along with original values, to get a visual feel of the fit

class_dec_pred4 <- c(ts(gp_consumer_APAC_Quantity),ts(gp_consumer_APAC_Quantity_out))
plot(consumer_APAC_Quantity_tts , col = "black", main = "Original and Forecasted")
lines(class_dec_pred4, col = "magenta")

#Prediction for next 6 months of Quantity of consumer segment of APAC region
consumer_apac_pred_6_mnth_quantity <- predict(lf_consumer_APAC_Quantity_pts_sm, data.frame(Month = Next_6_month))
consumer_apac_pred_6_mnth_quantity

###Model Building using "auto ARIMA" function
#--------------------------------------------

#AutoArima - "Consumer EU Sales"
consumer_EU_Sales_a_arm <- auto.arima(consumer_EU_Sales_pts )
consumer_EU_Sales_a_arm
tsdiag(consumer_EU_Sales_a_arm)
plot(consumer_EU_Sales_a_arm$x, col="black", main= "EU Consumer - Sales")
lines(fitted(consumer_EU_Sales_a_arm), col="red")
legend("bottomright", c("Original", "ARIMA Fit"), col = c("Black", "red"), lwd =2)

#Again, let's check if the residual series is white noise
resi_auto_arima1 <- consumer_EU_Sales_pts - fitted(consumer_EU_Sales_a_arm)

adf.test(resi_auto_arima1,alternative = "stationary") #adf says that the residual series is not stationary
kpss.test(resi_auto_arima1) #KPSS says that the residual series is stationary

#Also, let's evaluate the model using MAPE
fcast_consumer_EU_Sales <- forecast(consumer_EU_Sales_a_arm, n.ahead = 6) #Forecasting for next 6 month to evaluate

MAPE_auto_arima1 <- accuracy(fcast_consumer_EU_Sales$mean,outdata1[,2])[5]
MAPE_auto_arima1 #15.75223 - Quite improvment as compared to classical decomposition

#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
consumer_EU_Sales_a_arm_pred <- c(fitted(consumer_EU_Sales_a_arm),ts(fcast_consumer_EU_Sales$mean))
plot(consumer_EU_Sales_tts, col = "black", main = "Original and Forecasted")
lines(consumer_EU_Sales_a_arm_pred, col = "magenta")

#Next 6 month predicted value
fcast_consumer_EU_Sales_n6 <- forecast(consumer_EU_Sales_a_arm, n.ahead = 6)
fcast_consumer_EU_Sales_n6$mean[7:12]

##-------
#AutoArima - "Consumer APAC Sales"
consumer_APAC_Sales_a_arm <- auto.arima(consumer_APAC_Sales_pts )
consumer_APAC_Sales_a_arm
tsdiag(consumer_APAC_Sales_a_arm)
plot(consumer_APAC_Sales_a_arm$x, col="black", main = "Consumer APAC - Sales")
lines(fitted(consumer_APAC_Sales_a_arm), col="red")
legend("bottomright", c("Original", "ARIMA Fit"), col= c("Black", "Red"), lwd=2)


#Again, let's check if the residual series is white noise
resi_auto_arima2 <- consumer_APAC_Sales_pts - fitted(consumer_APAC_Sales_a_arm)

adf.test(resi_auto_arima2,alternative = "stationary") #adf says that the residual series is not stationary
kpss.test(resi_auto_arima2) #KPSS says that the residual series is stationary

#Also, let's evaluate the model using MAPE
fcast_consumer_APAC_Sales <- forecast(consumer_APAC_Sales_a_arm, n.ahead = 6) #Forecasting for next 6 month to evaluate

MAPE_auto_arima2 <- accuracy(fcast_consumer_APAC_Sales$mean,outdata2[,2])[5]
MAPE_auto_arima2 #11.77618 - Quite improvment as compared to classical decomposition

#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
consumer_APAC_Sales_a_arm_pred <- c(fitted(consumer_APAC_Sales_a_arm),ts(fcast_consumer_APAC_Sales$mean))
plot(consumer_APAC_Sales_tts, col = "black", main = "Original and Forecasted")
lines(consumer_APAC_Sales_a_arm_pred, col = "magenta")

#Next 6 month predicted value
fcast_consumer_APAC_Sales_n6 <- forecast(consumer_APAC_Sales_a_arm, n.ahead = 6)
fcast_consumer_APAC_Sales_n6$mean[7:12]

##-------
#AutoArima - "Consumer EU Quantity"
consumer_EU_Quantity_a_arm <- auto.arima(consumer_EU_Quantity_pts)
consumer_EU_Quantity_a_arm
tsdiag(consumer_EU_Quantity_a_arm)
plot(consumer_EU_Quantity_a_arm$x, col="black", main = "Consumer EU - Quantity")
lines(fitted(consumer_EU_Quantity_a_arm), col="red")
legend("bottomright", c("Original", "ARIMA Fit"), col= c("Black", "Red"), lwd=2)

#Again, let's check if the residual series is white noise
resi_auto_arima3 <- consumer_EU_Quantity_pts - fitted(consumer_EU_Quantity_a_arm)

adf.test(resi_auto_arima3,alternative = "stationary") #adf says that the residual series is not stationary
kpss.test(resi_auto_arima3) #KPSS says that the residual series is stationary

#Also, let's evaluate the model using MAPE
fcast_consumer_EU_Quantity <- forecast(consumer_EU_Quantity_a_arm, n.ahead = 6) #Forecasting for next 6 month to evaluate

MAPE_auto_arima3 <- accuracy(fcast_consumer_EU_Quantity$mean,outdata3[,2])[5]
MAPE_auto_arima3 #17.61852 - Quite improvment as compared to classical decomposition

#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
consumer_EU_Quantity_a_arm_pred <- c(fitted(consumer_EU_Quantity_a_arm),ts(fcast_consumer_EU_Quantity$mean))
plot(consumer_EU_Quantity_tts, col = "black", main = "Original and Forecasted")
lines(consumer_EU_Quantity_a_arm_pred, col = "magenta")

#Next 6 month predicted value
fcast_consumer_EU_Quantity_n6 <- forecast(consumer_EU_Quantity_a_arm, n.ahead = 6)
fcast_consumer_EU_Quantity_n6$mean[7:12]

##-------
#AutoArima - "Consumer APAC Quantity"
consumer_APAC_Quantity_a_arm <- auto.arima(consumer_APAC_Quantity_pts)
consumer_APAC_Quantity_a_arm
tsdiag(consumer_APAC_Quantity_a_arm)
plot(consumer_APAC_Quantity_a_arm$x, col="black", main = "Consumer APAC - Quantity")
lines(fitted(consumer_APAC_Quantity_a_arm), col="red")
legend("bottomright", c("Original", "ARIMA Fit"), col= c("Black", "Red"), lwd=2)

#Again, let's check if the residual series is white noise
resi_auto_arima4 <- consumer_APAC_Quantity_pts - fitted(consumer_APAC_Quantity_a_arm)

adf.test(resi_auto_arima4, alternative = "stationary") #adf says that the residual series is not stationary
kpss.test(resi_auto_arima3) #KPSS says that the residual series is stationary

#Also, let's evaluate the model using MAPE
fcast_consumer_APAC_Quantity <- forecast(consumer_APAC_Quantity_a_arm, n.ahead = 6) #Forecasting next 12 months(6 for evaluation and 6 for forecast)

MAPE_auto_arima4 <- accuracy(fcast_consumer_APAC_Quantity$mean,outdata4[,2])[5]
MAPE_auto_arima4 #13.36779 - Quite improvment as compared to classical decomposition

#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
consumer_APAC_Quantity_a_arm_pred <- c(fitted(consumer_APAC_Quantity_a_arm),ts(fcast_consumer_APAC_Quantity$mean))
plot(consumer_APAC_Quantity_tts, col = "black", main = "Original and Forecasted")
lines(consumer_APAC_Quantity_a_arm_pred, col = "magenta")

#Next 6 month predicted value
fcast_consumer_APAC_Quantity_n6 <- forecast(consumer_APAC_Quantity_a_arm, n.ahead = 6)
fcast_consumer_APAC_Quantity_n6$mean[7:12]

#------------
## Conclusion
#------------

##Model successfully made using "classical decomposition" and "auto ARIMA", also Sales and Quantity are predicted for next 6 months

#----------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------