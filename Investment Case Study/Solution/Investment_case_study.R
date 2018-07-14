## Checkpoint 1 : Data Cleaning 1

# Loading the companies and rounds2 files into dataframe companies & rounds2 respectively.

companies <- read.delim("companies.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")

rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)

# Loading packages in library to work with data frames

library(stringr)
library(dplyr)
library(tidyr)

# Lowercase the company_permalink of rounds2 df.

rounds2$company_permalink <- str_to_lower(rounds2$company_permalink, locale = "e")
head(rounds2)

# Lowercase the permalink of companies df.

companies$permalink <- str_to_lower(companies$permalink, locale = "e")
head(companies)

# Storing the distinct unique companies present in rounds2 df in rs_permalinks vector. 

rs_permalinks <- distinct(rounds2, company_permalink)
head(rs_permalinks)

# Counting the unique companies present in rounds2 df based on permalink column as it is unique for each company. 
nrow(rs_permalinks)

# Total unique companies present in rounds2 file are 66368.


# Counting the unique companies present in companies df based, for every company one row is there in df. 
length(unique(companies$permalink))

# Total unique companies present in companies file are 66368.


#Finding the companies in the rounds2 df which are not present in companies df, storing in companynp
companynp <- rounds2$company_permalink[which(rs_permalinks != companies$permalink)]
companynp

# There is no company in rounds2 file which are not present in companies file. 

# Merging the two data frames i.e. companies & rounds2 in master_frame.
master_frame <- merge(companies, rounds2, by.x = "permalink", by.y = "company_permalink", all.x = T)
head(master_frame)

#Finding the number of observation present in master_frame
nrow(master_frame)

# Number of observations present in master_frame are 114949.

#-------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------

## Checkpoint 2 : Funding type Analysis

#Finding if there are NA values present in the column "raised_amount_usd" of master_frame. 
sum(is.na(master_frame$raised_amount_usd))

# Assumption :
# There are missing/NA values in raised_amount_usd column of rounds2. However, funded_at is present for those.
# We are assuming those values to be 0.

#Replacing NA values of raised_amount_usd column of master_frame df with zero(0).
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <-0

#Grouping the master_frame based on funding types to calculate the average funding amount for each funding type and storing in funding_grp df.
funding_grp <- group_by(master_frame, funding_round_type)%>%
  summarise(mean(raised_amount_usd))
  
#Making the data frame of funding_grp and naming the column names.
funding_grp <- data.frame(funding_grp)
names(funding_grp) <- c("funding_type", "average_investment_amount")
funding_grp

# Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, finding the which investment type
# is suitable for them. 

suitable_typ <- filter(funding_grp, between(average_investment_amount, 5000000, 15000000))
suitable_typ

# "Venture" investment type is most suitable type for investment. 

#--------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------

## Checkpoint 3 : Country Analysis

#Grouping the master_frame df based on country for venture funding, finding the total funding and storing in countries_vnt df. 

countries_vnt <- master_frame %>%
filter(funding_round_type == "venture") %>%
  group_by(country_code) %>%
  summarise(sum(raised_amount_usd)) 
head(countries_vnt)

#Making countries_vnt as data frame   

countries_vnt <- data.frame(countries_vnt, stringsAsFactors = FALSE)

#Naming the columns
names(countries_vnt) <- c("country", "funding")

#Ordering countries_vnt df in desc based on funding.
countries_vnt <- arrange(countries_vnt,desc(funding))
head(countries_vnt)

# Storing top 9 funding countries in top9 df and identifying the top 3 english speaking funding countries.  
top9 <- head(countries_vnt, 9)
top9

# Indentifying the top 3 english speaking countries from dataframe top9 using the pdf provided for countries.
# Top 3 countries are USA, GBR & IND.

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

## Checkpoint 4 : Sector Analysis 1

# Extracting the primary sector of each category from the category_list column of master_frame df and storing in new column primsector.

master_frame$primsector <- str_split_fixed(master_frame$category_list,"\\|",2)[,1]
                                           
nrow(master_frame)
head(master_frame)

#Lower case the primsector column of master_frame df.
master_frame$primsector <- str_to_lower(master_frame$primsector, locale = "e")

#Loading the mapping file into mapping data frame. 

mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE)
head(mapping)

#Changing the wide data format of mapping df to long format and storing in mapping1 df

mapping1 <- gather(mapping, key = "main_sector", value = "c_val", 2:10)
head(mapping1)

mapping1 <- mapping1[!(mapping1$c_val == 0), ]

#Removing c_val column from mapping1 df
mapping1 <- mapping1[-3]
head(mapping1)

#Lower case the category_list column of mapping1 df.
mapping1$category_list <- str_to_lower(mapping1$category_list, locale = "e")

#category_list column of mapping1 df has discrepancy, as in many values "na" is replaced with "0". For e.g. "Analytics" is written as "A0lytics".
# So converting "0" with "na" using gsub function. 

mapping1$category_list <- gsub("0", "na", mapping1$category_list)

#Merging master_frame df & mapping1 based on primary sector & category list respectively, and storing in master_frame. 

master_frame <- master_frame <- merge(master_frame, mapping1, by.x = "primsector", by.y = "category_list", all.x = TRUE)
nrow(master_frame)

#Extracting CSV file "master_frame.csv" from R to do the plotting  in Tableau.

write.csv(master_frame, file = "master_frame.csv",row.names=FALSE)

#-----------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------

##Checkpoint 5: Sector Analysis 2

## So far doing the analysis, we have found that most suitable funding type is "venture" and the top three english speaking countries
## where investment is occuring are USA, GBR & IND. 

# Finding the 3 data frames for each of the three countries. 

# 1. USA
#..........

# Creating data frame D1_USA containing all the columns of the master_frame for the "venture" investment falling with in the 5-15 million
# USD range. 

D1_USA <- master_frame %>%
  filter(funding_round_type == "venture" & country_code == "USA")

# Creating data frame D2_USA contains the total number(count) investment for each main sector in a seperate column.

D2_USA <- D1_USA %>%
  group_by(main_sector) %>%
  summarise(count=n()) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  arrange(desc(count))

head(D2_USA)

#Finding the total number of investments(count) for USA.
sum(D2_USA$count)

# Creating data frame D3_USA contains the total amount invested for each main sector in a seperate column.

D3_USA <- D1_USA %>%
  group_by(main_sector) %>%
  summarise(sum(raised_amount_usd)) %>%
  data.frame(stringsAsFactors = FALSE)

names(D3_USA) <- c("main_sector", "total_investment")
D3_USA <- arrange(D3_USA, desc(total_investment))  

#Finding the total investment for USA.
sum(D3_USA$total_investment)

# For top sector count wise i.e "Others", grouping by permalink and calculating the total investment for each company
# Then arrange on the descending order of total investment which companies have recieved and storing in top1_usa_c.

top1_usa_c <- filter(D1_USA, main_sector == "Others" ) %>%
  group_by(permalink) %>%
  summarise(sum(raised_amount_usd)) %>%
  data.frame(stringsAsFactors = FALSE)

names(top1_usa_c) <- c("permalink", "total_investment")

top1_usa_c <- arrange(top1_usa_c, desc(total_investment))

# Finding the Company that received highest investment for sector "Others" based on the permalink.

top1_usa <- filter(master_frame, permalink == top1_usa_c$permalink[1]) %>%
  head(1)

top1_usa$name

## So the company is "SoFi or Social Finance".

# For 2nd best sector count wise i.e "Cleantech...Semiconductors", grouping by permalink and calculating the total investment for each company
# Then arrange on the descending order of total investment which companies have recieved and storing in top2_usa_c.

top2_usa_c <- filter(D1_USA, main_sector == "Cleantech...Semiconductors" ) %>%
  group_by(permalink) %>%
  summarise(sum(raised_amount_usd)) %>%
  data.frame(stringsAsFactors = FALSE)

names(top2_usa_c) <- c("permalink", "total_investment")

top2_usa_c <- arrange(top2_usa_c, desc(total_investment))
  
# Finding the Company that received highest investment for sector "Cleantech...Semiconductors" based on the permalink.

top2_usa <- filter(master_frame, permalink == top2_usa_c$permalink[1]) %>%
  head(1)

top2_usa$name

## So the company is "Freescale Semiconductor".

# 2. GBR
#..........

# Creating data frame D1_GBR containing all the columns of the master_frame for the "venture" investment falling with in the 5-15 million
# USD range. 

D1_GBR <- master_frame %>%
  filter(funding_round_type == "venture" & country_code == "GBR" )

# Creating data frame D2_GBR contains the total number(count) investment for each main sector in a seperate column.

D2_GBR <- D1_GBR %>%
  group_by(main_sector) %>%
  summarise(count=n()) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  arrange(desc(count))

head(D2_GBR)

#Finding the total number of investments(count) for GBR.
sum(D2_GBR$count)

# Creating data frame D3_GBR contains the total amount invested for each main sector in a seperate column.

D3_GBR <- D1_GBR %>%
  group_by(main_sector) %>%
  summarise(sum(raised_amount_usd)) %>%
  data.frame(stringsAsFactors = FALSE)

names(D3_GBR) <- c("main_sector", "total_investment")
D3_GBR <- arrange(D3_GBR, desc(total_investment))  

#Finding the total investment for GBR.
sum(D3_GBR$total_investment)

# For top sector count wise i.e "Others", grouping by permalink and calculating the total investment for each company
# Then arrange on the descending order of total investment which companies have recieved and storing in top1_gbr_c.

top1_gbr_c <- filter(D1_GBR, main_sector == "Others" ) %>%
  group_by(permalink) %>%
  summarise(sum(raised_amount_usd)) %>%
  data.frame(stringsAsFactors = FALSE)

names(top1_gbr_c) <- c("permalink", "total_investment")

top1_gbr_c <- arrange(top1_gbr_c, desc(total_investment))

# Finding the Company that received highest investment for sector "Others" based on the permalink.

top1_gbr <- filter(master_frame, permalink == top1_gbr_c$permalink[1]) %>%
  head(1)

top1_gbr$name

## So the company is "OneWeb".

# For 2nd best sector count wise i.e "Social..Finance..Analytics..Advertising", grouping by permalink and calculating the total investment for each company
# Then arrange on the descending order of total investment which companies have recieved and storing in top2_gbr_c.

top2_gbr_c <- filter(D1_GBR, main_sector == "Social..Finance..Analytics..Advertising" ) %>%
  group_by(permalink) %>%
  summarise(sum(raised_amount_usd)) %>%
  data.frame(stringsAsFactors = FALSE)

names(top2_gbr_c) <- c("permalink", "total_investment")

top2_gbr_c <- arrange(top2_gbr_c, desc(total_investment))

# Finding the Company that received highest investment for sector "Social..Finance..Analytics..Advertising" based on the permalink.

top2_gbr <- filter(master_frame, permalink == top2_gbr_c$permalink[1]) %>%
  head(1)

top2_gbr$name

## So the company is Powa Technologies.

# 3. IND
#..........

# Creating data frame D1_IND containing all the columns of the master_frame for the "venture" investment falling with in the 5-15 million
# USD range. 

D1_IND <- master_frame %>%
  filter(funding_round_type == "venture" & country_code == "IND" )

# Creating data frame D2_IND contains the total number(count) investment for each main sector in a seperate column.

D2_IND <- D1_IND %>%
  group_by(main_sector) %>%
  summarise(count=n()) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  arrange(desc(count))

head(D2_IND)

#Finding the total number of investments(count) for IND.
sum(D2_IND$count)

# Creating data frame D3_IND contains the total amount invested for each main sector in a seperate column.

D3_IND <- D1_IND %>%
  group_by(main_sector) %>%
  summarise(sum(raised_amount_usd)) %>%
  data.frame(stringsAsFactors = FALSE)

names(D3_IND) <- c("main_sector", "total_investment")
D3_IND <- arrange(D3_IND, desc(total_investment)) 

#Finding the total investment for IND.
sum(D3_IND$total_investment)

# For top sector count wise i.e "Others", grouping by permalink and calculating the total investment for each company
# Then arrange on the descending order of total investment which companies have recieved and storing in top1_ind_c.

top1_ind_c <- filter(D1_IND, main_sector == "Others" ) %>%
  group_by(permalink) %>%
  summarise(sum(raised_amount_usd)) %>%
  data.frame(stringsAsFactors = FALSE)

names(top1_ind_c) <- c("permalink", "total_investment")

top1_ind_c <- arrange(top1_ind_c, desc(total_investment))

# Finding the Company that received highest investment for sector "Others" based on the permalink.

top1_ind <- filter(master_frame, permalink == top1_ind_c$permalink[1]) %>%
  head(1)

top1_ind$name

## So the company is Flipkart.

# For 2nd best sector count wise i.e "Social..Finance..Analytics..Advertising", grouping by permalink and calculating the total investment for each company
# Then arrange on the descending order of total investment which companies have recieved and storing in top2_ind_c.

top2_ind_c <- filter(D1_IND, main_sector == "Social..Finance..Analytics..Advertising" ) %>%
  group_by(permalink) %>%
  summarise(sum(raised_amount_usd)) %>%
  data.frame(stringsAsFactors = FALSE)

names(top2_ind_c) <- c("permalink", "total_investment")

top2_ind_c<- arrange(top2_ind_c, desc(total_investment))

# Finding the Company that received highest investment for sector "Social..Finance..Analytics..Advertising" based on the permalink.

top2_ind <- filter(master_frame, permalink == top2_ind_c$permalink[1]) %>%
  head(1)

top2_ind$name

## So the company is ShopClues.com
