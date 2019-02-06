##----------------------------------------NYC Parking Case Study - Spark---------------------------------------------------##
##-------------------------------------------------------------------------------------------------------------------------##

# Problem Statement
#------------------

# New York City is a thriving metropolis. Just like most other metros that size, one of the biggest problems its citizens 
# face is parking. The classic combination of a huge number of cars and a cramped geography is the exact recipe that leads 
# to a huge number of parking tickets.
# In an attempt to scientifically analyse this phenomenon, the NYC Police Department has collected data for parking tickets. 
# Out of these, the data files from 2014 to 2017 are publicly available on Kaggle. We will try and perform some exploratory 
# analysis on this data.

# Load the Spark
#---------------
spark_path <- '/usr/local/spark'
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

# Initialise the sparkR session
sparkR.session(master = "yarn-client", sparkConfig = list(spark.driver.memory = "1g"))

# Adding a jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

library(stringr)
# Creating seperate Spark Dataframes for NYC Parking for 2015, 2016, 2017 fiscal year
nyc_parking_2015 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", 
                                    "CSV", header = "true", 
                                    inferSchema = "true",
                                    na.strings = c("", "NA"))
nyc_parking_2016 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", 
                                    "CSV", header="true", 
                                    inferSchema = "true",
                                    na.strings = c("", "NA"))
nyc_parking_2017 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", 
                                    "CSV", header="true", 
                                    inferSchema = "true",
                                    na.strings = c("", "NA"))

# Initial Exploration 
#---------------------
printSchema(nyc_parking_2015)
printSchema(nyc_parking_2016)
printSchema(nyc_parking_2017)

head(nyc_parking_2015) 
head(nyc_parking_2016)
head(nyc_parking_2017)

# Total Rows
nrow(nyc_parking_2015) #11809233 rows
nrow(nyc_parking_2016) #10626899 rows
nrow(nyc_parking_2017) #10803028 rows

# Structure of a file
str(nyc_parking_2015) #51 Variable
str(nyc_parking_2016) #51 Variable
str(nyc_parking_2017) #51 Variable

# Removing the spaces from column names
colnames(nyc_parking_2015) <- str_replace_all(colnames(nyc_parking_2015), pattern = " ", replacement = "")
colnames(nyc_parking_2016) <- str_replace_all(colnames(nyc_parking_2016), pattern = " ", replacement = "")
colnames(nyc_parking_2017) <- str_replace_all(colnames(nyc_parking_2017), pattern = " ", replacement = "")

# Dropping the rows if the all the value in rows contain NA values
nyc_parking_2015 <- dropna(nyc_parking_2015, how = c("all"))
nyc_parking_2016 <- dropna(nyc_parking_2016, how = c("all"))
nyc_parking_2017 <- dropna(nyc_parking_2017, how = c("all"))

# Dropping Duplicates rows bases on unique "Summons Number" column
nyc_parking_2015 <- dropDuplicates(nyc_parking_2015, "SummonsNumber")
nyc_parking_2016 <- dropDuplicates(nyc_parking_2016, "SummonsNumber")
nyc_parking_2017 <- dropDuplicates(nyc_parking_2017, "SummonsNumber")

# Creating a temporary view to execute the SQL queries
#-----------------------------------------------------
createOrReplaceTempView(nyc_parking_2015, "nyc_parking_2015_tbl")
createOrReplaceTempView(nyc_parking_2016, "nyc_parking_2016_tbl")
createOrReplaceTempView(nyc_parking_2017, "nyc_parking_2017_tbl")

# Adding JAR file in Rstudio
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

# Checking the NUll values in relavant column in above 3 data frames 

#--------Registration State Column
rs_nul_2015 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2015_tbl WHERE RegistrationState IS NULL")
head(rs_nul_2015) # No Null Values

rs_nul_2016 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2016_tbl WHERE RegistrationState IS NULL")
head(rs_nul_2016) # No Null Values

rs_nul_2017 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2017_tbl WHERE RegistrationState IS NULL")
head(rs_nul_2017) # No Null Values

#--------Violation Location Column
vl_nul_2015 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2015_tbl WHERE ViolationLocation IS NULL")
head(vl_nul_2015) # 1633006 Null Values
1633006/11809233 * 100 #13.82 %

vl_nul_2016 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2016_tbl WHERE ViolationLocation IS NULL")
head(vl_nul_2016) # 1868656 Null Values
1868656/10626899 * 100 #17.58 %

vl_nul_2017 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2017_tbl WHERE ViolationLocation IS NULL")
head(vl_nul_2017) # 2072400 Null Values
2072400/10803028 * 100 #19.18%

#--------Violation Code Column
vc_nul_2015 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2015_tbl WHERE ViolationCode IS NULL")
head(vc_nul_2015) # No Null Values

vc_nul_2016 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2016_tbl WHERE ViolationCode IS NULL")
head(vc_nul_2016) # No Null Values

vc_nul_2017 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2017_tbl WHERE ViolationCode IS NULL")
head(vc_nul_2017) # No Null Values

#--------Vehicle Body Type Column
vb_nul_2015 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2015_tbl WHERE VehicleBodyType IS NULL")
head(vb_nul_2015) # 41197 Null Values
41197/11809233 * 100 #0.34 %

vb_nul_2016 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2016_tbl WHERE VehicleBodyType IS NULL")
head(vb_nul_2016) # 39271 Null Values
39271/10626899 *100 #0.36 %

vb_nul_2017 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2017_tbl WHERE VehicleBodyType IS NULL")
head(vb_nul_2017) # 42695 Null Values
42695/10803028 *100 #0.39%

#--------Vehicle Make Column
vm_nul_2015 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2015_tbl WHERE VehicleMake IS NULL")
head(vm_nul_2015) # 68276 Null Values
68276/11809233 *100 #0.57 %

vm_nul_2016 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2016_tbl WHERE VehicleMake IS NULL")
head(vm_nul_2016) # 63578 Null Values
63578/10626899 *100 #0.59 %

vm_nul_2017 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2017_tbl WHERE VehicleMake IS NULL")
head(vm_nul_2017) # 73047 Null Values
73047/10803028 *100 #0.67%

#--------Violation Precint Column
vp_nul_2015 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2015_tbl WHERE ViolationPrecinct IS NULL")
head(vp_nul_2015) # No Null Values

vp_nul_2016 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2016_tbl WHERE ViolationPrecinct IS NULL")
head(vp_nul_2016) # 1 Null Value

vp_nul_2017 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2017_tbl WHERE ViolationPrecinct IS NULL")
head(vp_nul_2017) # No Null Values

#--------Issuer Preccint Column
ip_nul_2015 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2015_tbl WHERE IssuerPrecinct IS NULL")
head(ip_nul_2015) # No Null Values

ip_nul_2016 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2016_tbl WHERE IssuerPrecinct IS NULL")
head(ip_nul_2016) # 1 Null Value

ip_nul_2017 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2017_tbl WHERE IssuerPrecinct IS NULL")
head(ip_nul_2017) # No Null Values

#----Checking if the data has rows not of that fiscal year mentioned
rows_nf_2015 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2015_tbl WHERE (substr(IssueDate,1,2) > '03' 
                            and substr(IssueDate,4,2) = '2015') or (substr(IssueDate,1,2) < '04' 
                            and substr(IssueDate,4,2) = '2014')")

head(rows_nf_2015) #No such value

rows_nf_2016 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2016_tbl WHERE (substr(IssueDate,1,2) > '03' 
                            and substr(IssueDate,4,2) = '2016') or (substr(IssueDate,1,2) < '04' 
                            and substr(IssueDate,4,2) = '2015')")

head(rows_nf_2016) #No such value

rows_nf_2017 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2017_tbl WHERE (substr(IssueDate,1,2) > '03' 
                            and substr(IssueDate,4,2) = '2017') or (substr(IssueDate,1,2) < '04' 
                            and substr(IssueDate,4,2) = '2016')")

head(rows_nf_2017) #No such value

# Creating new dataframes of nyc parking 2015, 2016 and 2017 dataframes where removing those rows where Vehicle Body Type,
# Vehicle Make, Violation Preccint and Issuer Preccint column have any null values 

# NYC Parking 2015
nyc_parking_2015_cln1 <- filter(nyc_parking_2015, isNotNull(nyc_parking_2015$VehicleBodyType) &
                                  isNotNull(nyc_parking_2015$VehicleMake) &
                                  isNotNull(nyc_parking_2015$ViolationPrecinct) &
                                  isNotNull(nyc_parking_2015$IssuerPrecinct))

head(nrow(nyc_parking_2015_cln1)) # 10853283 rows

# NYC Parking 2016
nyc_parking_2016_cln1 <- filter(nyc_parking_2016, isNotNull(nyc_parking_2016$VehicleBodyType) &
                                  isNotNull(nyc_parking_2016$VehicleMake) &
                                  isNotNull(nyc_parking_2016$ViolationPrecinct) &
                                  isNotNull(nyc_parking_2016$IssuerPrecinct))

head(nrow(nyc_parking_2016_cln1)) # 10533559 rows


# NYC Parking 2017
nyc_parking_2017_cln1 <- filter(nyc_parking_2017, isNotNull(nyc_parking_2017$VehicleBodyType) &
                                  isNotNull(nyc_parking_2017$VehicleMake) &
                                  isNotNull(nyc_parking_2017$ViolationPrecinct) &
                                  isNotNull(nyc_parking_2017$IssuerPrecinct))

head(nrow(nyc_parking_2017_cln1)) # 10698235 rows

# Creating temporary views of above dataframe to execute the SQL queries for further analysis
#--------------------------------------------------------------------------------------------
createOrReplaceTempView(nyc_parking_2015_cln1, "nyc_parking_2015_cln1_tbl")
createOrReplaceTempView(nyc_parking_2016_cln1, "nyc_parking_2016_cln1_tbl")
createOrReplaceTempView(nyc_parking_2017_cln1, "nyc_parking_2017_cln1_tbl")

#----------------------------------------Examining the data-----------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------

# 1. Finding the total number of tickets for each fiscal year
#------------------------------------------------------------

n_tckt_nyc_2015 <- SparkR::sql("SELECT COUNT(SummonsNumber) AS number_of_ticket FROM nyc_parking_2015_cln1_tbl")
head(n_tckt_nyc_2015) #10853283 tickets

n_tckt_nyc_2016 <- SparkR::sql("SELECT COUNT(SummonsNumber) AS number_of_ticket FROM nyc_parking_2016_cln1_tbl")
head(n_tckt_nyc_2016) #10533559 tickets

n_tckt_nyc_2017 <- SparkR::sql("SELECT COUNT(SummonsNumber) AS number_of_ticket FROM nyc_parking_2017_cln1_tbl")
head(n_tckt_nyc_2017) #10698235 tickets

# 2. Finding the number of unique states for each year from where the cars that got parking tickets came from
#----------------------------------------------------------------------------------------------

n_unq_state_nyc_2015 <- SparkR::sql("SELECT COUNT(*) AS number_of_ticket, RegistrationState FROM nyc_parking_2015_cln1_tbl
                                    GROUP BY RegistrationState ORDER BY number_of_ticket DESC")
head(n_unq_state_nyc_2015, 100) # 69 unique states

n_unq_state_nyc_2016 <- SparkR::sql("SELECT COUNT(*) AS number_of_ticket, RegistrationState FROM nyc_parking_2016_cln1_tbl
                                    GROUP BY RegistrationState ORDER BY number_of_ticket DESC")
head(n_unq_state_nyc_2016, 100) # 68 unique states

n_unq_state_nyc_2017 <- SparkR::sql("SELECT COUNT(*) AS number_of_ticket, RegistrationState FROM nyc_parking_2017_cln1_tbl
                                    GROUP BY RegistrationState ORDER BY number_of_ticket DESC")
head(n_unq_state_nyc_2017, 100) # 67 unique states


# There is an erroneous entry "99" in registration states of 2015, 2016 & 2017 dataframes, replacing it with state having maximum
# entries i.e. "NY".

nyc_parking_2015_cln1$RegistrationState <- ifelse(nyc_parking_2015_cln1$RegistrationState == "99", 
                                                  "NY", nyc_parking_2015_cln1$RegistrationState)
nyc_parking_2016_cln1$RegistrationState <- ifelse(nyc_parking_2016_cln1$RegistrationState == "99", 
                                                  "NY", nyc_parking_2016_cln1$RegistrationState)
nyc_parking_2017_cln1$RegistrationState <- ifelse(nyc_parking_2017_cln1$RegistrationState == "99", 
                                                  "NY", nyc_parking_2017_cln1$RegistrationState)

# Creating temporary views of above dataframe to execute the SQL queries for further analysis
#--------------------------------------------------------------------------------------------
createOrReplaceTempView(nyc_parking_2015_cln1, "nyc_parking_2015_cln1_tbl")
createOrReplaceTempView(nyc_parking_2016_cln1, "nyc_parking_2016_cln1_tbl")
createOrReplaceTempView(nyc_parking_2017_cln1, "nyc_parking_2017_cln1_tbl")


# 3. Finding the number of parking tickets for each year, which don't have the adress for violation loation
#-----------------------------------------------------------------------------------------------------------

vlocation_null_nyc2015 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2015_cln1_tbl WHERE ViolationLocation IS NULL")
head(vlocation_null_nyc2015) # 1627827 tickets
1627827/10853283 #15 %

vlocation_null_nyc2016 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2016_cln1_tbl WHERE ViolationLocation IS NULL")
head(vlocation_null_nyc2016) # 1864336 tickets
1864336/10533559 #17.70%

vlocation_null_nyc2017 <- SparkR::sql("SELECT COUNT(*) AS NULL_COUNT FROM nyc_parking_2017_cln1_tbl WHERE ViolationLocation IS NULL")
head(vlocation_null_nyc2017) # 2067584 tickets
20677584/10698235 #19.33%

#----------------------------------------Aggregation Task-------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------

# 1. Display the frequency of top five violation codes
#------------------------------------------------------------

vcode_freq_nyc2015 <- SparkR::sql("SELECT COUNT(*) AS Count, ViolationCode FROM nyc_parking_2015_cln1_tbl GROUP BY ViolationCode 
                                  ORDER BY Count DESC LIMIT 5")
head(vcode_freq_nyc2015)
#  Count        ViolationCode                                                         
#1 1490694            21
#2 1323329            38
#3  917658            14
#4  760582            36
#5  745545            37

vcode_freq_nyc2016 <- SparkR::sql("SELECT COUNT(*) AS Count, ViolationCode FROM nyc_parking_2016_cln1_tbl GROUP BY ViolationCode 
                                  ORDER BY Count DESC LIMIT 5")
head(vcode_freq_nyc2016)
#   Count ViolationCode                                                         
#1 1518437            21
#2 1251826            36
#3 1142736            38
#4  868949            14
#5  686045            37

vcode_freq_nyc2017 <- SparkR::sql("SELECT COUNT(*) AS Count, ViolationCode FROM nyc_parking_2017_cln1_tbl GROUP BY ViolationCode 
                                  ORDER BY Count DESC LIMIT 5")
head(vcode_freq_nyc2017) 
#    Count ViolationCode                                                         
#1 1511576            21
#2 1398699            36
#3 1059855            38
#4  885245            14
#5  614181            20


# 2. Finding the top 5 Vehicle Body Type and Vehicle Make that got parking tickets
#----------------------------------------------------------------------------------

# Vehicle Body Type
#------------------

vbodytype_freq_nyc2015 <- SparkR::sql("SELECT COUNT(*) AS Count, VehicleBodyType FROM nyc_parking_2015_cln1_tbl GROUP BY VehicleBodyType 
                                      ORDER BY Count DESC LIMIT 5")
head(vbodytype_freq_nyc2015)
#    Count VehicleBodyType                                                       
#1 3445204            SUBN
#2 3102508            4DSD
#3 1597628             VAN
#4  825212            DELV
#5  444251             SDN

vbodytype_freq_nyc2016 <- SparkR::sql("SELECT COUNT(*) AS Count, VehicleBodyType FROM nyc_parking_2016_cln1_tbl GROUP BY VehicleBodyType 
                                      ORDER BY Count DESC LIMIT 5")
head(vbodytype_freq_nyc2016)
#        Count VehicleBodyType                                                       
#1 3460015            SUBN
#2 2992104            4DSD
#3 1512149             VAN
#4  740947            DELV
#5  415295             SDN

vbodytype_freq_nyc2017 <- SparkR::sql("SELECT COUNT(*) AS Count, VehicleBodyType FROM nyc_parking_2017_cln1_tbl GROUP BY VehicleBodyType 
                                      ORDER BY Count DESC LIMIT 5")
head(vbodytype_freq_nyc2017)
#   Count     VehicleBodyType                                                       
#1 3713303            SUBN
#2 3081720            4DSD
#3 1403553             VAN
#4  671930            DELV
#5  429420             SDN

# Vehicle Make
#-------------

vmake_freq_nyc2015 <- SparkR::sql("SELECT COUNT(*) AS Count, VehicleMake FROM nyc_parking_2015_cln1_tbl GROUP BY VehicleMake 
                                  ORDER BY Count DESC LIMIT 5")
head(vmake_freq_nyc2015)
# Count     VehicleMake                                                           
#1 1413943        FORD
#2 1121173       TOYOT
#3 1015865       HONDA
#4  835718       NISSA
#5  834512       CHEVR

vmake_freq_nyc2016 <- SparkR::sql("SELECT COUNT(*) AS Count, VehicleMake FROM nyc_parking_2016_cln1_tbl GROUP BY VehicleMake 
                                  ORDER BY Count DESC LIMIT 5")
head(vmake_freq_nyc2016)
#   Count    VehicleMake                                                           
#1 1321814        FORD
#2 1152385       TOYOT
#3 1011789       HONDA
#4  832874       NISSA
#5  758078       CHEVR

vmake_freq_nyc2017 <- SparkR::sql("SELECT COUNT(*) AS Count, VehicleMake FROM nyc_parking_2017_cln1_tbl GROUP BY VehicleMake 
                                  ORDER BY Count DESC LIMIT 5")
head(vmake_freq_nyc2017)
#    Count VehicleMake                                                           
#1 1277651        FORD
#2 1208878       TOYOT
#3 1076547       HONDA
#4  916337       NISSA
#5  712969       CHEVR

# 3. Finding the top 5 Violation Precinct and Issuer Precint where Violation occured and ticket being issued respectively
#------------------------------------------------------------------------------------------------------------------------

# Violation Precinct
#-------------------

vprecinct_freq_nyc2015 <- SparkR::sql("SELECT COUNT(*) AS Count, ViolationPrecinct FROM nyc_parking_2015_cln1_tbl GROUP BY ViolationPrecinct 
                                      ORDER BY Count DESC LIMIT 5")
head(vprecinct_freq_nyc2015)
#   Count      ViolationPrecinct                                                     
#1 1627827                 0
#2  556374                19
#3  398540                18
#4  381175                14
#5  303554                 1

#Column ViolationPrecinct has 0 values, igoring such values for all of the year as these are erronoues values

vprecinct_freq_nyc2015_1 <- SparkR::sql("SELECT COUNT(*) AS Count, ViolationPrecinct FROM nyc_parking_2015_cln1_tbl 
                                        WHERE ViolationPrecinct !=0 GROUP BY ViolationPrecinct 
                                        ORDER BY Count DESC LIMIT 5")
head(vprecinct_freq_nyc2015_1)
#  Count         ViolationPrecinct                                                      
#1 556374                19
#2 398540                18
#3 381175                14
#4 303554                 1
#5 299599               114

vprecinct_freq_nyc2016 <- SparkR::sql("SELECT COUNT(*) AS Count, ViolationPrecinct FROM nyc_parking_2016_cln1_tbl 
                                      WHERE ViolationPrecinct !=0 GROUP BY ViolationPrecinct 
                                      ORDER BY Count DESC LIMIT 5")
head(vprecinct_freq_nyc2016)
#  Count      ViolationPrecinct                                                      
#1 551536                19
#2 329409                18
#3 321059                14
#4 299709                 1
#5 290442               114

vprecinct_freq_nyc2017 <- SparkR::sql("SELECT COUNT(*) AS Count, ViolationPrecinct FROM nyc_parking_2017_cln1_tbl 
                                      WHERE ViolationPrecinct !=0 GROUP BY ViolationPrecinct 
                                      ORDER BY Count DESC LIMIT 5")
head(vprecinct_freq_nyc2017)
#   Count ViolationPrecinct                                                      
#1 531578                19
#2 347240                14
#3 326576                 1
#4 302558                18
#5 295294               114

# Issuer Precinct
#-------------------

iprecinct_freq_nyc2015 <- SparkR::sql("SELECT COUNT(*) AS Count, IssuerPrecinct FROM nyc_parking_2015_cln1_tbl GROUP BY IssuerPrecinct 
                                      ORDER BY Count DESC LIMIT 5")
head(iprecinct_freq_nyc2015)
#      Count IssuerPrecinct                                                        
#1 1814415              0
#2  542703             19
#3  389948             18
#4  367593             14
#5  295934              1

#Column IssuerPrecinct has 0 values, igoring such values for all of the year as these are erronoues values

iprecinct_freq_nyc2015_1 <- SparkR::sql("SELECT COUNT(*) AS Count, IssuerPrecinct FROM nyc_parking_2015_cln1_tbl 
                                        WHERE IssuerPrecinct != 0 GROUP BY IssuerPrecinct 
                                        ORDER BY Count DESC LIMIT 5")
head(iprecinct_freq_nyc2015_1)
#  Count      IssuerPrecinct                                                         
#1 542703             19
#2 389948             18
#3 367593             14
#4 295934              1
#5 294878            114

iprecinct_freq_nyc2016 <- SparkR::sql("SELECT COUNT(*) AS Count, IssuerPrecinct FROM nyc_parking_2016_cln1_tbl 
                                      WHERE IssuerPrecinct != 0 GROUP BY IssuerPrecinct 
                                      ORDER BY Count DESC LIMIT 5")
head(iprecinct_freq_nyc2016)
#  Count      IssuerPrecinct                                                         
#1 538293             19
#2 321685             18
#3 312887             14
#4 292569              1
#5 286237            114

iprecinct_freq_nyc2017 <- SparkR::sql("SELECT COUNT(*) AS Count, IssuerPrecinct FROM nyc_parking_2017_cln1_tbl 
                                      WHERE IssuerPrecinct != 0 GROUP BY IssuerPrecinct 
                                      ORDER BY Count DESC LIMIT 5")
head(iprecinct_freq_nyc2017)
#   Count      IssuerPrecinct                                                         
#1  517984             19
#2  340578             14
#3  317357              1
#4  293047             18
#5  288964            114

# 4. Finding the Violation Code frequency across three Precint Zones which have issued the most number of tickets 
#----------------------------------------------------------------------------------------------------------------

#2015
vcode_freq_top3_iprecint_nyc_2015 <- SparkR::sql("SELECT sub.IssuerPrecinct as Precint, m.ViolationCode as Violation_Code, COUNT(*) as Count
                                                 from nyc_parking_2015_cln1_tbl m JOIN                                         
                                                 (SELECT COUNT(*) AS Count, IssuerPrecinct FROM nyc_parking_2015_cln1_tbl 
                                                 WHERE IssuerPrecinct != 0 GROUP BY IssuerPrecinct 
                                                 ORDER BY Count DESC LIMIT 3)sub on sub.IssuerPrecinct = m.IssuerPrecinct
                                                 GROUP BY sub.IssuerPrecinct, m.ViolationCode
                                                 ORDER BY Precint, Count DESC")
head(vcode_freq_top3_iprecint_nyc_2015, 250)

#2016
vcode_freq_top3_iprecint_nyc_2016 <- SparkR::sql("SELECT sub.IssuerPrecinct as Precint, m.ViolationCode as Violation_Code, COUNT(*) as Count
                                                 from nyc_parking_2016_cln1_tbl m JOIN                                         
                                                 (SELECT COUNT(*) AS Count, IssuerPrecinct FROM nyc_parking_2016_cln1_tbl 
                                                 WHERE IssuerPrecinct != 0 GROUP BY IssuerPrecinct 
                                                 ORDER BY Count DESC LIMIT 3)sub on sub.IssuerPrecinct = m.IssuerPrecinct
                                                 GROUP BY sub.IssuerPrecinct, m.ViolationCode
                                                 ORDER BY Precint, Count DESC")
head(vcode_freq_top3_iprecint_nyc_2016, 250)

#2017
vcode_freq_top3_iprecint_nyc_2017 <- SparkR::sql("SELECT sub.IssuerPrecinct as Precint, m.ViolationCode as Violation_Code, COUNT(*) as Count
                                                 from nyc_parking_2017_cln1_tbl m JOIN                                         
                                                 (SELECT COUNT(*) AS Count, IssuerPrecinct FROM nyc_parking_2017_cln1_tbl 
                                                 WHERE IssuerPrecinct != 0 GROUP BY IssuerPrecinct 
                                                 ORDER BY Count DESC LIMIT 3)sub on sub.IssuerPrecinct = m.IssuerPrecinct
                                                 GROUP BY sub.IssuerPrecinct, m.ViolationCode
                                                 ORDER BY Precint, Count DESC")
head(vcode_freq_top3_iprecint_nyc_2017, 250)

# 5. Analysis on Parking Violations across different times of the day
#--------------------------------------------------------------------

# a. Checking the Null values in Violation Time column
null_vltime_2015 <- SparkR::sql("SELECT count(*) as No_of_Count_Values from nyc_parking_2015_cln1_tbl
                                WHERE ViolationTime is NULL")
head(null_vltime_2015) #1514 Values


null_vltime_2016 <- SparkR::sql("SELECT count(*) as No_of_Count_Values from nyc_parking_2016_cln1_tbl
                                WHERE ViolationTime is NULL")
head(null_vltime_2016) #4179 Values


null_vltime_2017 <- SparkR::sql("SELECT count(*) as No_of_Count_Values from nyc_parking_2017_cln1_tbl
                                WHERE ViolationTime is NULL")
head(null_vltime_2017) #60 Values

# b. Dropping all these rows from dataframes and creating new temprary views 
nyc_parking_2015_cln2<- dropna(nyc_parking_2015_cln1, cols = "ViolationTime")
nrow(nyc_parking_2015_cln2) #10851769 rows

nyc_parking_2016_cln2 <- dropna(nyc_parking_2016_cln1, cols = "ViolationTime")
nrow(nyc_parking_2016_cln2) #10529380 rows

nyc_parking_2017_cln2 <- dropna(nyc_parking_2017_cln1, cols = "ViolationTime")
nrow(nyc_parking_2017_cln2) #10698175 rows

# c. Violation Time field is specfied in a strange format making it into a time attribute

## 2015 
nyc_parking_2015_cln2$col_pfx <- "M"

nyc_parking_2015_cln2$ViolationTime1 <- concat(nyc_parking_2015_cln2$ViolationTime, nyc_parking_2015_cln2$col_pfx) #Adding "M" prefix to make AM & PM
nyc_parking_2015_cln2$H <- substr(nyc_parking_2015_cln2$ViolationTime1, 1, 2) #Extracting Hour
nyc_parking_2015_cln2$M <- substr(nyc_parking_2015_cln2$ViolationTime1, 4, 5) #Extracting Min 
nyc_parking_2015_cln2$AM_PM <- substr(nyc_parking_2015_cln2$ViolationTime1, 6, 7) #Extracting AM and PM

nyc_parking_2015_cln2$H <- regexp_replace(x = nyc_parking_2015_cln2$H, pattern = "\\00", replacement = "12") #Replacing "00" with "12"
nyc_parking_2015_cln2$ViolationTime1 <- concat(nyc_parking_2015_cln2$H, nyc_parking_2015_cln2$M, nyc_parking_2015_cln2$AM_PM) #Concat Hour, Min and AM_PM column
nyc_parking_2015_cln2$ViolationTime1 <- to_timestamp(x = nyc_parking_2015_cln2$ViolationTime1, format = "hhmma") #Converting to Time Stamp

head(nrow(filter(nyc_parking_2015_cln2, isNull(nyc_parking_2015_cln2$ViolationTime1)))) #60763

#60763 NA values comes up in Violation Time because of strange format of time, removing such rows
nyc_parking_2015_cln3 <- filter(nyc_parking_2015_cln2, isNotNull(nyc_parking_2015_cln2$ViolationTime1))


## 2016
nyc_parking_2016_cln2$col_pfx <- "M"

nyc_parking_2016_cln2$ViolationTime1 <- concat(nyc_parking_2016_cln2$ViolationTime, nyc_parking_2016_cln2$col_pfx) #Adding "M" prefix to make AM & PM
nyc_parking_2016_cln2$H <- substr(nyc_parking_2016_cln2$ViolationTime1, 1, 2) #Extracting Hour
nyc_parking_2016_cln2$M <- substr(nyc_parking_2016_cln2$ViolationTime1, 4, 5) #Extracting MIN 
nyc_parking_2016_cln2$AM_PM <- substr(nyc_parking_2016_cln2$ViolationTime1, 6, 7) #Extracting AM and PM

nyc_parking_2016_cln2$H <- regexp_replace(x = nyc_parking_2016_cln2$H, pattern = "\\00", replacement = "12") #Replacing "00" with "12"
nyc_parking_2016_cln2$ViolationTime1 <- concat(nyc_parking_2016_cln2$H, nyc_parking_2016_cln2$M, nyc_parking_2016_cln2$AM_PM) #Concat Hour, Min and AM_PM column
nyc_parking_2016_cln2$ViolationTime1 <- to_timestamp(x = nyc_parking_2016_cln2$ViolationTime1, format = "hhmma") #Converting to Time Stamp

head(nrow(filter(nyc_parking_2016_cln2, isNull(nyc_parking_2016_cln2$ViolationTime1))))

#63370 NA values comes up in Violation Time because of strange format of time, removing such rows
nyc_parking_2016_cln3 <- filter(nyc_parking_2016_cln2, isNotNull(nyc_parking_2016_cln2$ViolationTime1))

## 2017
nyc_parking_2017_cln2$col_pfx <- "M"

nyc_parking_2017_cln2$ViolationTime1 <- concat(nyc_parking_2017_cln2$ViolationTime, nyc_parking_2017_cln2$col_pfx) #Adding "M" prefix to make AM & PM
nyc_parking_2017_cln2$H <- substr(nyc_parking_2017_cln2$ViolationTime1, 1, 2) #Extracting Hour
nyc_parking_2017_cln2$M <- substr(nyc_parking_2017_cln2$ViolationTime1, 4, 5) #Extracting Min 
nyc_parking_2017_cln2$AM_PM <- substr(nyc_parking_2017_cln2$ViolationTime1, 6, 7) #Extracting AM and PM

nyc_parking_2017_cln2$H <- regexp_replace(x = nyc_parking_2017_cln2$H, pattern = "\\00", replacement = "12") #Replacing "00" with "12"
nyc_parking_2017_cln2$ViolationTime1 <- concat(nyc_parking_2017_cln2$H, nyc_parking_2017_cln2$M, nyc_parking_2017_cln2$AM_PM) #Concat Hour, Min and AM_PM column
nyc_parking_2017_cln2$ViolationTime1 <- to_timestamp(x = nyc_parking_2017_cln2$ViolationTime1, format = "hhmma") #Converting to Time Stamp

head(nrow(filter(nyc_parking_2017_cln2, isNull(nyc_parking_2017_cln2$ViolationTime1))))

#58120 NA values comes up in Violation Time because of strange format of time, removing such rows
nyc_parking_2017_cln3 <- filter(nyc_parking_2017_cln2, isNotNull(nyc_parking_2017_cln2$ViolationTime1))

# d. Dividing the Violation Time into time slots :
nyc_parking_2015_cln3$timeslots <- ifelse(hour(nyc_parking_2015_cln3$ViolationTime1) < "04", "0 to 4", 
                                          ifelse(hour(nyc_parking_2015_cln3$ViolationTime1) < "08", "4 to 8",
                                                 ifelse(hour(nyc_parking_2015_cln3$ViolationTime1) < "12", "8 to 12",
                                                        ifelse(hour(nyc_parking_2015_cln3$ViolationTime1) < "16", "12 to 16",
                                                               ifelse(hour(nyc_parking_2015_cln3$ViolationTime1) < "20", "16 to 20", "20 to 24")))))

nyc_parking_2016_cln3$timeslots <- ifelse(hour(nyc_parking_2016_cln3$ViolationTime1) < "04", "0 to 4", 
                                          ifelse(hour(nyc_parking_2016_cln3$ViolationTime1) < "08", "4 to 8",
                                                 ifelse(hour(nyc_parking_2016_cln3$ViolationTime1) < "12", "8 to 12",
                                                        ifelse(hour(nyc_parking_2016_cln3$ViolationTime1) < "16", "12 to 16",
                                                               ifelse(hour(nyc_parking_2016_cln3$ViolationTime1) < "20", "16 to 20", "20 to 24")))))


nyc_parking_2017_cln3$timeslots <- ifelse(hour(nyc_parking_2017_cln3$ViolationTime1) < "04", "0 to 4", 
                                          ifelse(hour(nyc_parking_2017_cln3$ViolationTime1) < "08", "4 to 8",
                                                 ifelse(hour(nyc_parking_2017_cln3$ViolationTime1) < "12", "8 to 12",
                                                        ifelse(hour(nyc_parking_2017_cln3$ViolationTime1) < "16", "12 to 16",
                                                               ifelse(hour(nyc_parking_2017_cln3$ViolationTime1) < "20", "16 to 20", "20 to 24")))))

# e. Finding the top 3 commonly occuring violation codes for each 6 time slots
ws <- orderBy(windowPartitionBy("timeslots"), "count1") #Setting Window

#2015
#----
top3_vc_dfrnt_timeslot_2015 <- nyc_parking_2015_cln3 %>%
  group_by(nyc_parking_2015_cln3$timeslots, nyc_parking_2015_cln3$ViolationCode) %>%
  count() %>%
  mutate(count1 = -column("count")) %>%                        
  mutate(n = over(dense_rank(), ws)) %>%
  filter(column("n") <= 3) %>%
  collect()

head(top3_vc_dfrnt_timeslot_2015, 18)


#2016
#----
top3_vc_dfrnt_timeslot_2016 <- nyc_parking_2016_cln3 %>%
  group_by(nyc_parking_2016_cln3$timeslots, nyc_parking_2016_cln3$ViolationCode) %>%
  count() %>%
  mutate(count1 = -column("count")) %>%                        
  mutate(n = over(dense_rank(), ws)) %>%
  filter(column("n") <= 3) %>%
  collect()

head(top3_vc_dfrnt_timeslot_2016, 18)

#2017
#----
top3_vc_dfrnt_timeslot_2017 <- nyc_parking_2017_cln3 %>%
  group_by(nyc_parking_2017_cln3$timeslots, nyc_parking_2017_cln3$ViolationCode) %>%
  count() %>%
  mutate(count1 = -column("count")) %>%                        
  mutate(n = over(dense_rank(), ws)) %>%
  filter(column("n") <= 3) %>%
  collect()

head(top3_vc_dfrnt_timeslot_2017, 18)

# f. For the 3 most commonly occuring violation codes, finding the most common time of the day.

### Creating temporary view :
createOrReplaceTempView(nyc_parking_2015_cln3, "nyc_parking_2015_cln3_tbl1")
createOrReplaceTempView(nyc_parking_2016_cln3, "nyc_parking_2016_cln3_tbl1")
createOrReplaceTempView(nyc_parking_2017_cln3, "nyc_parking_2017_cln3_tbl1")

## 2015
#------
mst_3_vc_mst_cmn_time_2015 <- SparkR::sql(" SELECT sub.ViolationCode as ViolationCode, timeslots, count(*) as count from nyc_parking_2015_cln3_tbl1 main
                                          join (SELECT ViolationCode, count(*) as count from nyc_parking_2015_cln3_tbl1
                                          group by ViolationCode order by count desc limit 3)sub on main.ViolationCode = sub.ViolationCode
                                          group by sub.ViolationCode, main.timeslots 
                                          order by ViolationCode, count desc")

head(mst_3_vc_mst_cmn_time_2015, 24)

#Violation Code	Time Slots	Number of Tickets
#1	14	           8 to 12	296049
#2	21	           8 to 12	1185207
#3	38	           12 to 16	567768


## 2016
#------
mst_3_vc_mst_cmn_time_2016 <- SparkR::sql(" SELECT sub.ViolationCode as ViolationCode, timeslots, count(*) as count from nyc_parking_2016_cln3_tbl1 main
                                          join (SELECT ViolationCode, count(*) as count from nyc_parking_2016_cln3_tbl1
                                          group by ViolationCode order by count desc limit 3)sub on main.ViolationCode = sub.ViolationCode
                                          group by sub.ViolationCode, main.timeslots 
                                          order by ViolationCode, count desc")

head(mst_3_vc_mst_cmn_time_2016, 24)

#Violation Code	Time Slots	Number of Tickets
#1  21	           8 to 12	1200730
#2	36	           8 to 12	585988
#3	38	           12 to 16	487902

## 2017
#------
mst_3_vc_mst_cmn_time_2017 <- SparkR::sql(" SELECT sub.ViolationCode as ViolationCode, timeslots, count(*) as count from nyc_parking_2017_cln3_tbl1 main
                                          join (SELECT ViolationCode, count(*) as count from nyc_parking_2017_cln3_tbl1
                                          group by ViolationCode order by count desc limit 3)sub on main.ViolationCode = sub.ViolationCode
                                          group by sub.ViolationCode, main.timeslots 
                                          order by ViolationCode, count desc")


head(mst_3_vc_mst_cmn_time_2017, 24)

#	Violation Code	Time Slots	Number of Tickets
#1  	 21	        8 to 12	    1171632
#2   	 36	        8 to 12	    750339
#3	   38	        12 to 16 	  461762

# 6. Analysis on Parking Violations across different Seasons
#-----------------------------------------------------------
# Using IssueDate column for analysis

# a. Checking the Null values in Issue Date column

null_idate_2015 <- SparkR::sql("SELECT count(*) as No_of_Count_Values from nyc_parking_2015_cln3_tbl1
                               WHERE IssueDate is NULL")
head(null_idate_2015) #No Null Values


null_idate_2016 <- SparkR::sql("SELECT count(*) as No_of_Count_Values from nyc_parking_2016_cln3_tbl1
                               WHERE IssueDate is NULL")
head(null_idate_2016) #No Null Values


null_idate_2017 <- SparkR::sql("SELECT count(*) as No_of_Count_Values from nyc_parking_2017_cln3_tbl1
                               WHERE IssueDate is NULL")
head(null_idate_2017) #No Null Values

# b. Dividing the Issue Date into number of Seasons as per below :
# Winter Season - Dec, Jan and Feb
# Spring Season - March, April and May
# Summer Season - June, July and August
# Fall Season - September, October and November

## Creatin Issue Month Column from Issue Data
nyc_parking_2015_cln3$Issue_Month <- substr(nyc_parking_2015_cln3$IssueDate,1,2)
nyc_parking_2016_cln3$Issue_Month <- substr(nyc_parking_2016_cln3$IssueDate,1,2)
nyc_parking_2017_cln3$Issue_Month <- substr(nyc_parking_2017_cln3$IssueDate,1,2)

# 2015
#-----
nyc_parking_2015_cln3$season <- ifelse(nyc_parking_2015_cln3$Issue_Month %in% c("12", "01", "02"), "Winter Season", 
                                       ifelse(nyc_parking_2015_cln3$Issue_Month %in% c("03", "04", "05"), "Spring Season",
                                              ifelse(nyc_parking_2015_cln3$Issue_Month %in% c("06", "07", "08"), "Summer Season","Fall Season")))


# 2016
#-----
nyc_parking_2016_cln3$season <- ifelse(nyc_parking_2016_cln3$Issue_Month %in% c("12", "01", "02"), "Winter Season", 
                                       ifelse(nyc_parking_2016_cln3$Issue_Month %in% c("03", "04", "05"), "Spring Season",
                                              ifelse(nyc_parking_2016_cln3$Issue_Month %in% c("06", "07", "08"), "Summer Season","Fall Season")))

# 2017
#-----
nyc_parking_2017_cln3$season <- ifelse(nyc_parking_2017_cln3$Issue_Month %in% c("12", "01", "02"), "Winter Season", 
                                       ifelse(nyc_parking_2017_cln3$Issue_Month %in% c("03", "04", "05"), "Spring Season",
                                              ifelse(nyc_parking_2017_cln3$Issue_Month %in% c("06", "07", "08"), "Summer Season","Fall Season")))


### Creating temporary view :
createOrReplaceTempView(nyc_parking_2015_cln3, "nyc_parking_2015_cln3_tbl2")
createOrReplaceTempView(nyc_parking_2016_cln3, "nyc_parking_2016_cln3_tbl2")
createOrReplaceTempView(nyc_parking_2017_cln3, "nyc_parking_2017_cln3_tbl2")

# c. Finding the frequency of tickets across each season

#2015
#----
freq_tckt_acrs_each_sesn_2015 <- SparkR::sql("SELECT season, count(*) as frequency from nyc_parking_2015_cln3_tbl2
                                             group by season order by season desc")
head(freq_tckt_acrs_each_sesn_2015)

#  Season   	 Number of Tickets
#Winter Season	2151302
#Summer Season	3051379
#Spring Season	2910064
#Fall Season  	2678261

#2016
#----
freq_tckt_acrs_each_sesn_2016 <- SparkR::sql("SELECT season, count(*) as frequency from nyc_parking_2016_cln3_tbl2
                                             group by season order by frequency desc")
head(freq_tckt_acrs_each_sesn_2016)

#Season	      Number of Tickets
#Winter Season	2386229
#Summer Season	2396359
#Spring Season	2750952
#Fall Season	  2932470

#2017
#----
freq_tckt_acrs_each_sesn_2017 <- SparkR::sql("SELECT season, count(*) as frequency from nyc_parking_2017_cln3_tbl2
                                             group by season order by frequency desc")
head(freq_tckt_acrs_each_sesn_2017)

#Season	       Number of Tickets
#Winter Season	2446502
#Summer Season	2565140
#Spring Season	2835476
#Fall Season	  2792937

# d. Finding the three most common violations for each of these seasons
ws1 <- orderBy(windowPartitionBy("season"), "count1") #Setting Window

#2015
top3_vc_dfrnt_season_2015 <- nyc_parking_2015_cln3 %>%
  group_by(nyc_parking_2015_cln3$season, nyc_parking_2015_cln3$ViolationCode) %>%
  count() %>%
  mutate(count1 = -column("count")) %>%                        
  mutate(n = over(dense_rank(), ws1)) %>%
  filter(column("n") <= 3) %>%
  collect()

head(top3_vc_dfrnt_season_2015, 12)

#2016
top3_vc_dfrnt_season_2016 <- nyc_parking_2016_cln3 %>%
  group_by(nyc_parking_2016_cln3$season, nyc_parking_2016_cln3$ViolationCode) %>%
  count() %>%
  mutate(count1 = -column("count")) %>%                        
  mutate(n = over(dense_rank(), ws1)) %>%
  filter(column("n") <= 3) %>%
  collect()

head(top3_vc_dfrnt_season_2016, 12)

#2017
top3_vc_dfrnt_season_2017 <- nyc_parking_2017_cln3 %>%
  group_by(nyc_parking_2017_cln3$season, nyc_parking_2017_cln3$ViolationCode) %>%
  count() %>%
  mutate(count1 = -column("count")) %>%                        
  mutate(n = over(dense_rank(), ws1)) %>%
  filter(column("n") <= 3) %>%
  collect()

head(top3_vc_dfrnt_season_2017, 12)


# 7. Estimating the revenue for top 3 violation codes with maximum tickets
#-------------------------------------------------------------------------

#2015
top3_violation_code_2015 <- SparkR::sql("SELECT ViolationCode, count(*) as frequency from nyc_parking_2015_cln3_tbl2
                                        group by ViolationCode order by frequency desc limit 3")
head(top3_violation_code_2015)

#ViolationCode frequency                                                       
#1            21   1464359
#2            38   1323314
#3            14    909938

# As checked on NYC website, Violation code 21, 38 & 14 have average fines of $55, $51.5 & $115 respectively.

## Calculatiing the total amount collected for these three violation codes

#Violation Code - 21
1464359 * 55 # $80539745

#Violation Code - 38
1323314 * 51.5 # $68150671

#Violation Code - 14
909938 * 115 # $104642870


#2016
top3_violation_code_2016 <- SparkR::sql("SELECT ViolationCode, count(*) as frequency from nyc_parking_2016_cln3_tbl2
                                        group by ViolationCode order by frequency desc limit 3")
head(top3_violation_code_2016)

#     ViolationCode frequency                                                       
#1            21   1490489
#2            36   1251826
#3            38   1142471

# As checked on NYC website, Violation code 21, 36 & 38 have average fines of $55, $50 & $51.5 respectively.

## Calculatiing the total amount collected for these three violation codes

#Violation Code - 21
1490489 * 55 # $81976895

#Violation Code - 36
1251826 * 50 # $62591300

#Violation Code - 14
1142471 * 51.5 # $58837256

#2017
top3_violation_code_2017 <- SparkR::sql("SELECT ViolationCode, count(*) as frequency from nyc_parking_2017_cln3_tbl2
                                        group by ViolationCode order by frequency desc limit 3")
head(top3_violation_code_2017)

#     ViolationCode frequency                                                       
#1            21   1487708
#2            36   1398699
#3            38   1059839

# As checked on NYC website, Violation code 21, 36 & 38 have average fines of $55, $50 & $51.5 respectively.

## Calculatiing the total amount collected for these three violation codes

#Violation Code - 21
1487708 * 55 # $81823940

#Violation Code - 36
1398699 * 50 # $69934950

#Violation Code - 14
1059839 * 51.5 # $54581708
