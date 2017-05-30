
#  ------------------------------------------------------------------
#  |FILE NAME:      Working with SQLite databases in R
#  |DATE:           05/14/17
#  |CREATED BY:     Ezugo Nwosu 
#  |DATA Source:    https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DESample01.html
#  |DATA FILE Name: DE1.0 Sample 1 2008-2010 Prescription Drug Events
#  |----------------------------------------------------------------


#  |------------------------------------------------------------------
#  |STEPS:               
#  |
#  |  STEP 1: Install and load the required packages and libraries 
#  |  STEP 2: Create the database
#  |  STEP 3a: Get data from Excel as a data frame
#  |  STEP 3b: View and examine the imported Excel data  
#  |  STEP 4a: Import the data frame into database
#  |  STEP 4b: View and examine the database data
#  |  STEP 5: Run queries on the database content
#  |  STEP 6: Perform some statistics on the data 
#  |------------------------------------------------------------------

#  *------------------------------------------------------------------*
#  | STEP 1: Install and load the required packages and libraries 
#  *------------------------------------------------------------------*
# We will use functions from RSQLite for our analysis. Alternatively, 
# we could have used functions from sqldf.
#install.packages("sqldf") # sqldf makes use of RSQLite and the DBI (DataBase Interface) package
#install.packages("XLConnectJars", type="win.binary") # Required for XLConnect
#install.packages("XLConnect", type="win.binary") # Allow direct access to Excel files

#library(sqldf)
#library(XLConnectJars)
#library(XLConnect)

#  *------------------------------------------------------------------*
#  | STEP 2: Create the database
#  *------------------------------------------------------------------*
# dbConnect will open the db. If the db does not exist, it is first created. 
# Technically speaking, the db will only exist once it has data
db <- dbConnect(SQLite(), dbname="MySQLIteDB.sqlite")

#  *------------------------------------------------------------------*
#  | STEP 3a: Get data from Excel as a data frame
#  *------------------------------------------------------------------*
# First read the csv files into R as a data frame. Change this path to the file path on your computer.
prescription_drug_event_data = read.csv("C:/Users/Documents/RStudio_WorkFile/SQLite_DB_in_R/DE1_0_2008_to_2010_Prescription_Drug_Events_Sample_1.csv", header=TRUE)

#  *------------------------------------------------------------------*
#  | STEP 3b: View and examine the imported Excel data 
#  *------------------------------------------------------------------*
# Just review the first 6 data rows
head(prescription_drug_event_data)

# Just review the last 6 data rows
tail(prescription_drug_event_data)

# Check to see if it has NA for missing values.
#is.na(prescription_drug_event_data)
which (is.na(prescription_drug_event_data))

# Data Summary
summary(prescription_drug_event_data)

# Data Structure
str(prescription_drug_event_data)

# Data Dimensions
dim(prescription_drug_event_data)

# Data attributes
attributes(prescription_drug_event_data)

# Data class
class(prescription_drug_event_data)

#  *------------------------------------------------------------------*
#  | STEP 4a: Import the data frame into database 
#  *------------------------------------------------------------------*

# Remove tables if they already exist
dbRemoveTable(db, "prescription_drug_event_data")

# Next, import the data frames into the database
dbWriteTable(conn = db, name = "prescription_drug_event_data", value = prescription_drug_event_data, row.names = FALSE)

#  *------------------------------------------------------------------*
#  | STEP 4b: View and examine the database data
#  *------------------------------------------------------------------*
# List of tables in the database
dbListTables(db)

# List of fields (i.e. attributes) in a particular database
dbListFields(db, "prescription_drug_event_data" )

# Content of a specific table in the database
dbReadTable(db, "prescription_drug_event_data" )

#  *------------------------------------------------------------------*
#  | STEP 5: Run queries on the database content
#  *------------------------------------------------------------------*
# Let's see how to use RSQLite for queries. ??rsqlite will show the 
# help documentation

# Similar to the default head command in R, let's list the first 6 rows
dbGetQuery(db, 'SELECT * FROM prescription_drug_event_data LIMIT 6')
# Compare to the head command
head(prescription_drug_event_data)

# Search for all records where QTY_DSPNSD_NUM == 30 and print all the respective columns
dbGetQuery(db, 'SELECT * FROM prescription_drug_event_data WHERE QTY_DSPNSD_NUM == 30')

# Search for the first 6 records where SRVC_DT == 20080103 and print only the following columns DESYNPUF_ID, PROD_SRVC_ID and SRVC_DT
dbGetQuery(db, 'SELECT DESYNPUF_ID, PROD_SRVC_ID, SRVC_DT FROM prescription_drug_event_data WHERE SRVC_DT == 20080103 LIMIT 6')

# Search the first 15 records where DAYS_SUPLY_NUM is between 10 and 20 and print all columns
dbGetQuery(db, 'SELECT * FROM prescription_drug_event_data WHERE DAYS_SUPLY_NUM BETWEEN 10 AND 20 LIMIT 15')

# Search for the first 20 records where the value of DESYNPUF_ID starts with "0001" and ORDER BY DAYS_SUPLY_NUM. 
dbGetQuery(db, 'SELECT * FROM prescription_drug_event_data WHERE DESYNPUF_ID LIKE "0001%" ORDER BY DAYS_SUPLY_NUM LIMIT 20')


#  *------------------------------------------------------------------*
#  | STEP 6: Perform some statistics on the data 
#  *------------------------------------------------------------------*

# Extract the QTY_DSPNSD_NUM, PTNT_PAY_AMT, TOT_RX_CST_AMT data columns into a variable
QTY_DSPNSD_NUM_db_data = dbGetQuery(db, 'SELECT QTY_DSPNSD_NUM FROM prescription_drug_event_data')

PTNT_PAY_AMT_db_data = dbGetQuery(db, 'SELECT PTNT_PAY_AMT FROM prescription_drug_event_data')

TOT_RX_CST_AMT_db_data = dbGetQuery(db, 'SELECT TOT_RX_CST_AMT FROM prescription_drug_event_data')

# Print out summary statistics
summary(QTY_DSPNSD_NUM_db_data)
summary(PTNT_PAY_AMT_db_data)
summary(TOT_RX_CST_AMT_db_data)

# Averages
lapply(QTY_DSPNSD_NUM_db_data, mean, na.rm=T)
lapply(PTNT_PAY_AMT_db_data, mean, na.rm=T)
lapply(TOT_RX_CST_AMT_db_data, mean, na.rm=T)

# Standard Deviation
lapply(QTY_DSPNSD_NUM_db_data, sd, na.rm=T)
lapply(PTNT_PAY_AMT_db_data, sd, na.rm=T)
lapply(TOT_RX_CST_AMT_db_data, sd, na.rm=T)

# Box plots 
par(mfrow=c(1,3)) 
boxplot(QTY_DSPNSD_NUM_db_data, names = c('QTY_DSPNSD_NUM_db_data'), main="Plot of QTY_DSPNSD_NUM")
boxplot(PTNT_PAY_AMT_db_data, names = c('PTNT_PAY_AMT_db_data'), main="Plot of PTNT_PAY_AMT")
boxplot(TOT_RX_CST_AMT_db_data, names = c('TOT_RX_CST_AMT_db_data'), main="Plot of TOT_RX_CST_AMT")
# Reset the mfrow parameter
par(mfrow=c(1,1))