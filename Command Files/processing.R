# Author: Sneha Verma
# Date of Creation: 31 July 2022
# Purpose of Script: This script will process the data. It details the import, manipulation, and wrangling of the data.

### Import
# There are three datasets that will be used for this project:

weight = read.csv("../Data/Importable Data/weightLogInfo_merged_edit.csv")
sleep = read.csv("../Data/Importable Data/sleepDay_merged_edit.csv")
activity = read.csv("../Data/Importable Data/dailyActivity_merged.csv")

### Exploratory Analysis

## Column Names
colnames(weight)
colnames(sleep)
colnames(activity)

# all the dataframes have 'Id' as a common field which can be used to merge the data.

## Data Types
str(weight)
# Most variables are numeric (decimal or integers).
# There are logical and level variables (Date).

str(sleep)
# Most variables are integers. There are some level variables (e.g. Date).

str(activity)
# Most variables are numeric (decimal or integers).

## Column Size
length(sleep$Id) # 413 observations
length(activity$Id) # 940 observations
length(weight$Id) # 67 observations
# Each dataframe has a different length indicating that the number of people in each dataset is different.

## Unique Values
unique(sleep$Id) # 24 values
table(sleep$Id) # there are duplicated ids
# Some are duplicated much more than others.

unique(activity$Id) # 33 values
table(activity$Id) # All values are duplicated.
# Most Ids are duplicated 31 times.

unique(weight$Id) # 8 values
table(weight$Id) # Most values are duplicated.
# Two Ids are repeated the most times. 
# This can cause the data to be unreliable as it is dependent on the values of just two participants.


