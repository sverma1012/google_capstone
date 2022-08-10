# Author: Sneha Verma
# Date of Creation: 4 August 2022
# Purpose of Script: This script will analyze the data. It details the import, manipulation, and wrangling of the data.

## Some exploratory analysis has been completed in processing.R.

### Import data sets

activity = read.csv("../Data/Importable Data/dailyActivity_merged.csv")
sleepActivity = read.csv("../Data/Importable Data/sleepActivity.csv")
weightActivity = read.csv("../Data/Importable Data/weightActivity.csv")

### Exploratory Data Analysis

## explore activity
colnames(activity) # 15 columns
n_distinct(activity$Id) # 33 individual participants
nrow(activity) # 940 observations in total

## explore sleepActivity
colnames(sleepActivity) # 3 columns
n_distinct(sleepActivity$Id) # 24 individual participants
nrow(sleepActivity) # 24 observations

## explore weightActivity
colnames(weightActivity) # 4 columns
n_distinct(weightActivity$Id) # 8 individual participants
nrow(weightActivity) # 8 observations

# The activity data frame has the most participants compared to the other frames.
# Results from this analysis may not be reliable due to the small sample of each data frame.

## summary statistics for activity
activity %>%
  select(TotalSteps, TotalDistance, VeryActiveMinutes, SedentaryMinutes, Calories) %>%
  summary()

# The results show that the average steps taken by all participants is 7638.
# The median and mean have a difference of approx. 200 steps.
# The average distance walked/exercised is ~ 5.2 miles.
# People were mainly sedentary compared to being activity (991 vs 21 mins).
# The mean and median of active and sedentary mins have a large difference
# implying that there are huge outliers in the values of these columns (as evidenced by the range).
# The average calories lost of all participants is 2304 with a wide range from 0 to 4900.

## summary statistics for weightActivity
weightActivity %>%
  select(WeightKg_name, BMI_name, calories) %>%
  summary()

# The average weight of all participants in the data frame is ~ 78 kgs with a BMI of 28.
# The average calories of this set of participants is slightly higher than the one from the activity data frame
# at 2342 calories.

## summary statistics of sleepActivity
sleepActivity %>%
  select(minutesAsleep, calories) %>%
  summary()

# The average minutes asleep is 378 minutes.
# The average calories of this set of participants is 2290, approx. the same as the activity data frrame.
# The minutes asleep describes how much, on average, each participant slept while their data was being recorded.

### Data Analysis

## Explore relationships

# 
