# Author: Sneha Verma
# Date of Creation: 4 August 2022
# Purpose of Script: This script will analyze the data.
# It details the import, manipulation, and wrangling of the data.

## Some exploratory analysis has been completed in processing.R.

install.packages("dplyr")
library(dplyr)

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

# The activity data frame has the most participants
# compared to the other frames.
# Results from this analysis may not be reliable
# due to the small sample of each data frame.

## summary statistics for activity
activity %>%
  select(TotalSteps, TotalDistance, VeryActiveMinutes, 
         SedentaryMinutes, Calories) %>%
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

# Weight and BMI

install.packages("tidyverse")

library(ggplot2)

ggplot(data = weight, aes(x=WeightKg, y=BMI)) + 
  geom_point(alpha = 1/5, size = 2) + 
  geom_smooth(method = lm)

# There is an outlier in the data with a weight of approx. 140 kgs,
# which could cause errors in further analysis.
# According to the trend line, there is a positive correlation
# between BMI and weight.

ggplot(data = weight, aes(x = WeightKg, y = BMI)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)

# The boxplot confirms that there are outliers in this data frame.

# Weight and Calories

ggplot(data = weightActivity, aes(x=WeightKg_name, y=calories)) +
  geom_point() +
  geom_smooth(method = lm)

cor(weightActivity$WeightKg_name, weightActivity$calories) # 0.37

# This visualization shows that there is a weak positive correlation.
# As evidenced by the correlation coefficient.
# Hence, there is not strong evidence to prove that weight is correlated
# to the calories lost.
# From the visualization, it appears that people between 85 and 95 kgs
# lost the most calories.

# Sleep and Calories

sleepActivity %>%
  ggplot(aes(x = minutesAsleep, y = calories)) +
  geom_point() +
  geom_smooth(method = lm)

cor(sleepActivity$minutesAsleep, sleepActivity$calories) # -0.04

# Close to no correlation between calories lost and minutes asleep.
# However, the visualization shows that most of the participants
# sleep between 300 and 500 minutes per day (around 8 hours),
# and these participants lose a range of calories.

# Total steps and Calories

activity %>%
  ggplot(aes(x = TotalSteps, y = Calories)) +
  geom_point() +
  geom_smooth(method = lm)

cor(activity$TotalSteps, activity$Calories) # 0.6

# The correlation coefficient shows that there is moderate correlation
# between the two variables.
# The visualization corroborates this where a clear positive trend can
# be seen. Most participants had steps of around 0 and 2000 per day.

# Total Distance and Calories

activity %>%
  ggplot(aes(x = TotalDistance, y = Calories)) +
  geom_point() +
  geom_smooth(method = lm)

cor(activity$TotalDistance, activity$Calories) # 0.65

# There is moderate positive correlation between the distance travelled
# and calories lost. The 95% confidence interval is small (as seen by
# the viz.) indicating a shorter range of possible values.
# Hence, as the distance travelled increases, so does the calories lost.

# Sedentary Minutes and Calories

activity %>%
  ggplot(aes(x = SedentaryMinutes, y = Calories)) +
  geom_point() +
  geom_smooth(method = lm)

cor(activity$SedentaryMinutes, activity$Calories) # -0.11

# There is close to no correlation between sedentary minutes and calories
# lost. However, there is a negative trend line indicating that participants
# who are inactive for more minutes in a day lose less calories.
# As seen by the viz., the points are scattered across the graph and
# hence it is not possible to make a strong correlation statement.

# Fairly Active Minutes and Calories

activity %>%
  ggplot(aes(x = FairlyActiveMinutes, y = Calories)) +
  geom_point() +
  geom_smooth(method = lm)

cor(activity$FairlyActiveMinutes, activity$Calories) # 0.3

# The points are gathered at the left side of the viz. showing that
# most participants have less than 75 active minutes.
# The correlation coefficient shows that there is not a strong or 
# moderate correlation between fairly active minutes and calories lost.

## Summary



## SHARE: Focus more on people between 85 and 95 kgs.
  # Focus on people who sleep between 300 to 500 mins. 
  # Help people increase steps.
# Growth opporrtunity because people are not as active.



