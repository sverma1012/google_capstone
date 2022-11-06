# Author: Sneha Verma
# Date of Creation: 16 August 2022
# Purpose of Script: Communicate the results of the analysis and
# relate it to the business problem

# The goal of this project is to analyze consumer's use of a competitor
# for growth opportunities and recommendations for the marketing team.

# Import datasets

activity = read.csv("../Data/Importable Data/dailyActivity_merged.csv")
sleepActivity = read.csv("../Data/Importable Data/sleepActivity.csv")
weightActivity = read.csv("../Data/Importable Data/weightActivity.csv")


# The following visualizations describe the results from the analysis.

library(ggplot2)

# Weight and Calories

ggplot(data = weightActivity, aes(x=WeightKg_name, y=calories)) +
  geom_point(color = 'blue') +
  geom_smooth(method = lm, color = 'black') +
  ggtitle('Weight v/s Calories') +
  theme(plot.title = element_text(hjust = 0.5, size = 20, 
                                  face = 'bold')) +
  xlab('Weight (Kgs)') +
  ylab('Calories') + 
  ylim(1500, 3500)

# This visualization shows that most participants lost approximately
# 2000 calories. However, there are outliers- participants between
# the weight of 85 and 95 kgs lost the most calories.

# Sleep and Calories

ggplot(data = sleepActivity, aes(x = minutesAsleep, y = calories)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle('Minutes Asleep v/s Calories') +
  theme(plot.title = element_text(hjust = 0.5, size = 20, 
                                  face = 'bold')) +
  xlab('Minutes Asleep') +
  ylab('Calories')

# As it can be seen from the visualization, there is not a strong
# correlation between the minutes participants sleep and the calories
# that the participants lost.
# The participants had a large range of asleep minutes. However,
# most participants slept between 300 and 500 minutes, and lost
# between 1500 to 3500 calories.

# Total Steps and Calories

ggplot(data = activity, aes(x = TotalSteps, y = Calories)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle('Total Steps v/s Calories') +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = 'bold'))

# There is a clear correlation between the steps that participants
# took and the calories they lost.

# Very Active Minutes and Total Steps (the following code is the same as in the analysis file)

activity %>%
  ggplot(aes(x = VeryActiveMinutes, y = TotalSteps)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle('Total Steps v/s Very Active Minutes') +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = 'bold'))

cor(activity$VeryActiveMinutes, activity$TotalSteps) # 0.67

mean(activity$VeryActiveMinutes) # 21 minutes
mean(activity$SedentaryMinutes) # 991 minutes

# Participants who were active are more likely to have taken 
# more steps and traveled longer distances.
# There is a large difference between how long people were active
# and were sedentary, as witnessed by their averages. This shows
# that the participants were mostly inactive.

# Table Comparison of Distance and Minutes

distance = matrix(c(
  mean(activity$VeryActiveDistance),
  mean(activity$ModeratelyActiveDistance),
  mean(activity$LightActiveDistance),
  mean(activity$SedentaryActiveDistance)
),ncol=1,byrow=TRUE)

colnames(distance) = c('Distance Type')
rownames(distance) = c(
  'Very Active Distance',
  'Moderately Active Distance',
  'Light Active Distance',
  'Sedentary Active Distance'
)

distance

# The average of 'Light Active Distance' is the highest followed by 'Very Active
# Distance.' This indicates that when people traveled, they were mostly active.


minutes = matrix(c(
  mean(activity$VeryActiveMinutes),
  mean(activity$FairlyActiveMinutes),
  mean(activity$LightlyActiveMinutes),
  mean(activity$SedentaryMinutes)
),ncol=1,byrow=TRUE)

colnames(minutes) = c('Minutes Type')
rownames(minutes) = c(
  'Very Active Minutes',
  'Fairly Active Minutes',
  'Lightly Active Minutes',
  'Sedentary Minutes'
)

minutes

# On the other hand, it appears that participants were mostly sedentary with the highest
# average of 991 minutes. 
# This raises a question of how the different types of distances and minutes were
# calculated.

# The visualizations shown above show that people between 85 and 95 kgs
# were the most active and lost the most calories, making these people
# a great target audience for Bellabeat products. Further, participants who had
# an average sleep time between 300 and 500 mins lost the most calories
# showing that healthy sleep may be an important aspect of using health devices.
# One great technique would be to motivate people to increase their steps
# and sleep times to help them use the devices for beneficial purposes.
# It was seen that most participants were mostly sedentary while using this
# device, so Bellabeat's smart devices can advice people on how to 
# positively change their lifestyles.
