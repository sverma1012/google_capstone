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



## SHARE: Focus more on people between 85 and 95 kgs.
# Focus on people who sleep between 300 to 500 mins. 
# Help people increase steps.
# Growth opportunity because people are not as active.
# people get it to motivate themselves to workout