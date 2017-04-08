setwd("/Users/davidtan/Code/learn-r/data-analytics-with-r/Final Project/Project Data")

# Problem statement
# As an operator of a bike sharing business, I want to maximize utilisation of the service during lean demand periods which can facilitate the goal of increasing the share of bike modeas means of travel. To that end, I want to develop an incentive scheme, which can be monetary or non-monetary, so as to encourage people to take up bike sharing. To develop such a scheme, it is essential to understand when users choose to/not to use the service using the data I have

# 0) Load the packages
packages = c('ggplot2', 'car')
install.packages(packages)
lapply(packages, require, character.only = TRUE)

# 1) Data Cleaning & Processing : Detecting and correcting (or removing) corrupt or Inaccurate records that may distort the results.
day_data <- read.csv('./day.csv')

# inspect data
str(day_data)
summary(day_data)

instant <- day_data$instant
dteday <- day_data$dteday
season <- day_data$season
yr <- day_data$yr
mnth <- day_data$mnth
hr <- day_data$hr
holiday <- day_data$holiday
weekday <- day_data$weekday
workingday <- day_data$workingday
weathersit <- day_data$weathersit
temp <- day_data$temp
atemp <- day_data$atemp
hum <- day_data$hum
windspeed <- day_data$windspeed
casual <- day_data$casual
registered <- day_data$registered
cnt <- day_data$cnt

# 2) Data Exploration and Visualization: 
day_df = data.frame(day_data) # Creating a data frame because ggplot2 requires it

# Preliminary inspect on our hypothesis on whether there is a relationship between:
# - season and cnt?
# - working day and cnt?
# - weekday and cnt?
# - weather sit and cnt?
# 
# Whether temp and atemp had a different effect on sales
# 
# Whether the effects that we discovered differed among casual users and registered users

# 2.1 Plotting everything and saving it
jpeg("./images/all_charts.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
plot(day_data) # not so useful. let's get more granular visualisations
dev.off()

# 2.2 plotting season against cnt
jpeg("./images/season_cnt.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(season), cnt)) + geom_boxplot(outlier.colour = 'red')
dev.off()

jpeg("./images/season_registered.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(season), registered)) + geom_boxplot(outlier.colour = 'red')
dev.off()

jpeg("./images/season_casual.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(season), casual)) + geom_boxplot(outlier.colour = 'red')
dev.off()


# 2.3 working day and cnt?
jpeg("./images/workingday_cnt.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(workingday), cnt)) + geom_boxplot(outlier.colour = 'red') # non-working days see higher sales
dev.off()

jpeg("./images/workingday_registered.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(workingday), registered)) + geom_boxplot(outlier.colour = 'red') # working days see higher sales
dev.off()

jpeg("./images/workingday_casual.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(workingday), casual)) + geom_boxplot(outlier.colour = 'red')  # non-working days see higher sales
dev.off()

# 2.4 weekday and cnt?
jpeg("./images/weekday_cnt.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(weekday), cnt)) + geom_violin(color='grey', fill='grey') # not much pattern can be observed
dev.off()

jpeg("./images/weekday_registered.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(weekday), registered)) + geom_violin(color='grey', fill='grey') # lots of registered users on weekdays
dev.off()

jpeg("./images/weekday_casual.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(weekday), casual)) + geom_violin(color='grey', fill='grey') # casual users mainly use the bikes on weekends
dev.off()

# 2.5 weather sit and cnt?
jpeg("./images/weathersit_cnt.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(weathersit), cnt)) + geom_boxplot(outlier.colour = 'red') # weathersits 1 and 2 are generally fine. sales dip in weathersit 3
dev.off()

jpeg("./images/weathersit_registered.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(weathersit), registered)) + geom_boxplot(outlier.colour = 'red') # same pattern for registered users
dev.off()

jpeg("./images/weathersit_casual.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(weathersit), casual)) + geom_boxplot(outlier.colour = 'red') # casual users dip more drastically in weathersit 3
dev.off()

# 2.6 Whether temp and atemp had a different effect on sales
jpeg("./images/temp_atemp.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df) + geom_point(aes(x=atemp, y=cnt), colour='#00C8E8') + geom_point(aes(x=temp, y=cnt), colour='#FFE402') + labs(x = "normalized temperature", y = "cnt")
dev.off()
#TODO: davidtan [2017-04-02] fix the legend

# 2.7 holiday and count?
jpeg("./images/holiday_cnt.jpeg", width = 3.5, height = 2.5, units = 'in', res = 500)
ggplot(day_df, aes(factor(holiday), cnt)) + geom_boxplot(outlier.colour = 'red')
dev.off()
#TODO: davidtan [2017-04-02] fix the legend

ggplot(day_df) + geom_point(aes(x=dteday, y=cnt))