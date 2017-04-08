# Set working directory and Import datasets
setwd("/Users/NugNug/Downloads")
day=read.csv("day.csv")
hour=read.csv("hour.csv",header = F,col.names=c("instant","dteday","season","yr","mnth","hr", "holiday","weekday","workingday","weathersit","temp","atemp","hum","windspeed","casual","registered","cnt"))


# Data cleaning and processing
# Checking NA values
index<-which(!complete.cases(day))
day[index,]
# Remove all NA
dayclean<-day[-index,]

# Checking invalid values in categorical variables
table(dayclean$season) #season has one wrong record (5)
which(dayclean$season==5)
fix<-which(dayclean$season==5)
dayclean$season[390:410] #Fixed the value by using the neighbour value
dayclean$season[fix]<-1
table(dayclean$season)

table(dayclean$yr)
table(dayclean$mnth)
table(dayclean$holiday) #holiday has one wrong record (3)

which(dayclean$holiday==3)
index1<-which(dayclean$holiday==3)
dayclean<-dayclean[-index1,] #remove the wrong record

table(dayclean$weekday)
table(dayclean$workingday)
table(dayclean$weathersit) #only 3 kinds of weather instead of 4 as in the data description

instant <- dayclean$instant
dteday <- dayclean$dteday
season <- dayclean$season
yr <- dayclean$yr
mnth <- dayclean$mnth
hr <- dayclean$hr
holiday <- dayclean$holiday
weekday <- dayclean$weekday
workingday <- dayclean$workingday
weathersit <- dayclean$weathersit
temp <- dayclean$temp
atemp <- dayclean$atemp
hum <- dayclean$hum
windspeed <- dayclean$windspeed
casual <- dayclean$casual
registered <- dayclean$registered
cnt <- dayclean$cnt

#Correlation between usage (cnt) and all other factors
Model<-lm(cnt~factor(season)+factor(holiday)+factor(weekday)+factor(workingday)+factor(weathersit)+temp+hum+windspeed)
summary(Model)
par(mfrow=c(2,2))
plot(Model)

#Correlation between usage by registered users (registered) and all other factors
Model1<-lm(registered~factor(season)+factor(holiday)+factor(weekday)+factor(workingday)+factor(weathersit)+temp+hum+windspeed)
summary(Model1)
par(mfrow=c(2,2))
plot(Model1)

#Correlation between usage by casual users (registered) and all other factors
Model2<-lm(casual~factor(season)+factor(holiday)+factor(weekday)+factor(workingday)+factor(weathersit)+temp+hum+windspeed)
summary(Model2)
par(mfrow=c(2,2))
plot(Model2)

#timeseries data

dayclean$date <- as.Date(paste(dayclean$dteday, dayclean$mnth, dayclean$yr), sep = ".",format = "%d.%m.%y")
dayclean$date

#use linear regression
#subset data: take hour data of every day to do linear regression ymd_hms(data$datetime)
#take log of count data to normal distribution so that it is easier to do linear regression
