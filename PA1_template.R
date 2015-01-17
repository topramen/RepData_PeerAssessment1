if  (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}
if  (!require(plyr)) {
  install.packages("plyr")
  library(plyr)
}
if  (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if  (!require(xts)){
  install.packages("xts")
  library(xts)
}
if  (!require(reshape2)){
  install.packages("reshape2")
  library(reshape2)
}


steps <- read.csv("activity.csv", stringsAsFactors=F)
names(steps)
summary(steps)


steps$date <- ymd(steps$date)
steps.complete <- steps[complete.cases(steps),]
steps.daily.complete <- aggregate(steps ~ date, steps.complete, sum)
str(steps.daily.complete)

#histogram of daily steps
hist(steps.daily.complete$steps)

#mean and median of daily steps
mean.sdc <- mean(steps.daily.complete$steps)
median.sdc <- median(steps.daily.complete$steps)

#average day
avg.day <- aggregate(steps ~ interval, steps.complete, mean)
#row.names(avg.day) <- hm(ymd_hm(paste("20150101", formatC(avg.day$interval, width = 4, format = "d", flag = "0")))
row.names(avg.day) <- ymd_hm(paste("20150101", formatC(avg.day$interval, width = 4, format = "d", flag = "0")))
max.steps.interval <- avg.day[which.max(avg.day$steps), "interval"]
#For imputing missing values in steps, lookup the steps for that interval from avg.day 
steps <- merge (steps, avg.day, by="interval")
#remove the interval column so that this dataframe can be used as a zoo object
avg.day <- subset (avg.day, select = -c(interval))
#Time Series Plot of average day
plot.zoo(as.xts(avg.day), ylab="Steps", xlab="Hour of the day")


#Handling NA values
na.rows <- is.na(steps$steps.x) 
#How many NAs are there 
sum(na.rows)
#impute the na values in a new data set
#steps$steps.x[na.rows] <- steps$steps.y[na.rows]
steps2 <- transform(steps, steps = ifelse(na.rows, steps$steps.y, steps$steps.x))


#histogram with the imputed data
steps2 <- subset (steps2, select = -c(steps.x, steps.y))
steps2.daily <- aggregate(steps ~ date, steps2, sum)
#histogram of imputed daily steps
hist(steps2.daily$steps)
#mean and median of imputed daily steps
mean.step2.daily <- mean(steps2.daily$steps)
median.steps2.daily <- median(steps2.daily$steps)


#add weekday/weekend identifier to imputed DF steps2
steps2$day <- weekdays(steps2$date)
# Does the day begin with an 's'?
steps2$isWeekend <- grepl("^S", steps2$day)
steps2$dayType <- factor(steps2$isWeekend, levels = c(F, T), labels = c("Weekday", "Weekend"))
head(steps2)
avg.day.imputed <- aggregate(steps ~ interval + dayType, steps2, mean)
avg.day.imputed <- dcast(avg.day.imputed, interval ~ dayType)
row.names(avg.day.imputed) <- ymd_hm(paste("20150101", formatC(avg.day.imputed$interval, width = 4, format = "d", flag = "0")))
avg.day.imputed <- subset (avg.day.imputed, select = -c(interval))
plot.zoo(as.xts(avg.day.imputed), xlab="Interval", main="Number of Steps")
