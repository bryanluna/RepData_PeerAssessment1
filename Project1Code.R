#Read in the CSV data, assuming the file is already downloaded and unzipped in the wd
data <- read.csv("activity.csv")

#Transform data for analysis

##Convert dates to date format
data$date <- as.POSIXct(data$date, format="%Y-%m-%d")
##Add day of week to data
data <- data.frame(date=data$date, 
                           weekday=tolower(weekdays(data$date)), 
                           steps=data$steps, 
                           interval=data$interval)
##Name each day weekday or weekend
data <- cbind(data, daytype=ifelse(data$weekday == "saturday" | 
                                        data$weekday == "sunday", "weekend", "weekday"))
##Create final data frame for analysis
activity <- data.frame(date=data$date, 
                       weekday=data$weekday, 
                       daytype=data$daytype, 
                       interval=data$interval,
                       steps=data$steps)

##Print first couple rows of final data frame
head(activity)

#What is mean total number of steps taken per day?

##Calculate the total number of steps taken per day
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
names(sum_data) <- c("date", "steps")
##Make a histogram of the total number of steps taken each day
hist(sum_data$steps,
     breaks=seq(from=0, to=25000, by=2500),
     xlab="Total Steps",
     ylim=c(0, 20), 
     main="Histogram of Total Steps Each Day")
##Calculate and report the mean and median of the total number of steps taken per day
mean <- mean(sum_data$steps)
median <-median(sum_data$steps)

#What is the average daily activity pattern?

##Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
mean_data <- aggregate(activity$steps,
                       by=list(activity$interval),
                       FUN=mean, 
                       na.rm=TRUE)
names(mean_data) <- c("interval", "mean")
plot(mean_data$interval,
     mean_data$mean, 
     type="l",
     lwd=2, 
     xlab="Interval", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals")
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval <- mean_data[which.max(mean_data$mean),1]

#Imputing missing values

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
NA_count <- sum(is.na(activity$steps))
##Devise a strategy for filling in all of the missing values in the dataset.
na_pos <- which(is.na(activity$steps))
mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))
##Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity[na_pos, "steps"] <- mean_vec
##Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum)
names(sum_data) <- c("date", "steps")
hist(sum_data$steps, 
     breaks=seq(from=0, to=25000, by=2500),
     xlab="Total Steps", 
     ylim=c(0, 30), 
     main="Histogram of Total Steps Each Day\nwith NAs Replaced by Mean of Steps")
mean2 <- mean(sum_data$steps)
median2 <-median(sum_data$steps)

#Are there differences in activity patterns between weekdays and weekends?

##Create a new factor variable in the dataset with two levels - “weekdays” and “weekend”
###This was already done when orignally transforming data for analysis
##Make a panel plot containing a time series plot of the 5- minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
library(lattice)
mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)
names(mean_data) <- c("daytype", "weekday", "interval", "mean")
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
