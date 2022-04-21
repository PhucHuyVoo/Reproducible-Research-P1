#Commit containing full submission
#Step 1:
#Code for reading in the dataset and/or processing the data
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}
activity<-read.csv("activity.csv")

#Step 2:
#Histogram of the total number of steps taken each day
totalStepsByDay<-aggregate(steps~date, activity, sum)

png('plot2.png')
hist(totalStepsByDay$steps, xlab="Class of Total Number of Steps per day", 
     ylab="Number of Days", main="Total Number of Steps taken each day")
dev.off()

#Step 3:
#Mean and median number of steps taken each day
mean_raw<-mean(totalStepsByDay$steps)
median_raw<-median(totalStepsByDay$steps)

#Step 4:
#Time series plot of the average number of steps taken
averageStepsbyInterval<-aggregate(steps~interval, activity, mean)
png('plot4.png')
with(averageStepsbyInterval, plot(interval, steps, type = "l"))
dev.off()

#Step 5:
#The 5-minute interval that, on average, contains the maximum number of steps
averageStepsbyInterval[which.max(averageStepsbyInterval[,2]),1]

#Step 6:
#Code to describe and show a strategy for imputing missing data
#Find the total number of missing value
missingIndex<-is.na(activity[,1])
#Find the mean number of steps per Interval
m<-mean(averageStepsbyInterval$steps)
# Create the new dataset from the old one but with the missing data filled in
activity1<-activity
activity1[missingIndex,1]<-m

#Step 7:
#Histogram of the total number of steps taken each day after missing values are imputed
png('plot7.png')
totalStepsByDay1<-aggregate(steps~date, activity1, sum)
hist(totalStepsByDay1$steps, xlab="Class of Total Number of Steps per day",
     ylab="Number of Days", main="Number of Steps taken each day after missing values are imputed")
dev.off()

totalStepsByDay1<-aggregate(steps~date, activity1, sum)
mean_afterImput<-mean(totalStepsByDay1$steps)
median_afterImput<-median(totalStepsByDay1$steps)

#Step 8:
#Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis)
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
#See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
activity1$date<-as.Date(activity1$date)
activity2<-activity1%>%
        mutate(dayType= ifelse(weekdays(activity1$date)=="Saturday" | weekdays(activity1$date)=="Sunday", "Weekend", "Weekday"))

averageStepByDayTypeAndInterval<-activity2 %>%
        group_by(dayType, interval) %>%
        summarize(averageStepByDay=sum(steps))
png('plot8.png')
with(averageStepByDayTypeAndInterval, 
     xyplot(averageStepByDay ~ interval | dayType, 
            type = "l",      
            main = "Total Number of Steps within Intervals by dayType",
            xlab = "Daily Intervals",
            ylab = "Average Number of Steps"))
dev.off()


