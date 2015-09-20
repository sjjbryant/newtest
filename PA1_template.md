---
title: "Reproducible Research: Peer Assessment 1"
author: "Steve Bryant"
date: "September 20, 2015"
output: html_document
    keep_md: yes
  pdf_document: default
---

## Loading and preprocessing the data

Download the file from the internet.


```r
if (!file.exists("activity.zip")){
      download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                    destfile = "activity.zip", method = "wget")
      unzip("activity.zip")
}
```

```
## Warning: running command 'wget "https://d396qusza40orc.cloudfront.net/
## repdata%2Fdata%2Factivity.zip" -O "activity.zip"' had status 127
```

```
## Warning in download.file("https://d396qusza40orc.cloudfront.net/repdata
## %2Fdata%2Factivity.zip", : download had nonzero exit status
```

```
## Warning in unzip("activity.zip"): error 1 in extracting from zip file
```

```r
activity <- read.csv("~/RepData_PeerAssessment1/activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'C:/Users/Steve/Documents/
## RepData_PeerAssessment1/activity.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```


## What is mean total number of steps taken per day?

Calculate the total number of steps per day.


```r
steps <- aggregate(steps ~ date, sum, data=activity, na.action=na.omit)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

Make a histogram of the total number of steps taken each day


```r
hist(steps$steps, main="Average Steps Per Day", xlab="Steps Per Day")
```

```
## Error in hist(steps$steps, main = "Average Steps Per Day", xlab = "Steps Per Day"): object 'steps' not found
```

```r
hist(total.steps, breaks=10, main="Histogram of total steps per day", xlab="Steps Per Day", col="gray")
```

```
## Error in hist(total.steps, breaks = 10, main = "Histogram of total steps per day", : object 'total.steps' not found
```

Calculate and report the mean and median total number of steps taken per day


```r
mean(steps$steps)
```

```
## Error in mean(steps$steps): object 'steps' not found
```

```r
median(steps$steps)
```

```
## Error in median(steps$steps): object 'steps' not found
```

## What is the average daily activity pattern?


```r
intervalData <- aggregate(steps ~ interval, mean, data=activity, 
                     na.action=na.omit)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
plot(intervalData$interval, intervalData$steps, type="l", 
     main="Steps in 5 minutes", xlab="Intervals", ylab="Steps")
```

```
## Error in plot(intervalData$interval, intervalData$steps, type = "l", main = "Steps in 5 minutes", : object 'intervalData' not found
```

What interval did the subject usually take the most steps in?


```r
intervalData[intervalData$steps == max(intervalData$steps),]
```

```
## Error in eval(expr, envir, enclos): object 'intervalData' not found
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

Create a new dataset that is equal to the original dataset but with the missing data filled in using average.


```r
for(i in 1:length(activity$steps)) {
  if(is.na(activity[i,1])) {
    activity[i,1] <- intervalData[intervalData$interval == activity[i,3], 2]
  }
}
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

Re-graph the total number of steps per day with missing data filled in.
Does it make a big difference?
Is there any difference?


```r
steps <- aggregate(steps ~ date, sum, data=activity, na.action=na.omit)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
hist(steps$steps, main="Average Steps Per Day", xlab="Steps Per Day")
```

```
## Error in hist(steps$steps, main = "Average Steps Per Day", xlab = "Steps Per Day"): object 'steps' not found
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean of steps per day.


```r
mean(steps$steps)
```

```
## Error in mean(steps$steps): object 'steps' not found
```

Median of steps per day.


```r
median(steps$steps)
```

```
## Error in median(steps$steps): object 'steps' not found
```

Averages are about the same. There is a lower variance.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity$date <- as.Date(activity$date)
```

```
## Error in as.Date(activity$date): object 'activity' not found
```

```r
activity$day <- activity$steps
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
for(i in 1:length(activity$day)) {
  if(weekdays(activity[i,"date"]) == "Sunday") {
    activity[i,"day"] <- "weekend"
  }else if(weekdays(activity[i,"date"]) == "Saturday") {
    activity[i,"day"] <- "weekend"
  }else{
    activity[i,"day"] <- "weekday"
  }
}
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
activity$day <- as.factor(activity$day)
```

```
## Error in is.factor(x): object 'activity' not found
```

```r
weekends <- activity[activity$day == "weekend",]
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
weekends <- aggregate(steps ~ interval, mean, data=weekends, 
                     na.action=na.omit)
```

```
## Error in eval(expr, envir, enclos): object 'weekends' not found
```

```r
weekends$day <- rep.int("weekend", length(weekends$steps))
```

```
## Error in rep.int("weekend", length(weekends$steps)): object 'weekends' not found
```

```r
weekdays <- activity[activity$day == "weekday",]
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
weekdays <- aggregate(steps ~ interval, mean, data=weekdays, 
                     na.action=na.omit)
```

```
## Error in as.data.frame.default(data, optional = TRUE): cannot coerce class ""function"" to a data.frame
```

```r
weekdays$day <- rep.int("weekday", length(weekends$steps))
```

```
## Error in rep.int("weekday", length(weekends$steps)): object 'weekends' not found
```

```r
intervalData <- rbind(weekdays, weekends)
```

```
## Error in rbind(weekdays, weekends): object 'weekends' not found
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)
xyplot(steps ~ interval | day, data = intervalData, layout=c(1,2),
       main = "Steps per 5 minutes", xlab="interval", ylab="step",
       type="l")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'intervalData' not found
```

Activity pattern is different between weekends and weekdays. Maybe they sit at a desk during the week.

Weekend average per 5 minute interval.

```r
mean(weekends$steps)
```

```
## Error in mean(weekends$steps): object 'weekends' not found
```

Weekday average per 5 minute interval.

```r
mean(weekdays$steps)
```

```
## Error in weekdays$steps: object of type 'closure' is not subsettable
```
