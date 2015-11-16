---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
if(!file.exists("activity.csv")) {
        unzip("activity.zip");
}
activity <- read.csv("activity.csv", colClasses=c("numeric", "Date", "integer"))
dim(activity)
```

```
## [1] 17568     3
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

- Calculate the total number of steps taken per day

```r
stepsPerDay <- with(activity, aggregate(steps, by=list(date), FUN=sum, na.rm=TRUE))
names(stepsPerDay) <- c("date", "steps")
stepsPerDay
```

```
##          date steps
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01     0
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04     0
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09     0
## 41 2012-11-10     0
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14     0
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30     0
```

- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
# histogram groups close values together and gives frequencies of values for each group
hist(stepsPerDay$steps, col="salmon")
rug(stepsPerDay$steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


```r
# barplot makes bar for each value in order it apears
barplot(stepsPerDay$steps, names.arg=stepsPerDay$date)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

- Calculate and report the mean and median of the total number of steps taken per day

```r
# Mean of the total number of steps taken per day:
mean(stepsPerDay$steps, na.rm=TRUE )
```

```
## [1] 9354
```


```r
# Median of the total number of steps taken per day:
median(stepsPerDay$steps, na.rm=TRUE)
```

```
## [1] 10395
```



## What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsPerInterval <- with(activity, aggregate(steps, by=list(interval), FUN=mean, na.rm=TRUE))
names(stepsPerInterval) <- c("interval", "steps")
plot(steps ~ interval, data=stepsPerInterval, type="l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepsPerInterval[stepsPerInterval$steps == max(stepsPerInterval$steps), ]
```

```
##     interval steps
## 104      835 206.2
```


## Imputing missing values

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```
- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# strategy is to use mean for that 5-minute interval, where steps is missing
#      - date and interval has no NA values -> no need to change anything
```

- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity2 <- activity
for(i in seq_along(activity2$steps)) {
    if(is.na(activity2$steps[i])) {
        meanValue <- stepsPerInterval[stepsPerInterval$interval == activity2$interval[i], ]
        activity2$steps[i] <- meanValue$steps
    }
}
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsPerDay2 <- with(activity2, aggregate(steps, by=list(date), FUN=sum))
names(stepsPerDay2) <- c("date", "steps")

#barplot(stepsPerDay2$steps, names.arg=stepsPerDay2$date)
hist(stepsPerDay2$steps, col="green")
rug(stepsPerDay2$steps)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```r
# Mean of the total number of steps taken per day:
mean(stepsPerDay2$steps)
```

```
## [1] 10766
```


```r
# Median of the total number of steps taken per day:
median(stepsPerDay2$steps)
```

```
## [1] 10766
```



## Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
whatday <- function(x) {
    day <- weekdays(x)
    if((day=="Sunday") || (day=="Saturday")) {
        day <- "weekend"
    } else {
        day <- "weekday"
    }
    day
}
activity2$day <- factor(mapply(whatday, activity2$date))
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
stepsPerIntWeekday <- with(subset(activity2, day=="weekday"),
                           aggregate(steps, by=list(interval), FUN=mean))
names(stepsPerIntWeekday) <- c("interval", "steps")

stepsPerIntWeekend <- with(subset(activity2, day=="weekend"),
                           aggregate(steps, by=list(interval), FUN=mean))
names(stepsPerIntWeekend) <- c("interval", "steps")


par(mfcol=c(2,1))
plot(steps ~ interval, data=stepsPerIntWeekday, type="l", main="Weekday")
plot(steps ~ interval, data=stepsPerIntWeekend, type="l", main="Weekend")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 
