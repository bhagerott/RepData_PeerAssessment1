---
title: "Reproducible Research Course Project 1"
output: html_document
keep_md: true
---


```r
library(knitr)
library(ggplot2)
mydata<-read.csv("activity.csv", stingsAsFactors=FALSE, header=TRUE, sep=",")
```

```
## Error in read.table(file = file, header = header, sep = sep, quote = quote, : unused argument (stingsAsFactors = FALSE)
```

```r
echo=TRUE
```


```r
mydata$date<-as.Date(mydata$date)
mydataNA<-na.omit(mydata)
echo=TRUE
```


```r
aggregate(steps~date, data=mydata, FUN=function(mydata) c(mean=mean(mydata), count=length(mydata)))
```

```
##          date  steps.mean steps.count
## 1  2012-10-02   0.4375000 288.0000000
## 2  2012-10-03  39.4166667 288.0000000
## 3  2012-10-04  42.0694444 288.0000000
## 4  2012-10-05  46.1597222 288.0000000
## 5  2012-10-06  53.5416667 288.0000000
## 6  2012-10-07  38.2465278 288.0000000
## 7  2012-10-09  44.4826389 288.0000000
## 8  2012-10-10  34.3750000 288.0000000
## 9  2012-10-11  35.7777778 288.0000000
## 10 2012-10-12  60.3541667 288.0000000
## 11 2012-10-13  43.1458333 288.0000000
## 12 2012-10-14  52.4236111 288.0000000
## 13 2012-10-15  35.2048611 288.0000000
## 14 2012-10-16  52.3750000 288.0000000
## 15 2012-10-17  46.7083333 288.0000000
## 16 2012-10-18  34.9166667 288.0000000
## 17 2012-10-19  41.0729167 288.0000000
## 18 2012-10-20  36.0937500 288.0000000
## 19 2012-10-21  30.6284722 288.0000000
## 20 2012-10-22  46.7361111 288.0000000
## 21 2012-10-23  30.9652778 288.0000000
## 22 2012-10-24  29.0104167 288.0000000
## 23 2012-10-25   8.6527778 288.0000000
## 24 2012-10-26  23.5347222 288.0000000
## 25 2012-10-27  35.1354167 288.0000000
## 26 2012-10-28  39.7847222 288.0000000
## 27 2012-10-29  17.4236111 288.0000000
## 28 2012-10-30  34.0937500 288.0000000
## 29 2012-10-31  53.5208333 288.0000000
## 30 2012-11-02  36.8055556 288.0000000
## 31 2012-11-03  36.7048611 288.0000000
## 32 2012-11-05  36.2465278 288.0000000
## 33 2012-11-06  28.9375000 288.0000000
## 34 2012-11-07  44.7326389 288.0000000
## 35 2012-11-08  11.1770833 288.0000000
## 36 2012-11-11  43.7777778 288.0000000
## 37 2012-11-12  37.3784722 288.0000000
## 38 2012-11-13  25.4722222 288.0000000
## 39 2012-11-15   0.1423611 288.0000000
## 40 2012-11-16  18.8923611 288.0000000
## 41 2012-11-17  49.7881944 288.0000000
## 42 2012-11-18  52.4652778 288.0000000
## 43 2012-11-19  30.6979167 288.0000000
## 44 2012-11-20  15.5277778 288.0000000
## 45 2012-11-21  44.3993056 288.0000000
## 46 2012-11-22  70.9270833 288.0000000
## 47 2012-11-23  73.5902778 288.0000000
## 48 2012-11-24  50.2708333 288.0000000
## 49 2012-11-25  41.0902778 288.0000000
## 50 2012-11-26  38.7569444 288.0000000
## 51 2012-11-27  47.3819444 288.0000000
## 52 2012-11-28  35.3576389 288.0000000
## 53 2012-11-29  24.4687500 288.0000000
```

```r
echo=TRUE
```


```r
daysteps<-aggregate(steps~date,mydataNA, sum)
hist(daysteps$steps, freq=TRUE, main="Total Steps Taken in a Day", xlab="Steps", ylab="Frequency of occurance", ylim=c(0,30))
```

![plot of chunk Make a histogram of the total number of steps taken each day](figure/Make a histogram of the total number of steps taken each day-1.png)

```r
echo=TRUE
```


```r
mean(daysteps$steps)
```

```
## [1] 10766.19
```

```r
median(daysteps$steps)
```

```
## [1] 10765
```

```r
echo=TRUE
```
The mean is 10766.19 and the median is 10765.


```r
daysteps5min<-aggregate(steps~interval, mydataNA, mean)
plot(daysteps5min$interval, daysteps5min$steps, type="l",main="Steps Taken per 5-minute Interval", xlab="Time", ylab="Steps Taken", lwd=2)
```

![plot of chunk Make a time series plot of the 5-minute interval and average number of steps take across all days](figure/Make a time series plot of the 5-minute interval and average number of steps take across all days-1.png)

```r
echo=TRUE
```


```r
max<-which.max(daysteps5min$steps)
max
```

```
## [1] 104
```

```r
echo=TRUE
```
104


```r
sum(is.na(mydata))
```

```
## [1] 2304
```

```r
echo=TRUE
```
2304


```r
meanvalue<-mean(mydata[,"steps"], na.rm=TRUE)
mydata2<-mydata
mydata2[is.na(mydata2$steps), "steps"] <-meanvalue
echo=TRUE
```
I chose to take the mean of the values and replace the NA"s with this figure. The mean value was 37.38.The new data set morphed existing values with the newly replaced NA figures.


```r
finalsteps<-aggregate(mydata2$steps, by=list(category=mydata2$date), FUN=sum)
hist(finalsteps$x, freq=TRUE, main= "Total Steps Taken in a Day (cleaned)", xlab="Steps", ylab="Frequency of occurance")
```

![plot of chunk Make a hist of the total number of steps taken each data and the mean and median number of steps taken per day](figure/Make a hist of the total number of steps taken each data and the mean and median number of steps taken per day-1.png)

```r
mean(finalsteps$x)
```

```
## [1] 10766.19
```

```r
median(finalsteps$x)
```

```
## [1] 10766.19
```

```r
echo=TRUE
```
The mean remains the same (10766.19) while the median value increased slightly from 10765 to 10766.19.



```r
##Organize data into two variables, weekday and weekend, by reverting back to original class and seperating it out
##I used the ggplot2 system for simplicity's sake
mydata2[,"date"] <- as.POSIXct(as.character(mydata2$date))
mydata2[,"weekdays"] <- ifelse(weekdays(mydata2$date) == "Sunday" | weekdays(mydata2$date) =="Saturday", "weekends", "weekdays")
mydata2[,"weekdays"] <- as.factor(mydata2$weekdays)
workdaydata <- mydata2[mydata2$weekdays == "weekdays",]
weekenddata <- mydata2[mydata2$weekdays == "weekends",]
weekdays <- aggregate(workdaydata$steps, by=list(interval=workdaydata$interval), FUN=mean, na.rm=TRUE)
weekdays[,"weekdays"] <- as.factor("weekdays")
weekenddays <- aggregate(weekenddata$steps, by=list(interval=weekenddata$interval), FUN=mean, na.rm=TRUE)
weekenddays[, "weekdays"] <- as.factor("weekends")
mydata3 <- rbind(weekenddays, weekdays)
colnames(mydata3)[colnames(mydata3)=="x"] <- "steps"
qplot(interval, steps, data=mydata3, geom="line", facets = weekdays ~., main="Average Steps taken per 5-,minute interval for Weekdays and Weekends")
```

![plot of chunk Compare the data for the weekday and weekend](figure/Compare the data for the weekday and weekend-1.png)

```r
echo=TRUE
```
