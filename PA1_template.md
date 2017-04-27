---
title: "Reproducible Research"
output: html_document
---



## Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken

So there are a few steps to be taken in order to complete this assignment.

##STEP 1 : LOAD THE DATA AND REQUIRED PACKAGES

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
if(!file.exists("./data"))dir.create("./data")
fileUrl<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/activity.zip", mode="wb")
unzip('./data/activity.zip', exdir='./data')

activity <- read.csv("./data/activity.csv", header = TRUE, sep = ","
                 , colClasses = c("numeric", "character","integer"))

activity<-mutate(activity,date=as.Date(date))
```

##STEP 2:CALCULATE THE MEAN TOTAL NUMBERS OF STEPS TAKEN PER DAY

NOTE: As this assignment can ignore the NA values, the na.rm=FALSE is used in the summarise function. NAs will be generated in the days where there is no available data. The histogram function will not plot these days.

1.Calculate the total number of steps taken per day.


```r
#Prepare data 
data<-activity%>%
      group_by(date)%>% 
      summarise(dailyTotalSteps=sum(steps,na.rm=FALSE))
```

2. A histogram of the total number of steps taken each day is created.

```r
hist(data$dailyTotalSteps, col="blue",main="Histogram of Total Number of Steps Each Day", xlab="Sum of Steps per Day", ylab="Frequency")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

3.Calculate and report the mean and median of the total number of steps taken per day


```r
mean(data$dailyTotalSteps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(data$dailyTotalSteps,na.rm=TRUE)
```

```
## [1] 10765
```

## STEP 3: CALCULATE THE AVERAGE DAILY ACTIVITY PATTERN
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
data<-activity %>%
      group_by(interval)%>% 
      summarise(averageDailyAct=mean(steps,na.rm=TRUE))

with(data,plot(interval,averageDailyAct,type="l",col="red",main="Average Daily Activity Pattern", xlab="5-min Interval", ylab="Average Steps"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
with(data,interval[which.max(averageDailyAct)])
```

```
## [1] 835
```

## STEP4: IMPUTING MISSING VALUES

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data<-inner_join(activity,data) %>%
  mutate(steps=ifelse(is.na(steps),averageDailyAct,steps)) %>%
  select(-averageDailyAct)
```

```
## Joining, by = "interval"
```

```r
#Example of first few rows of new dataset
head(data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```


3 a) Make a histogram of the total number of steps taken each day.


```r
#Summarise and plot new data 
data1<-data%>%
      group_by(date)%>% 
      summarise(dailyTotalSteps=sum(steps))

hist(data1$dailyTotalSteps, col="pink",main="Histogram of Total Number of Steps Each Day", xlab="Total Numebr of Steps Each Day",ylab="Frequency")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

3 b)Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
mean(data1$dailyTotalSteps)
```

```
## [1] 10766.19
```

```r
median(data1$dailyTotalSteps)
```

```
## [1] 10766.19
```

In conclusion, there is no big difference in the results for both original and new modified datasets. 
The total steps in the modified data is greater but both histograms look similar.

##STEP 5: CALCULATE DIFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS 

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
library(lubridate)
data<-data %>% 
         mutate(day=as.factor(ifelse(wday(date) %in% c(1,7),"weekend","weekday")))

#Show the first few rows of the new dataset
head(data)
```

```
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```
2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
data<- data %>% 
          group_by(day,interval) %>%
          summarise(meansteps=mean(steps))
with (data, 
      xyplot(meansteps ~ interval|day, type="l", 
             ylab="Number of steps",layout=c(2,1)))
```

![plot of chunk unnamed-chunk-12](RepData_PeerAssessment1/unnamed-chunk-12-1.png)

In conclusion, there is only a small difference between these two periods. During weekdays, people move more actively in the day. 

===================================
