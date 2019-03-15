---
title: "PA1"
author: "Freddy Li"
output:
  html_document: 
  keep_md: true
  
---


```r
library(knitr)
```

## Loading and preprocessing the data  

```r
if(!file.exists("activity.csv")){
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  temp <- tempfile()
  download.file(url,temp)
  unzip(temp)
  unlink(temp)
}
data <- read.csv("activity.csv",stringsAsFactors = FALSE)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

Transform the data into suitable format  

```r
library(lubridate)
library(dplyr)
data$date <- ymd(data$date)
data <- tbl_df(data)
```

## What is the average daily activity pattern?  
#### 1. Calculate the total number of steps taken per day  

```r
total_perday <- aggregate(steps~date,data,sum,na.rm=TRUE)
```

#### 2. Make a histogram of the total number of steps taken each day  

```r
with(total_perday,hist(steps,col="blue",xlab="Numbers of Steps",main = "Total Steps Taken Each Day"))
```

<img src="PA1_files/figure-html/unnamed-chunk-24-1.png" width="672" />

#### 3. Calculate and report the mean and median of the total number of steps taken per day  

```r
mean_perday <- mean(total_perday$steps)
median_perday <- median(total_perday$steps)
mean_perday
```

```
## [1] 10766.19
```

```r
median_perday
```

```
## [1] 10765
```
The mean and median of the total number of steps taken per day are 1.0766189\times 10^{4} and 10765 respectively.  

## What is the average daily activity pattern?  
#### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```r
steps_interval <- aggregate(steps~interval, data, mean, na.rm=TRUE)
plot(steps_interval$interval, steps_interval$steps, type="l",col="blue",lwd=1,xlab="Interval",ylab="Number of Steps",main="Average Number of Steps for given intervals across all days")
```

<img src="PA1_files/figure-html/unnamed-chunk-26-1.png" width="672" />


#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
max_interval <-steps_interval$interval[max(steps_interval$steps)] 
```
On average across all the days in the dataset, the 5-minute interval, 1705, contains the maximum number of steps.  

## Imputing missing values  
#### 1. Calculate and report the total number of missing values in the dataset  

```r
na_sum <- sum(is.na(data$steps))
na_percent <- round(mean(is.na(data$steps))*100,2)
```
The total number of missing values is 2304, which is 13.11% of total observations.    

#### 2. Devise a strategy for filling in all of the missing values in the dataset.  
For the missing values, the mean for that 5-minute interval will be used to fill in.  

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  

```r
updated_data <-data
updated_data$steps <- ifelse(!is.na(data$steps),data$steps,steps_interval$steps[match(data$interval,steps_interval$interval)])
```

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  

```r
total_new <- aggregate(steps~date,updated_data,sum)
with(total_new,hist(steps,col="red",xlab="Numbers of Steps",main = "Total Steps Taken Each Day"))
```

<img src="PA1_files/figure-html/unnamed-chunk-30-1.png" width="672" />

```r
mean_new <- mean(total_new$steps)
median_new <- median(total_new$steps)
diff_mean <- mean_new - mean_perday
diff_median <- round(median_new - median_perday,2)
```

The mean and median of the total number of steps taken per day after filling missing values are 1.0766189\times 10^{4}, which is same with the old mean due to difference of 0, and 1.0766189\times 10^{4},which is higher than the old median due to difference of 1.19, respectively. The imputed value changes the median of total daily steps a little.  

## Are there differences in activity patterns between weekdays and weekends?  
#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
updated_data$weekday <- factor(ifelse(weekdays(updated_data$date) %in% c("Saturday","Sunday"),"Weekend","Weekday"),levels = c("Weekend","Weekday"))
```

#### 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
steps_interval_new <- aggregate(steps~interval+weekday,updated_data,mean)
xyplot(steps_interval_new$steps~steps_interval_new$interval|steps_interval_new$weekday,type="l",xlab="Interval",ylab="Number of steps",layout=c(1,2))
```

<img src="PA1_files/figure-html/unnamed-chunk-32-1.png" width="672" />
