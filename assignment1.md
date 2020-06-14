---
title: "Assignment 1"
author: "Manan Rajvir"
date: "12/06/2020"
output: html_document
keep_md: true
---

### Step 1: Loading and Processing the Data    
The first step is to read the data from the CSV file and convert the Date column
into the date format.

```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```


### Step 2: Analysing steps taken per day    
This step involves:   
1. Calculating the total number of steps taken per day  
2. Plotting a Histogram of total number of steps taken each day  
3. Calculating the mean and median of total number of steps taken per day

1. **Total Number of Steps:**

```r
total <- with(data,tapply(steps,date,sum,na.rm=TRUE))
as.table(total)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##          0        126      11352      12116      13294      15420      11015 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##          0      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##      10139      15084      13452      10056      11829      10395       8821 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##      13460       8918       8355       2492       6778      10119      11458 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##       5018       9819      15414          0      10600      10571          0 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##      10439       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336          0         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##       8841       4472      12787      20427      21194      14478      11834 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##      11162      13646      10183       7047          0
```

2. **Histogram**

```r
hist(total,xlab="Number of Daily Steps", ylab = "Frequency", main = "Histogram for Daily Steps")
```

![plot of chunk Histogram](figure/Histogram-1.png)

3. **Mean and Median of total steps taken per day**

```r
mean_steps <- mean(total,na.rm=TRUE)
median_steps <- median(total,na.rm=TRUE)
print(mean_steps)
```

```
## [1] 9354.23
```

```r
print(median_steps)
```

```
## [1] 10395
```

### Step 3: Analysing average daily activity pattern  
This step involves:
1. Making a plot of the 5 minute interval and the average number of steps taken
2. Finding which 5 minute interval, on average across all days contains maximum steps

1. **Time Series Plot**

```r
intervals_steps <- aggregate(x=list(steps=data$steps),by=list(interval=data$interval),mean,na.rm=TRUE)
plot(intervals_steps,type="l",xlab="Interval Periods",ylab = "Average Steps",main="Average Daily Activity Pattern")
```

![plot of chunk TimeSeries](figure/TimeSeries-1.png)

2. **Find Interval with maximum average steps**

```r
intervals_steps[which.max(intervals_steps$steps),"interval"]
```

```
## [1] 835
```

### Step 4: Imputing Missing Values  
The presence of missing values in the data may affect the results.  
Therefore we will:  
1. Calculate total number of missing values in the dataset  
2. Devise a strategy to fill the missing value  
3. Create a new dataset with all the missing values filled  
4. Analyse the new dataset

1. **Calculate missing values**

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. **Filling the Missing Values**
The missing values in the dataset will be filled by using the average value for steps in the particular interval from which the data is missing.

```r
replace_NA <- function(steps, interval) {
     filled <- NA
     if (!is.na(steps)) 
         filled <- c(steps) else filled <- (intervals_steps[intervals_steps$interval == interval, "steps"])
     return(filled)
}
```

3. **Create the new dataset with no missing values**

```r
newdf <- data
newdf$steps <- mapply(replace_NA, newdf$steps, newdf$interval)
sum(is.na(newdf$steps))
```

```
## [1] 0
```

4. **Analyse the new dataset**
The new dataset will be analysed to see if there is any major changes in the histogram for daily steps. We will also analyse the changes in mean and median number of daily steps.

```r
total_new <- with(newdf,tapply(steps,date,sum,na.rm=TRUE))
hist(total_new,xlab="Number of Daily Steps",ylab = "Frequency",main = "Histogram for Daily Steps")
```

![plot of chunk CompareResults](figure/CompareResults-1.png)

```r
mean_steps2 <- mean(total_new,na.rm=TRUE)
median_steps2 <- median(total_new,na.rm=TRUE)
print(mean_steps2)
```

```
## [1] 10766.19
```

```r
print(median_steps2)
```

```
## [1] 10766.19
```

Differences between the two datasets:

```r
cat("Change in mean between 2 datasets = ",mean_steps2-mean_steps)
```

```
## Change in mean between 2 datasets =  1411.959
```

```r
cat("Change in median between 2 datasets = ",median_steps2-median_steps)
```

```
## Change in median between 2 datasets =  371.1887
```

### Step 5: Comparing Weekdays and Weekends
A time series plot will be created to see if there is any difference between the average activities during all intervals on weekdays and weekends


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following objects are masked _by_ '.GlobalEnv':
## 
##     %+%, diamonds, mpg
```

```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}
newdf$date <- as.Date(newdf$date)
newdf$day <- sapply(newdf$date, FUN = weekday.or.weekend)
averages <- aggregate(steps ~ interval + day, data = newdf, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk Weekdays](figure/Weekdays-1.png)
