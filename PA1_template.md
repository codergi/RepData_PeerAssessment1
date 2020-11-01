---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", na.string = TRUE)
```



## What is mean total number of steps taken per day?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
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
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.3
```

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
data <- read.csv("activity.csv")
good <- complete.cases(data) #Removing NAs
data <- data[good, ]
data$steps <- as.integer(data$steps)
data <- subset(data, steps > 0)

data$date <- as.Date(as.character(data$date), format = "%Y-%m-%d")
sum_steps <- data %>%
        group_by(date) %>%
        summarise_at(vars("steps"), sum)
ggplot(data = sum_steps, aes(x = date, y = steps)) + geom_bar(stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_steps <- data %>%
        group_by(date) %>%
        summarise_at(vars("steps"), mean)

median_steps <- data %>%
        group_by(date) %>%
        summarise_at(vars("steps"), median)

colnames(mean_steps) <- c("Date", "Mean.Steps")
colnames(median_steps) <- c("Date", "Median.Steps")
merge(mean_steps, median_steps)
```

```
##          Date Mean.Steps Median.Steps
## 1  2012-10-02   63.00000         63.0
## 2  2012-10-03  140.14815         61.0
## 3  2012-10-04  121.16000         56.5
## 4  2012-10-05  154.58140         66.0
## 5  2012-10-06  145.47170         67.0
## 6  2012-10-07  101.99074         52.5
## 7  2012-10-09  134.85263         48.0
## 8  2012-10-10   95.19231         56.5
## 9  2012-10-11  137.38667         35.0
## 10 2012-10-12  156.59459         46.0
## 11 2012-10-13  119.48077         45.5
## 12 2012-10-14  160.61702         60.5
## 13 2012-10-15  131.67532         54.0
## 14 2012-10-16  157.12500         64.0
## 15 2012-10-17  152.86364         61.5
## 16 2012-10-18  152.36364         52.5
## 17 2012-10-19  127.19355         74.0
## 18 2012-10-20  125.24096         49.0
## 19 2012-10-21   96.93407         48.0
## 20 2012-10-22  154.71264         52.0
## 21 2012-10-23  101.34091         56.0
## 22 2012-10-24  104.43750         51.5
## 23 2012-10-25   56.63636         35.0
## 24 2012-10-26   77.02273         36.5
## 25 2012-10-27  134.92000         72.0
## 26 2012-10-28  110.17308         61.0
## 27 2012-10-29   80.93548         54.5
## 28 2012-10-30  110.32584         40.0
## 29 2012-10-31  179.23256         83.5
## 30 2012-11-02  143.24324         55.5
## 31 2012-11-03  117.45556         59.0
## 32 2012-11-05  141.06757         66.0
## 33 2012-11-06  100.40964         52.0
## 34 2012-11-07  135.61053         58.0
## 35 2012-11-08   61.90385         42.5
## 36 2012-11-11  132.71579         55.0
## 37 2012-11-12  156.01449         42.0
## 38 2012-11-13   90.56790         57.0
## 39 2012-11-15   20.50000         20.5
## 40 2012-11-16   89.19672         43.0
## 41 2012-11-17  183.83333         65.5
## 42 2012-11-18  162.47312         80.0
## 43 2012-11-19  117.88000         34.0
## 44 2012-11-20   95.14894         58.0
## 45 2012-11-21  188.04412         55.0
## 46 2012-11-22  177.62609         65.0
## 47 2012-11-23  252.30952        113.0
## 48 2012-11-24  176.56098         65.5
## 49 2012-11-25  140.88095         84.0
## 50 2012-11-26  128.29885         53.0
## 51 2012-11-27  158.67442         57.0
## 52 2012-11-28  212.14583         70.0
## 53 2012-11-29  110.10938         44.5
```



## What is the average daily activity pattern?

```r
library(dplyr)
library(ggplot2)
library(data.table)

data <- read.csv("activity.csv")
good <- complete.cases(data) #Removing NAs
data <- data[good, ]
data$steps <- as.integer(data$steps)
data <- subset(data, steps > 0)

mean_steps <- data %>%
        group_by(interval) %>%
        summarise_at(vars("steps"), mean)

g <- ggplot(data = mean_steps, aes(x = interval, y = steps)) + 
  geom_line( color = "blue") + 
  geom_point(color = "black")
g + labs(title = "Average Daily Activity Pattern", 
         x = "5-minute Interval", 
         y = "Average Number of Steps Taken") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max_mean <- as.character(max(mean_steps$steps))
max <- as.list(mean_steps[grepl(max_mean, mean_steps$steps), ])
max_mean <- max$steps
max_interval <- max$interval
```


The 5-minute interval which contains the maximum number of steps on average 
across all the days in the dataset is on the 835-minute mark with a value of 352.4839

## Imputing missing values

```r
library(dplyr)
data <- read.csv("activity.csv")
na_values <- sum(is.na(data$steps))

data$date <- as.Date(as.character(data$date), format = "%Y-%m-%d")
mean_steps <- data %>%
        group_by(date) %>%
        summarise_at(vars("steps"), mean)

data[is.na(data)] <- mean(data$steps, na.rm = TRUE)

ggplot(data = data, aes(x = date, y = steps)) + geom_bar(stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean_steps <- data %>%
        group_by(date) %>%
        summarise_at(vars("steps"), mean)

median_steps <- data %>%
        group_by(date) %>%
        summarise_at(vars("steps"), median)

colnames(mean_steps) <- c("Date", "Mean.Steps")
colnames(median_steps) <- c("Date", "Median.Steps")
merge(mean_steps, median_steps)
```

```
##          Date Mean.Steps Median.Steps
## 1  2012-10-01 37.3825996      37.3826
## 2  2012-10-02  0.4375000       0.0000
## 3  2012-10-03 39.4166667       0.0000
## 4  2012-10-04 42.0694444       0.0000
## 5  2012-10-05 46.1597222       0.0000
## 6  2012-10-06 53.5416667       0.0000
## 7  2012-10-07 38.2465278       0.0000
## 8  2012-10-08 37.3825996      37.3826
## 9  2012-10-09 44.4826389       0.0000
## 10 2012-10-10 34.3750000       0.0000
## 11 2012-10-11 35.7777778       0.0000
## 12 2012-10-12 60.3541667       0.0000
## 13 2012-10-13 43.1458333       0.0000
## 14 2012-10-14 52.4236111       0.0000
## 15 2012-10-15 35.2048611       0.0000
## 16 2012-10-16 52.3750000       0.0000
## 17 2012-10-17 46.7083333       0.0000
## 18 2012-10-18 34.9166667       0.0000
## 19 2012-10-19 41.0729167       0.0000
## 20 2012-10-20 36.0937500       0.0000
## 21 2012-10-21 30.6284722       0.0000
## 22 2012-10-22 46.7361111       0.0000
## 23 2012-10-23 30.9652778       0.0000
## 24 2012-10-24 29.0104167       0.0000
## 25 2012-10-25  8.6527778       0.0000
## 26 2012-10-26 23.5347222       0.0000
## 27 2012-10-27 35.1354167       0.0000
## 28 2012-10-28 39.7847222       0.0000
## 29 2012-10-29 17.4236111       0.0000
## 30 2012-10-30 34.0937500       0.0000
## 31 2012-10-31 53.5208333       0.0000
## 32 2012-11-01 37.3825996      37.3826
## 33 2012-11-02 36.8055556       0.0000
## 34 2012-11-03 36.7048611       0.0000
## 35 2012-11-04 37.3825996      37.3826
## 36 2012-11-05 36.2465278       0.0000
## 37 2012-11-06 28.9375000       0.0000
## 38 2012-11-07 44.7326389       0.0000
## 39 2012-11-08 11.1770833       0.0000
## 40 2012-11-09 37.3825996      37.3826
## 41 2012-11-10 37.3825996      37.3826
## 42 2012-11-11 43.7777778       0.0000
## 43 2012-11-12 37.3784722       0.0000
## 44 2012-11-13 25.4722222       0.0000
## 45 2012-11-14 37.3825996      37.3826
## 46 2012-11-15  0.1423611       0.0000
## 47 2012-11-16 18.8923611       0.0000
## 48 2012-11-17 49.7881944       0.0000
## 49 2012-11-18 52.4652778       0.0000
## 50 2012-11-19 30.6979167       0.0000
## 51 2012-11-20 15.5277778       0.0000
## 52 2012-11-21 44.3993056       0.0000
## 53 2012-11-22 70.9270833       0.0000
## 54 2012-11-23 73.5902778       0.0000
## 55 2012-11-24 50.2708333       0.0000
## 56 2012-11-25 41.0902778       0.0000
## 57 2012-11-26 38.7569444       0.0000
## 58 2012-11-27 47.3819444       0.0000
## 59 2012-11-28 35.3576389       0.0000
## 60 2012-11-29 24.4687500       0.0000
## 61 2012-11-30 37.3825996      37.3826
```

The values when imputing the missing values had a drastic change and overall the 
mean and the median of the steps decreased compare when the missing values are not included in the computation


## Are there differences in activity patterns between weekdays and weekends?

```r
data <- read.csv("activity.csv")
data$day <- weekdays(as.Date(as.character(data$date), format = "%Y-%m-%d"))
#data$date <- factor(data$date, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
data[is.na(data)] <- 0
good <- complete.cases(data) #Removing NAs
data <- data[good, ]
data$steps <- as.integer(data$steps)
data <- subset(data, steps > 0)

data$wday <- ifelse(data$day == "Monday", "Weekday",
                    ifelse(data$day == "Tuesday", "Weekday",
                           ifelse(data$day == "Wednesday", "Weekday",
                                  ifelse(data$day == "Thursday", "Weekday",
                                         ifelse(data$day == "Friday", "Weekday",
                                                ifelse(data$day == "Saturday", "Weekend",
                                                       ifelse(data$day == "Sunday", "Weekend", 
                                                              NA )))))))
data$wday <- as.factor(data$wday)    

mean_steps <- data %>%
        group_by(interval, wday) %>%
        summarise_at(vars("steps"), mean)


ggplot(data = mean_steps, aes(x = interval, y = steps)) + 
  geom_line(stat = "identity", color = "blue") + 
  facet_grid(rows = vars(wday))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

