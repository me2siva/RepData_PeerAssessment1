# Reproducible Research: Peer Assessment 1


```r
knitr::opts_chunk$set(echo = TRUE)

# Load librarys

library(ggplot2) 
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
## Loading the data

activity <- read.csv("activity.csv")

## Preprocessing the data

activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
activity$interval <- as.factor(activity$interval)

str( activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

```r
without_na <- activity[complete.cases(activity),]
## What is mean total number of steps taken per day?

  #1.Calculate the total number of steps taken per day
    ## (total number of (steps taken per day))
    total <- aggregate(steps ~ date, without_na, sum)

    names(total)[2] <- "sum_steps"
    ## check out new data frame
    head(total, 5)
```

```
##         date sum_steps
## 1 2012-10-02       126
## 2 2012-10-03     11352
## 3 2012-10-04     12116
## 4 2012-10-05     13294
## 5 2012-10-06     15420
```

```r
  # Make a histogram of the total number of steps taken each day
  ## plot histogram, using breaks purely for better visuals.
    hist(
            total$sum_steps,
            col = "blue",
            main = "Histogram of the (Total Number of (Steps Taken Each Day))",
            xlab = "(Total Number of (Steps Taken Each Day))",
            breaks = 20
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
    ## mean
    mean(total$sum_steps)
```

```
## [1] 10766.19
```

```r
    ## median
    median(total$sum_steps)
```

```
## [1] 10765
```

```r
## What is the average daily activity pattern?
## the average number of steps taken, averaged across all days for each 5-minute interval
    interval <- aggregate(steps ~ interval, without_na, mean)
    
    names(interval)[2] <- "mean_steps"

    ## format plot margins to accommodate long text labels.
    par(mai = c(1.2,1.5,1,1))
    #Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number     of steps taken, averaged across all days (y-axis)
    
    ## plot time series
    plot(
            x = interval$interval,
            y = interval$mean_steps,
            type = "l",
            main = "Time Series Plot of the 5-Minute Interval\n and the Average Number of Steps Taken, Averaged Across All Days",
            xlab = "5-Minute Interval",
            ylab = "Average Number of Steps Taken,\n Averaged Across All Days"
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
## Imputing missing values
    #Calculate and report the total number of missing values in the dataset (i.e. the total number of       rows with NAs)
    nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

```r
    ## Are there differences in activity patterns between weekdays and weekends?
    ## merge original activity data frame with interval data frame
    newactivity <- merge(activity, interval, by = 'interval', all.y = F)
    
    ## Create a new dataset that is equal to the original dataset but with the missing data filled in.
    newactivity$steps[is.na(newactivity$steps)] <- as.integer(
            round(newactivity$mean_steps[is.na(newactivity$steps)]))

    ## drop and reorder columns to match original activity data frame
    keeps <- names(activity)
    newactivity <- newactivity[keeps]
    # 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
    ## (total number of (steps taken per day))
    newtotal <- aggregate(steps ~ date, newactivity, sum)
    
    ## add descriptive variable names
    names(newtotal)[2] <- "sum_steps"
    
    ## check out new data frame
    head(newtotal, 5)
```

```
##         date sum_steps
## 1 2012-10-01     10762
## 2 2012-10-02       126
## 3 2012-10-03     11352
## 4 2012-10-04     12116
## 5 2012-10-05     13294
```

```r
    ## plot histogram, using breaks purely for better visuals.
    hist(
            newtotal$sum_steps,
            col = "blue",
            main = "Histogram of the (Total Number of (Steps Taken Each Day))\nPart Deux",
            xlab = "(Total Number of (Steps Taken Each Day)) Part Deux",
            breaks = 20
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
  ## mean
  mean(newtotal$sum_steps)
```

```
## [1] 10765.64
```

```r
  ## median
  median(newtotal$sum_steps)
```

```
## [1] 10762
```

```r
#Are there differences in activity patterns between weekdays and weekends?
    #1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating     whether a given date is a weekday or weekend day..
    ## create new data frame
    n_activity <- newactivity
    weekend <- weekdays(as.Date(n_activity$date)) %in% c("Saturday", "Sunday")
    ## Fill in weekday column
    n_activity$daytype <- "weekday"
    ## replace "weekday" with "weekend" where day == Sat/Sun
    n_activity$daytype[weekend == TRUE] <- "weekend"
    ## convert new character column to factor
    n_activity$daytype <- as.factor(n_activity$daytype)
    weekdays(as.Date(n_activity$date[3]))
```

```
## [1] "Sunday"
```

```r
    #2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval             (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days          (y-axis)
       newinterval <- aggregate(steps ~ interval + daytype, n_activity, mean)
      ## add descriptive variable names
      names(newinterval)[3] <- "mean_steps"

    ## plot time series
    library(lattice)
    xyplot(
            mean_steps ~ interval | daytype,
            newinterval,
            type = "l",
            layout = c(1,2),
            main = "Time Series Plot of the 5-Minute Interval(x-axis)and the Average Number of Steps      Taken,Averaged Across All Weekday Days or Weekend Days(y-axis)",
            xlab = "5-Minute Interval",
            ylab = "Average Number of Steps Taken"
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-1-4.png)<!-- -->
