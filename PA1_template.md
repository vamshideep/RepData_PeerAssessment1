# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1) We check for data directory, if it doesn't exist we create a data directory
2) We then download the file into the data directory
3) Unzip the file into data directory and read the file
4) We convert date field to Date type


```r
library(knitr)
if(!file.exists("./data")){
  dir.create("./data")
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl,dest="./data/activity.zip")
  unzip("./data/activity.zip",exdir="./data")
  
  }
activity <- read.csv("./data/activity.csv")
activity$date <- as.Date(activity$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
library(reshape2)
activitymelt <- melt(activity,id=c("date","interval"),na.rm=TRUE)
activitydcast <- dcast(activitymelt,date~variable,sum,na.rm=TRUE)
mean(activitydcast$steps)
```

```
## [1] 10766
```

```r
median(activitydcast$steps)
```

```
## [1] 10765
```

```r
str(activitydcast)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

```r
hist(activitydcast$steps)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r
steps_per_day <- aggregate(steps ~ date, activity, sum)

head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
