### This script is to fulfill Assignment: Course Project 1, for the Coursera course, Reproducible Research by Johns Hopkins University, offered in March, 2016

#### Introduction:

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#### Are there differences in activity patterns between weekdays and weekends?

clear workspace too prevent introducing error and ad libraries needed in this script outputing associated library warnings

``` r
rm(list=ls()) 
library(data.table)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, mday, month, quarter, wday, week, yday, year

``` r
library(lattice)
```

The assignment template was forked from the Github account of Course instructor Roger Peng : <https://github.com/rdpeng/RepData_PeerAssessment1>

The data shown here were processed and analyzed beginning on 2016-03-07 02:04:22 using R version 3.2.3 (2015-12-10)

Set Path to working directory and create data directory if neededd

``` r
setPath <- "/Users/geoid2001/downloads"
setwd(setPath)
if(!file.exists("./data")){dir.create("./data")}
```

Unzip DataSet to /data directory if data is not yet there

``` r
unzip(zipfile="repdata-data-activity.zip",exdir="./data")
```

#### Loading and preprocessing the data

load the data. I load raw dataset with fread from data.table library amd make sure we know we have data completely loaded and identified by applying commands str, str, dim, head, and tail on the loaded data

``` r
activity_data <- fread("data/activity.csv")
str(activity_data)
```

    ## Classes 'data.table' and 'data.frame':   17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
names(activity_data)
```

    ## [1] "steps"    "date"     "interval"

``` r
dim(activity_data)
```

    ## [1] 17568     3

``` r
head(activity_data)
```

    ##    steps       date interval
    ## 1:    NA 2012-10-01        0
    ## 2:    NA 2012-10-01        5
    ## 3:    NA 2012-10-01       10
    ## 4:    NA 2012-10-01       15
    ## 5:    NA 2012-10-01       20
    ## 6:    NA 2012-10-01       25

``` r
tail(activity_data)
```

    ##    steps       date interval
    ## 1:    NA 2012-11-30     2330
    ## 2:    NA 2012-11-30     2335
    ## 3:    NA 2012-11-30     2340
    ## 4:    NA 2012-11-30     2345
    ## 5:    NA 2012-11-30     2350
    ## 6:    NA 2012-11-30     2355

Process/transform the data (if necessary) into a format suitable for your analysis It is useful to breakdown interval into hour and minute using element by element divsion, %/% for hours, and the modulus operator, && for minutes.

``` r
activity_data[,hr:=interval %/% 100]
activity_data[,min:=interval %% 100]
activity_data[,DATETIME:=ymd_hm(paste(date,hr,min))]
```

#### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset. Use lapply with data.table to sum step per day

``` r
DailySteps_withNA<- activity_data[, lapply(.SD, sum), by = date]
DailySteps_withoutNA<- DailySteps_withNA[!is.na(steps),list(steps)]  # remove missing values
```

It turns out that 53 of 61 days have countable data Here we make a histogram of the total number of steps taken each day

``` r
histogram(~steps,data=DailySteps_withoutNA, xlab="Total Daily Steps",breaks=25, type="count",ylab="count (days)",main="Histogram of Total Number of Steps Taken Each Day")
```

![https://github.com/geoid2001/RepData_PeerAssessment1/blob/master/instructions_fig/Histogram_TotDailySteps_woNAs.png](PA1_template_files/figure-markdown_github/unnamed-chunk-7-1.png)<!-- --> The mean total number of steps taken per day rounded to whole number steps is

``` r
round(mean(DailySteps_withoutNA[,steps]),0)
```

    ## [1] 10766

The median total number of steps taken per day rounded to whole number steps is

``` r
round(median(DailySteps_withoutNA[,steps]),0)
```

    ## [1] 10765

#### What is the average daily activity pattern?

Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
MeanStepsByInterval_withoutNA<- activity_data[, list(mean=mean(steps,na.rm=TRUE)), by = interval]


xyplot(mean ~ interval, data=MeanStepsByInterval_withoutNA, type="l", xlim=c(0,2400),scales=list(x=list(at=seq(0,2400,200))),grid=TRUE,ylab="Average Number of steps", xlab="5-minute intervals following midnight\nNote: Data only in first 60 minutes of each 100 minute interval", main="Average number of steps by 5-minutes intervals")
```

![https://github.com/geoid2001/RepData_PeerAssessment1/blob/master/instructions_fig/Avg_Number_Steps_5min_woNAs.png](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)<!-- --> Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? The max number of step occurs at 835 as obtained with the command

``` r
MeanStepsByInterval_withoutNA[order(-rank(mean)),interval][1]
```

    ## [1] 835

#### Imputing missing values

I find the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s) is 2304, for the steps data using the command

``` r
sum(is.na(activity_data[,steps]))
```

    ## [1] 2304

The other date and interval do not contain NA entries

``` r
sum(is.na(activity_data[,date]))
```

    ## [1] 0

``` r
sum(is.na(activity_data[,interval]))
```

    ## [1] 0

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. To me it makes sense to us the mean of the 5-minute interval data

``` r
activity_data[is.na(steps) & interval==MeanStepsByInterval_withoutNA[,interval], steps:=round(MeanStepsByInterval_withoutNA[,mean],0)]
```

    ## Warning in `[.data.table`(activity_data, is.na(steps) & interval ==
    ## MeanStepsByInterval_withoutNA[, : Coerced 'double' RHS to 'integer' to
    ## match the column's type; may have truncated precision. Either change the
    ## target column to 'double' first (by creating a new 'double' vector length
    ## 17568 (nrows of entire table) and assign that; i.e. 'replace' column), or
    ## coerce RHS to 'integer' (e.g. 1L, NA_[real|integer]_, as.*, etc) to make
    ## your intent clear and for speed. Or, set the column type correctly up front
    ## when you create the table and stick to it, please.

``` r
DailySteps_withNA<- activity_data[, lapply(.SD, sum), by = date]
histogram(~steps,data=DailySteps_withNA, xlab="Total Daily Steps",breaks=25, type="count",ylab="count (days)",main="Histogram of Total Number of Steps Taken Each Day\nUpon Replacement of NAs with the mean of the 5-minute interval data")
```

![https://github.com/geoid2001/RepData_PeerAssessment1/blob/master/instructions_fig/Hist_Total_Steps_reNAs_mean_5min.png](PA1_template_files/figure-markdown_github/unnamed-chunk-14-1.png)<!-- --> Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean total number of steps taken upon replacement of NAs per day rounded to whole number steps is

``` r
round(mean(DailySteps_withNA[,steps]),0)
```

    ## [1] 10766

The median total number of steps taken upon replacement of NAs per day rounded to whole number steps is

``` r
round(median(DailySteps_withNA[,steps]),0)
```

    ## [1] 10762

The mean is the same if we replace or don't replace the NAs with the mean 5-minute interval. The median is just 3 steps less out of 10765 if we replace the NAs with the mean 5-minute interval. This very insigficant fraction is likely in smaller than the uncertainties in the data collection itself, so using the the mean 5-minute interval as replacement for NA has no impact on the analysis

#### Are there differences in activity patterns between weekdays and weekends?

``` r
activity_data[,wkdy:=wday(DATETIME,  label = TRUE)]
activity_data[wkdy %in% c("Sat", "Sun"),dayType:="Weekend"]
activity_data[!wkdy %in% c("Sat", "Sun"),dayType:="Weekday"]
MeanStepsByInterval_byDayType<- activity_data[, list(mean=mean(steps,na.rm=TRUE)), by = list(interval,dayType)]
xyplot(mean ~ interval | dayType, data=MeanStepsByInterval_byDayType, type="l", xlim=c(0,2400),scales=list(x=list(at=seq(0,2400,200))),grid=TRUE,layout=c(1,2),ylab="Average Number of steps", xlab="5-minute intervals following midnight\nNote: Data only in first 60 minutes of each 100 minute interval", main="Average number of steps splitting Weekdays and Weekends\nby 5-minutes intervals with NAs Replaced")
```

![RepData_PeerAssessment1/instructions_fig/ave_steps_weekday_weekends.png](https://github.com/geoid2001/RepData_PeerAssessment1/blob/master/instructions_fig/ave_steps_weekday_weekends.png)<!-->

The number of steps is concentrated in the morning for weekdays whereas the number of steps in any given interval is smaller on the weekends and is spread through normal daily hours.
