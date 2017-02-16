

```r
---
title: "Reproducible Research - Week 2 Project"
author: "Shak"
date: "February 16, 2017"
output: html_document
---

###1.Code for reading in the dataset and/or processing the data
```

```
## Error: <text>:10:0: unexpected end of input
## 8: ###1.Code for reading in the dataset and/or processing the data
## 9: 
##   ^
```

```r
library(readr)
activity <- read_csv("C:/Users/Shak/Downloads/activity.csv", col_types = cols(steps = col_number()))
```

###2.Histogram of the total number of steps taken each day

```r
library(dplyr)
temp<-summarise(group_by(activity,date),sum(steps))
colnames(temp)<-c("date","steps_sum")
hist(temp$steps_sum,main="Histogram - Total number of steps taken each day",xlab="Steps/day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

###3.Mean and median number of steps taken each day

The mean is

```r
mean(activity$steps,na.rm=TRUE)
```

```
## [1] 37.3826
```

The median is

```r
median(activity$steps,na.rm=TRUE)
```

```
## [1] 0
```

###4.Time series plot of the average number of steps taken

```r
temp<-summarise(group_by(activity,interval),mean(steps,na.rm=TRUE))
colnames(temp)<-c("interval","steps_mean")
plot(temp$interval,temp$steps_mean,type="l",main="Avergae Daily Activity Pattern",xlab = "5-min interval",ylab="Avergae number of steps taken")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

###5.The 5-minute interval that, on average, contains the maximum number of steps

```r
temp<-summarise(group_by(activity,interval),mean(steps,na.rm=TRUE))
colnames(temp)<-c("interval","steps")
as.numeric(temp[which.max(temp$steps),1])
```

```
## [1] 835
```

###6. Code to describe and show a strategy for imputing missing data
We impute the random hot deck imputation with the interval and weekday as predictors to impute the missing values

```r
library(simputation)
library(VIM)
temp<-activity
temp$weekday<-weekdays(activity$date)
temp1<-impute_rhd(temp,steps~interval+weekday,backend="VIM",pool="univariate")
activity_imputed<-temp1[1:3]
```

The total number of missing values in the dataset is

```r
nrow(activity)-nrow(na.omit(activity))
```

```
## [1] 2304
```

The mean after imputation is

```r
mean(activity_imputed$steps,na.rm=TRUE)
```

```
## [1] 37.49294
```

The median after imputation is

```r
median(activity_imputed$steps,na.rm=TRUE)
```

```
## [1] 0
```

Imputing changes the mean and hence the distribution of the data. While the median has not changed in this case it generally is affected as well.


###7. Histogram of the total number of steps taken each day after missing values are imputed

```r
temp<-summarise(group_by(activity_imputed,date),sum(steps))
colnames(temp)<-c("date","steps_sum")
hist(temp$steps_sum,main="Histogram - Total number of steps taken each day",xlab="Steps/day")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

###8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
temp<-activity
temp$week <- ifelse(weekdays(temp$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
temp<-summarise(group_by(temp,week,interval),mean(steps,na.rm=TRUE))
colnames(temp)<-c("week","interval","steps_mean")
temp$week<-as.factor(temp$week)
library(ggplot2)
p<-ggplot(temp,aes(x=interval,y = steps_mean))+geom_line()+facet_wrap(~week,ncol=1)
p<-p+xlab("Interval")+ylab("Number of steps")
p<-p+ theme(panel.background = element_rect(colour = "black",fill="lightgrey"))
p
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
```

