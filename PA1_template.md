# Reproducible Research Assignment 1
Ruth  
January 19, 2018  



## Reading the Data



```r
getwd()
```

```
## [1] "C:/Users/Prisuser.PRIS-LT000120/Documents/Coursera/R_Programming"
```

```r
setwd(dir = "/Users/Prisuser.PRIS-LT000120/Documents/Coursera")
library(tidyr)
```

```
## Warning: package 'tidyr' was built under R version 3.3.3
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.3.3
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
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
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
data=read.csv("activity.csv", header=TRUE)
```
## Exploring Data on the Total Number of Steps Taken Daily

1) Total number of steps taken per day is as shown below:


```r
df<-aggregate(data$steps, by=list(date=data$date),FUN=sum, na.rm=TRUE)
names(df)<-c("Date(yyyy-mm-dd)","Number_of_Steps_Taken")
df
```

```
##    Date(yyyy-mm-dd) Number_of_Steps_Taken
## 1        2012-10-01                     0
## 2        2012-10-02                   126
## 3        2012-10-03                 11352
## 4        2012-10-04                 12116
## 5        2012-10-05                 13294
## 6        2012-10-06                 15420
## 7        2012-10-07                 11015
## 8        2012-10-08                     0
## 9        2012-10-09                 12811
## 10       2012-10-10                  9900
## 11       2012-10-11                 10304
## 12       2012-10-12                 17382
## 13       2012-10-13                 12426
## 14       2012-10-14                 15098
## 15       2012-10-15                 10139
## 16       2012-10-16                 15084
## 17       2012-10-17                 13452
## 18       2012-10-18                 10056
## 19       2012-10-19                 11829
## 20       2012-10-20                 10395
## 21       2012-10-21                  8821
## 22       2012-10-22                 13460
## 23       2012-10-23                  8918
## 24       2012-10-24                  8355
## 25       2012-10-25                  2492
## 26       2012-10-26                  6778
## 27       2012-10-27                 10119
## 28       2012-10-28                 11458
## 29       2012-10-29                  5018
## 30       2012-10-30                  9819
## 31       2012-10-31                 15414
## 32       2012-11-01                     0
## 33       2012-11-02                 10600
## 34       2012-11-03                 10571
## 35       2012-11-04                     0
## 36       2012-11-05                 10439
## 37       2012-11-06                  8334
## 38       2012-11-07                 12883
## 39       2012-11-08                  3219
## 40       2012-11-09                     0
## 41       2012-11-10                     0
## 42       2012-11-11                 12608
## 43       2012-11-12                 10765
## 44       2012-11-13                  7336
## 45       2012-11-14                     0
## 46       2012-11-15                    41
## 47       2012-11-16                  5441
## 48       2012-11-17                 14339
## 49       2012-11-18                 15110
## 50       2012-11-19                  8841
## 51       2012-11-20                  4472
## 52       2012-11-21                 12787
## 53       2012-11-22                 20427
## 54       2012-11-23                 21194
## 55       2012-11-24                 14478
## 56       2012-11-25                 11834
## 57       2012-11-26                 11162
## 58       2012-11-27                 13646
## 59       2012-11-28                 10183
## 60       2012-11-29                  7047
## 61       2012-11-30                     0
```

2) Histogram of the Total Number of Steps Taken Daily


```r
g<-ggplot(data=df,aes(Number_of_Steps_Taken))+
  geom_histogram(col="black",aes(fill=..count..),binwidth = 2500)+
  scale_fill_gradient("Count",low="red",high="green")+
  labs(title="Histogram for Total Number of Steps Taken Daily")

print(g)
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

3a) Mean number of steps taken daily


```r
mean(df$Number_of_Steps_Taken)
```

```
## [1] 9354.23
```

3b) Median number of steps taken daily


```r
median(df$Number_of_Steps_Taken)
```

```
## [1] 10395
```
## Looking at Average Daily Activity Pattern


```r
ds<-aggregate(data$steps,by=list(interval=data$interval),FUN=mean, na.rm=TRUE)

plot(ds[,1],ds[,2],type="l",main="Time Series Plot of 5-Minute Intervals, 1 Oct 2012 to 30 Nov 2012",xlab="5-minute Intervals (from 0 to 2355 in a day)",ylab="Average Number of Steps Taken across 61 days" )
```

![](PA1_template_files/figure-html/time_Series-1.png)<!-- -->


Finding interval with the Highest average number of steps taken:

```r
which.max(ds[,2])
```

```
## [1] 104
```

```r
ds[104,1]
```

```
## [1] 835
```

```r
ds[104,2]
```

```
## [1] 206.1698
```
Interval with the highest average number of steps of 206.1698 is:  **0835-0840 hrs**

## Imputing Missing Values
1)Total number of rows with NAs is:

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

2) Imputing missing values with the **mean of the 5-minute interval across all the 61 days** and
3) Creating a newdataset "newdata" equal to the original dataset "data" but with the missing data filled in

```r
impute<-function(x,fun){
  missing<-is.na(x)
  replace(x,missing,fun(x[!missing]))
}
newdata<-ddply(data,~interval,transform,steps=impute(steps,mean))
newdata<-arrange(newdata,date)
```
4) Histogram of the Total Number of Steps Taken Daily for the New Dataset (with imputed missing values)


```r
dfnew<-aggregate(newdata$steps, by=list(date=newdata$date),FUN=sum, na.rm=TRUE)

g<-ggplot(data=dfnew,aes(x))+
  geom_histogram(col="black",aes(fill=..count..),binwidth = 2500)+
  scale_fill_gradient("Count",low="red",high="green")+
  labs(title="Histogram for Total Number of Steps Taken Daily for New Dataset",x="Number of Steps Taken")

print(g)
```

![](PA1_template_files/figure-html/histogram_New-1.png)<!-- -->


4a) Mean number of steps taken daily for new dataset


```r
mean(dfnew$x)
```

```
## [1] 10766.19
```

4b) Median number of steps taken daily for new dataset


```r
median(dfnew$x)
```

```
## [1] 10766.19
```
The mean and median estimates for the new dataset differ from the initial estimates.

By imputing the missing values for each interval with the average across the 61 days, the mean is now moved close/equal to the median for the new dataset.

## Comparing Activity Patterns: Weekdays vs Weekends

```r
newdata1<-newdata
newdata1$date<-as.Date(newdata1$date)
newdata1$week<-ifelse(weekdays(newdata1$date) %in% c("Saturday","Sunday"),"weekend","weekday")

wkday<- filter(newdata1, week=="weekday")
wkend<- filter(newdata1, week=="weekend")


dsend<-aggregate(wkend$steps,by=list(interval=wkend$interval),FUN=mean, na.rm=TRUE)
dsday<-aggregate(wkday$steps,by=list(interval=wkday$interval),FUN=mean, na.rm=TRUE)

dsday$week=rep("weekday",288)
dsend$week=rep("weekend",288)

combined<-rbind(dsday,dsend)

g<-ggplot(combined,aes(interval,x))+
  geom_line()+
  facet_grid(week~.)+
  labs(title="Panel Plot: Weekdays vs Weekends, 1 Oct 2012 to 11 Nov 2012")+
  labs(y="Average Number of Steps")+
  labs(x="5-Minute Intervals")
print(g)
```

![](PA1_template_files/figure-html/panel_plot-1.png)<!-- -->

**Alternatives** making a panel plot using **base plotting system**:

```r
par(mfrow=c(2,1),mar=c(4,4,2,1))

plot(dsend[,1],dsend[,2],type="l",main="Time Series Plot of 5-Minute Intervals (Weekends)",xlab=" ",ylab=" " )
plot(dsday[,1],dsday[,2],type="l",main="Time Series Plot of 5-Minute Intervals (Weekdays)",xlab="5-minute Intervals (from 0 to 2355 on Weekdays)",ylab=" " )
```

![](PA1_template_files/figure-html/base-1.png)<!-- -->

and **lattice plotting system**:



```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.3.3
```

```r
xyplot(x~interval|week,data=combined,type="l", ylab = "Average Number of Steps",layout=c(1,2))
```

![](PA1_template_files/figure-html/lattice-1.png)<!-- -->

From the panel time series plots, there are differences in the activity patterns between weekdays and weekends. The weekends see more intervals with high number of steps (from late mornings onwards) than on weekdays. For weekdays, only the mornings see high number of steps, with the rest of the day depicting a sedentary lifestyle.
