---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

In this section we will load the data and give a **Date format** to date variable. Also, we will load the libraries that we need in this project.

```r
setwd("C:/Users/user/Desktop/R Exercises/RepData_PeerAssessment1-master")
data<-read.csv(unzip("./activity.zip"))
data$date<-as.Date(data$date, formar= "%Y-m-d%")

library(dplyr)
library(ggplot2)
library(knitr)
```


## What is mean total number of steps taken per day?



```r
### calculate steps by day ###
stepsperday <- data%>%
    group_by(date)%>%
    summarise(Mean=mean(steps, na.rm = T) , Total_Steps = sum(steps), .groups="keep")

### plot ###
with(stepsperday, hist(Total_Steps, main="", xlab="Total Steps Taken each day"))
abline(v=mean(stepsperday$Total_Steps, na.rm = T),col="blue")
abline(v=median(stepsperday$Total_Steps, na.rm = T), col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
### mean a median steps by day ###
summary(stepsperday$Total_Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```
The histogram shows us the distribution of the total number of steps taken each day, the red line shows the median, there is a blue line that represent the mean but those values are so close that is not possible see it, this is shown in the summary where the mean is  equal to *10765* and the median equal to *10766*.

## What is the average daily activity pattern?


```r
### data preparation ###
daily_pattern <- data%>%
    group_by(interval)%>%
    summarise(Mean=mean(steps, na.rm = T), .groups="keep")

### plot ###
with(daily_pattern, plot(interval, Mean, type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
### max number of steps ###
kable(daily_pattern[which.max(daily_pattern$Mean),])
```



| interval|     Mean|
|--------:|--------:|
|      835| 206.1698|
This plot shows us the average daily activity pattern, we can see also that the interval with the higher average steps is *835*.

## Imputing missing values


```r
### look for missing data ### 
missing_data<- data.frame("Steps" = sum(is.na(data$steps)), "date"=sum(is.na(data$date)), 
                          "interval" =sum(is.na(data$interval)), "Total" = sum(is.na(data)))
row.names(missing_data)<-"NAs"
kable(missing_data)                            
```



|    | Steps| date| interval| Total|
|:---|-----:|----:|--------:|-----:|
|NAs |  2304|    0|        0|  2304|

```r
### add column with means by interval ###
new_data<- data%>%
  group_by(interval)%>%
  mutate(Mean_int= mean(steps, na.rm = T))

### function to  imput data based on the column previously created ###
imputing <- function(x){
  for(i in 1:nrow(x)){
    if (is.na(x[i,1])){
      x$steps[i]<- x[[i, 4]]
    }
  }
  return(x)
}

### new imputing data ###
new_data<-imputing(new_data)

### plot comparation ###
stepsperday2 <- new_data%>%
    group_by(date)%>%
    summarise(Mean=mean(steps, na.rm = T) , Total_Steps = sum(steps), .groups="keep")

par(mfrow=c(1,2))
with(stepsperday, hist(Total_Steps, main="Original Data", xlab="Total Steps Taken each day"))
abline(v=mean(stepsperday$Total_Steps, na.rm = T),col="blue")
with(stepsperday2, hist(Total_Steps, main="Imputing Data", xlab="Total Steps Taken each day"))
abline(v=mean(stepsperday2$Total_Steps, na.rm = T),col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
### mean a median steps by day ###
sum_com<-data.frame("Original Data"= c(mean(stepsperday$Total_Steps,na.rm = T),
                                       median(stepsperday$Total_Steps,na.rm = T)),
                    "Imputing Data"= c(mean(stepsperday2$Total_Steps),
                                       median(stepsperday2$Total_Steps)))
row.names(sum_com)<-c("Mean", "Median")
kable(sum_com)
```



|       | Original.Data| Imputing.Data|
|:------|-------------:|-------------:|
|Mean   |      10766.19|      10766.19|
|Median |      10765.00|      10766.19|
Here we first look for the missing data, there was *2304* NAs in the database all of them in the steps variable. I choose imputing the missing data using the average numbers of steps in the interval where the NA was. In the histograms we can see the distribution of these variables as well as the mean pointed by a blue line, both plots seem very similar, in the last table we can see that there is a very small change in the mean between original and imputing data.


## Are there differences in activity patterns between weekdays and weekends?


```r
### add new factor ###
days<-c("sábado", "domingo")
new_data$day_type<- factor((weekdays(new_data$date) %in% days),levels=c(TRUE, FALSE), 
                   labels=c('weekend', 'weekday'))

### plot ###
data_plot<-new_data%>%
  group_by(day_type, interval)%>%
  summarise(Mean = mean(steps))

ggplot(data_plot, aes(interval, Mean, color=day_type))+
  geom_line()+
  labs(y= "Avagere Steps", title = "Activity Patterns")+
  facet_grid(~day_type)+
  theme_bw()+
  theme(legend.position = "none")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

We can see that there are some differences in the activity patterns between weekday and weekend, there is more average steps between the intervals 500 and 1000  in weekday subset, after those intervals the number of steps are less than in weekend.
