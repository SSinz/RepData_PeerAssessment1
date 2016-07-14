---
title: "PA1_template"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Set-up - Loading and Processing the Data
#Code for reading in the dataset and/or processing the data
===========================================================
##Set Working Directory
```{r set-up, include=TRUE}
setwd("~/Data/RepData_PeerAssessment1-master/RepData_PeerAssessment1-master")
```
##Unzip Activity.zip File
```{r unzip file, echo=TRUE}
unzip("activity.zip")
```
##Read in File = activity.csv
```{r read in file, echo=TRUE}
activity <- read.csv("activity.csv")
```
##Convert Date for Aesthetic Viewing
```{r date conversion, echo=TRUE}
activity$date <- as.Date(activity$date)
```
#What is the mean total of steps taken per day?
#Histogram of the total number of steps taken each day.
=======================================================
##Histogram showing the total number of steps taken each day
```{r steps a day, echo=TRUE}
library(reshape2)
Steps_within_Day <- melt(activity, id.vars = "date", measure.vars = "steps", na.rm = F)
Sum_Steps_Day <- dcast(Steps_within_Day, date ~ variable, sum)
plot(Sum_Steps_Day$date, Sum_Steps_Day$steps, type = "h", main = "Steps Taken Daily over 2 Month", ylab = "Steps Taken a Day", xlab = "Datum")
```
##Calculating the Median and Mean
```{r calculating Median and Mean, echo=TRUE}
steps_Median <- median(Sum_Steps_Day$steps, na.rm = TRUE)
steps_Mean <- mean(Sum_Steps_Day$steps, na.rm = TRUE)
```
The mean of steps taken is `steps_Mean`. The Median of Steps taken is `steps_Median`.
#What is the average daily acitivty pattern?
#Time Series Plot Identifiying averaged steps taken in 5 minute intervals
=========================================================================
```{r plotting steps in intervals, echo=TRUE}
Interval_Steps <- aggregate(steps ~ interval, activity, mean)
plot(Interval_Steps$interval, Interval_Steps$steps, type = "l", col = "green", main = "5-Min Interval Steps", xlab = "Intervals", ylab = "Steps")
MAXSTEPS <- Interval_Steps[which.max(Interval_Steps$steps), 1]
```
The Maximum number of steps taken are `MAXSTEPS`.
#Imputing Missing Values
##Calculate the Total Number of Missing Values in the Data set
```{r computing sum of missing values}
NAinActivity <- sum(is.na(activity$steps))
```
###There are `NAinActivity` missing values in the activity dataset
##Devise a Strategy for filling in the missing values
The strategy will be as following: the missing values in a givin day will be replaced with the median number of steps taken that day.
##Create a new dataset from the original dataset with the missing values filled in and create a histogram
```{r filling in missing values into original data set}
activity2 <- activity
activity2$steps[is.na(activity2$steps)] <- median(activity$steps, na.rm=TRUE)
activity2tag <- aggregate(steps ~ date, data=activity2, sum, na.rm=TRUE)
hist(activity2tag$steps,
     breaks=25,
     col = "green",
     border = "red",
     main="Daily Steps exclusing NA values",
     xlab="Steps", 
     ylab="Frequency")
```
##Compute the new Mean and Median
```{r computing the new mean and median}
steps_Median2 <- median(activity2tag$steps, na.rm = TRUE)
steps_Mean2 <- mean(activity2tag$steps, na.rm = TRUE)
```
The new mean is `steps_Mean2` compared to `steps_Mean`. 
The new median is `steps_Median2` compared to `steps_Median`.
When excluding the NA values from the dataset, one can see that the mean and median values
have decreased, giving a more accurate description.
#Are there differences in acitivty patterns when comparing the weekday to the weekend?
##Create a new factor vector to indicate if a given day is categorised as a weekday or weekend
```{r new factor vector to identfy weekdays from weekends}
activity2$date <- as.Date(activity2$date)
activity2$dayofweek <- weekdays(activity2$date)
activity2$weekend <- as.factor(ifelse(activity2$dayofweek == "Samstag" | activity2$dayofweek == "Sonntag", "weekend", "weekday"))
weekday_weekend_comparison <- aggregate(steps ~ interval + weekend, activity2, mean)
```
##Plotting graph - weekday steps vs weekend steps
```{r plotting comparison weekday vs weekend}
library(lattice)
xyplot(steps ~ interval | factor(weekend), 
       data=weekday_weekend_comparison, 
       xlab = 1,
       ylab = 2, 
       type="l")
```

