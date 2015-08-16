---
title: "Reproducible Research Peer Assessment-1"
output: html_document
---

``` {r Calling Required Libraries}
library(knitr)
library(ggplot2)
```

#### Downloading, Loading and Preprocessing the data

##### Data Load Chunk:

``` {r Load Chunk}
data <- read.csv("./data/activity.csv",header = TRUE)
str(data)
```

##### Data Preprocess Chunk:

``` {r Preprocess Chunk}
data$date_new <- as.Date(data$date, "%Y-%m-%d")
```


#### What is mean total number of steps taken per day?
##### 1. Calculate the total number of steps taken per day

``` {r Total Steps Taken Per Day}
daydata <- aggregate(steps ~ date_new, data = data, sum)
names(daydata)[1] = "Date"
names(daydata)[2] = "Total_Steps"
```

##### 2. Histogram of the total number of steps taken each day
``` {r Histogram}
qplot(Total_Steps, data = daydata, xlab = "Total Steps per Day", ylab = "Frequency", main = "Histogram of total number of steps taken per day", binwidth = 5000)
```

##### 3. Calculate and report the mean and median of the total number of steps taken per day

```{r Mean and Median Calculation}
mn_steps <- as.integer(mean(daydata$Total_Steps))
md_steps <- median(daydata$Total_Steps)
```

The Mean of total number of steps taken per day is **`r mn_steps`**  

The Median of total number of steps taken per day is **`r md_steps`**  


#### What is the average daily activity pattern?
##### 1. Time Series Plot: 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` {r Time Series Plot}
timedata <- aggregate(steps ~ interval, data = data, mean)
names(timedata)[1] <- "Interval"
names(timedata)[2] <- "Avg_Steps"

ggplot(timedata, aes(Interval, Avg_Steps)) + geom_line() + xlab("5-minute interval") + ylab("Average Steps Taken Across All Days") 
```

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
reqinterval <- timedata[timedata$Avg_Steps == max(timedata$Avg_Steps),1]
```

5-minute interval, on which average across all the days in the dataset, contains the maximum number of steps: **`r reqinterval`**


#### Imputing missing values

##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r Total Number of Missing Values}
missingrows <- nrow(data[is.na(data)==TRUE,])
```

Total number of missing values in the dataset (i.e. the total number of rows with NAs): **`r missingrows`**

##### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**Strategy for filling missing values:** 
Instead of using the mean/median for that day, or the mean for that 5-minute interval, we will use mean for the combination of 5-minute interval and the day of week. Hypothesis behind using  this combination of 5-minute interval and day of week is that the  number of steps would be different at different day of week for e.g. number of steps on say monday could be different from sunday at specific time interval.

```{r Average Number of Steps at Day of Week and Interval Combination}
# Creating Day of Week Variable
data$dow <- weekdays(data$date_new)

# Creating Combination of Day of Week & 5 minute interval Variable
data$dowinterval <- paste(data$dow, as.character(data$interval), sep = "_")

# Creating Average number of Steps taken on day of week and interval combination
avg_dow_int <- aggregate(steps ~ dowinterval, data = data, mean)
names(avg_dow_int)[1] <- "dowinterval"
names(avg_dow_int)[2] <- "Avg_Steps_Dow_Interval"

# Bringing this Average Steps at Day of Week and Interval Level on Data
data <- merge(data, avg_dow_int, by = "dowinterval", all.x = TRUE)

# Creating same steps column with name "Steps_Corrected" which we will use to replace missing values
data$Steps_Corrected <- data$steps

# Replacing Missing Values by Average Number of Steps at 5 Minute Interval and Day of Week Level
data[is.na(data$Steps_Corrected),max(ncol(data))] <- data[is.na(data$Steps_Corrected),7]
```

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
newdata <- data[,c("Steps_Corrected", "date_new", "interval")]
summary(newdata)
```

##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` {r Total Steps Taken Per Day On New Data}
newdaydata <- aggregate(Steps_Corrected ~ date_new, data = newdata, sum)
names(newdaydata)[1] = "Date"
names(newdaydata)[2] = "Total_Steps"
```

``` {r Histogram for New Data}
qplot(Total_Steps, data = newdaydata, xlab = "Total Steps per Day of New Data", ylab = "Frequency", main = "Histogram of total number of steps taken per day on New Data", binwidth = 5000)
```

```{r Mean and Median Calculation on New Data}
new_mn_steps <- as.integer(mean(newdaydata$Total_Steps))
new_md_steps <- as.integer(median(newdaydata$Total_Steps))
```

The Mean of total number of steps taken per day basis new data is **`r new_mn_steps`**  

The Median of total number of steps taken per day basis new data is **`r new_md_steps`**  

#### Are there differences in activity patterns between weekdays and weekends?

##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` {r Weekday Weekend Creation}
newdata$wkday_wkend_tag <- as.factor(ifelse (weekdays(newdata$date_new) %in% c("Saturday","Sunday"), "Weekend", "Weekday"))
```

##### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` {r Time Series Plot on New Data}
newtimedata <- aggregate(Steps_Corrected ~ wkday_wkend_tag + interval, data = newdata, mean)

ggplot(newtimedata, aes(interval, Steps_Corrected)) + geom_line() + facet_grid(wkday_wkend_tag~.) + xlab("5-minute interval") + ylab("Average Steps Taken") 
```
