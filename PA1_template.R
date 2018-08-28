## Loading the libraries 
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(lattice)
library(xtable)
library(magrittr)
library(plotly)
library(lubridate)

## Loading and preprocessing the data
## First, to create the dataframe and store in the variable "RawData".
setwd("D:/RStudio_Projects/RepDatActivity")
RawData <- read.table(unz("repdata_data_activity.zip", "activity.csv"), header = T, sep = ",") 
dim(RawData)
str(RawData)
## As can see, the date is Factor with 61 levels, we need to set the date to take 'YYYY-MM-DD' format spliting in the date per day. 
RawData$date <- as.Date(RawData$date, "%Y-%m-%d")
str(RawData)
## Now the date variable is with correct format!
## We need to transform our data.frame into a format suitable for our analysis.
## First, let us transform the interval variable in new interval2 with hour and minute readable.
## It will help us in the plotting of intervals when we answer - "What is the average daily activity pattern?"
inter <- c(RawData$interval)
inter2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(inter))
inter <- paste0(inter2, inter)
inter <- format(strptime(inter, format="%H%M"), format = "%H:%M")
RawData <- mutate(RawData, interval2 = inter)
RawData$interval2 <- as.factor(RawData$interval2)

# Futhermore information about the code above take look in this link of the Stackoverflow (https://stackoverflow.com/questions/25775375/create-a-24-hour-vector-with-5-minutes-time-interval-in-r)
# Now we going to create the hour and minute variables to calculate Which 5-minute interval,
# on average across all the days in the dataset.

RawData <- mutate(RawData, hour = interval %/% 100,  minute = interval %% 100)
#
head(RawData)
## Second, put the days of week and classificate it in weekdays or weekend,
## this will help us to answer - Are there differences in activity patterns between weekdays and weekends? 
RawData <- mutate(RawData, day_of_week = weekdays(x = date))
RawData$day_of_week <- as.factor(RawData$day_of_week)

## Classifing as "Weekday" or "Weekend"
RawData$typeweek <- as.factor(ifelse(weekdays(RawData$date) %in%
                                         c("Saturday", "Sunday"), "Weekend", "Weekday"))

head(RawData)

## More details about this suggestion we can find the links: (https://datascience-enthusiast.com/R/PA1_template.html)
## (https://rpubs.com/dernapo/118642) and (https://github.com/mGalarnyk/datasciencecoursera/blob/master/5_Reproducible_Research/project1/%F0%9D%99%BF%F0%9D%99%B0%F0%9D%9F%B7_%F0%9D%9A%9D%F0%9D%9A%8E%F0%9D%9A%96%F0%9D%9A%99%F0%9D%9A%95%F0%9D%9A%8A%F0%9D%9A%9D%F0%9D%9A%8E.md) 

## Continuing with assignment let's start to answer the questions. What is mean total number of steps taken per day?
## "For this part of the assignment, you can ignore the missing values in the dataset".
## In my opinion, when we say "ignore the missing values" this mean that we can let down it with other values,
## is not need remove them. Because do not have sense, in the question about "Imputing missing values" below, have 
## to remove the NA's value again!?
# 1 - Calculate the total number of steps taken per day (with NA's values)
# The Javier Saenz's opinion (https://rpubs.com/dernapo/118642) is very interesting and was adopted here.
# "In this script, we've used a special chaining operator, %>%, which was originally introduced in the magrittr
# R package and has now become a key component of dplyr. You can pull up the related documentation with ?chain.
# The benefit of %>% is that it allows us to chain the function calls in a linear fashion. The code to the
# right of %>% operates on the result from the code to the left of %>%." - Swirl Course (Getting and Cleaning Data)

TotalSteps <- RawData %>%
    select(steps, date) %>%
    group_by(date) %>%
    summarise(Total = sum(steps))
## Look the result!
head(TotalSteps, 10)
# We also can use the suggestion of Galarn (https://github.com/mGalarnyk/datasciencecoursera/blob/master/5_Reproducible_Research/project1/%F0%9D%99%BF%F0%9D%99%B0%F0%9D%9F%B7_%F0%9D%9A%9D%F0%9D%9A%8E%F0%9D%9A%96%F0%9D%9A%99%F0%9D%9A%95%F0%9D%9A%8A%F0%9D%9A%9D%F0%9D%9A%8E.md) 
# but before we must to transforme data.frame(RawData) in a data.table to use (.SD,.SDcols and by = ) variables.
# Follow a example:
#'NewDataName <- data.table(RawData)
#'TotalDates <- SplitDates[, c(lapply(.SD, sum, na.rm = F)), .SDcols = c("steps"), by = .(date)]
#'head(TotalDates, 10)
#
# 2 - Histogram of the total number of steps taken each day. 
# Let us use the ggplot with geom_histogram. Notice that we need put the option
# "na.rm = TRUE, otherwise a warning message will appear - "Removed 8 rows containing non-finite values (stat_bin)"
# 
##ggplot(TotalSteps, aes(x = Total)) + geom_histogram(fill = "blue", binwidth = 1000, na.rm = T) + 
    ##labs(title = "Total Number of Steps Taken Each Day (with NA's)", x = "steps", y = "Intervals")
histogram(TotalSteps$Total, breaks = 20, na.rm = T, xlab = "steps (with NA's)", col = "blue",
          border = "red", axes = FALSE, main = "Total Number of Steps - Each Day")
#
# 3 - Calculate and report the mean and median of the total number of steps taken per day
# Let us create 2 variables - TotalMeanSteps (to calculate mean ) and TotalMedianSteps (to calculate median)
# of the dataframe TotalSteps. These variables will be used in our report.
# One time that TotalSteps contain NA's values, we cannot forget it! 
#
TotalMeanSteps <- mean(TotalSteps$Total, na.rm = T)
TotalMedianSteps <- median(TotalSteps$Total, na.rm = T)
##
### What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type="l") 
# of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis)
 
AverageIntervals <- RawData %>%
    select(steps, interval2) %>%
    group_by(interval2) %>%
    summarise(Average = mean(steps, na.rm = T))
    
# Set up API credentials: https://plot.ly/r/getting-started

#s <- plot_ly(AverageIntervals, x = ~interval2, y = ~ Average, type = 'scatter', mode = 'lines')

# If appears this mensage "Warning messages:1: Unknown or uninitialised column: 'minute'." "
# do not worry, because the column minute does not realy exists.

## plot(AverageIntervals, type = "l", col = "blue", lwd = 0.5, main = "Average daily", xlab="5-minute interval", main = "The Average Daily Activity")
    
#plot(test2, type = "l", main = "Average daily", xlab="5-minute interval")
#plot(., type = "l", main = "Average daily", xlab = "5-minute interval", ylab = "Average across all days", main = "The Average Daily Activity")

# The result is...
head(AverageIntervals)

# Ploting the intervals. 
# Set up API credentials: https://plot.ly/r/getting-started

s <- plot_ly(AverageIntervals, x = ~interval2, y = ~ Average, type = 'scatter', mode = 'lines')

s

# Please hover the mouse under x-axis we can see the 5-minute intervals and its average.

# 0 to 55 min). Thus, it is necessary has 12 plots.
##qplot(minute, Average, data = AverageIntervals, geom = c("point"), facets = .~minute, color = Average, 
     ## xlab = "Minutes", ylab = "Average across all days", main = "The Average Daily Activity Pattern")

#g <- ggplot(AverageIntervals, aes(x = interval2, y = Average)) + geom_line(color = "blue", size = 1) + 
#    labs(x = "Minutes", y = "Average across all days", title = "The Average Daily Activity")

#g <- ggplot(data = AverageIntervals) + geom_point(mapping = aes(x = minute, y = Average, colour = minute), size = 2) + facet_wrap(~ minute, nrow = 2) + 
#    labs(x = "Minutes", y = "Average across all days", title = "The Average Daily Activity")

# print(g)

# Which 5-minute interval, on average across all days, contains the maximum number of steps?
# we need to create new setting to show in what time of day occur more activities. 
MaxInterval <- RawData %>%
    select(steps, hour, minute, interval2) %>%
    group_by(interval2, hour, minute) %>%
    summarise(Average = mean(steps, na.rm = T))
    
head(MaxInterval)

#hr <- MaxInterval$hour[1:288]
#minut <- MaxInterval$minute[1:288]

#MaxHour <- hr[which(MaxInterval$Average == max(MaxInterval$Average))]
#MaxMin <- minut[which(AverageIntervals$Average == max(AverageIntervals$Average))]

MaxHour <- MaxInterval$hour[(MaxInterval$Average == max(MaxInterval$Average))]
MaxMin <-  MaxInterval$minute[which(AverageIntervals$Average == max(AverageIntervals$Average))]

cat("The time where maximun number of steps occurs at:", MaxHour,":", MaxMin, "AM")

# Set up API credentials: https://datascience-enthusiast.com/R/PA1_template.html

# Imputing missing values
# How many NA's values has in the RawData data.frame? 


# There are (TotalNAs)
TotalNAs <- sum(is.na(RawData))

# Creating NewRawData 

NewRawData <- RawData
summary(NewRawData)

# Notice that NA's values still over there in our new database (NA's: 2304).

# Changing the NA's values found out in the NewRawDAta to the mean. 
# This solution belongs the Mahajang more details access the link (https://github.com/mahajang/PA1/blob/master/PA1_template.md)
# Thank you for help!

NAsNewDt <- is.na(NewRawData$steps)
ChangeValueNAs <- tapply(NewRawData$steps, NewRawData$interval, mean, na.rm = TRUE)
NewRawData$steps[NAsNewDt] <- ChangeValueNAs[as.character(NewRawData$interval[NAsNewDt])]

# Checking out if has NA's value in NewRawData 
sum(is.na(NewRawData))

head(NewRawData, 12)

# Let's use the same logical has been applied to data with NA's values.
# Make a histogram of the total number of steps taken each day and Calculate
# and report the mean and median total number of steps taken per day.
TotalNewSteps <- NewRawData %>%
    select(steps, date) %>%
    group_by(date) %>%
    summarise(Total = sum(steps))

head(TotalNewSteps, 10)

# Ploting the histogram.

#histogram(TotalNewSteps$Total, breaks = 10, xlab = "steps", col = "red", border = "blue")

histogram(TotalNewSteps$Total, breaks = 20, xlab = "steps (without NA's)", col = "red",
          border = "blue", axes = FALSE, main = "Total Number of Steps - Each Day")


# Calculating the new mean and median.

TotalNewMean <- mean(TotalNewSteps$Total)
TotalNewMedian <- median(TotalNewSteps$Total)


# The mean is same in both data.frame, but the median has a discret increase.
# Do these values differ from the estimates from the first part of the assignment? The only value that a differ between first and second assignment was the Median, the mean was same in both.
# What is the impact of imputing missing data on the estimates of the total daily number of steps? In real value the impact was around 15%

# Are there differences in activity patterns between weekdays and weekends?

# 1 - Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
# We already had done it previously in the begin this code!
# Here is it!
str(NewRawData)

## 2 -  Make a panel plot containing a time series plot 
## (i.e. type = "l"\color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and 
## the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

# Firstly, We can not use the "interval" column values because it add the 5-minute previous to next,
# by causing a accumlated of the values. Look the head(NewRawData) and compare tail(NewRawData)
# below. The column of minute is correct but of the interval contain accumulated 
# values of the minutes. This can generate false positive in the results.    
#head(NewRawData)
#
#tail(NewRawData)

## Hence, let us star creating the variables that will be used to 2 plots where first it will show 
## the average of the steps made during the weekday and weekend.
## Second, we need to show a plotting of the average of the steps each day of week.
#
## 1 - Creating database to plot (weekday vs. weekend)
AvgTypeWeek <- NewRawData %>%
    group_by(interval2, typeweek) %>%
    summarise(Average = mean(steps))

head(AvgTypeWeek)

# Plotting the AvgTypeWeek data.frame. 
# Please, hover the graphs to see the values!

Weekday <- NewRawData %>%
    select(typeweek, interval2, steps)%>%
    filter(typeweek == "Weekday")%>%
    group_by(interval2)%>%
    summarise(Average = mean(steps))

head(Weekday)

Weekend <- NewRawData %>%
    select(typeweek, interval2, steps)%>%
    filter(typeweek == "Weekend")%>%
    group_by(interval2)%>%
    summarise(Average = mean(steps))
    
head(Weekend)

wd <- plot_ly(Weekday, x = ~interval2, y = ~Average) %>%
    add_lines(name = "Weekday")
we <- plot_ly(Weekend, x = ~interval2, y = ~Average) %>%
    add_lines(name = "Weekend")
subplot(wd, we)

# Set up API credentials: https://plot.ly/r/getting-started


#vTypeWeekGraph <-ggplot(AvgTypeWeek, aes(x = interval2, y = Average, color = typeweek))+ geom_line() +
#    labs(title = "Average Daily Steeps", x = "Interval", y = "Total of Steps") +
#    facet_wrap(~typeweek, ncol = 1, nrow = 2)


#TypeWeekGraph <-ggplot(AvgTypeWeek, aes(x = interval2, y = Average, color = typeweek))+ geom_line() +
#    labs(title = "Average Daily Steeps", x = "Interval", y = "Total of Steps") +
#    facet_wrap(~typeweek)

#print(p)

## 2 - Average of the all days

Monday <- NewRawData %>%
    select(day_of_week, interval2, steps) %>%
    filter(day_of_week == "Monday") %>%
    group_by(interval2) %>%
    summarise(avg_Mon = mean(steps))

Tuesday <- NewRawData %>%
    select(day_of_week, interval2, steps)%>%
    filter(day_of_week == "Tuesday")%>%
    group_by(interval2)%>%
    summarise(avg_Tue = mean(steps))

Wednesday <- NewRawData %>%
    select(day_of_week, interval2, steps) %>%
    filter(day_of_week == "Wednesday") %>%
    group_by(interval2) %>%
    summarise(avg_Wed = mean(steps))

Thursday <- NewRawData%>%
    select(day_of_week, interval2, steps) %>%
    filter(day_of_week == "Thursday")%>%
    group_by(interval2)%>%
    summarise(avg_Thu = mean(steps))

Friday <- NewRawData%>%
    select(day_of_week, interval2, steps)%>%
    filter(day_of_week == "Friday")%>%
    group_by(interval2)%>%
    summarise(avg_Fri = mean(steps))

Saturday <- NewRawData%>%
    select(day_of_week, interval2, steps)%>%
    filter(day_of_week == "Saturday")%>%
    group_by(interval2)%>%
    summarise(avg_Sat = mean(steps))

Sunday <- NewRawData%>%
    select(day_of_week, interval2, steps)%>%
    filter(day_of_week == "Sunday")%>%
    group_by(interval2)%>%
    summarise(avg_Sun = mean(steps))

StepsDays <- as.factor(Monday$interval2)

#StepsDays <- NewRawData %>%
#    select(day_of_week, interval2, steps)%>%
#    filter(typeweek)%>%
#    group_by()%>%
#    summarise(Average = mean(steps))


#StepsDays <- NewRawData%>%
#     select(minute, day_of_week, steps)%>%
#     group_by(minute, day_of_week)%>%
#     summarise(Total = sum(steps))

# Consolidated data

BigData <- data.frame(StepsDays, Monday$avg_Mon, Tuesday$avg_Tue, Wednesday$avg_Wed, Thursday$avg_Thu, Friday$avg_Fri, Saturday$avg_Sat, Sunday$avg_Sun)

# Plotting BigData

# Befour I want to advice the learners that the graphs will appear mixed all informations
# You only need to clic on day of the week that even to find top right of graph to select it or not.
# For example, clic on names - Tuesday, Wednesday and Saturday to see what happen!

p <- plot_ly(BigData, x = ~StepsDays, y = ~Monday.avg_Mon, name = "Monday",type = 'scatter', mode = 'lines', line = list(color = 'greay', width = 1))%>%
    add_trace(y = ~Tuesday.avg_Tue, name = 'Tuesday', line = list(color = "greenlight", width = 1))%>%
    add_trace(y = ~Wednesday.avg_Wed, name = 'Wednesday', line = list(color = "red", width = 1))%>%
    add_trace(y = ~Thursday.avg_Thu, name = 'Thursday', line = list(color = "orange", width = 1))%>%
    add_trace(y = ~Friday.avg_Fri, name = 'Friday', line = list(color = "black", width = 1))%>%
    add_trace(y = ~Saturday.avg_Sat, name = 'Saturday', line = list(color = "blown", width = 1))%>%
    add_trace(y = ~Sunday.avg_Sun, name = 'Sunday', line = list(color = "blue", width = 1))%>%
    layout(title = "Average of The Steps Realized Daily", xaxis = list(title = "Intervals"), yaxis = list(title = "Steps"))

print(p)










# Plotting the total of steps in the 5-interval per Day.

h <- qplot(minute, Total, data = StepsDays , group = day_of_week, geom = c("point", "line"), 
      color = day_of_week, xlab = "Minutes", ylab = "Total Steps Per Day", main = "The Daily Activity", 
      facets = .~ day_of_week)
#

print(h)

q <- ggplot(data = test3) + geom_point(mapping = aes(x = minute, y = total, colour = day_of_week), size = 2) + facet_wrap(~ day_of_week, nrow = 1) + 
    labs(x = "Minutes", y = "Total Steps Per Day", title = "The Daily Activity")

print(q)

# The graphs



