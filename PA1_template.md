Introduction
============

"This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day."

### Assignment

"Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo=TRUE so that someone else will be able to read the code. **This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis**.

#### 1 - Loading and preprocessing the data

``` r
#Loading the libraries:
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
library(htmltools)
library(htmlwidgets)
library(devtools)
library(phantom)
```

**1. Load the data** First, creating the main dataframe named "RawData".

``` r
setwd("D:/RStudio_Projects/RepDatActivity")
RawData <- read.table(unz("repdata_data_activity.zip", "activity.csv"), header = T, sep = ",")
```

``` r
#Verifing the structure of data.frame
dim(RawData)
```

    ## [1] 17568     3

``` r
str(RawData)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

The Date variable must be set to the format "%Y-%m-%d"

``` r
RawData$date <- as.Date(RawData$date, "%Y-%m-%d")
str(RawData)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

**2. Process/transform the data (if necessary) into a format suitable for your analysis**

Let's create variables that have helped us in solving this task.

``` r
inter <- c(RawData$interval)
inter2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(inter))
inter <- paste0(inter2, inter)
inter <- format(strptime(inter, format="%H%M"), format = "%H:%M")
RawData <- mutate(RawData, interval2 = inter)
RawData$interval2 <- as.factor(RawData$interval2)
```

Set up API credentials: [Stackoverflow](https://stackoverflow.com/questions/25775375/create-a-24-hour-vector-with-5-minutes-time-interval-in-r)

We now will create the variables hour, minute, day of the week and typeweek.Embedding them in the RawData data.frame.

``` r
RawData <- mutate(RawData, hour = interval %/% 100,  minute = interval %% 100)
RawData <- mutate(RawData, day_of_week = weekdays(x = date))
RawData$day_of_week <- as.factor(RawData$day_of_week)
## Classifing as "Weekday" or "Weekend"
RawData$typeweek <- as.factor(ifelse(weekdays(RawData$date)
                                     %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

head(RawData)
```

    ##   steps       date interval interval2 hour minute day_of_week typeweek
    ## 1    NA 2012-10-01        0     00:00    0      0      Monday  Weekday
    ## 2    NA 2012-10-01        5     00:05    0      5      Monday  Weekday
    ## 3    NA 2012-10-01       10     00:10    0     10      Monday  Weekday
    ## 4    NA 2012-10-01       15     00:15    0     15      Monday  Weekday
    ## 5    NA 2012-10-01       20     00:20    0     20      Monday  Weekday
    ## 6    NA 2012-10-01       25     00:25    0     25      Monday  Weekday

Thanks the links: [DataScience](https://datascience-enthusiast.com/R/PA1_template.html), [RPubs](https://rpubs.com/dernapo/118642) and [GitHub](https://github.com/mGalarnyk/datasciencecoursera/blob/master/5_Reproducible_Research/project1/%F0%9D%99%BF%F0%9D%99%B0%F0%9D%9F%B7_%F0%9D%9A%9D%F0%9D%9A%8E%F0%9D%9A%96%F0%9D%9A%99%F0%9D%9A%95%F0%9D%9A%8A%F0%9D%9A%9D%F0%9D%9A%8E.md) in development of this code chunk.

#### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

**1.Calculate the total number of steps taken per day**

``` r
TotalSteps <- RawData %>%
    select(steps, date) %>%
    group_by(date) %>%
    summarise(Total = sum(steps))

#Outcome
head(TotalSteps, 10)
```

    ## # A tibble: 10 x 2
    ##    date       Total
    ##    <date>     <int>
    ##  1 2012-10-01    NA
    ##  2 2012-10-02   126
    ##  3 2012-10-03 11352
    ##  4 2012-10-04 12116
    ##  5 2012-10-05 13294
    ##  6 2012-10-06 15420
    ##  7 2012-10-07 11015
    ##  8 2012-10-08    NA
    ##  9 2012-10-09 12811
    ## 10 2012-10-10  9900

**2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day**

``` r
histogram(TotalSteps$Total, breaks = 20, na.rm = T, xlab = "steps (with NA's)", col = "blue",
          border = "red", axes = FALSE, main = "Total Number of Steps - Each Day")
```

![](PA1_template_v_original1_files/figure-markdown_github/histograma1-1.png)

**3.Calculate and report the mean and median of the total number of steps taken per day**

``` r
TotalMeanSteps <- mean(TotalSteps$Total, na.rm = T)
```

``` r
TotalMedianSteps <- median(TotalSteps$Total, na.rm = T)
```

The Mean of the total number of steps was **1.076618910^{4}** and Median was **10765**

#### What is the average daily activity pattern?

**1.Make a time series plot (i.e. type = "l"type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

``` r
AverageIntervals <- RawData %>%
    select(steps, interval2) %>%
    group_by(interval2) %>%
    summarise(Average = mean(steps, na.rm = T))
#
head(AverageIntervals)
```

    ## # A tibble: 6 x 2
    ##   interval2 Average
    ##   <fct>       <dbl>
    ## 1 00:00      1.72  
    ## 2 00:05      0.340 
    ## 3 00:10      0.132 
    ## 4 00:15      0.151 
    ## 5 00:20      0.0755
    ## 6 00:25      2.09

Ploting the intervals. Set up API credentials: <https://plot.ly/r/getting-started>

``` r
s <- plot_ly(AverageIntervals, x = ~interval2, y = ~ Average, type = 'scatter', mode = 'lines')
```

``` r
s
```

![](PA1_template_v_original1_files/figure-markdown_github/unnamed-chunk-26-1.png) *Please, to see the results of each 5-interval hover the mouse over the graph according to the desired interval.*

**2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

``` r
MaxInterval <- RawData %>%
    select(steps, hour, minute, interval2) %>%
    group_by(interval2, hour, minute) %>%
    summarise(Average = mean(steps, na.rm = T))
#
head(MaxInterval)
```

    ## # A tibble: 6 x 4
    ## # Groups:   interval2, hour [6]
    ##   interval2  hour minute Average
    ##   <fct>     <dbl>  <dbl>   <dbl>
    ## 1 00:00         0      0  1.72  
    ## 2 00:05         0      5  0.340 
    ## 3 00:10         0     10  0.132 
    ## 4 00:15         0     15  0.151 
    ## 5 00:20         0     20  0.0755
    ## 6 00:25         0     25  2.09

``` r
#
MaxHour <- MaxInterval$hour[(MaxInterval$Average == max(MaxInterval$Average))]
MaxMin <-  MaxInterval$minute[which(AverageIntervals$Average == max(AverageIntervals$Average))]
```

The period of the day where step activities occur most is at **8**:**35**

#### Imputing missing values

**1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with <span style="color:red">NAs</span>)**

``` r
TotalNAs <- sum(is.na(RawData))
```

The total of NA's is **<span style="color:red">2304</span>**

**2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

**3.Create a new dataset that is equal to the original dataset but with the missing data filled in.**

Creating NewRawData

``` r
NewRawData <- RawData
summary(NewRawData)
```

    ##      steps             date               interval        interval2    
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   00:00  :   61  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   00:05  :   61  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5   00:10  :   61  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   00:15  :   61  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   00:20  :   61  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   00:25  :   61  
    ##  NA's   :2304                                           (Other):17202  
    ##       hour           minute         day_of_week      typeweek    
    ##  Min.   : 0.00   Min.   : 0.00   Friday   :2592   Weekday:12960  
    ##  1st Qu.: 5.75   1st Qu.:13.75   Monday   :2592   Weekend: 4608  
    ##  Median :11.50   Median :27.50   Saturday :2304                  
    ##  Mean   :11.50   Mean   :27.50   Sunday   :2304                  
    ##  3rd Qu.:17.25   3rd Qu.:41.25   Thursday :2592                  
    ##  Max.   :23.00   Max.   :55.00   Tuesday  :2592                  
    ##                                  Wednesday:2592

``` r
NAsNewDt <- is.na(NewRawData$steps)
ChangeValueNAs <- tapply(NewRawData$steps, NewRawData$interval, mean, na.rm = TRUE)
NewRawData$steps[NAsNewDt] <- ChangeValueNAs[as.character(NewRawData$interval[NAsNewDt])]
```

Changing the NA's values found out in the NewRawDAta to the mean. This solution belongs the Mahajang, more details access the link. (<https://github.com/mahajang/PA1/blob/master/PA1_template.md>) Thank you for help!

``` r
head(NewRawData, 12)
```

    ##        steps       date interval interval2 hour minute day_of_week
    ## 1  1.7169811 2012-10-01        0     00:00    0      0      Monday
    ## 2  0.3396226 2012-10-01        5     00:05    0      5      Monday
    ## 3  0.1320755 2012-10-01       10     00:10    0     10      Monday
    ## 4  0.1509434 2012-10-01       15     00:15    0     15      Monday
    ## 5  0.0754717 2012-10-01       20     00:20    0     20      Monday
    ## 6  2.0943396 2012-10-01       25     00:25    0     25      Monday
    ## 7  0.5283019 2012-10-01       30     00:30    0     30      Monday
    ## 8  0.8679245 2012-10-01       35     00:35    0     35      Monday
    ## 9  0.0000000 2012-10-01       40     00:40    0     40      Monday
    ## 10 1.4716981 2012-10-01       45     00:45    0     45      Monday
    ## 11 0.3018868 2012-10-01       50     00:50    0     50      Monday
    ## 12 0.1320755 2012-10-01       55     00:55    0     55      Monday
    ##    typeweek
    ## 1   Weekday
    ## 2   Weekday
    ## 3   Weekday
    ## 4   Weekday
    ## 5   Weekday
    ## 6   Weekday
    ## 7   Weekday
    ## 8   Weekday
    ## 9   Weekday
    ## 10  Weekday
    ## 11  Weekday
    ## 12  Weekday

``` r
TotalNewSteps <- NewRawData %>%
    select(steps, date) %>%
    group_by(date) %>%
    summarise(Total = sum(steps))

head(TotalNewSteps, 10)
```

    ## # A tibble: 10 x 2
    ##    date        Total
    ##    <date>      <dbl>
    ##  1 2012-10-01 10766.
    ##  2 2012-10-02   126 
    ##  3 2012-10-03 11352 
    ##  4 2012-10-04 12116 
    ##  5 2012-10-05 13294 
    ##  6 2012-10-06 15420 
    ##  7 2012-10-07 11015 
    ##  8 2012-10-08 10766.
    ##  9 2012-10-09 12811 
    ## 10 2012-10-10  9900

The new histogram

``` r
histogram(TotalNewSteps$Total, breaks = 20, xlab = "steps (without NA's)", col = "red",
          border = "blue", axes = FALSE, main = "Total Number of Steps - Each Day")
```

![](PA1_template_v_original1_files/figure-markdown_github/unnamed-chunk-31-1.png) The new Median and Median.

``` r
TotalNewMean <- mean(TotalNewSteps$Total)
TotalNewMedian <- median(TotalNewSteps$Total)
```

Comparing mensuarements
-----------------------

| Mensure | With NA's       | Without NA's    |
|:--------|:----------------|:----------------|
| Mean    | 1.076618910^{4} | 1.076618910^{4} |
| Median  | 10765           | 1.076618910^{4} |

------------------------------------------------------------------------

*We notice that the Mean there was no change, only in the Median there was a slight increase.*

**4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

``` r
## 1 - Creating database to plot (weekday vs. weekend)
AvgTypeWeek <- NewRawData %>%
    group_by(interval2, typeweek) %>%
    summarise(Average = mean(steps))

head(AvgTypeWeek)
```

    ## # A tibble: 6 x 3
    ## # Groups:   interval2 [3]
    ##   interval2 typeweek Average
    ##   <fct>     <fct>      <dbl>
    ## 1 00:00     Weekday   2.25  
    ## 2 00:00     Weekend   0.215 
    ## 3 00:05     Weekday   0.445 
    ## 4 00:05     Weekend   0.0425
    ## 5 00:10     Weekday   0.173 
    ## 6 00:10     Weekend   0.0165

``` r
# Plotting the AvgTypeWeek data.frame. 
# Please, hover the graphs to see the values!

Weekday <- NewRawData %>%
    select(typeweek, interval2, steps)%>%
    filter(typeweek == "Weekday")%>%
    group_by(interval2)%>%
    summarise(Average = mean(steps))

head(Weekday)
```

    ## # A tibble: 6 x 2
    ##   interval2 Average
    ##   <fct>       <dbl>
    ## 1 00:00      2.25  
    ## 2 00:05      0.445 
    ## 3 00:10      0.173 
    ## 4 00:15      0.198 
    ## 5 00:20      0.0990
    ## 6 00:25      1.59

``` r
Weekend <- NewRawData %>%
    select(typeweek, interval2, steps)%>%
    filter(typeweek == "Weekend")%>%
    group_by(interval2)%>%
    summarise(Average = mean(steps))
    
head(Weekend)
```

    ## # A tibble: 6 x 2
    ##   interval2 Average
    ##   <fct>       <dbl>
    ## 1 00:00     0.215  
    ## 2 00:05     0.0425 
    ## 3 00:10     0.0165 
    ## 4 00:15     0.0189 
    ## 5 00:20     0.00943
    ## 6 00:25     3.51

``` r
wd <- plot_ly(Weekday, x = ~interval2, y = ~Average) %>%
    add_lines(name = "Weekday")
we <- plot_ly(Weekend, x = ~interval2, y = ~Average) %>%
    add_lines(name = "Weekend")
subplot(wd, we)
```

![](PA1_template_v_original1_files/figure-markdown_github/unnamed-chunk-33-1.png)

A challenge!!! Creating a graph to show the measures of steps every day of the week.

``` r
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
```

Consolidated data
=================

``` r
BigData <- data.frame(StepsDays, Monday$avg_Mon, Tuesday$avg_Tue, Wednesday$avg_Wed, Thursday$avg_Thu, Friday$avg_Fri, Saturday$avg_Sat, Sunday$avg_Sun)
```

#### Plotting BigData

**<span style="color:blue&gt;First of all,  I want to let learners know that at first the graph will appear with all days of the week together, but in the upper right of the graph there is a menu with the weekdays names, you just need to click on the day to disable or enable it again.&lt;/span&gt;**  
You can also disable every day and select the one that you are interest in.
Thank you!&lt;/span&gt;

```r
p &lt;- plot_ly(BigData, x = ~StepsDays, y = ~Monday.avg_Mon, name = " monday",type="scatter" ,="" mode="lines" ,="" line="list(color" =="" 'greay',="" width="1))%">First of all, I want to let learners know that at first the graph will appear with all days of the week together, but in the upper right of the graph there is a menu with the weekdays names, you just need to click on the day to disable or enable it again.</span>**
You can also disable every day and select the one that you are interest in. Thank you!</span>

``` r
p <- plot_ly(BigData, x = ~StepsDays, y = ~Monday.avg_Mon, name = "Monday",type = 'scatter', mode = 'lines', line = list(color = 'greay', width = 1))%>%
    add_trace(y = ~Tuesday.avg_Tue, name = 'Tuesday', line = list(color = "greenlight", width = 1))%>%
    add_trace(y = ~Wednesday.avg_Wed, name = 'Wednesday', line = list(color = "red", width = 1))%>%
    add_trace(y = ~Thursday.avg_Thu, name = 'Thursday', line = list(color = "orange", width = 1))%>%
    add_trace(y = ~Friday.avg_Fri, name = 'Friday', line = list(color = "black", width = 1))%>%
    add_trace(y = ~Saturday.avg_Sat, name = 'Saturday', line = list(color = "blown", width = 1))%>%
    add_trace(y = ~Sunday.avg_Sun, name = 'Sunday', line = list(color = "blue", width = 1))%>%
    layout(title = "Average of The Steps Performed Daily", xaxis = list(title = "Intervals"), yaxis = list(title = "Steps"))

p
```

![](PA1_template_v_original1_files/figure-markdown_github/FinalPloting-1.png)

*I appreciate all the links the codes that I found on the internet that helped me complete this job.Thanks a lot! * - <https://plot.ly/r/line-charts/> - <https://rpubs.com/dernapo/118642> - <https://datascience-enthusiast.com/R/PA1_template.html> - <https://github.com/mahajang/PA1/blob/master/PA1_template.md>

God bless you!
