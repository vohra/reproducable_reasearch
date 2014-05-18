programming assignment1
========================================================





```r
# get input file
steps <- read.csv("activity.csv", stringsAsFactors = F, header = T)
str(steps)  # show structure of raw data
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r

# construct time variable from date and interval
steps$Time <- as.POSIXct(paste(steps$date, sprintf("%i%s%0i", steps$interval%/%100, 
    ":", steps$interval%%100), sep = " "), format = "%Y-%m-%d %H:%M")
steps$ndays <- as.integer(difftime(steps$Time, steps$Time[1], units = "days"))
hour <- function(interval) {
    # function to convert 5 minute intervals to decimal hours
    interval%/%100 + (interval/100 - interval%/%100) * 100/60
}

# generate factor variables for later analysis
steps$dow <- weekdays(steps$Time, abbrev = T)  # gets day of the week (dow)
steps$dow <- factor(steps$dow, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", 
    "Sat", "Sun"))
steps$wkend <- ifelse(steps$dow == "Sat" | steps$dow == "Sun", "weekend", "weekday")  # is day Sat or Sun - weekend?
steps$wkend <- factor(steps$wkend)
str(steps)  # show structure of transformed data
```

```
## 'data.frame':	17568 obs. of  7 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ Time    : POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
##  $ ndays   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ dow     : Factor w/ 7 levels "Mon","Tue","Wed",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ wkend   : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r

# sum no. of steps for each day then take the mean and median on the daily
# sum.  missing data included
sum.steps <- as.integer(with(steps, tapply(steps, ndays, sum, na.rm = F, simplify = F)))
sum.steps <- as.data.frame(sum.steps)
names(sum.steps) <- "totsteps"
ave.steps <- mean(sum.steps$totsteps, na.rm = T)
med.steps <- median(sum.steps$totsteps, na.rm = T)

# plot histogram
require(ggplot2)
p1 <- ggplot(data = sum.steps, aes(x = totsteps)) + geom_histogram(binwidth = 1000, 
    color = "black", fill = "#DD6888") + xlab("Total daily steps") + ylab("Frequency") + 
    theme_bw()
x1 <- theme(axis.title.x = element_text(face = "bold", size = 20), axis.text.x = element_text(face = "bold", 
    vjust = 0.5, size = 16))
y1 <- theme(axis.title.y = element_text(face = "bold", size = 20), axis.text.y = element_text(face = "bold", 
    vjust = 0.5, size = 16))
p1 + x1 + y1
```

![plot of chunk task1](figure/task1.png) 

Figure 1. Histogram of the daily number of steps taken. The dataset included missing values. The most frequent number of steps is in the interval 10000 - 11000 steps.

## What is mean total number of steps taken per day?
The mean and median were calculated by summing the number of steps for each day including missing values -- this gave a missing value for the sum when missing values were present. The average number of steps taken daily was 10766 and 10765 was the median number of steps.  These 2 values are close suggesting the distribution is possibly symmetric and shown to be approximately so from Figure 1.

## What is the average daily activity pattern?

```r
# extract mean number of steps for each interval
avedaily <- with(steps, tapply(steps, interval, mean, na.rm = T))
daily <- data.frame(unique(steps$interval), as.numeric(avedaily))
names(daily) <- c("interval", "average")

with(daily, {
    opar <- par(mar = c(5, 6, 4, 2))
    plot(hour(interval), average, type = "n", lwd = 2, xlab = "Time of day (hours)", 
        ylab = "Mean number of steps", xaxt = "n", las = 2, cex.axis = 1.5, 
        cex.lab = 1.5)
    lines(hour(interval), average, col = "blue")
    axis(side = 1, at = pretty(hour(interval), n = 12), labels = T, cex.axis = 1.5)
    par(opar)
})
```

![plot of chunk task2](figure/task2.png) 

Figure 2. Plot of average daily number of steps against hour of day. Note the peak number of steps occured between 8 and 10 o'clock in the morning.


```r
# extract interval at which maximum steps are taken
with(daily, {
    max.intvl <- match(max(average), average)
    hr <- as.integer(hour(interval[max.intvl]))
    mins <- interval[max.intvl] - hr * 100
})
```


The five minute intervals were transformed to decimal hours which shows the time of day where maximum activity took place.  The daily mean maximum number of steps of 206 occured for interval 

```

Error in eval(expr, envir, enclos) : object 'max.intvl' not found

```

 or at 

```

Error in eval(expr, envir, enclos) : object 'hr' not found

```

:

```

Error in eval(expr, envir, enclos) : object 'mins' not found

```

am.

## Imputing missing values

```r
nmissing <- sum(is.na(steps$steps))
```



```r
# using the mice package to impute missing values
require(mice)
impsteps <- mice(steps[, c(1, 3)], method = "pmm", seed = 1234)
impvec <- complete(impsteps, "r")

# take mean of the 5 iterations
impmean <- as.integer(rowMeans(impvec[, 1:5]))
Steps <- steps$steps
Steps <- ifelse(is.na(steps$steps), impmean, steps$steps)
steps$steps <- Steps
```



```r
# plot histogram
sum.steps <- as.integer(with(steps, tapply(steps, ndays, sum, na.rm = F, simplify = F)))
sum.steps <- as.data.frame(sum.steps)
names(sum.steps) <- "totsteps"
ave.stepsi <- mean(sum.steps$totsteps, na.rm = T)
med.stepsi <- median(sum.steps$totsteps, na.rm = T)

p1 <- ggplot(data = sum.steps, aes(x = totsteps)) + geom_histogram(binwidth = 1000, 
    color = "black", fill = "#DD6888") + xlab("Total daily steps") + ylab("Frequency") + 
    theme_bw()
x1 <- theme(axis.title.x = element_text(face = "bold", size = 20), axis.text.x = element_text(face = "bold", 
    vjust = 0.5, size = 16))
y1 <- theme(axis.title.y = element_text(face = "bold", size = 20), axis.text.y = element_text(face = "bold", 
    vjust = 0.5, size = 16))
p1 + x1 + y1
```

![plot of chunk task3&4a](figure/task3_4a.png) 

Figure 3. Histogram of the daily number of steps taken for the imputed missing values dataset.  The histogram of Figure 2 is similiar to that of Figure 1 (missing data)

The number of missing values for _steps_ was 2304.  The missing values where imputed using the _mice_ package (van Buuren & Groothuis-Oudshoorn 2011) by the method of predictive mean matching. Five imputation vectors were derived and the mean vector of the 5 imputations was used to fill in the missing values. The recalculated mean and median were 10596 and 10439 respectively. These parameter values were about 1,000 steps less than those calculated were missing values were present. In this case, including imputed values lowered the 2 parameters. If we chose to fill in the missing values with the mean values, the mean would have been unaffected but the median is likely to be affected.  The histogram (Figure 3) shows a peak in the number of steps taken daily between 10000 -- 11000 (cf Figure 1).

## Are there differences in activity patterns between weekdays and weekends?

```r
avehrly <- with(steps, tapply(steps, list(wkend, interval), mean, na.rm = T))
weekday <- as.numeric(avehrly["weekday", ])
weekend <- as.numeric(avehrly["weekend", ])
fweek <- rep(c("weekday", "weekend"), each = 288)
hourly <- data.frame(rep(daily$interval, 2), c(weekday, weekend), fweek)
names(hourly) <- c("interval", "steps", "type")
library(lattice)
xyplot(steps ~ hour(interval) | type, data = hourly, layout = c(1, 2), type = "h", 
    xlab = "Hour of day", ylab = "Number of steps", scales = list(alternating = c(3), 
        cex = 1.25))
```

![plot of chunk task5](figure/task5.png) 

Figure 4. Lattice plots of average number of steps taken daily versus hour of day for activity during the weekend (top panel) and weekdays (bottom panel). __Note__ that the graph is drawn as a line histogram (type="h") rather than a line plot (type="l") as, in my opinion, it is easier to compare the two plots.

There is a peak of activity around 9:00 -- 9:30am at the weekends  around 200 steps compared to 150 stepsat an earlier time around 8:30 -- 9:00am during the weekdays. Weekend activity is at a higher level during the day compared to activity during the weekdays. Very little activity occurs from about 10:00pm through to 6:00am the next morning. I guess the subject was getting a well earned sleep!
  

