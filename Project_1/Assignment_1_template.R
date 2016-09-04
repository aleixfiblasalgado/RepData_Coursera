## 1st STEP: LOAD DATA
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv", header = TRUE, sep = ",", 
                 colClasses = c("numeric", "character", "numeric"))
## tidy the data
data$date <- as.Date(data$date, format = "%Y - %m - %d")
data$interval <- as.factor(data$interval)

## 2nd STEP: HISTOGRAM OF THE NUMBER OF STEPS EACH DAY
library(ggplot2)
library(data.table)
steps_per_day <- aggregate(steps~date, data, sum)
colnames(steps_per_day) <- c("date", "steps")
ggplot(steps_per_day, aes(x = steps)) + 
        geom_histogram(fill = "red", binwidth = 1000) +
        labs(title = "Histogram of Steps taken each day",
             x = "# of Steps per Day", y = "# of times in a Day (count)") + theme_bw()

## 3rd STEP: Mean and median number of steps taken each day
stepsmean <- mean(steps_per_day$steps, na.rm = TRUE)
stepsmedian <- median(steps_per_day$steps, na.rm = TRUE)

## 4th STEP: Time series plot of the average number of stpes taken
averages <- aggregate(data$steps, by = list(interval = data$interval),
                      FUN = mean, na.rm = TRUE)
## convert to integers
averages$interval <- as.integer(levels(averages$interval)
                                [averages$interval])
colnames(averages) <- c("interval", "av.steps")

ggplot(averages, aes(x = interval, y = av.steps)) +
        geom_line(color = "darkblue", size = 1) +
        labs(title = "Average Daily Activity Pattern", x = "5 sec Interval", 
             y = "# of Steps") + theme_bw()

## 5th STEP: 5-minute interval with the max average of steps 
max_interval <- averages [which.max(averages$steps), ]

## 6th STEP: IMPUTING MISSING DATA
missing <- is.na(data$steps)
table(missing)

## STRATEGY: Replace them with the mean value at the same interval across the
## whole time period. 
na_fill <- function(data, averages) {
        na_index <- which(is.na(data$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                averages[averages$interval == interval,]$av.steps
        }))
        fill_steps <- data$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

data_fill <- data.frame(  
        steps = na_fill(data, averages),  
        date = data$date,  
        interval = data$interval)
str(data_fill)
## Check if there are missing values remaining
sum(is.na(data_fill$steps))

## 7th STEP: HISTOGRAM OF THE TOTAL STEPS AFTER IMPUTING MISSING VALUES
steps_fill_per_day <- aggregate(steps~date, data_fill, sum)
colnames(steps_fill_per_day) <- c("date", "steps")
ggplot(steps_fill_per_day, aes(x = steps)) + 
        geom_histogram(fill = "orange", binwidth = 1000) +
        labs(title = "Histogram of Steps taken each day",
             x = "# of Steps per Day", y = "# of times in a Day (count)") + theme_bw()

steps_mean_fill   <- mean(steps_fill_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(steps_fill_per_day$steps, na.rm=TRUE)

## 8th STEP: Differences in patterns between weekdays and weekends
weekdays_steps <- function(data) {
        weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                                    FUN=mean, na.rm=T)
        # convert to integers
        weekdays_steps$interval <- 
                as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
        colnames(weekdays_steps) <- c("interval", "steps")
        weekdays_steps
}

data_by_weekdays <- function(data) {
        data$weekday <- 
                as.factor(weekdays(data$date)) # weekdays
        weekend_data <- subset(data, weekday %in% c("sabado","domingo"))
        weekday_data <- subset(data, !weekday %in% c("sabado","domingo"))
        
        weekend_steps <- weekdays_steps(weekend_data)
        weekday_steps <- weekdays_steps(weekday_data)
        
        weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
        weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))
        
        data_by_weekdays <- rbind(weekend_steps, weekday_steps)
        data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
        data_by_weekdays
}

data_weekdays <- data_by_weekdays(data_fill)
## Plot comparison between average number of steps (weekdays and weekend)
ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="gold") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()