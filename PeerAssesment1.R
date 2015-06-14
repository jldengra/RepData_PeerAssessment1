#######################################
# Project 1 for Reproducible Research #
#######################################


# Firstly, it is set as working directory the folder where the source file is.
# The archive with the project data files is downloaded and extracted to a "data"
# folder in this directory. 

setwd("D:/Training/Data Science/JHU Specialization/Reproducible Research/Peer Assesments/Peer Assesment1/RepData_PeerAssessment1")
if (!file.exists("./data")) { dir.create("./data") }
unzip("activity.zip", exdir = "./data")


# Loading and preprocessing the data

# Let's load the data

activity <- read.csv("./data/activity.csv")

str(activity)
# The variables included in this dataset are:
# - steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# - date: The date on which the measurement was taken in YYYY-MM-DD format
# - interval: Identifier for the 5-minute interval in which measurement was taken

# 'data.frame':        17568 obs. of  3 variables:
# $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

summary(activity)
# steps                date          interval     
# Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
# 1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
# Median :  0.00   2012-10-03:  288   Median :1177.5  
# Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
# 3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
# Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
# NA's   :2304     (Other)   :15840  

# Processing/transforming the data into a format suitable for the analysis

# There are 2.304 not available values for steps. We cannot exclude them because they
# can be ignored only for the first part of the assigned, while they will need to be
# imputed later for another part.

# Dates values are represented as a factor. Let's convert this variable type to date
# in order to be able to compute week days and week ends for the last part.

activity$date <- as.Date(activity$date)

str(activity)
# 'data.frame':        17568 obs. of  3 variables:
# $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ date    : Date, format: "2012-10-01" "2012-10-01" "2012-10-01" ...
# $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

# Since the locale in my system is not English, I need to set it to make week days
# appear in English

Sys.setlocale("LC_TIME", "English")
# [1] "English_United States.1252"

# Since there are 24*60 = 1440 minutes in a day, and the maximum value for interval is
# 2355, and the values begin with 0, 5, 10, 15, 20, 25. ... it seems to be denoting 
# hour and minute but not in a string format HHMM but with an integer computed as 
# hour * 100 + minute, so 5 =  0 hours 5 minutes, 2355 = 23 hours 55 minutes, and 
# 200 denotes 2 hours 0 minutes.

# This is a valid representation, an its order is compatible with the chronological
# order of the minutes through the day. Therefor they are not equally separated. 
# For example, the distance between 4h 55min and 5h 00min is 500 - 455 = 45 is
# difference from the distance between 5h 00min and 5h 05min is 505 - 500 = 5, while
# their distance in time is 5 minutes for both. It is better to transform this 
# variable in another without this inconvenient in order to distribute each 5 minutes
# uniformly in plots. For this purpose, I will add a new variable to the dataframe
# computing the number of minutes from 00:00 to the starting minute of a given 5-minute 
# interval.

# %% indicates x mod y and %/% indicates integer division
activity$startingminute <- ((activity$interval %/% 100) * 60) + activity$interval %% 100

# As expected, the starting minute for the intervals comprehend from 0 minutes to 
# 1435 = 1440 - 5.
summary(activity$startingminute)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0   358.8   717.5   717.5  1076.0  1435.0 

# We could consider joining date and interval to compose a full date consisting of
# year, month, day, hour and minute, but for the questions can be adressed working
# with date and time separately, so it is not necessary. 

# What is mean total number of steps taken per day?
# 
# For this part of the assignment, you can ignore the missing values in the dataset.
# 
#  - Calculate the total number of steps taken per day
# 
#  - If you do not understand the difference between a histogram and a barplot, 
#    research the difference between them. Make a histogram of the total number of 
#    steps taken each day
# 
#  - Calculate and report the mean and median of the total number of steps taken per day

# Total number of steps taken per day, ignoring missing values (taking complete cases)

if (!"dplyr" %in% installed.packages()[ , 1]) { install.packages("dplyr") }
library(dplyr)

days <- group_by(activity[complete.cases(activity), ], date)
dailysteps <- summarize(days, steps = sum(steps))

# Histogram of the total number of steps taken each day

hist(dailysteps$steps, col = "lightgreen", breaks = 50, 
     main = "Histogram of daily summarized steps", 
     xlim = c(0, max(dailysteps$steps) + 5000), ylim = c(0, 10),
     xlab = "Total number of steps", ylab = "Frequency (days)")

# Mean and median of the total number of steps taken per day

mean(dailysteps$steps)
# [1] 10766.19
median(dailysteps$steps)
# [1] 10765

# Add RMarkdown here to report these statistics in a summary table

# What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)

# At this point we are still considering to ignore not available values
# Making use the starting minute of intervals instead of the original value make
# the interval points to be distributed uniformly, such that each 5-minute interval
# has the same width. 

intervalstart <- group_by(activity[complete.cases(activity), ], startingminute)
intervalmeans <- summarize(intervalstart, meansteps = mean(steps))
with (intervalmeans, plot(meansteps ~ startingminute, type = "l",
                 main = "Daily activity pattern",
                 xlab = "5-minute interval starting minute", 
                 ylab = "Average number of steps", 
                 cex.lab = .75, cex.axis = .75))


# The same plot attending to the starting hour looks like that:

intervalmeans$startinghour <- intervalmeans$startingminute / 60
with (intervalmeans, plot(meansteps ~ startinghour, type = "l",
                          main = "Daily activity pattern",
                          xlab = "5-minute interval starting hour", 
                          ylab = "Average number of steps",  
                          xaxt = "n",
                          cex.lab = .75, cex.axis = .75))
axis(1, at = 0:24)


# Which 5-minute interval, on average across all the days in the dataset, contains 
# the maximum number of steps?

max( intervalmeans$meansteps )
# [1] 206.1698

# This is the interval starting minute for the maximum number of steps
intervalmeans[ which.max( intervalmeans$meansteps ), ]$startingminute
# [1] 515

# It's the 5-minute interval starting on the minute 515, so (515, 520). 
# Translated to hours and minutes, it's (08:35, 08:40):

515 %/% 60
# [1] 8
515 %% 60
# [1] 35

# Just to check this is correct, let's see that corresponding interval value
# in the aggregated complete cases is 835, since this value should be the result 
# of 8 * 100 + 35.

activity[complete.cases(activity), ][][ which.max( intervalmeans$meansteps ), ]$interval
# [1] 835

activity[complete.cases(activity), ][ which.max( intervalmeans$meansteps ), ]$interval

# So the 5-minute interval, on average across all the days in the dataset, containing 
# the maximum number of steps is the interval 835, with starting minute 515, corresponding
# to the interval (08:35, 08:40) with an average of 206.1698 steps.

# Imputing missing values

# Note that there are a number of days/intervals where there are missing values 
# (coded as NA). The presence of missing days may introduce bias into some 
# calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with NAs)

summary(activity)
#     steps             date               interval      startingminute  
# Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Min.   :   0.0  
# 1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.: 358.8  
# Median :  0.00   Median :2012-10-31   Median :1177.5   Median : 717.5  
# Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Mean   : 717.5  
# 3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.:1076.2  
# Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Max.   :1435.0  
# NA's   :2304  

# Since there are only NA's in the variable sript, we can compute the number
# of missing values as the number of rows having NA in steps: 

nrow(activity[is.na(activity$steps), ])
# [1] 2304

# Further than to get a number of missing values, let's see which days are the
# dates with that missing values.

activityNA <- activity[is.na(activity$steps), ]
unique(activityNA$date)
# [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09" "2012-11-10" "2012-11-14"
# [8] "2012-11-30"

table(activityNA$date)
# 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 2012-11-14 2012-11-30 
# 288        288        288        288        288        288        288        288 

# The previous table shows that all those 8 days have no data for 288 5-minutes intervals, 
# so in other words, no data during 5 * 288 = 1440 minutes, that is no data during the 
# whole day. 

# Add RMarkdown here to report these calculation

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
#    The strategy does not need to be sophisticated. For example, you could use the 
#    mean/median for that day, or the mean for that 5-minute interval, etc.

# I prefer to consider median instead of mean because the median is a robust 
# measure of center not impacted by skewness or extreme values. 

# On the other side, since all the number of steps are N/A for every 5-minute interval
# in the 8 dates with missing values, my approach will be to fill the missing values 
# by computing the median number of steps in the same 5-minute interval than the 
# observation.


# 3. Create a new dataset that is equal to the original dataset but with the missing 
#    data filled in.

# Firstly, we need to compute the median number of steps for every interval.
# We can make use of the previous aggrupation by interval starting minute.
intervalmedians <- summarize(intervalstart, mediansteps = median(steps))

# Imputation

activitycompleted <- merge(activity, intervalmedians, by.x = "startingminute", 
                     by.y = "startingminute", all = FALSE)
activitycompleted <- mutate(activitycompleted, 
                            steps = ifelse (is.na(steps), mediansteps, steps))
# Deletion of the auxiliar variable mediansteps for filling the missing values
activitycompleted <- activitycompleted[ , !(names(activitycompleted) %in% "mediansteps")]

# DELETE THIS FROM THE MARKDOWN, but keep for testing
# Keep this comparison that works with dates (but delete from Markdown)
# activitycompleted1 <- activitycompleted[as.character(activitycompleted$date) == "2012-10-01", ]

# Let's check there are no missing values in the resulting dataset

summary(activitycompleted)
#    startingminute       steps          date               interval     
# Min.   :   0.0   Min.   :  0   Min.   :2012-10-01   Min.   :   0.0  
# 1st Qu.: 358.8   1st Qu.:  0   1st Qu.:2012-10-16   1st Qu.: 588.8  
# Median : 717.5   Median :  0   Median :2012-10-31   Median :1177.5  
# Mean   : 717.5   Mean   : 33   Mean   :2012-10-31   Mean   :1177.5  
# 3rd Qu.:1076.2   3rd Qu.:  8   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
# Max.   :1435.0   Max.   :806   Max.   :2012-11-30   Max.   :2355.0  


# 
# 4. Make a histogram of the total number of steps taken each day and Calculate and 
#    report the mean and median total number of steps taken per day. Do these values
#    differ from the estimates from the first part of the assignment? What is the 
#    impact of imputing missing data on the estimates of the total daily number of steps?


dayscompleted <- group_by(activitycompleted, date)
dailystepscompleted <- summarize(dayscompleted, steps = sum(steps))

# DELETE THIS FROM THE MARKDOWN, but keep for testing
# hist(activity$steps, col = "lightgreen", breaks = 50, 
#      main = "Histogram of steps in 5-minute intervals", 
#      #      xlim = c(0, max(activity$steps) + 5000), ylim = c(0, 10),
#      xlab = "Number of steps", ylab = "Frequency (5 min. intervals)")
# hist(activitycompleted$steps, col = "lightgreen", breaks = 50, 
#      main = "Histogram of steps in 5-minute intervals", 
#      #      xlim = c(0, max(activity$steps) + 5000), ylim = c(0, 10),
#      xlab = "Number of steps", ylab = "Frequency (5 min. intervals)")
# 
# 

hist(dailystepscompleted$steps, col = "orange", breaks = 50, 
     main = "Histogram of daily summarized steps", 
     xlim = c(0, max(dailystepscompleted$steps) + 5000), ylim = c(0, 10),
     xlab = "Total number of steps", ylab = "Frequency (days)")


# Calculate and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment?

# Mean and median of the total number of steps taken per day

mean(dailystepscompleted$steps)
# [1] 9503.869
# Previous ignoring missing values: [1] 10766.19
median(dailystepscompleted$steps)
# [1] 10395
# Previous ignoring missing values: [1] 10765

# Both values differ from the estimates from the first part of the assignment, 
# by decreasing their values from the earlier estimations without imputed values.

# Add RMarkdown here to report these statistics in a summary table

# The impact of imputing missing data is that the 8 days with missing data
# draw a new bar in the histogram with height of 8 comprehending all of them. 
# This bar appears on the left, between 0 and 5000, near from 1000, and we 
# can figure out # the accurate x-value of this new bar by getting the summarized
# number of steps for any of this 8 day with NA's. Let's see for the first date 
# with missing values, which is the date "2012-10-01".

dailystepscompleted[as.character(dailystepscompleted$date) == "2012-10-01", ]$steps
# [1] 1141

# Then the position of the additional bar is 1141 (number of steps) in the X axis.
# This 1141 comes from adding the median steps for each interval, which are the 
# estimates for these days. 

# Are there differences in activity patterns between weekdays and weekends?

# For this part the weekdays() function may be of some help here. Use the dataset 
# with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels - "weekday" and 
#    "weekend" indicating whether a given date is a weekday or weekend day.

activitycompleted$daytype <- as.factor(ifelse (weekdays(activitycompleted$date)
                                 %in% c("Saturday", "Sunday"), "weekend", "weekday"))


# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
#    interval (x-axis) and the average number of steps taken, averaged across all weekday
#    days or weekend days (y-axis). See the README file in the GitHub repository to see 
#    an example of what this plot should look like using simulated data.

# In the sample plot given in the repository, the interval variable is used in the x-axis 
# without being transformated to provide equally distant interval identifiers. 
# Being aware that the provided interval values are not uniformly distant
# across the x axis, I make use of the variable startingminute that I added to beat this lack. 

intervalstartcompleted <- group_by(activitycompleted, startingminute, daytype)
intervalmeanscompleted <- summarize(intervalstartcompleted, meansteps = mean(steps))

if (!"ggplot2" %in% installed.packages()[ , 1]) { install.packages("ggplot2") }
library(ggplot2)
g <- ggplot(intervalmeanscompleted, aes(x = startingminute, meansteps))
g + geom_line( col = "#5AA2FF" ) + facet_wrap ( ~ daytype) +
    theme_bw() + xlab("Interval starting minute") + ylab("Average number of steps") +
    ggtitle("Comparative analysis of activity pattern on day type")    
 