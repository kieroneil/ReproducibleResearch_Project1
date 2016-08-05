rm(list=ls())
# setwd("~/Analytics/JH Courses/04_Reproducable Research/Project1")
setwd("~/Analytics Course/04_ReproducibleResearch/Project1")
# setwd("~/Analytics/JH Courses/04_Reproducable Research/Project1/Project1b")

library(dplyr)
library(readr)
library(ggplot2)
library(rpart)
library(lubridate)
#library(Amelia)
#library(Hmisc)

###########################################################
# Assignment:
# Commit containing full submission
# 
# 1. Code for reading in the dataset and/or processing the data
# 2. Histogram of the total number of steps taken each day
# 3. Mean and median number of steps taken each day
# 4. Time series plot of the average number of steps taken
# 5. The 5-minute interval that, on average, contains the maximum 
#    number of steps
# 6. Code to describe and show a strategy for imputing missing data
# 7. Histogram of the total number of steps taken each day after 
#    missing values are imputed
# 8. Panel plot comparing the average number of steps taken per 
#    5-minute interval across weekdays and weekends
# 9. All of the R code needed to reproduce the results (numbers, 
#    plots, etc.) in the report
#
##################################################################
# 1. Code for reading in the dataset and/or processing the data
##################################################################
# Get raw data
activity_raw <- read_csv("activity.csv")

##################################################################
# My initial investigation of this data set revealed that there are days at the beginning 
# and end of the dataset that should not be included in the data analysis.
range(activity_raw$date)
max(range(activity_raw$date)) - min(range(activity_raw$date))
# Each day has 288 records.  This query shows that there are several days will all NA values.
# Since one of the days is the very first date and one is the very last date, they will be 
# trimmed from the final data set.
activity_raw %>%
    group_by(date) %>%
    filter(is.na(steps)) %>%
    summarise(num_of_na = length(steps))

# The second issue is with the 2nd day, 2012-10-02.  It contains all zeros except for 
# two entries. 
b <- activity_raw %>%
    filter(date == ymd("2012-10-02"))
    
c <- b %>%
    filter(steps == 0)

dim(b)
dim(c)
# We can see that 286 of the 288 records for 2012-10-02 has a value of zero, which may
# indicate the the subject was not wearing the device for most of the day.

# For these reasons 2012-10-01, 2012-10-02 & 2012-11-30 will be ommitted from the 
# analysis test set
ar_trimmed <- activity_raw %>%
    filter(date >= ymd("2012-10-03"), date < ymd("2012-11-30"))

# Part of the assignment is to impute NA values.  Let's make sure that we still
# have a few NAs to work with.
ar_trimmed %>%
    filter(is.na(steps))
# There are still 1,728  NAs that can be imputed 

# This shows the days that still have NAs.  Note that NAs occur during an entire day,
# There are no days will just a few NA records
ar_trimmed %>%
    group_by(date) %>%
    filter(is.na(steps)) %>%
    summarise(num_of_na = length(steps))

# Remove unneeded variables
rm(b, c)

######################################################################################
# Dataset with With NAs removed
# This dataset removes all records with NAs, not just the first and last days
######################################################################################
ar_na_omit <- na.omit(ar_trimmed)

# Summarise total number of steps taken per day
total_steps_na_omit <- ar_na_omit %>%
    select(date, interval, steps) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))
#
# 2. Histogram of the total number of steps taken each day
# NAs omitted
ggplot(total_steps_na_omit, aes(total_steps)) + geom_histogram()

#
# 3. Mean and median number of steps taken each day
#
# Calculate and report the mean and median of the total number of steps taken 
# per day with NA records omitted
mean(total_steps_na_omit$total_steps)
median(total_steps_na_omit$total_steps)

# Plot of data, mean, and median with NA records omitted
# ggplot(total_steps_na_omit, aes(date, total_steps)) + geom_line()+
#     geom_hline(aes(yintercept=mean_steps_na_omit), colour = "red", linetype = 2) +
#     annotate("text", x=ymd("2012-10-17"), y=20000, 
#              label = paste("Mean with NAs omitted = ", mean_steps_na_omit), 
#              colour = "red") +
#     geom_hline(aes(yintercept=median_steps_na_omit), colour = "blue", linetype = 3) +
#     annotate("text", x=ymd("2012-10-16"), y=18000,
#              label = paste("Median with NAs omitted = ", median_steps_na_omit), 
#              colour = "blue")


ggplot(total_steps_na_omit, aes(total_steps)) + geom_histogram() + stat_bin(bins = 15)

########################################################################
# 4. Time series plot of the average number of steps taken
# 5. The 5-minute interval that, on average, contains the maximum 
#    number of steps
#
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis).
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
# NOTE:  This part omits all NAs
average_per_interval <- ar_na_omit %>%
    select(date, interval, steps) %>%
    group_by(interval) %>%
    summarise(average_per_5_minutes = mean(steps))

# Determine the interval with the maximum average steps
max_interval <- average_per_interval %>%
    filter(average_per_5_minutes == max(average_per_5_minutes))

max_interval <- max_interval[1]
# max_interval

ggplot(average_per_interval, aes(interval, average_per_5_minutes)) + geom_line() +
    geom_vline(aes(xintercept = max_interval), colour = "red") +
    annotate("text", x=1500, y = 175, label = "Interval with highest average steps",
             colour = "red")

######################################################################################
# Create dataset with NAs imputed by a formula
######################################################################################
# 6. Code to describe and show a strategy for imputing missing data
#
# Strategy:
# To impute the values of the NAs in 'steps' I decided to use a the rpart
# library which uses recursive partitioning.  I like it because it can impute the 
# value based on any number of parameters even though this case only derives
# it based on the interval that it occurs in.
#####################################################################################

# Start with the trimmed raw data set
ar_imputed <- ar_trimmed 

# Impute the missing 'steps' using rpart
library(rpart)

# Create prediction formula
predicted_steps <- rpart(steps ~ interval, data = ar_imputed[!is.na(ar_imputed$steps),], 
                         method = "anova")

# Apply prediction to ar_imputed NA records
ar_imputed$steps[is.na(ar_imputed$steps)] <- predict(predicted_steps, 
                                                     ar_imputed[is.na(ar_imputed$steps),])

# Summarise total number of steps taken per day
total_steps_imputed <- ar_imputed %>%
    select(date, interval, steps) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

# Calculate mean steps per day
mean(total_steps_imputed$total_steps)
median(total_steps_imputed$total_steps)

# 7. Histogram of the total number of steps taken each day after 
#    missing values are imputed
ggplot(total_steps_imputed, aes(total_steps)) + geom_histogram() +
    stat_bin(bins=15)

# 8. Panel plot comparing the average number of steps taken per 
#    5-minute interval across weekdays and weekends

# Create the is_weekday column in ar_imputed
is_weekday <- !(weekdays(as.Date(ar_imputed$date)) %in% c('Saturday','Sunday'))
ar_imputed_days <- cbind(is_weekday, ar_imputed)
ar_imputed_days$is_weekday <- is_weekday

total_steps_days <- ar_imputed_days %>%
    group_by(is_weekday, interval) %>%
    summarise(average_steps = mean(steps))
    
p <- ggplot(total_steps_days, aes(interval, average_steps)) +geom_line()
p + facet_wrap(~is_weekday, ncol=2, labeller = "label_both")







# Plot of data, mean, and median with imputed values for NA records
ggplot(total_steps_imputed, aes(date, total_steps)) + geom_line()+
    geom_hline(aes(yintercept=mean_steps_imputed), colour = "red", linetype = 2) +
    annotate("text", x=ymd("2012-10-17"), y=20000, 
             label = paste("Mean with imputed NAs = ", mean_steps_imputed), 
             colour = "red") +
    geom_hline(aes(yintercept=median_steps_imputed), colour = "blue", linetype = 3) +
    annotate("text", x=ymd("2012-10-18"), y=18000,
             label = paste("Median with imputed NAs = ", median_steps_imputed), 
             colour = "blue")

###################################################################
# What is the average daily activity pattern?
#
########################################################################
#
# Note that there are a number of days/intervals where there are missing 
# values (coded as NA). The presence of missing days may introduce bias 
# into some calculations or summaries of the data.
#
# Calculate and report the total number of missing values in the 
# dataset (i.e. the total number of rows with NAs)
#
# NOTE: Remember that I have trimmed off two days in the beginning and one day at
# the end of the data set so my numbers may be different than yours.
NA_records <- ar_trimmed %>%
    select(date, interval, steps) %>%
    mutate(missing_records = as.integer(is.na(steps))) %>%
    group_by(missing_records) %>%
    summarise(record_count = length(missing_records))

print(paste("The number of NA records =", NA_records[2,2]))
print(paste("out of a total of", NA_records[2,2]+NA_records[1,2]))
print(paste("This means that ", round(NA_records[2,2]/(NA_records[2,2]+NA_records[1,2]), digits = 3), "records are NA."))

ggplot(missing, aes(factor(missing_records), record_count, 
       ylim=range(0,20000)), 
       title = "Record count for non-missing & missing steps") + 
    geom_bar(position = "stack", stat = "identity", width = 0.5) + 
    labs(x = "0 = Not Missing; 1 = Missing", y = "Count") +
    scale_fill_hue(c=40) +
    geom_text(stat = "identity", 
              label = c("Records with data", "Records without data"), 
              y = c(13000, 3000), colour = c("white", "black"))


# In this plot I offset the mean and median lines slightly because they
# have the exact same value.
# ggplot(an1, aes(total_steps)) + geom_histogram(binwidth=1000, 
#                                                fill = "burlywood2",
#                                                colour = "gray78") +
#         geom_vline(aes(xintercept=average_steps_imputed+20), colour = "red") +
#     annotate("text", x=15000, y=13, 
#              label = paste("Mean =",average_steps_imputed) , colour = "red", vjust = "inward") +
#     geom_vline(aes(xintercept=median_steps_imputed-20), colour = "blue") +
#     annotate("text", x=15000, y=10, 
#              label = paste("Median = ",median_steps_imputed), 
#              colour = "blue", vjust = "inward") 

##################################################################################
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total 
# daily number of steps?
#
# Create line plots that shows the original data and the new data with 
# imputed values.

mean_before_impute <- mean(total_steps_na_omit$total_steps)
median_before_impute <- median(total_steps_na_omit$total_steps)

mean_after_impute <- mean(total_steps_imputed$total_steps)
median_after_impute <- median(total_steps_imputed$total_steps)

mean_before_after_diff <- round(abs(mean_before_impute)) - round(abs(mean_after_impute))
median_before_after_diff <- round(abs(median_before_impute)) - round(abs(median_after_impute))

ggplot(total_steps_na_omit, aes(date, total_steps, color = "black")) + geom_line() +
    geom_line(data = total_steps_imputed, aes(date, total_steps, color = "red")) +
    theme(legend.position="none") +
    annotate("text", x=ymd("2012-10-01"), y=20000, 
             label="The red lines represent imputed values",
             vjust="inward", hjust="inward")


ggplot(before, aes(date, total_steps, colour = "red")) + geom_line() + geom_smooth() +
    geom_line(data=after, aes(date, total_steps, colour="blue")) + geom_smooth()
