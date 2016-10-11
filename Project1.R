rm(list=ls())
# setwd("~/Analytics/JH Courses/04_Reproducable Research/Project1")
setwd("C:/Users/Kier/Documents/Analytics Course/04_ReproducibleResearch/Project1")

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
## 1. Code for reading in the dataset and/or processing the data
# Get raw data

activity_raw <- read_csv("activity.csv")


## Initial Findings  
# This query shows that there are several days with all NA values. 
# Since one of the days is the very first date and one is the very last date, 
#they will be trimmed from the final data set as they are not representative 
# of the rest of the data set.  

range(activity_raw$date)

activity_raw %>%
    group_by(date) %>%
    filter(is.na(steps)) %>%
    summarise(num_of_na = length(steps))

# The second issue is with the 2nd day, 2012-10-02.  It contains all zeros except for two entries. 

# Retrieve all records for Day 2, October 12, 2012
b <- activity_raw %>%
    filter(date == ymd("2012-10-02"))
# Get records where steps == zero on Oct 12, 2012
c <- b %>%
    filter(steps == 0)

dim(b)
dim(c)

# We can see that 286 of the 288 records for 2012-10-02 has a value of zero, 
# which may indicate the the subject was not wearing the device for most of the day.

# For these reasons 2012-10-01, 2012-10-02 & 2012-11-30 will be omitted from the analysis test set.

ar_trimmed <- activity_raw %>%
    filter(date >= ymd("2012-10-03"), date < ymd("2012-11-30"))

# Part of the assignment is to impute NA values.  
# Let's make sure that we still have a few NAs to work with.

ar_trimmed %>%
filter(is.na(steps))

# There are still 1,728  NAs that can be imputed 

## Days with NAs
# This shows the days that still have NAs.  
# Note that NAs occur during an entire day.  This may indicate that the 
# device was not worn at all during the day.
# There are no days with just a few NA records*  

ar_trimmed %>%
group_by(date) %>%
filter(is.na(steps)) %>%
summarise(num_of_na = length(steps))

## Remove unneeded variables

rm(b, c)

# Create Dataset with With NAs removed
# This dataset removes all records with NAs, not just the first and last days.  

ar_na_omit <- na.omit(ar_trimmed)


## Summarise total number of steps taken per day

total_steps_na_omit <- ar_na_omit %>%
select(date, interval, steps) %>%
group_by(date) %>%
summarise(total_steps = sum(steps))

## 2. Histogram of the total number of steps taken each day
### NAs omitted

p <- ggplot(total_steps_na_omit, aes(total_steps)) + 
    geom_histogram(bins = 20)


## 3. Mean and median number of steps taken each day
# Calculate and report the mean and median of the total number of steps 
# taken per day with NA records omitted.  

(total_steps_mean <- mean(total_steps_na_omit$total_steps))
(total_steps_median <- median(total_steps_na_omit$total_steps))

p <- p + annotate("text", x = 0, y = c(7.5, 6), 
                  label = c("Mean steps per day = 10,970.81",
                            "Median steps per day = 10,890"), 
                  hjust = 0) 
p

## 4. Time series plot of the average number of steps taken
## 5. The 5-minute interval that, on average, contains the 
#     maximum number of steps
# Make a time series plot (i.e. type = "l") of the 5-minute 
# interval (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis).  
# Which 5-minute interval, on average across all the days in 
# the dataset, contains the maximum number of steps?  
# NOTE:  This dataset omits all NAs

average_per_interval <- ar_na_omit %>%
select(date, interval, steps) %>%
group_by(interval) %>%
summarise(average_per_5_minutes = mean(steps))


# Determine the interval with the maximum average steps

max_interval <- average_per_interval %>%
filter(average_per_5_minutes == max(average_per_5_minutes))

max_interval <- max_interval[1]
max_interval

ggplot(average_per_interval, aes(interval, average_per_5_minutes)) + geom_line() +
geom_vline(aes(xintercept = max_interval), colour = "red") +
annotate("text", x=1500, y = c(175, 150), 
         label = c("Interval with highest average steps",
                   "0835am is the most active, on average"),
         colour = "red")


# Create dataset with NAs imputed by a formula  
## 6. Code to describe and show a strategy for imputing missing data  

### Strategy for imputing NA values:  
# To impute the values of the NAs in 'steps' I decided 
# to use a the rpart library which uses recursive partitioning.  
# I like it because it can impute the value based on any 
# number of parameters even though this case only derives it 
# based on the interval that it occurs in.  

# Start with the trimmed raw data set

ar_imputed <- ar_trimmed 


# Impute the missing 'steps' using rpart

#library(rpart)

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
total_steps_imputed_mean <- mean(total_steps_imputed$total_steps)
total_steps_imputed_median <- median(total_steps_imputed$total_steps)

## 7. Histogram of the total number of steps taken each 
# day after  missing values are imputed.

p <- ggplot(total_steps_imputed, aes(total_steps)) + 
     geom_histogram(bins = 15) + 
     annotate("text", x = 0, y = c(7.5, 6), 
              label = c("Mean steps per day = 10,970.81",
                        "Median steps per day = 10,970.81"), 
                       hjust = 0) 
# Interesting that the mean and median or identical now.

## 8. Panel plot comparing the average number of steps 
# taken per 5-minute interval across weekdays and weekends.

# Create the is_weekday column in ar_imputed

is_weekday <- !(weekdays(as.Date(ar_imputed$date)) %in% c('Saturday','Sunday'))
ar_imputed_days <- cbind(is_weekday, ar_imputed)
ar_imputed_days$is_weekday <- is_weekday

total_steps_days <- ar_imputed_days %>%
group_by(is_weekday, interval) %>%
summarise(average_steps = mean(steps))

p <- ggplot(total_steps_days, aes(interval, average_steps)) +geom_line()
p + facet_wrap(~is_weekday, ncol=2, labeller = "label_both")





##################################################################
# Get raw data
activity_raw <- read_csv("activity.csv")

# My initial investigation of this data set revealed that the first two days contain data that
# should not be included in the data analysis.
range(activity_raw$date)

activity_raw %>%
    group_by(date) %>%
    filter(is.na(steps)) %>%
    summarise(num_of_na = length(steps))
# 2012-10-01 contains all NA's
length(activity_raw$steps[activity_raw$steps == NA & activity_raw$date == ymd("2012-10-01")])
# If we look at another date with no NAs
length(activity_raw$steps[activity_raw$date == ymd("2012-11-01")])
length(activity_raw$steps[activity_raw$steps == NA & activity_raw$date == ymd("2012-11-01")])
# Record length per day
len <- activity_raw %>%
    group_by(date) %>%
    summarise(length(interval))
len
# We can see that each day has 288 records

# Now let's look at the number of NAs on 2012-10-01
len_na_on_20121001 <- activity_raw %>%
    filter(date == ymd("2012-10-01"))
length(is.na(len_na_on_20121001$steps))
# 288 records

 
# # 2012-10-02 has incomplete data; it looks like subject was not wearing device most of day
b <- activity_raw %>%
    filter(date == ymd("2012-10-02")) %>%
    arrange(date, interval)
dim(b) 
c <- b %>%
    filter(steps == 0)
dim(c)
# We can see that 286 of the 288 records for 2012-10-02 has a value of zero, which may
# indicate the the subject was not wearing the device for most of the day.

# For these reasons 2012-10-01 & 2012-10-02 will be ommitted from the analysis test set

ar_trimmed <- activity_raw %>%
    filter(date >= ymd("2012-10-03"))

# Part of the assignment is to impute NA values.  Let's make sure that we still
# have a few NAs to work with.
ar_trimmed %>%
    filter(is.na(steps))

# There are still 2,006 NAs that can be imputed 
ar_trimmed %>%
    group_by(date) %>%
    filter(is.na(steps)) %>%
    summarise(num_of_na = length(steps))
# # Remove NA records
# # 2012-10-02 has incomplete data and should be removed, but for the purposes
# # of this assignment I will leave them
# na_omit <- na.omit(activity_raw) #%>%
#     #filter(date != ymd("2012-10-02"))
# 
# rm(a,b)
# 
# # Look around a little bit
# attach(activity_na_omit)
# str(activity_na_omit)
# head(activity_na_omit, 25)
# tail(activity_na_omit)
# summary(activity_na_omit)
# range(date)
# unique(interval)
# detach(activity_na_omit)

# Now let's get down to business...
#
# First I am going to create three summary data sets.
# These all summarise total_steps per date
# 1. With NAs removed
# 2. With NAs imputed by a formula
# 3. With first two days removed and NA's imputed by formula
#
# What I noticed is that all of the first day is NAs and the 2nd day only has a 
# couple of entries that are non-zero.  Because these two days are at the beginning as 
# opposed to the middle indicates that they can safely removed from the data set and 
# produce more accurate results.

######################################################################################
# 1. With NAs removed
######################################################################################
ar_na_omit <- na.omit(activity_raw)

# Summarise total number of steps taken per day
total_steps_na_omit <- ar_na_omit %>%
    select(date, interval, steps) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

# Calculate and report the mean and median of the total number of steps taken 
# per day with NA records omitted
#
# Calculate mean steps per day
mean_steps_na_omit <- total_steps_na_omit %>%
    summarise(average_steps = mean(total_steps)) %>%
    round(digits=3) %>%
    unlist()
# Calculate median steps per day
median_steps_na_omit <- total_steps_na_omit %>%
    summarise(median_steps = median(total_steps)) %>%
    round(digits=3) %>%
    unlist()

# Plot of data, mean, and median with NA records omitted
ggplot(total_steps_na_omit, aes(date, total_steps)) + geom_line()+
    geom_hline(aes(yintercept=mean_steps_na_omit), colour = "red", linetype = 2) +
    annotate("text", x=ymd("2012-10-17"), y=20000, 
             label = paste("Mean with NAs omitted = ", mean_steps_na_omit), 
             colour = "red") +
    geom_hline(aes(yintercept=median_steps_na_omit), colour = "blue", linetype = 3) +
    annotate("text", x=ymd("2012-10-16"), y=18000,
             label = paste("Median with NAs omitted = ", median_steps_na_omit), 
             colour = "blue")

# What is mean total number of steps taken per day?
# Answer: 10,766.189

# Make a histogram of the total number of steps taken each day
hist(total_steps_na_omit$total_steps, breaks = 15, 
     xlim=range(0,25000), xlab = "Total Steps per Day", 
     main = "Histogram of Total Steps per Day")

######################################################################################
# 2. With NAs imputed by a formula
######################################################################################
# Strategy:
# To impute the values of the NAs in 'steps' I decided to use a the rpart
# library which uses recursive partitioning.  I like it because it can impute the 
# value based on any number of parameters even though this case only derives
# it based on the interval that it occurs in.
# NOTE: This approach uses the full data set inlcuding all of the zeros on 2012-10-2
#####################################################################################

# Start with the raw data set 
ar_imputed <- activity_raw 

# Impute the missing 'steps' using rpart
library(rpart)

# Create prediction formula
predicted_steps <- rpart(steps ~ interval,
                         data = ar_imputed[!is.na(ar_imputed$steps),], 
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
mean_steps_imputed <- total_steps_imputed %>%
    summarise(average_steps = mean(total_steps)) %>%
    round(digits=3) %>%
    unlist()

# Calculate median steps per day
median_steps_imputed <- total_steps_imputed %>%
    summarise(median_steps = median(total_steps)) %>%
    round(digits=3) %>%
    unlist()

# Plot of data, mean, and median with imputed balues for NA records
ggplot(total_steps_imputed, aes(date, total_steps)) + geom_line()+
    geom_hline(aes(yintercept=mean_steps_imputed), colour = "red", linetype = 2) +
    annotate("text", x=ymd("2012-10-17"), y=20000, 
             label = paste("Mean with imputed NAs = ", mean_steps_imputed), 
             colour = "red") +
    geom_hline(aes(yintercept=median_steps_imputed), colour = "blue", linetype = 3) +
    annotate("text", x=ymd("2012-10-18"), y=18000,
             label = paste("Median with imputed NAs = ", median_steps_imputed), 
             colour = "blue")


######################################################################################
# 3. With first two days trimmed and rest of NAs imputed
######################################################################################

# Start with raw data
ar_imputed_trimmed <- activity_raw %>%
    filter(date >= ymd("2012-10-03")) %>%
    select(date, interval, steps)

# Create prediction formula
predicted_steps <- rpart(steps ~ interval,
                         data = ar_imputed_trimmed[!is.na(ar_imputed_trimmed$steps),], 
                         method = "anova")

# Apply prediction to ar_imputed_trimmed NA records
# This should produce more accurate predictions because all of the zeros on 
# 2012-10-02 is probably skewing the prediction
ar_imputed_trimmed$steps[is.na(ar_imputed_trimmed$steps)] <- predict(predicted_steps, 
                                                                     ar_imputed_trimmed[is.na(ar_imputed_trimmed$steps),])

# Summarise total number of steps taken per day
total_steps_imputed_trimmed <- ar_imputed_trimmed %>%
    select(date, interval, steps) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

# Calculate mean steps per day
mean_steps_imputed_trimmed <- total_steps_imputed_trimmed %>%
    summarise(average_steps = mean(total_steps)) %>%
    round(digits=3) %>%
    unlist()

# Calculate median steps per day
median_steps_imputed_trimmed <- total_steps_imputed_trimmed %>%
    summarise(median_steps = median(total_steps)) %>%
    round(digits=3) %>%
    unlist()

# Plot of data, mean, and median with imputed balues for NA records
ggplot(total_steps_imputed_trimmed, aes(date, total_steps)) + geom_line()+
    geom_hline(aes(yintercept=mean_steps_imputed_trimmed), colour = "red", linetype = 2) +
    annotate("text", x=ymd("2012-10-19"), y=20000, 
             label = paste("Mean with imputed NAs = ", mean_steps_imputed_trimmed), 
             colour = "red") +
    geom_hline(aes(yintercept=median_steps_imputed_trimmed), colour = "blue", linetype = 3) +
    annotate("text", x=ymd("2012-10-20"), y=18000,
             label = paste("Median with imputed NAs = ", median_steps_imputed_trimmed), 
             colour = "blue")

############################################################################
# Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. 
total_steps_per_day_imputted <- ar_na_imputed %>%
    select(date, interval, steps) %>%
    arrange(date, interval) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

average_steps_imputed <- round(mean(total_steps_per_day_imputted$total_steps), digits = 3)
median_steps_imputed <- round(median(total_steps_per_day_imputted$total_steps), digits = 3)



##############################################################
# Create a summarised data set with NA records omitted
##############################################################


#   
#########################################################################


# What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis).
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
average_per_interval <- na_omit %>%
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

# Imputing missing values

# Note that there are a number of days/intervals where there are missing 
# values (coded as NA). The presence of missing days may introduce bias 
# into some calculations or summaries of the data.

# Calculate and report the total number of missing values in the 
# dataset (i.e. the total number of rows with NAs)

# We're going to start out with the raw data
missing <- activity_raw %>%
    select(date, interval, steps) %>%
    mutate(missing_records = as.integer(is.na(steps))) %>%
    group_by(missing_records) %>%
    summarise(record_count = length(missing_records))

ggplot(missing, aes(factor(missing_records), record_count, 
       ylim=range(0,20000)), 
       title = "Record count for non-missing & missing steps") + 
    geom_bar(stat = "identity", width = 0.5) + 
    labs(x = "0 = Not Missing; 1 = Missing", y = "Count") +
    geom_text(stat = "identity", 
              label = c("Records with data", "Records without data"), 
              y = c(13000, 3000), colour = c("white", "black"))
    # directlabels::geom_dl(aes(label = record_count), method="smart.grid") +
    
####################################################################
# Devise a strategy for filling in all of the missing values in the 
# dataset. 
####################################################################

# In this plot I offset the mean and median lines slightly because they
# have the exact same value.
ggplot(an1, aes(total_steps)) + geom_histogram(binwidth=1000, 
                                               fill = "burlywood2",
                                               colour = "gray78") +
        geom_vline(aes(xintercept=average_steps_imputed+20), colour = "red") +
    annotate("text", x=15000, y=13, 
             label = paste("Mean =",average_steps_imputed) , colour = "red", vjust = "inward") +
    geom_vline(aes(xintercept=median_steps_imputed-20), colour = "blue") +
    annotate("text", x=15000, y=10, 
             label = paste("Median = ",median_steps_imputed), 
             colour = "blue", vjust = "inward") 


# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total 
# daily number of steps?
# Create line plots that shows the original data and the new data with 
# imputed values.
na_omit <- na.omit(activity_raw)
before_impute <- na_omit %>% # NA records removed
    select(date, interval, steps) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

after_impute <- ar_na_imputed %>%
    select(date, interval, steps) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

mean_before_impute <- mean(before_impute$total_steps)
median_before_impute <- median(before_impute$total_steps)

mean_after_impute <- mean(after_impute$total_steps)
median_after_impute <- median(after_impute$total_steps)

mean_before_after_diff <- round(abs(mean_before_impute)) - round(abs(mean_after_impute))
median_before_after_diff <- round(abs(median_before_impute)) - round(abs(median_after_impute))

ggplot(before, aes(date, total_steps, colour = "red")) + geom_line() + geom_smooth() +
    geom_line(data=after, aes(date, total_steps, colour="blue")) + geom_smooth()

#################################################
# Compare above results to an alternative data set that trims the first two
# days of data, which were incomplete at best.
# You will find that the difference in mean virtually disappears.  It dropped
# to a difference of 1.81 insead of 204.62

# '_trimmed' refers to a dataset with the first two days removed
before_trimmed <- before %>%
    filter(date >= "2012-10-03") %>%
    mutate(d_type = 1) %>%
    select(d_type, date, total_steps)

after_trimmed <- after %>%
    filter(date >= "2012-10-03") %>%
    mutate(d_type = 2) %>%
    select(d_type, date, total_steps)

# Get the single mean & median of all days in the 'before' data set, 
# trimmed of first two days.
mean_before_trimmed <- round(mean(before_trimmed$total_steps), digits=3)
median_before_trimmed <- round(median(before_trimmed$total_steps), digits=3)

# Get the single mean & median of all days in the 'after' data set, 
# trimmed of first two days.
mean_after_trimmed <- round(mean(after_trimmed$total_steps), digits = 3)
median_after_trimmed <- round(median(after_trimmed$total_steps), digits = 3)

mean_diff_trimmed <- round(abs(mean_before_trimmed - mean_after_trimmed), digits=3)
median_diff_trimmed <- round(abs(median_before_trimmed - median_after_trimmed), digits=3)

# bind the two datasets together
full_trimmed <- rbind(before_trimmed, after_trimmed)

# Plot of 'before' and 'after' data
ggplot(full_trimmed, aes(date, total_steps, group=d_type, colour = d_type)) + 
    ggtitle("Total steps with Oct 2 & 3 Omitted") + geom_line() +
    geom_hline(yintercept = mean_before_trimmed, linetype = 2) + 
    geom_hline(yintercept = median_after_trimmed, linetype = 4) +
    annotate("text", x=ymd("2012-10-23"), y=20000, 
             label = paste("Mean of before data = ", mean_before_trimmed)) +
    annotate("text", x=ymd("2012-10-23"), y=18000, 
             label = paste("Mean of after data = ", mean_after_trimmed)) +
    annotate("text", x=ymd("2012-10-23"), y=6000, 
             label = paste("Median of before data = ", median_before_trimmed)) +
    annotate("text", x=ymd("2012-10-23"), y=4000, 
             label = paste("Median of after data = ", median_after_trimmed))

# Plot of 'after' data
ggplot(full_trimmed, aes(date, total_steps, group=d_type, colour = d_type)) + 
    ggtitle("Total steps with Oct 2 & 3 Omitted") + geom_line() +
    geom_hline(yintercept = mean_before_trimmed, linetype = 2, color = "red") + 
    geom_hline(yintercept = median_after_trimmed, linetype = 4, color = "blue") +
    annotate("text", x=ymd("2012-10-23"), y=6000, 
             label = paste("Mean of before data = ", mean_before_trimmed)) +
    annotate("text", x=ymd("2012-10-23"), y=4000, 
             label = paste("Median of before data = ", median_before_trimmed)) 

ggplot(before_trimmed, aes(date, total_steps),ylim(5000, 15000) + geom_line() + 
    geom_hline(yintercept = mean_before_trimmed, colour = "red", 
               label = mean_before_trimmed) + 
    geom_hline(yintercept = median_after_trimmed, colour = "pink")

############################################################
for i in seq(from = 10, to = 50, by = 5)
hist(mean_steps_per_day$average_steps, breaks = i)

par(mfrow=c(2,3))
for(i in seq(from=15, to=30, by=5)){
    #  create histogram
    hist(mean_steps_per_day$average_steps, breaks = i, 
         main= paste("Breaks = ", i), xlim = range(0,80), xlab = "Average Steps")
}

par(mfrow=c(2,3))
for(i in seq(from=15, to=30, by=5)){
    #  create histogram
    ggplot(mean_steps_per_day, aes(average_steps)) + 
        geom_histogram(binwidth = i) 
         #main= paste("Breaks = ", i), xlim = range(0,80), xlab = "Average Steps")
}

#########################################################
ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
str(Oxboys)
head(Oxboys)
tail(Oxboys)

ggplot(an1, aes(date, steps)) + geom_boxplot()








