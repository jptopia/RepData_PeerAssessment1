
library(dplyr, warn.conflicts = FALSE)
library(mice, quietly = TRUE)
library(lattice)


# Read in data
if(!file.exists("activity.zip")) {
  file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(file_url, destfile = "activity.zip", method = "auto")
}

if(!file.exists("activity.csv")) {
    unzip(zipfile = "activity.zip")
}

activity <- read.csv(file = "activity.csv",
                     colClasses = c("integer", "Date", "integer"))



# PART 2

library(dplyr)

part2 <- activity %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps, na.rm = TRUE))

hist(part2$total_steps,
     col = "blue",
     main = "Histogram of Total Steps per Day"
     xlab = "Total Steps per Day")

# PART 3

part3 <- activity %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))

with(part3, plot(interval, mean_steps,
                 type = "l",
                 main = "Mean Steps by 5-Minute Time Interval",
                 xlab = "5-Minute Time Interval",
                 ylab = "Mean Steps"))
max(part3$mean_steps)

# PART 4

md.pattern(activity)

activity_2 <- activity %>%
  group_by(interval) %>%
  mutate(steps_fill = ifelse(is.na(steps), round(mean(steps, na.rm = TRUE)), steps))

part4 <- activity_2 %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps_fill, na.rm = FALSE))

hist(part4$total_steps,
     col = "darkgray",
     main = "Histogram of Total Steps per Day",
     xlab = "Total Steps per Day (with imputed missing values)")

mean(part4$total_steps)

# PART 5

dt_func <- function(dt) {
  if(weekdays(dt) %in% c("Saturday", "Sunday")) {
    return("Weekend")
  }
  else {
    return("Weekday")
  }
}

activity_2$week_part <- factor(sapply(activity$date, FUN = dt_func))

rm(dt_func)

part5 <- activity_2 %>%
  group_by(week_part, interval) %>%
  summarize(mean_steps = mean(steps_fill, nq.rm = FALSE))

with(part5, xyplot(mean_steps ~ interval | week_part,
                   layout = c(1, 2),
                   type = "l",
                   main = "Mean Steps by 5-Minute Time Interval and Part of Week",
                   xlab = "5-Minute Time Interval",
                   ylab = "Mean Steps"))

