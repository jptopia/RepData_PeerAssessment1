
# Read in data
if(!file.exists("activity.csv")) {
    unzip(zipfile = "activity.zip")
}

activity <- read.csv(file = "activity.csv",
                     colClasses = c("integer", "Date", "integer"))

dt_func <- function(dt) {
  if(weekdays(dt) %in% c("Saturday", "Sunday")) {
    return("Weekend")
  }
  else {
    return("Weekday")
  }
}

activity$week_part <- sapply(activity$date, FUN = dt_func)

rm(dt_func)
