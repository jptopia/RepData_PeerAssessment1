


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

# PART 2


