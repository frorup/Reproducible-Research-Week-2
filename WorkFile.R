####################################################################
# Step 1 
####################################################################
# Defining the urlprovided from the task description:
urlProvided <-   "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# Downloading the file 
download.file(urlProvided, destfile = "activity.zip")
# Unzip 
unzip("activity.zip")
# load data
dataBase <- read.csv("activity.csv")
# Convert date to date format
dataBase$date <- as.Date(dataBase$date)
dataBase$interval <- as.integer(dataBase$interval)

####################################################################
# Step 2 
####################################################################
stepsperday <- aggregate(dataBase$steps, list(Day = dataBase$date), FUN = sum )
hist(stepsperday$x, breaks = 20, main = "Steps taken each day", xlab = "Steps by day")

####################################################################
# Step 4 
####################################################################
filt_dataBase <- dataBase[ is.na(dataBase$steps) != TRUE , ]
stepsperinterval <- with(filt_dataBase, aggregate(steps, list(interval), FUN = mean ))
with(stepsperinterval, plot(Group.1, x, type = "l", xlab = "Interval", ylab = "Steps"))
