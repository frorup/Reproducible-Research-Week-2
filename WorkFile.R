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

####################################################################
# Step 5 
####################################################################
stepsperinterval[which(max(stepsperinterval$x)==stepsperinterval$x),]

####################################################################
# Step 6 
####################################################################
### Total number of missing values in the data set
print(sum(is.na(dataBase)))
### Imputed Mean and Median 
stepsperintMedian <- with(filt_dataBase, aggregate(steps, list(interval), FUN = median ))
imputedMean <- dataBase
imputedMedi <- dataBase
entriesdb   <- dim(dataBase)[1]
for ( i in seq(1,entriesdb)) {
  if (is.na(imputedMean$steps[i]==TRUE)) {
    
    imputedMean$steps[i] <- 
      stepsperinterval$x[stepsperinterval$Group.1 == imputedMean$interval[i]]
    imputedMedi$steps[i] <- 
      stepsperinterval$x[stepsperinterval$Group.1 == imputedMedi$interval[i]]
  }
  
}
stepsdayMean <- aggregate(imputedMean$steps, list(Day = imputedMean$date), FUN = sum )
stepsdayMedi <- aggregate(imputedMedi$steps, list(Day = imputedMedi$date), FUN = sum )

par(mfrow = c(1,3))
hist(stepsperday$x, breaks = 20, main = "Original expluding NA", xlab = "Steps by day")
hist(stepsdayMean$x, breaks = 20, main = "NA emputed with mean", xlab = "Steps by day")
hist(stepsdayMedi$x, breaks = 20, main = "NA emputed with median", xlab = "Steps by day")

####################################################################
# Step 8
####################################################################
filt_dataBase$weekday <- weekdays(filt_dataBase$date, abbreviate = TRUE)
filt_dataBase$weekend <- ifelse(filt_dataBase$weekday=="Sat" |              
           filt_dataBase$weekday=="Sun", "Weekend", "Weekday" )
plot_data <- aggregate(filt_dataBase$steps, 
                          list(filt_dataBase$interval, filt_dataBase$weekend),
                          FUN = sum)
colnames(plot_data) <- c("Interval", "Weeksplit", "Steps")
weekdayData <- plot_data[plot_data$Weeksplit == "Weekday", -2]
weekendData <- plot_data[plot_data$Weeksplit == "Weekend", -2]

par(mfrow = c(1,1))
plot(weekdayData$Interval, weekdayData$Steps, type = "l", 
     col = "black", xlab = "Interval", ylab = "Steps")
points(weekendData$Interval, weekendData$Steps, type = "l", col = "red")
legend("topright", type = "l", col = c("black", "red"), legend = c("Week day", "Weekend"))
