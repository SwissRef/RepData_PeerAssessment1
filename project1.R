library(lattice)
options(scipen=20)

## Step 1 - read in data
filepath <- getwd()
file <- paste(filepath,"/activity/activity.csv",sep="")
data <- read.csv(file, header=TRUE)

data$date <- as.Date(data$date)
data$interval <- factor(data$interval)

## Step 2 - part 1 of project: calculate mean & median of total steps per day. Plot Histogram. 

## Calculation of the total steps per day
dailytotal <- aggregate(steps ~ date, data, sum) 
## create graph
graph1 <- histogram(dailytotal$steps, col="green", breaks=50, xlab="Total Steps", 
                    main="Initial Total Steps per Day", type="count")
## calculate required outputs
dailytotalmean <- mean(dailytotal$steps)
dailytotalmedian <- median(dailytotal$steps)

## Step 3 - part 2 of project: calculate mean of steps per interval. Plot line graph. Calc which interval has
## max steps on average

## Calculation of the mean steps per interval
intervalmean <- aggregate(steps ~ interval, data, mean) 
## next conversion of formats
intervalmean$interval <- as.numeric(levels(intervalmean$interval))[intervalmean$interval]
intervalmean$interval <- strptime(sprintf("%04d",intervalmean$interval), "%H%M")
## create the graph
graph2 <- xyplot(steps ~ as.POSIXct(interval), data = intervalmean, type="l", xlab="Interval", 
                 ylab="Average Steps", main="Average Steps per Interval", scales=list(
                  x=list(at= seq(as.POSIXct(intervalmean$interval[1]), by="2 hour", length=13), 
                  labels=format(seq(as.POSIXct(intervalmean$interval[1]), by="2 hour", length=13), "%H:%M"))))
## calculate the interval with the maximum average steps
maxinterval <- format(intervalmean[which.max(intervalmean$step),"interval"],"%H:%M")


## Step 4 - part 3 of project: Calculate number of missing data fields. Replace missing values. 

## Create a new data set, and also find out where and how many NA values there are
data2 <- data
dataNA <- is.na(data2$steps)
datamissing <- sum(dataNA)
## Replace NA values with the mean for that interval
data2[dataNA, "steps"]  <-  intervalmean[data2[dataNA,  "interval"], "steps"]
## Recalculate the mean, median and plot the required graph
newdailytotal <- aggregate(steps ~ date, data2, sum)
newdailytotalmean <- mean(newdailytotal$steps)
newdailytotalmedian <- median(newdailytotal$steps)
graph3 <- histogram(newdailytotal$steps, col="green", breaks=50, xlab="Total Steps", 
                    main="Corrected Total Steps per Day", type="count")

## Step 5 - part 4 of project: Split into weekend/weekday and plot pattern. 

## Create a column for Weekend/Weekday
weekend <- as.factor(ifelse(weekdays(data2$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday"))
newdata <- cbind(data2, weekend)
## Create the mean, but include the weekend split, and plot graph.
newintervalmean <- aggregate(steps ~ interval + weekend, newdata, mean)
newintervalmean$interval <- as.numeric(levels(newintervalmean$interval))[newintervalmean$interval]
newintervalmean$interval <- strptime(sprintf("%04d",newintervalmean$interval), "%H%M")
graph4 <- xyplot(steps ~ as.POSIXct(interval) | weekend, data = newintervalmean, type="l", layout=c(1,2), 
                 xlab="Interval", ylab="Average Steps", main="Average Steps per Interval", scales=list(
                  x=list(at= seq(as.POSIXct(newintervalmean$interval[1]), by="2 hour", length=13), 
                  labels=format(seq(as.POSIXct(newintervalmean$interval[1]), by="2 hour", length=13), "%H:%M"))))



