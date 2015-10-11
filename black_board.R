getwd();
workingDir <- "D:/development/datasciencecoursera/5-reproducible-research/redocp1/";
setwd(workingDir);

downloadUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip";
downloadedFile <- "downloaded.zip";
downloadMethod <- "curl";
downloadMode <- "wb";

## Handles Downloaded file
if (file.exists(targetFile)) {
  print("File downloaded.")
} else {
  print("File NOT downloaded. Proceed to download.")
  downloadStatus <- download.file(
    url = downloadUrl, 
    destfile = downloadedFile,
    method = downloadMethod,
    mode = downloadMode);
  
  if (downloadStatus == 0) {
    print("File downloaded!")
  }
}

targetDownloadedFile <- paste(getwd(), downloadedFile, sep = "/");
csvFile <- "activity.csv";
targetCsvFile <- paste(getwd(), csvFile, sep = "/");
currentDirectory <- "./";

## Handles CSV file
if (file.exists(targetCsvFile)) {
  print("CSV file is ready.")
} else {
  print("Unzip package.")
  unzipStatus <- unzip(targetDownloadedFile);
  if (file.exists(unzipStatus)) {
    print("Package unziped.")
  }
}

## Read data from CSV
ACTIVITY_DATA <- read.csv(file = targetCsvFile, header = TRUE);

## Get a sense of data
class(ACTIVITY_DATA)## data.frame
summary(ACTIVITY_DATA)## Summary information by columns
str(ACTIVITY_DATA)

## Enrich with dateTime column Take2
ACTIVITY_DATA$timeStr <- NA;

### Interval to time string like %H:%M
ACTIVITY_DATA[which(ACTIVITY_DATA$interval < 10),]$timeStr <- paste(
  paste("00", ACTIVITY_DATA[which(ACTIVITY_DATA$interval < 10),]$interval, sep = ":"),
  "0", sep = "");

ACTIVITY_DATA[which(ACTIVITY_DATA$interval >= 10 & ACTIVITY_DATA$interval < 100),]$timeStr <- paste(
  "00", ACTIVITY_DATA[which(ACTIVITY_DATA$interval >= 10 & ACTIVITY_DATA$interval < 100),]$interval, 
  sep = ":");

ACTIVITY_DATA[which(ACTIVITY_DATA$interval >= 100 & ACTIVITY_DATA$interval < 1000),]$timeStr <- paste(
  0,
  paste(
    substr(ACTIVITY_DATA[
      which(ACTIVITY_DATA$interval >= 100 & ACTIVITY_DATA$interval < 1000),]$interval, 1, 1),
    substr(ACTIVITY_DATA[
      which(ACTIVITY_DATA$interval >= 100 & ACTIVITY_DATA$interval < 1000),]$interval, 2, 3), 
    sep = ":"),
  sep = ""); 

ACTIVITY_DATA[which(ACTIVITY_DATA$interval >= 1000),]$timeStr <- paste(
  substr(ACTIVITY_DATA[
    which(ACTIVITY_DATA$interval >= 1000),]$interval, 1, 2),
  substr(ACTIVITY_DATA[
    which(ACTIVITY_DATA$interval >= 1000),]$interval, 3, 4), 
  sep = ":");

length(unique(ACTIVITY_DATA$timeStr)) == 287

ACTIVITY_DATA$dateTimeStr <- paste(ACTIVITY_DATA$date, ACTIVITY_DATA$timeStr, sep = " ");

head(ACTIVITY_DATA, 20)

dateTimeFormat <- "%Y-%m-%d %H:%M";
timeZone <- "GMT";

ACTIVITY_DATA$dateTime <- strptime(
  as.character(ACTIVITY_DATA$dateTimeStr), 
  format = dateTimeFormat, 
  tz = timeZone);

## What is mean total number of steps taken per day?
ACTIVITY_DATA_STEPS_NO_NA <- ACTIVITY_DATA[
  which(!is.na(ACTIVITY_DATA$steps)), 
  c("date", "steps")];
summary(ACTIVITY_DATA_STEPS_NO_NA)
str(ACTIVITY_DATA_STEPS_NO_NA)
head(ACTIVITY_DATA_STEPS_NO_NA)

ACTIVITY_DATA_SUM <- aggregate(steps ~ date, data = ACTIVITY_DATA_STEPS_NO_NA, sum)
summary(ACTIVITY_DATA_SUM)

### What is a Histogram?
### https://www.mathsisfun.com/data/histograms.html
### Present occurenceness of sample

### What is a Bar Graphs?
### https://www.mathsisfun.com/data/bar-graphs.html
### Present relativity between groups in sample

# par(mar = c(5, 8, 5, 8))
# hist(
#   x = ACTIVITY_DATA_SUM$steps,
#   xlab = "Number of Steps Walked",
#   ylab = "Number of Occurence",
#   main = "Occurence of Steps Walked") ## Minimal asthetics

library(ggplot2)
if (!"ggplot2" %in% rownames(installed.packages())) {
  stop("Package ggplot2 not found.")
}

HIST <- ggplot(
  data = ACTIVITY_DATA_SUM,
  aes(ACTIVITY_DATA_SUM$steps));
HIST <- HIST + geom_histogram(breaks = seq(0, 22000, by = 500));
HIST <- HIST + labs(title = "Occurence of Total Number of Steps Walked");
HIST <- HIST + labs(y = "Number of Occurence");
HIST <- HIST + labs(x = "Total Number of Steps");
print(HIST);

BAR_PLOT <- ggplot(
  ACTIVITY_DATA_SUM, 
  aes(x = factor(ACTIVITY_DATA_SUM$date), y = ACTIVITY_DATA_SUM$steps));
BAR_PLOT <- BAR_PLOT + geom_bar(stat = "identity");
BAR_PLOT <- BAR_PLOT + labs(title = "Total Number of Steps Walked by Date");
BAR_PLOT <- BAR_PLOT + labs(x = "Date");
BAR_PLOT <- BAR_PLOT + labs(y = "Total Number of Steps Walked");
BAR_PLOT <- BAR_PLOT + theme(axis.text.x = element_text(angle = -90, hjust = 1)); # xlab vertical
print(BAR_PLOT);

ACTIVITY_DATA_MEAN <- mean(ACTIVITY_DATA_SUM$steps)

### What is median
### https://www.mathsisfun.com/median.html
ACTIVITY_DATA_MEDIAN <- median(ACTIVITY_DATA_SUM$steps)

ACTIVITY_DATA_MEAN;
ACTIVITY_DATA_MEDIAN;

## What is the average daily activity pattern?
# plot(x = ACTIVITY_DATA$dateTime, y = ACTIVITY_DATA$steps, 
#      type = "l", main = "Number of Steps Time Series",
#      ylab = "Number of Stepts", xlab = "Date Time") 

ACTIVITY_DATA_AVG_DAILY <- aggregate(steps ~ date, data = ACTIVITY_DATA, FUN = mean);
str(ACTIVITY_DATA_AVG_DAILY);
head(ACTIVITY_DATA_AVG_DAILY);

ACTIVITY_DATA_AVG_5MIN <- aggregate(steps ~ interval, data = ACTIVITY_DATA, FUN = mean);
str(ACTIVITY_DATA_AVG_5MIN);
head(ACTIVITY_DATA_AVG_5MIN);

# plot(x = ACTIVITY_DATA_AVG_5MIN$interval, y = ACTIVITY_DATA_AVG_5MIN$steps, 
#      type = "l", main = "Average Number of Steps Time Series",
#      ylab = "Average Number of Steps", xlab = "Time Identifier"); 

TIME_SERIES <- ggplot(
  ACTIVITY_DATA, aes(ACTIVITY_DATA_AVG_5MIN$interval, ACTIVITY_DATA_AVG_5MIN$steps));
TIME_SERIES <- TIME_SERIES + geom_line();
TIME_SERIES <- TIME_SERIES + labs(title = "Average Number of Steps Time Series Plot")
TIME_SERIES <- TIME_SERIES + labs(x = "Time Identifier")
TIME_SERIES <- TIME_SERIES + labs(y = "Average Number of Steps")
print(TIME_SERIES)

ACTIVITY_DATA_AVG_5MIN[which(ACTIVITY_DATA_AVG_5MIN$step == max(ACTIVITY_DATA_AVG_5MIN$steps)), ];

## Imputing missing values
sum(!complete.cases(ACTIVITY_DATA$steps))

ACTIVITY_DATA2 <- ACTIVITY_DATA
# ACTIVITY_DATA2[is.na(ACTIVITY_DATA2$steps), ]
# ACTIVITY_DATA2[which(is.na(ACTIVITY_DATA2$steps)), ]

ACTIVITY_DATA2[is.na(ACTIVITY_DATA2$steps), ]$steps <- mean(ACTIVITY_DATA2$steps, na.rm = TRUE)

sum(!complete.cases(ACTIVITY_DATA$steps)) == 0

summary(ACTIVITY_DATA)
summary(ACTIVITY_DATA2)

ACTIVITY_DATA_SUM2 <- aggregate(steps ~ date, data = ACTIVITY_DATA2, sum)
HIST2 <- ggplot(
  data = ACTIVITY_DATA_SUM2,
  aes(ACTIVITY_DATA_SUM2$steps));
HIST2 <- HIST2 + geom_histogram(breaks = seq(0, 22000, by = 500));
HIST2 <- HIST2 + labs(title = "Occurence of Total Number of Steps Walked (New)");
HIST2 <- HIST2 + labs(y = "Number of Occurence");
HIST2 <- HIST2 + labs(x = "Total Number of Steps");
print(HIST2);

mean(ACTIVITY_DATA_SUM$steps);
mean(ACTIVITY_DATA_SUM2$steps);

median(ACTIVITY_DATA_SUM$steps)
median(ACTIVITY_DATA_SUM2$steps)

# Are there differences in activity patterns between weekdays and weekends?
WEEKENDS <- c("Saturday", "Sunday") 
## where WEEKDAYS == !WEEKENDS
ACTIVITY_DATA2$day <- weekdays(ACTIVITY_DATA2$dateTime)
ACTIVITY_DATA2$dayType <- NA

ACTIVITY_DATA2[
  which(ACTIVITY_DATA2$day %in% c(WEEKENDS)), ]$dayType <- "WEEKENDS";
ACTIVITY_DATA2[
  which(!ACTIVITY_DATA2$day %in% c(WEEKENDS)), ]$dayType <- "WEEKDAYS";

summary(ACTIVITY_DATA2);
str(ACTIVITY_DATA2);

# xyplot(steps ~ interval | factor(day), data = ACTIVITY_DATA_2, type = "l")
MULTI_PANEL <- ggplot(ACTIVITY_DATA2, aes(x = interval, y = steps));
MULTI_PANEL <- MULTI_PANEL + geom_line();
MULTI_PANEL <- MULTI_PANEL + facet_wrap(facets = ~ dayType, nrow = 2, ncol = 1);
print(MULTI_PANEL);

ACTIVITY_DATA3 <- ACTIVITY_DATA2;
ACTIVITY_DATA3$date <- as.Date(ACTIVITY_DATA3$date);
ACTIVITY_DATA_SUM_BY_DATE <- aggregate(steps ~ date, ACTIVITY_DATA2, sum);

ACTIVITY_DATA_SUM_BY_DATE$date <- as.Date(ACTIVITY_DATA_SUM_BY_DATE$date);
str(ACTIVITY_DATA_SUM_BY_DATE);

ACTIVITY_DATA_SUM_BY_DATE$day <- weekdays(as.Date(ACTIVITY_DATA_SUM_BY_DATE$date));
str(ACTIVITY_DATA_SUM_BY_DATE);
ACTIVITY_DATA_SUM_BY_DATE$dayType <- NA

ACTIVITY_DATA_SUM_BY_DATE[
  which(ACTIVITY_DATA_SUM_BY_DATE$day %in% c(WEEKENDS)), ]$dayType <- "WEEKENDS";
ACTIVITY_DATA_SUM_BY_DATE[
  which(!ACTIVITY_DATA_SUM_BY_DATE$day %in% c(WEEKENDS)), ]$dayType <- "WEEKDAYS";

ACTIVITY_DATA_SUM_BY_DATE$dayType <- factor(ACTIVITY_DATA_SUM_BY_DATE$dayType);

summary(ACTIVITY_DATA_SUM_BY_DATE);
str(ACTIVITY_DATA_SUM_BY_DATE);

ACTIVITY_DATA_SUM_BY_DATE <- ACTIVITY_DATA_SUM_BY_DATE[, c("date", "dayType", "steps")];
str(ACTIVITY_DATA_SUM_BY_DATE);

MULTI_PANEL2 <- ggplot(ACTIVITY_DATA_SUM_BY_DATE, aes(x = date, y = steps));
MULTI_PANEL2 <- MULTI_PANEL2 + geom_line();
MULTI_PANEL2 <- MULTI_PANEL2 + facet_wrap(~ dayType, ncol = 1)
print(MULTI_PANEL2);

## 
## Testing Multi Panel plot with sample data.frame
##
# dateCol <- as.Date(
#   c("2015-09-01", "2015-09-02", "2015-09-03", "2015-09-04", "2015-09-05", "2015-09-06"));
# dataCol <- as.numeric(c(10, 20, 30, 40, 50, 60));
# 
# dataFrame <- data.frame(date = dateCol, value = dataCol);
# dataFrame$day <- weekdays(dataFrame$date);
# dataFrame$dayType <- NA;
# dataFrame[which(!dataFrame$day %in% WEEKENDS), ]$dayType <- "WEEKDAYS";
# dataFrame[which(dataFrame$day %in% WEEKENDS), ]$dayType <- "WEEKENDS";
# summary(dataFrame)
# str(dataFrame);
# head(dataFrame)
# 
# SAMPLE_PLOT <- ggplot(dataFrame, aes(date, value));
# SAMPLE_PLOT <- SAMPLE_PLOT + geom_line() + facet_wrap(~ dayType);
# print(SAMPLE_PLOT);

################################################################################
################################################################################
################################################################################
library(datasets)
ls(airquality)# Header
