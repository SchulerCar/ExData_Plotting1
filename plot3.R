##########################################
## Coursera - Exploratory Data Analysis
## Course Project 1
## 
## Examine how household energy usage varies over a 2-day period in February,
## 2007. Your task is to reconstruct the plots in the repo,
##     https://github.com/rdpeng/ExData_Plotting1
## 
## all of which were constructed using the base plotting system.
## 
## For each plot you should: 
##
## 1. Construct the plot and save it to a PNG file with a width of 480 pixels and a 
##    height of 480 pixels.
## 2. Name each of the plot files as lot1.png, plot2.png, etc.
## 3. Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the 
##    corresponding plot, i.e. code in plot1.R constructs the plot1.png plot.
##    Your code file should include code for reading the data so that the plot 
##    can be fully reproduced. You must also include the code that creates the 
##    PNG file.
## 4. Add the PNG file and R code file to the top-level folder of your git 
##    repository (no need for separate sub-folders)

##
## When you are finished with the assignment, push your git repository to GitHub 
## so that the GitHub version of your repository is up to date. There should be 
## four PNG files and four R code files, a total of eight files in the top-level
## folder of the repo.
## 

## PLOT 3

# libraries

library(lubridate)
library(dplyr)

# Read the data
columnHeaders<-as.character(read.table("household_power_consumption.txt",nrows=1,
                                       sep=";"))
data <- tbl_df(read.table("household_power_consumption.txt", skip=66637, 
                          nrows=2880, sep=";", header = FALSE,
                          colClasses = c(rep("character",2),
                                         rep("numeric",7)),
                          na.strings = "?"))
names(data)<-columnHeaders
data <- mutate(data, Date = dmy(Date),Time= hms(Time))

# Plot the linechart
plotWidth <- 480; plotHeight <- 480
png(filename = "plot3.png", width = plotWidth, height = plotHeight)

x<-with(data,(Date+Time))
xLimits <- c(min(x),max(x))


with(data,{
        x<-Date+Time
        xLimits <- c(min(x),max(x))
        yLimits <- c(min(Sub_metering_1,Sub_metering_2,Sub_metering_3),
                     max(Sub_metering_1,Sub_metering_2,Sub_metering_3))
        plot(x,Sub_metering_1,xlab="", ylab="Energy sub metering", main="",
             xlim= xLimits, ylim= yLimits, bg="white", col = "black", type = "l")
        lines(x,Sub_metering_2,col="red")
        lines(x,Sub_metering_3,col="blue")
        legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
               col=c("black","red","blue"),lty=c(1,1,1))
})

dev.off()