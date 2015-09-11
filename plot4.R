# This script implements plot #4 for assignment #1 of exploratory data analysis
# 
# It reads the data file, reformats columns as needed and 
# renders a plot to a png file
#
# I've reused (via copy/paste) functions from earlier parts of the assignment.
# Not because I consider that best practice - only to match the instructions.
###############################################################################

#
# Read the household poer consumption csv file - all char
# Identify ? as NA
# Create a dateTime column using POSIXct
# Recode all needed measures as.numeric
###############################################################################
GetHPC <- function(){
  df <- read.csv(file="household_power_consumption.txt",
                 sep=";", 
                 nrows=100000,             # cheezy but 100k includes target dates
                 na.strings = "?",
                 colClasses = "character")
  
  # subset the df for dates
  strDateTime <- paste(df$Date, df$Time)
  df$dateTime <- as.POSIXct(strptime(strDateTime, "%d/%m/%Y %H:%M:%S"))
  
  df$Date     <- as.Date(df$Date, format="%d/%m/%Y")
  df          <- df[df$Date >= "2007-02-01" & df$Date <= "2007-02-02",]
  
  df$Global_active_power   <- as.numeric(df$Global_active_power)
  df$Sub_metering_1        <- as.numeric(df$Sub_metering_1)
  df$Sub_metering_2        <- as.numeric(df$Sub_metering_2)
  df$Sub_metering_3        <- as.numeric(df$Sub_metering_3)
  df$Voltage               <- as.numeric(df$Voltage)
  df$Global_reactive_power <- as.numeric(df$Global_reactive_power)
  return(df)
}

#
# Plot Global Active Power as a function of time
# xlab/ylab are parameterized for reuse in plot 4
###############################################################################
plot2 <- function(x, y, xlab="", ylab=""){
  par(mar=c(5.1,5.1,4.1,2.1))
  plot(x, y,
       type="l",
       xlab=xlab,
       ylab=ylab,
       main="")
}

#
# Plot 3 types of Energy sub metering as a function of time
# Red, blue and black are used to distinguish between the types
# A legend is added in the upper right of the plot
# bty is parameterized for reuse - the legend box can be turned off
###############################################################################
plot3 <- function(df, bty="o"){
  par(mar=c(5.1,5.1,4.1,2.1))
  plot(df$dateTime, df$Sub_metering_1,
       type="l",
       xlab="",
       ylab="Energy sub metering",
       main="")
  lines(df$dateTime, df$Sub_metering_2, col="red")
  lines(df$dateTime, df$Sub_metering_3, col="blue")
  legend("topright", names(df[7:9]), col=c("black","red","blue"), lty=1, bty=bty)
}

#
# Use plot 2 and plot 3 and mfrow/par to create a 2X2
###############################################################################
plot4 <- function(df){
  par(mar=c(5.1,5.1,4.1,2.1), mfrow=c(2,2))
  plot2(x=df$dateTime, 
        y=df$Global_active_power,     # poor label - just matching assignment
        ylab="Global Active Power")
  plot2(x=df$dateTime, 
        y=df$Voltage,
        xlab="datetime",
        ylab="Voltage")
  plot3(df, bty="n")
  plot2(x=df$dateTime, 
        y=df$Global_reactive_power,  # poor label - just matching assignment
        xlab="datetime",
        ylab="Global_reactive_power")
}

#
# This area executes when you source the file
###############################################################################
df <- GetHPC()                                      # Read and format the data
png(filename="plot4.png", width=480, height=480)    # Setup the device
plot4(df)                                           # Create plot on  device
dev.off()                                           # Turn off device & render
