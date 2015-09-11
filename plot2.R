# This script implements plotting for assignment #1
# of the exploratory data analysis course
############################################################
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
######################################################################
plot2 <- function(x, y, xlab="", ylab=""){
  par(mar=c(5.1,5.1,4.1,2.1))
  plot(x, y,
       type="l",
       xlab=xlab,
       ylab=ylab,
       main="")
}

##################################################
df <- GetHPC()

png(filename="plot2.png", width=480, height=480)
plot2(x=df$dateTime, 
      y=df$Global_active_power, 
      ylab="Global Active Power (kilowatts)")
dev.off()
