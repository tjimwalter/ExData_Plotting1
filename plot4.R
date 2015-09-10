# This script implements plot #4 for assignment #1
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

############################################################
plot4 <- function(df){
  par(mar=c(5.1,5.1,4.1,2.1), mfrow=c(2,2))
  plot2(x=df$dateTime, 
        y=df$Global_active_power, 
        ylab="Global Active Power")
  plot2(x=df$dateTime, 
        y=df$Voltage,
        xlab="datetime",
        ylab="Voltage")
  plot3(df, bty="n")
  plot2(x=df$dateTime, 
        y=df$Global_reactive_power, 
        xlab="datetime",
        ylab="Global_reactive_power")
}

##################################################
df <- GetHPC()

png(filename="plot4.png", width=480, height=480)
plot4(df)
dev.off()
