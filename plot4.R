# The program creates a specific plot from household_power_consumption.txt.
# It assumes that the data file is in the working directory and writes the
# correspoding plot file in format png in the same directory.
# This program needs data.table package to run.
library(data.table)

plot4 <- function() 
{
  createplot1 <- function(hpc) {
    plot(hpc$date_time,
         hpc$Global_active_power, 
         type="l",
         main="", 
         ylab="Global Active Power", 
         xlab="")
  }
  
  createplot2 <- function(hpc) {
    plot(hpc$date_time,
         hpc$Voltage, 
         type="l",
         main="", 
         ylab="Voltage", 
         xlab="datetime")
  }
  
  createplot3 <- function(hpc) {
    plot(hpc$date_time,
         hpc$Sub_metering_1,
         type="l",
         main="",
         ylab="Energy sub metering",
         xlab="")
    lines(hpc$date_time,hpc$Sub_metering_2,type="l", col="red")
    lines(hpc$date_time,hpc$Sub_metering_3,type="l", col="blue")
    legtxts <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
    legcols <- c("black","red","blue")
    legend("topright",legtxts,col=legcols,cex=0.75,lty=1,bty="n")
  }
  
  createplot4 <- function(hpc) {
    plot(hpc$date_time,
         hpc$Global_reactive_power, 
         type="l",
         main="", 
         ylab="Global_reactive_power", 
         xlab="datetime")
  }
  
  # first load data into variable hpc=housholdpowerconsumption
  hpc<-data.table(read.table("household_power_consumption.txt",
                             sep=";",
                             header=TRUE,
                             stringsAsFactors=FALSE,
                             na.strings="?"))
  
  # get datetime by concatenating and converting Date and Time columns into POSIX datetime
  date_time <- as.POSIXct(strptime(paste(hpc$Date, hpc$Time),format="%d/%m/%Y %H:%M:%S"))
  
  # add this datetime as column to the data
  hpc<-cbind(date_time,hpc)
  
  # convert Date column from character into Date class, 
  # this will make searching for the dates easier
  hpc$Date <- as.Date(hpc$Date,format="%d/%m/%Y")

  # get the data for the 2 days
  hpc <- hpc[Date=="2007-02-02" | Date=="2007-02-01"]

  # we need the data as numeric
  hpc$Voltage               <- as.numeric(hpc$Voltage)
  hpc$Sub_metering_1        <- as.numeric(hpc$Sub_metering_1)
  hpc$Sub_metering_2        <- as.numeric(hpc$Sub_metering_2)
  hpc$Sub_metering_3        <- as.numeric(hpc$Sub_metering_3)
  hpc$Global_active_power   <- as.numeric(hpc$Global_active_power)
  hpc$Global_reactive_power <- as.numeric(hpc$Global_reactive_power)
  
  png("plot4.png")
  par(mfrow=c(2,2))
  createplot1(hpc)
  createplot2(hpc)
  createplot3(hpc)
  createplot4(hpc)
  dev.off()
}
