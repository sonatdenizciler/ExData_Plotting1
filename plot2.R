# The program creates a specific plot from household_power_consumption.txt.
# It assumes that the data file is in the working directory and writes the
# correspoding plot file in format png in the same directory.
# This program needs data.table package to run.
library(data.table)

plot2 <- function() 
{
  # first load data into variable hpc=housholdpowerconsumption
  hpc<-data.table(read.table("household_power_consumption.txt",
                             sep=";",
                             header=TRUE,
                             stringsAsFactors=FALSE,
                             na.strings="?"))
  
  # we only need 3 columns
  colsofinterest = c("Date","Time","Global_active_power")
  hpc <- hpc[,colsofinterest,with=FALSE]
  
  # get datetime by concatenating and converting Date and Time columns into POSIX datetime
  date_time <- as.POSIXct(strptime(paste(hpc$Date, hpc$Time),format="%d/%m/%Y %H:%M:%S"))
  
  # add this datetime as column to the data
  hpc<-cbind(date_time,hpc)
  
  # convert Date column from character into Date class, 
  # this will make searching for the dates easier
  hpc$Date <- as.Date(hpc$Date,format="%d/%m/%Y")

  # get the data for the 2 days
  hpc <- hpc[Date=="2007-02-02" | Date=="2007-02-01"]

  # we need the Global_active_power data as numeric
  hpc$Global_active_power <- as.numeric(hpc$Global_active_power)
  
  png("plot2.png")
  plot(hpc$date_time,
       hpc$Global_active_power, 
       type="l",
       main="", 
       ylab="Global Active Power (kilowatts)", 
       xlab="")
  dev.off()
}
