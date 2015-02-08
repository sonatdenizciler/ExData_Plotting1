# The program creates a specific plot from household_power_consumption.txt.
# It assumes that the data file is in the working directory and writes the
# correspoding plot file in format png in the same directory.
# This program needs data.table package to run.
library(data.table)

plot1 <- function() 
{
  # first load data into variable hpc=housholdpowerconsumption
  hpc<-data.table(read.table("household_power_consumption.txt",
                             sep=";",
                             header=TRUE,
                             stringsAsFactors=FALSE,
                             na.strings="?"))
  
  # we only need 2 columns for this plot
  colsofinterest = c("Date","Global_active_power")
  hpc <- hpc[,colsofinterest,with=FALSE]
  
  # convert Date column from character into Date class, 
  # this will make searching for the dates easier
  hpc$Date <- as.Date(hpc$Date,format="%d/%m/%Y")
  
  # get the data for the 2 days in February 2007
  hpc <- hpc[Date=="2007-02-02" | Date=="2007-02-01"]
  
  # we need the Global_active_power data as numeric
  hpc$Global_active_power <- as.numeric(hpc$Global_active_power)
  
  # we create and write the plot into the png device
  png("plot1.png")
  hist(tmp$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
  dev.off()
}
