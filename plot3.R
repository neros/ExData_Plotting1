##Function: init()
##Description: This will download the necessary data, extract it and import it into R and filter
##the data frame according to the dates of interest.
init <- function()
{
     library(lubridate)
     
     if (!file.exists("./exdata-data-household_power_consumption.zip"))
     {
          print("Data file does not exist.  Downloading data")
          download.file(url="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile="./exdata-data-household_power_consumption.zip")
     }
     if (!file.exists("./household_power_consumption.txt"))
     {
          print("Extracting data from archive")
          unzip(zipfile="exdata-data-household_power_consumption.zip",exdir=".",junkpaths=TRUE)
     }
     print("Reading data from file")
     raw_hpc <- read.table(file="./household_power_consumption.txt", header=TRUE, sep=";", stringsAsFactors=FALSE)
     
     all_hpc <<- data.frame(date = dmy(raw_hpc$Date), time = hms(raw_hpc$Time), global_active_power = as.numeric(raw_hpc$Global_active_power), global_reactive_power = as.numeric(raw_hpc$Global_reactive_power), voltage = as.numeric(raw_hpc$Voltage), global_intensity = as.numeric(raw_hpc$Global_intensity), sub_met1 = as.numeric(raw_hpc$Sub_metering_1), sub_met2 = as.numeric(raw_hpc$Sub_metering_2), sub_met3 = as.numeric(raw_hpc$Sub_metering_3))
     rm(raw_hpc)
     
     hpc <<- all_hpc[all_hpc$date >= ymd("2007-02-01") & all_hpc$date <= ymd("2007-02-02"),]
     return(hpc)
}

##Function: do_plot3()
##Description: draws and writes to file plot type 3
do_plot3 <- function()
{
     if (!exists("hpc")) init()
     
     png(filename="./plot3.png", width=480, height=480, unit="px")
     plot(type="l", x=hpc$sub_met1, col="black", ylab="Energy sub metering", xlab="", xaxt="n")
     points(x=hpc$sub_met2,type="l",col="red")
     points(x=hpc$sub_met3,type="l",col="blue")
     axis(side=1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"), tick=TRUE)
     legend(x="topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("black","red","blue"), lty=1)
     dev.off()
}