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

##Function: do_plot1()
##Description: draws and writes to file plot type 1
do_plot1 <- function()
{
     png(filename="./plot1.png", width=480, height=480, unit="px")
     hist(x = hpc$global_active_power, col = "red", main = "Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency")
     dev.off()     
}

##Function: do_plot2()
##Description: draws and writes to file plot type 2
do_plot2 <- function()
{
     png(filename="./plot2.png", width=480, height=480, unit="px")
     plot(type="l", x=hpc$global_active_power, col="black", ylab="Global Active Power (kilowatts)", xlab="", xaxt="n")
     axis(side=1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"), tick=TRUE)
     dev.off()
}

##Function: do_plot3()
##Description: draws and writes to file plot type 3
do_plot3 <- function()
{
     png(filename="./plot3.png", width=480, height=480, unit="px")
     plot(type="l", x=hpc$sub_met1, col="black", ylab="Energy sub metering", xlab="", xaxt="n")
     points(x=hpc$sub_met2,type="l",col="red")
     points(x=hpc$sub_met3,type="l",col="blue")
     axis(side=1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"), tick=TRUE)
     legend(x="topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("black","red","blue"), lty=1)
     dev.off()
}

##Function: do_plot4()
##Description: draws and writes to file plot type 4
do_plot4 <- function()
{
     png(filename="./plot4.png", width=480, height=480, unit="px")
     par(mfrow=c(2,2))
 
     ##Global Active Power
     plot(type="l", x=hpc$global_active_power, col="black", ylab="Global Active Power", xlab="", xaxt="n")
     axis(side=1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"), tick=TRUE)
     
     ##Voltage
     plot(type="l", x=hpc$voltage, ylim=c(min(hpc$voltage,na.rm=TRUE),max(hpc$voltage,na.rm=TRUE)),xlim=c(0,length(hpc$voltage)),xaxt="n",xlab="datetime",ylab="Voltage")
     axis(side=1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"), tick=TRUE)     

     ##Sub metering
     plot(type="l", x=hpc$sub_met1, col="black", ylab="Energy sub metering", xlab="", xaxt="n")
     points(x=hpc$sub_met2,type="l",col="red")
     points(x=hpc$sub_met3,type="l",col="blue")
     axis(side=1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"), tick=TRUE)
     legend(x="topright", box.lty = "blank", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("black","red","blue"), lty=1, bg="transparent", cex=.95)
     
     
     ##Global Reactive Power
     plot(type="l", x=hpc$global_reactive_power, ylim=c(min(hpc$global_reactive_power,na.rm=TRUE),max(hpc$global_reactive_power,na.rm=TRUE)),xlim=c(0,length(hpc$voltage)),xaxt="n",xlab="datetime",ylab="Global_reactive_power")
     axis(side=1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"), tick=TRUE)     
     dev.off()
}

##Function: run()
##Description: will run the complete assignment.  Call this function from command line.
run <-function()
{
     print("Initializing Data")
     init()
     print("Drawing plot 1")
     do_plot1()
     print("Drawing plot 2")
     do_plot2()     
     print("Drawing plot 3")
     do_plot3()     
     print("Drawing plot 4")     
     do_plot4()     
     print("Completed")
}