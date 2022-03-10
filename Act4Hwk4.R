######Activity 4-------- 
#Load data 
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A") 

metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")

sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")




#load packages 
#install.packages(c("dplyr","ggplot2","lubridate"))

library(dplyr)
library(ggplot2)
library(lubridate)


#removing data 
# parse date
weather$dateF <- mdy_hm(weather$Date)
# create a month column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()

#converting to NA
# add a column to weather:
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            # evaluate if the doy is between May 1 and July 7 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation

#create flag 
weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero
#checking data 
#interval data: look at first 2 observations as interval
# Time 1 %--% Time 2
weather$dateF[1] %--% weather$dateF[2]

# look at the interval length from the first to the second observation:
int_length(weather$dateF[1] %--% weather$dateF[2])

# set up intervals
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1] # start date %--% end date
# interval starts with row 1, and row 2
# and ends on second to last row and final row

# calculate interval times
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]

#create function for checking for irregular intervals that
# deviate from 900 seconds
# only argument is x, a vector of POSIXct formatted dates to be checked
# with an expected interval of 900 seconds
# the output is all observations that deviate from 900 seconds
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
# run on weather data
timeCheck900(weather$dateF)


#In class activity 
average <- function(x){     #name of function and any argument want to include
  x.no = na.omit (x)
  sum(x.no)/length(x.no)
}

average(weather$AirTemp)



######In Class prompt------ 
#prompt 1 
#expect longer days and brightest times 
#bird poop results in sensors reporting data close to night time 
#not a lot of concern that light is being blocked with solar radiation sensor
#may 
ggplot(data=weather[weather$doy >= 121 & weather$doy <= 151 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()

#june 
ggplot(data=weather[weather$doy > 152 & weather$doy < 181 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()

#July
ggplot(data=weather[weather$doy > 182 & weather$doy < 212 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()



#prompt 2
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
}

# run on weather data
timeCheck900(weather$dateF)

#minimal impact 
#don't try and fix dates know issues exist 

#interval 
intv <- weather$dateF[1] %--% weather$dateF[2]
int_length(weather$dateF[1] %--% weather$dateF[2])
intervals[1:6]
interval_times <- int_length(intervals)
intervals[interval_times !=900]





#####Homework Questions------
#Question 1

weather$precip_new <- ifelse(weather$AirTemp < 0 | 
                         weather$YLevel < -2 | 
                         weather$YLevel > 2 |
                         weather$XLevel < -2 |
                         weather$XLevel > 2,  
                         NA, weather$Precip)

length(weather$precip_new[is.na(weather$precip_new)])





#Question 2
weather$BatteryFlag <- ifelse(weather$BatVolt <= 8500, 1, 0)
list(weather$BatteryFlag)

#Question 3
#temperature function check 
TempCheck <- function(x){
  upper <- (32)
  lower <- (-28)
  length(x[x > upper | x < lower])
}

TempCheck(weather$AirTemp)



#solar radiation function check
SolCheck <- function(x) {
  upper = (1000)
  lower = (0)
  length(x[x > upper | x < lower])
}

SolCheck(weather$SolRad)

  

#Question 4
ggplot(data=weather[weather$doy >= 1 & weather$doy <= 90 ,],
       aes(x=dateF,
           y=AirTemp))+
  ggtitle("Air Temperature in 2021")+
  scale_x_datetime(limits = ymd_hms(c("2021-01-01 00:00:00", "2021-04-01 00:00:00"))) +
  geom_col(color="royalblue4")+
  xlab("Date(2021)")+
  ylab("Air Temperature")+
  theme_classic()







