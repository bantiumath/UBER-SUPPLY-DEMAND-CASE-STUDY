# UBER Assignment

#Including packages which are useful in Data Analysis
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)

#setting up working directory

#setwd("D:\\PGDDA\\Course 2\\Uber Assignment")

uber <- read.csv("Uber Request data.csv", header = T, stringsAsFactors = F)
str(uber)

#Converting Request ID and Driver ID into factor. It should be unique.

uber$Request.id <- as.factor(uber$Request.id)
uber$Driver.id <- as.factor(uber$Driver.id)

str(uber)

#Data Cleaning and Preparation
#Converting Date-Time format into standard R Date-Time format.

uber$Request.timestamp <- str_replace_all(uber$Request.timestamp, "/", "-")
index <- str_which(uber$Request.timestamp, "[:space:][:digit:]{1,2}:[:digit:]{1,2}$")
uber$Request.timestamp[index] <- paste(uber$Request.timestamp[index], ":00", sep ="" )
uber$Request.timestamp <- as.POSIXct(uber$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")

uber$Drop.timestamp <- str_replace_all(uber$Drop.timestamp, "/", "-")
index <- str_which(uber$Drop.timestamp, "[:space:][:digit:]{1,2}:[:digit:]{1,2}$")
uber$Drop.timestamp[index] <- paste(uber$Drop.timestamp[index], ":00", sep ="" )
uber$Drop.timestamp <- as.POSIXct(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")

#Fecthing extra derived column from Request.timestamp Column
uber$req_date <- format(uber$Request.timestamp,"%d")
uber$req_month <- format(uber$Request.timestamp,"%m")
uber$req_year <- format(uber$Request.timestamp,"%Y")
uber$req_hr <- format(uber$Request.timestamp,"%H")
uber$req_min <- format(uber$Request.timestamp,"%M")


#Calculating the Journey Time
uber$diff_time=difftime(uber$Drop.timestamp,uber$Request.timestamp, unit="mins")
summary(uber)
str(uber)

#Univariate Analysis
#Duplicate exists or not

nrow(uber)

apply(uber,2,function(x)length(unique(x)))

#Redundant column doesn't exists

#Keeping only useful columns.
uber <- uber[ ,c(1:6, 10,11)]

#Rounding the hour column if it is more than 30 minutes
uber <- mutate(uber, round.min = if_else(req_min>30,1,0))
uber$hour <- (as.numeric(uber$req_hr)+uber$round.min)%%24

uber <- uber[ ,c(-7:-9)]


#Bivariate Analysis
#Creating Plots to visualise the frequency of request that get Cancelled and No cars available 
uber1 <- uber[which(uber$Status == "Cancelled" | uber$Status == "No Cars Available"), ]
plot1 <- ggplot(uber1, aes(x = factor(Status)))
plot1 + geom_bar(fill = "slateblue3") + geom_text(stat = "count", aes(label = ..count..), vjust=-1,position = position_dodge(0.9)) + labs(title = "Frequency of Status for Cab Request",x="Cab Request in a day(hrs)") 
plot1 <- plot1 + aes(fill = Pickup.point)
plot1 + geom_bar(position = "dodge") + geom_text(stat = "count", aes(label=..count..), vjust=-1,position = position_dodge(0.9)) + labs(title = "Frequency of Request", x = "Status") 


#Finding out the demand every day and Identifying the peak hours through plot
plot2 <- ggplot(uber,aes(x=uber$hour))+geom_bar(fill = "slateblue3") + geom_text(stat = "count", aes(label=..count..), vjust= 0,position = position_dodge(0.9))
plot2 + labs(title = "Cab Request Pattern during the day(hrs)",subtitle= "Peak hours is between 5-10 & 18-22",x= "Cab Request in a day(hrs)")

#CANCELLATION AND NON CAR AVAILABILITY ARE HIGH DURING PEAK HOURS , DUE TO WHICH UBER IS LOOSING BUSINESS .
#CAB Request Pattern by Status
req_status <- ggplot(uber,aes(x = hour,fill = Status))+geom_bar() + labs(title = " Cab Request Pattern during the day", subtitle = "Peak hours is between 5:00-10:00 IST & 18:00-22:00 IST High Cab Unavailability",x="Cab Request in a day(hrs)")

req_status


#CAB Request Pattern by pickup_point 

req_pickup <- ggplot(uber,aes(x = hour,fill = Pickup.point)) + geom_bar() + labs(title = " Cab Request Pattern during the day", subtitle = "5:00-10:00 IST City to airport Request & 18:00-22:00 IST Airport to city Request",x="Cab Request in a day(hrs)")
req_pickup

#Analysing both graphs together
grid.arrange(req_status,req_pickup,ncol=2)

#We can break our hour in 4 time slots(from above plots where variation is more or less similar) w.r.t PLOT0:
#Morning_to_noon   5AM to 10AM
#Noon_to_evening  11AM to 4PM
#Evening_to_night   5PM to 10PM
#Night_to_morning 11PM to 4 AM
#I have used the rounded version of hours and minutes described in data cleaning section
uber <- mutate(uber,Time_slot = if_else(hour==23 | (hour>=0 & hour<5),'Night_to_Morning' 
                                     , if_else(hour>=5 & hour<11,'Morning_to_Noon'
                                               ,if_else(hour>=11 & hour<17,'Noon_to_Evening','Evening_to_Night'))))

demand_supply <- as.data.frame(uber %>% group_by(Status,Pickup.point,Time_slot)%>% summarize(count=n()))

demand <- as.data.frame(uber %>% group_by(Pickup.point,Time_slot)%>% summarize(count=n()))

supply <- subset(demand_supply,Status=='Trip Completed')

colnames(supply)[4] <- 'Supply'

colnames(demand)[3] <- 'Demand'

demand_supply <- cbind(demand,supply$Supply)

colnames(demand_supply)[4]='Supply'

demand_supply$demand_supply_gap <- demand_supply$Demand - demand_supply$Supply

demand_supply$demand_supply_gap_percent <- (demand_supply$demand_supply_gap*100)/demand_supply$Supply



plot3 <- ggplot(demand_supply, aes(x = Time_slot, y = demand_supply_gap,fill=Pickup.point))
plot3 <- plot3 + geom_bar(stat = "identity", width = 0.5, position = "dodge")  + scale_y_continuous(labels = percent_format())
plot3 + ggtitle('Demand-Supply Gap')


plot4 <- ggplot(demand_supply, aes(x = Time_slot, y = demand_supply_gap_percent,fill = Pickup.point))
#plot4 <- plot4 + geom_bar(stat = "identity", width = 0.5, position = "dodge") 
plot4 + ggtitle("Demand-Supply Gap Percentage")


##Infrence:
#By analysing plot 3-4, we come to know that Evening to night and morning to noon there is demand supply gap
#For "Evening to Night" Time Slot, the request "Airport to City" is most severe.
#for "Morning to Noon" Time Slot, the request "City to Airport" is more severe.




#CANCELLATION

#Testing hypothesis,  the cab cancellation pattern during morning peak hours holds good by
#analyzing per hour data during all days.

# filtering Status "Trip Cancelled" rows from uber_data to further analysis

cancelled_trip <-  uber %>% filter(Status == "Cancelled") %>% group_by(Driver.id)
write.csv(cancelled_trip, "cancel.csv")

#View(cancelled_trip)



gap_notrips <-
  ggplot(cancelled_trip, mapping = aes(x = Pickup.point, fill = Time_slot)) + 
  geom_bar() + theme(title = element_text(size=9, face="bold")) +
  labs(title = "Trip Cancellation duirng the Day from Pickup point",
       subtitle ="Inference: Trip Cancellations are high from city during peak morning hours (5-10AM)",
       x = "Trip Cancellations Pickup Point ",y ="count of Cab Request")


gap_days_notrips <-
  ggplot(data = cancelled_trip, mapping = aes(x = hour, fill = Time_slot)) + 
  geom_bar()+facet_wrap(~Pickup.point) + theme(title = element_text(size=9, face="bold")) +
  labs(title = "Trip Cancellation duirng the Day from Pickup point",
       subtitle ="Inference: Trip Cancellations are high from city during peak morning hours (5-10AM)",
       x = "Trip Cancellations Pickup Point ",y ="Total No. of Cab Request")

grid.arrange(gap_notrips, gap_days_notrips,ncol=2)

# clearly cancellations are high in the peak hours ( 5-10AM) morning time and high from city




#.................................................................................


#                  NO CABS AVAIALBLE.


# hypothesis testing,  the cab unavailability pattern during evening peak hours holds good 
# by analyzing per hour data during all days.

# ASSUMPTIONS
#Cab unavailable, When customer wants to book a trip but cab is unavailable. So Driver ID and
#Drop Time stamp data is unavailable for the analysis
#From the given Uber data set, we extracted data/rows related NO CAB AVAILABLE status only 
#and analyzed patterns on this data. Hence, data considered for plots in this slide consists
#of 'NO CAB AVAILABLE' data only

# filtering Status "No Cars Available" rows from uber_data to further analysis 

cabs_unavailable <- uber %>% filter(Status == "No Cars Available") 
write.csv(cabs_unavailable, "unavailable.csv")

#View(cabs_unavailable)
gap_no_uber <-
  ggplot(cabs_unavailable, mapping = aes(x = Pickup.point, fill = Time_slot)) + 
  geom_bar() + theme(title = element_text(size=9, face="bold")) +
  labs(title = "Cab Unavailability during Time slots To/from City/Airport", 
       subtitle ="Inference: Cab Unavailability is high during evening hours( 5 :00 PM to 10:00 PM) from Airport",
       x = "Pickup Point Unavailability", y = "Total No.of Cab Requests")


gap_days_no_uber <-
  ggplot(cabs_unavailable, mapping = aes(x = hour, fill = Time_slot)) + 
  geom_bar() + facet_wrap(~Pickup.point) + 
  theme(title = element_text(size=9, face="bold"))+
  labs(title = "Cab unavaialble during various days To/from City/Airport", 
       subtitle ="Inference: Cab Unavailability persists through all days in peak evening hours from Airport",
       x = "Pickup Point Unavailability", y = "Total No.of Cab Requests")


grid.arrange(gap_no_uber,gap_days_no_uber,ncol=2)


