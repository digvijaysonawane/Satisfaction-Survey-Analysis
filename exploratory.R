library(data.table)
library(plotly)
library(ggplot2)
survey= data.table::fread("C:/Users/aarth/Downloads/Satisfaction.csv")
#str(survey)

#### Removing Na's #####################
survey <- na.omit(survey)
#survey <- subset(survey,!is.na(survey$Satisfaction)) ### Subsetting data without NA's in satisfaction column
survey$Satisfaction <- as.double(survey$Satisfaction) ### Converting the data type of satisfaction to double

### Dimensions of Data####

dim(survey)

####### Exploratory Analysis ############

#### Type of reason for Travel #### 

travel_reason = data.frame(table(as.factor(survey$`Type of Travel`)))
names(travel_reason) <- c("Travel Type","Count")
travel_reason_plot <- plot_ly(travel_reason, x = travel_reason$`Travel Type`, y =travel_reason$Count, type = 'bar', name = 'Travel Reasons') 
travel_reason_plot


##### Type of Travel for Southeast ##########
southeast_airlines = subset(survey,survey$`Airline Name`=="Southeast Airlines Co.")
travel_reasons_southeast = data.frame(table(as.factor(southeast_airlines$`Type of Travel`)))
names(travel_reasons_southeast) <- c("Travel Type","Count")
travel_reasons_southeast_plot <- plot_ly(travel_reasons_southeast, x = travel_reasons_southeast$`Travel Type`, y =travel_reasons_southeast$Count, type = 'bar', name = 'Travel Reasons') 
travel_reasons_southeast_plot


###### Type of Class #####

southeast_class = data.frame(table(as.factor(southeast_airlines$Class)))
names(southeast_class) <- c("Class","Count")
southeast_class_plot <- plot_ly(southeast_class, x = southeast_class$Class,
                                y =southeast_class$Count, type = 'bar', name = 'Class')
southeast_class_plot


##### Since There are lot more people travelling in ECO class in SOutheast Airlines,
##### we are going to focus our analysis in this business segment

southeast_airlines_eco = subset(southeast_airlines,southeast_airlines$Class=="Eco")

#### Reasons for travelling in ECO on southeast Airlines ### 
reason_eco_southeast_count = data.frame(table(as.factor(southeast_airlines_eco$`Type of Travel`)))
names(reason_eco_southeast_count) <- c("Type of travel","Count")

southeast_reason_eco_count <- plot_ly(reason_eco_southeast_count, x = reason_eco_southeast_count$`Type of travel`,
                                      y =reason_eco_southeast_count$Count, type = 'bar', name = 'Travel Reasons')
southeast_reason_eco_count              

##################################### Frequency of travel by month ##########################
library(lubridate)
survey$`Flight date` <- mdy(survey$`Flight date`)
month_travel=month(survey$`Flight date`)
month_travel_freq= data.frame(table(month_travel))

month_travel_freq$month_travel <- c("Jan","Feb","March")
month_travel_plot = plot_ly(month_travel_freq, x = month_travel_freq$month_travel,
                            y =month_travel_freq$Freq, type = 'bar', name = 'Month Travel')

########## Business Travel in Economy in Southeast Airlines ########
southeast_airlines_eco_business<-subset(southeast_airlines_eco,southeast_airlines_eco$`Type of Travel`=='Business travel')
View(southeast_airlines_eco_business)

states_eco_business_southeast = tapply(southeast_airlines_eco_business$Satisfaction,southeast_airlines_eco_business$`Destination State`,mean)
states_eco_business_southeast
table(as.factor(southeast_airlines_eco_business$`Destination State`))
##########################################################################

################ 
eco_business<-subset(survey,survey$`Type of Travel`=='Business travel'& survey$Class=='Eco')
View(eco_business)
airlines_eco_business<-tapply(eco_business$Satisfaction,eco_business$`Airline Name`,mean)
airlines_eco_business

###
states_eco_business<-tapply(eco_business$Satisfaction,eco_business$`Destination State`,mean)
states_eco_business
##
eco<-subset(survey,survey$Class=='Eco')
nrow(eco)
eco_performance = tapply(eco$Satisfaction,eco$`Airline Name`,mean)
library(dplyr)
#### COUNT OF SURVEY ###
airline_rating_count<-survey%>%count(`Airline Name`,Satisfaction)
airline_rating_count

##### Ratings and Airline Status ##### 
ratings_airline <- data.frame(aggregate(survey$Satisfaction,by=list(survey$`Airline Status`),FUN=mean))
names(ratings_airline) <- c("Airline Status","Satisfaction")
ratings_airline_plot <- plot_ly(ratings_airline, x = ratings_airline$`Airline Status`,
                                y =ratings_airline$Satisfaction, type = 'bar', name = 'Month Travel')
ratings_airline_plot


########################################

####### Age Group Analysis #########

survey$Age <- if (survey$Age>0 & survey$Age<=30){
  survey$Age="Youth"
} else if (survey$Age>30 & survey$Age<=60){
  survey$Age="Adult "
} else {
  survey$Age="Senior"
}


############## Destination Flights Analysis ###################
flights_dest= data.frame(table(as.factor(survey$`Destination State`)))
names(flights_dest)=c("Destination","Count")
flights_dest
flights_dest_count <- plot_ly(flights_dest, x = flights_dest$Destination,
                              y =flights_dest$Count, type = 'bar', name = 'Month Travel')
flights_dest_count
################################################################


########### Time of month #################
survey$Age <- if(survey$`Day of Month`> 0 & survey$`Day of Month` <= 10){
  survey$`Day of Month`="starting"} else 
    if (survey$Age > 10 & survey$Age <= 20){
      survey$`Day of Month`="Middle "} else
      {
        survey$`Day of Month`="Last"}



##average departure delay for each airline
airline_departure_delay<- data.frame(aggregate(survey$`Departure Delay in Minutes`,by=list(survey$`Airline Name`),FUN=mean))
names(airline_departure_delay) <- c("Airline name","delay")
airline_departure_delay
airline_departure_delay_plot <- plot_ly(airline_departure_delay, x = airline_departure_delay$`Airline name`,y =airline_departure_delay$delay, type = 'bar', name = 'delay')
airline_departure_delay_plot

##average arrival delay for each airline
airline_arrival_delay<- data.frame(aggregate(survey$`Arrival Delay in Minutes`,by=list(survey$`Airline Name`),FUN=mean))
names(airline_arrival_delay) <- c("Airline name","delay")
airline_arrival_delay
airline_arrival_delay_plot <- plot_ly(airline_arrival_delay, x = airline_arrival_delay$`Airline name`,y =airline_arrival_delay$delay, type = 'bar', name = 'delay')
airline_arrival_delay_plot
##number of flights per arrival city
flights_per_arrival_city<- data.frame(aggregate(survey$`Airline Name`,by=list(survey$`Destination City`,survey$`Airline Name`),FUN=length))
names(flights_per_arrival_city) <- c("city","Airline name","count")
View(flights_per_arrival_city)

##southeast flies to charlotte, nc, phoenix, az, philly,PA most
southeast<-subset(survey,survey$`Airline Name`=='Southeast Airlines Co.')
se_destcity_count<-data.frame(southeast%>%count(`Destination City`))
names(se_destcity_count)<-c("destination city","count")
se_destcity_count

popularcities<-subset(survey, survey$`Destination City`=='Charlotte, NC'| survey$`Destination City`=='Phoenix, AZ'|survey$`Destination City`=='Philadelphia, PA')
popularcities<- data.frame(aggregate(survey$Satisfaction,by=list(survey$`Airline Name`),FUN=length))
popularcities
popularcities$avg<-aggregate(survey$Satisfaction,by=list(survey$`Airline Name`),FUN=mean)
popularcities