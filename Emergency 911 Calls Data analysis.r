#########################################
######title: 'Emergency 911 Calls EDA'
#########################################

library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggmap)
library(viridis)
library(plotly)
install.packages("gridExtra")
library(gridExtra)
### Sneak peak into the dataset



callData <- read.csv("/Users/lokeshpalacharla/Library/Mobile Documents/com~apple~CloudDocs/NEU/Classes/Fall 2019/Data Mining /FInal Project/911.csv")

dim(callData)

str(callData)

summary(callData)




# Zip should be a factor
# time stamp should be of type date 
# Can remove dummy variable if it is always 1

### Cleaning the dataset

# Changing the zip class
# Removing the dummy variable
# Changing the class of timestamp


callData$zip <- factor(callData$zip)

callData <- callData[,-9]

callData$timeStamp <- as.POSIXct(callData$timeStamp)

callData$Date <- as.Date(callData$timeStamp)



callData <- separate(callData, col = title, into = c("Type", "SubType"), sep = ":")

callData$SubType <- gsub(" -", "", callData$SubType)



### Creating new variables

# Creating type and subtype based on the title column
# Creating the Year and Month from the timestemp column


callData$Year <- year(callData$timeStamp)
callData$Month <- month(callData$timeStamp)
callData$Day <- day(callData$timeStamp)
callData$Hour <- hour(callData$timeStamp)
callData$Weekday <- weekdays(callData$timeStamp)

callData$Year <- factor(callData$Year)
callData$Month <- factor(callData$Month)
callData$Day <- factor(callData$Day)
callData$Hour <- factor(callData$Hour)
callData$Weekday <- factor(callData$Weekday)

callData <- callData[,-7]

View(callData)

### Number of calls over the period of time and by type?


calls_by_date <- callData %>% group_by(Date) %>% summarise(Total = n())

head(calls_by_date)

View(calls_by_date)

ggplot(calls_by_date, aes(Date, Total)) + geom_line(color = "dodgerblue3", size = 1) + theme_bw()

calls_by_date_type <- callData %>% group_by(Date, Type) %>% summarise(Total = n())

View(calls_by_date_type)

calls_by_date_type$Type <- factor(calls_by_date_type$Type)

ggplot(calls_by_date_type, aes(Date, Total)) + geom_line( aes(color = Type), size = 0.6) + theme_bw()

ggplot(calls_by_date_type, aes(Date, Total)) + geom_line( aes(color = Type), size = 0.6) + facet_wrap(~Type) + theme(legend.position="none") + theme_bw()


# Only few times Traffic calls have exceeded the EMS calls
# Only on Jan 23 2016 number of traffic calls has almost gone up by 3 to 4 times.

### How many calls hourly, monthly and yearly?



table(callData$Year)

table(callData[callData$Year==2016,]$Month)

table(callData[callData$Year==2016,]$Month)
#2016
ggplot(callData[callData$Year==2016,], aes(Month, fill = Month)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 - Monthly ")+theme_bw()

#2015 had only december month data

#2017
ggplot(callData[callData$Year==2017,], aes(Month, fill = Month)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2017 - Monthly ")+theme_bw()
#2018
ggplot(callData[callData$Year==2018,], aes(Month, fill = Month)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2018 - Monthly ")+theme_bw()

#hourly trend
ggplot(callData, aes(Hour, fill = Hour)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls - Hourly") +theme_bw()

#weekday trend
ggplot(callData, aes(Weekday, fill = Weekday)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls - Weekday") + theme_bw()




# First table gave information about calls year wise but when looked at the data more closer found that 2015 records have only calls of December.
# January has highest number of calls so far this year with July being the second 
# Majority of the calls are during day time.

### How many calls based on type?



table(callData$Type)

ggplot(callData, aes(Type, fill = Type)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls by Type")+ theme_bw()

prop.table(table(callData$Type))

ggplot(as.data.frame(prop.table(table(callData$Type))), aes(Var1, Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 - Type Percentage") + xlab("Type") + ylab("Percentage of Calls") + theme_bw()




# Nearly 50% of the calls are of Type EMS
# Traffic related come second.

### How many calls from each subtypes in overall dataset and also subtypes within each type?



top_subtypes_calls <- as.data.frame(table(callData$SubType))
top_subtypes_calls <- top_subtypes_calls[order(-top_subtypes_calls$Freq),]
top10_subtypes_calls <- top_subtypes_calls[1:10,]
top10_subtypes_calls$Perc <- top10_subtypes_calls$Freq/sum(top_subtypes_calls$Freq) * 100
top10_subtypes_calls

ggplot(top10_subtypes_calls, aes(reorder(Var1, Freq), Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls") + xlab("Subtype") + ylab("Number of 911 Calls")  + coord_flip() + theme_bw()

ggplot(top10_subtypes_calls, aes(reorder(Var1, Perc), Perc, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls") + xlab("Subtype") + ylab("Percentage of 911 Calls")  + coord_flip()+ theme_bw()


gettop10subtypes <- function(type) {
  mytype <- subset(callData, Type == type)
  mytype$SubType <- factor(mytype$SubType)
  mytype_subtypes <- as.data.frame(table(mytype$SubType))
  mytype_subtypes <- mytype_subtypes[order(-mytype_subtypes$Freq),]
  top10_types_substype <- mytype_subtypes[1:10,]
  top10_types_substype$Perc <- top10_types_substype$Freq/sum(mytype_subtypes$Freq) * 100
  return(top10_types_substype)
}

gettop10subtypes("EMS")

ggplot(gettop10subtypes("EMS"), aes(reorder(Var1, Freq), Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016") + xlab("Subtype") + ylab("Number of EMS Calls")  + coord_flip()

ggplot(gettop10subtypes("EMS"), aes(reorder(Var1, Perc), Perc, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016") + xlab("Subtype") + ylab("Percentage of EMS Calls")  + coord_flip()


gettop10subtypes("Fire")

ggplot(gettop10subtypes("Fire"), aes(reorder(Var1, Freq), Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 ") + xlab("Subtype") + ylab("Number of Fire Calls")  + coord_flip()

ggplot(gettop10subtypes("Fire"), aes(reorder(Var1, Perc), Perc, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 ") + xlab("Subtype") + ylab("Percentage of Fire Calls")  + coord_flip()

gettop10subtypes("Traffic")[1:7,]

ggplot(gettop10subtypes("Traffic")[1:7,], aes(reorder(Var1, Freq), Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016") + xlab("Subtype") + ylab("Number of Traffic Calls")  + coord_flip()

ggplot(gettop10subtypes("Traffic")[1:7,], aes(reorder(Var1, Perc), Perc, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 ") + xlab("Subtype") + ylab("Percentage of Traffic Calls")  + coord_flip()



# Nearly 29% of all the 911 calls are for Vehicle accident. 
# Nearly 8% are for Disabled vehicle which comes second in overall calls.
# EMS alone has 72 subtypes
# Respiratory and cardiac emergency are the top subtypes among EMS
# Out of all the calls for EMS, Respiratory and Cardiac contribute to 10% of the calls.
# Fall Victim coming third
# Fire alarm calls contribute 37% of the Fire related calls
# Vehicle accidents contribute 11% of Fire calls
# Vehicle accidents contribute 65% of the Traffic calls with disabled vehicles coming second with 21%

### Which zip codes have the highest number of calls?


top_zip <- as.data.frame(table(callData$zip))
top_zip <- top_zip[order(-top_zip$Freq),]
top10_zip <- top_zip[1:10,]

names(top10_zip) <- c("Zip", "Total")
top10_zip$Perc <- top10_zip$Total/sum(top_zip$Freq) * 100
top10_zip$Zip <- factor(top10_zip$Zip)

top10_zip

ggplot(top10_zip, aes(reorder(Zip, -Total), Total, fill = Zip)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls - Top 10 Zip ") + xlab("Zip codes with most number of calls") + theme_bw()

ggplot(top10_zip, aes(reorder(Zip, -Perc), Perc, fill = Zip)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls - Top 10 Zip ") + xlab("Zip codes  Percentage of calls")



# Around 45% of the calls are from the top 20 zip codes
# It will be interesting to know the population of all the zip codes to find if there is any relationship between population and count of calls.


### Which township contributes to majority of the calls?




length(unique(callData$twp))

top_twp <- as.data.frame(table(callData$twp))
top_twp <- top_twp[order(-top_twp$Freq),]
top10_twp <- top_twp[1:10,]

names(top10_twp) <- c("Twp", "Total")
top10_twp$Perc <- top10_twp$Total/sum(top_twp$Freq) * 100
top10_twp$Twp <- factor(top10_twp$Twp)

top10_twp

ggplot(top10_twp, aes(reorder(Twp, -Total), Total, fill = Twp)) + geom_bar(stat = "identity")  + ggtitle("911 Emergency Calls- Top 10 Townships ") + xlab("Townships with most number of calls")  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))

ggplot(top10_twp, aes(reorder(Twp, -Perc), Perc, fill = Twp)) + geom_bar(stat = "identity") + ggtitle("911 Emergency Calls- Top 10 Townships ") + xlab("Townships Percentage of calls")  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5)) 



# Out of 69 townships, top 10 contribute around 48% of the calls
# It would be interesting to know the developmental ratings of these townships for getting more insights


### HeatMaps

# Generating heatmaps of calls by Month, day and hour.



## Get the count of the calls by Month and Hour
day_hour <- callData[callData$Year == "2016", c("Day", "Hour")] %>% group_by(Day, Hour) %>% summarise(Count = n())
day_hour <- as.data.frame(day_hour)

## Change the type of the variables
day_hour$Day <- as.factor(day_hour$Day)
day_hour$Hour <- as.factor(day_hour$Hour)

## Building heatmap using ggplot2
ggplot(day_hour, aes(Day, Hour, fill = Count)) + geom_tile(color = "white", size = 0.1) + scale_fill_viridis(name="#Calls") + coord_equal() + labs(title="911 Calls by Day and Hour") 


## Get the count of the calls by Month and Day
month_day <- callData[callData$Year == "2016", c("Month", "Day")] %>% group_by(Month, Day) %>% summarise(Count = n())
month_day <- as.data.frame(month_day)

## Change the type of the variables
month_day$Month <- as.factor(month_day$Month)
month_day$Day <- as.factor(month_day$Day)

## Change the levels of the Month 
levels(month_day$Month) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

## Building heatmap using ggplot2
ggplot(month_day, aes(Day, Month, fill = Count)) + geom_tile(color = "white", size = 0.1) + scale_fill_viridis(name="#Calls") + coord_equal() + labs(title="911 Calls by Month and Day") 



# Majority of the calls are during the day time.
# There are days when the calls were huge in number specially Jan 23.
# When you look closer at what hour of Jan 23 you find that around 4 and 5 in the evening there were huge number of calls.


### How does the type vary among top 10 township?


ggplot(callData[callData$twp %in% top10_twp$Twp, ], aes(twp, fill = Type )) + geom_bar(position = "dodge")  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))  + xlab("Township") + ggtitle("Type of calls among Top 10 Townships")

ggplot(callData[callData$twp %in% top10_twp$Twp, ], aes(twp, fill = Type)) + geom_bar()  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))  + xlab("Township") + ggtitle("Type of calls among Top 10 Townships") + facet_wrap(~Type) + theme(legend.position="none")



# Lower and Upper Merion have highest number of Traffic calls
# Norristown has highest number of EMS calls
# Lower Merion also has highest number of Fire calls.
# Is there a infrastructure problem in Lower and Upper Merion? Or Is the traffic calls due to reckless driving by large number of people in those areas? 


### How does the Top 5 subtypes vary among top 10 twp?



top3 <- top10_subtypes_calls[1:5,]$Var1

sample <- callData[callData$twp %in% top10_twp$Twp,]
sample <- sample[sample$SubType %in% top3, ]
dim(sample)

ggplot(sample, aes(twp, fill = SubType )) + geom_bar(position = "dodge")  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))  + xlab("Township") + ggtitle("Subtype distribution among Top 10 Townships")



# Vehicle accidents are huge among all the townships which is very concerning given the number of lives at risk during each accident

### Vehicle accidents by hour, day and type


vehicle <- callData[callData$SubType ==" VEHICLE ACCIDENT", ]

table(vehicle$Type)

ggplot(vehicle, aes(Hour, fill = Type)) + geom_bar(position = "dodge") + ggtitle("Vehicle Accidents by Hours")

day_hour <- vehicle[vehicle$Year == "2016", c("Day", "Hour")] %>% group_by(Day, Hour) %>% dplyr::summarise(Count = n())
day_hour <- as.data.frame(day_hour)

day_hour$Day <- as.factor(day_hour$Day)
day_hour$Hour <- as.factor(day_hour$Hour)

ggplot(day_hour, aes(Day, Hour, fill = Count)) + geom_tile(color = "white", size = 0.1) + scale_fill_viridis(name="#Calls") + coord_equal() + labs(title="Vehicle Accident Calls by Day and Hour - 2016")



# Majority of the Vehicle Accident calls are during the evening peak hours.
# People should be educated about the rush hour accidents and make them drive with more caution


### CARDIAC EMERGENCY by hour and day



cardiac <- callData[callData$SubType ==" CARDIAC EMERGENCY", ]

day_hour <- cardiac[cardiac$Year == "2016", c("Day", "Hour")] %>% group_by(Day, Hour) %>% dplyr::summarise(Count = n())
day_hour <- as.data.frame(day_hour)
 View(day_hour)

day_hour$Day <- as.factor(day_hour$Day)
day_hour$Hour <- as.factor(day_hour$Hour)

ggplot(day_hour, aes(Day, Hour, fill = Count)) + geom_tile(color = "white", size = 0.1) + scale_fill_viridis(name="#Calls") + coord_equal() + labs(title="CARDIAC EMERGENCY Calls by Day and Hour - 2016") 



# Majority of the Cardiac emergencies are during day time mostly middle part of the day.
# Is it because people are more stressed out during day time?



day_hour <- callData[callData$Year == "2016", c("Type","Year","Month","Day", "Hour")] %>% group_by(Type,Year,Month,Day, Hour) %>% dplyr::summarise(Count = n())
day_hour <- as.data.frame(day_hour)
tail(day_hour)

#multiple regression

lm2<- lm(Count~., data = day_hour[,3:6])

summary(lm2)

#regression tree

library(rpart)
library(DMwR) 

rt2<-rpart(Count~., data=day_hour)
prettyTree(rt2)
summary(rt2)


#predictions
lm.predictions<-predict(lm2,day_hour)
rt.predictions<-predict(rt2,day_hour)
nmse.lm<-mean((lm.predictions-day_hour[,"Count"])^2)/mean((mean(day_hour$Count)-day_hour[,"Count"])^2)
nmse.rt<-mean((rt.predictions-day_hour[,"Count"])^2)/mean((mean(day_hour$Count)-day_hour[,"Count"])^2)
print(nmse.lm) # 0.7919719
print(nmse.rt) # 0.5363266

lmpltdata1=data.frame(cbind(lm.predictions,day_hour[,"Count"]))
colnames(lmpltdata1)<-c("lm.predictions","Count")
rtpltdata1=data.frame(cbind(rt.predictions,day_hour[,"Count"]))
colnames(rtpltdata1)<-c("rt.predictions","Count")

day_hour$Type<-as.factor(day_hour$Type)

errplt.lt1=ggplot(lmpltdata1,aes(lm.predictions,Count))+
  geom_point(aes(color=day_hour[,"Type"]))+
  xlab("Predicted Count of calls (Linear Model)")+
  ylab("Actual Count of calls")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+theme_bw()+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Emergency call type's Linear Regression")
errplt.lt1

errplt.rt1=ggplot(rtpltdata1,aes(rt.predictions,Count))+
  geom_point(aes(color=day_hour[,"Type"]))+
  xlab("Predicted Count of calls (Regression Tree Model)")+
  ylab("Actual Count of calls")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+ theme_bw()+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Emergency call type's Regression Tree")
errplt.rt1


#Random Forests
library(randomForest)
set.seed(4543)
rf2<-randomForest(Count~., data=day_hour[,1:6], ntree=500, importance=T)
rf2
rf.predictions<-predict(rf2,day_hour)
nmse.rf<-mean((rf.predictions-day_hour[,"Count"])^2)/mean((mean(day_hour$Count)-day_hour[,"Count"])^2)
print(nmse.rf) # 0.6697675



rfpltdata1=data.frame(cbind(lm.predictions,day_hour[,"Count"]))
colnames(rfpltdata1)<-c("rf.predictions","Count")


errplt.rf1=ggplot(rfpltdata1,aes(lm.predictions,Count))+
  geom_point(aes(color=day_hour[,"Type"]))+
  xlab("Predicted Count of calls (Random forest)")+
  ylab("Actual Count of calls")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+ theme_bw()+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Emergency call type's Random forest")

errplt.rf1

grid.arrange(errplt.rf1, errplt.lt1,errplt.rt1,nrow=3)

varImpPlot(rf2,type=1)
