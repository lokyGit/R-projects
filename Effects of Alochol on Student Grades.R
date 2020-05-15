################################
# ANALYZING ALCHOL CONSUMPTION
# EFFECT ON STUDENT GRADES
################################

library(ggplot2)
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("gridExtra")
library(gridExtra)
install.packages("alluvial")
library(alluvial)
install.packages("extrafont")
library(extrafont)

d1=read.table("E:/NEU/Classes/Summer 2019/Intermediate Analytics/Final Project/student-alcohol-consumption data set/student-mat.csv",sep=",",header=TRUE)
View(d1)
d2=read.table("E:/NEU/Classes/Summer 2019/Intermediate Analytics/Final Project/student-alcohol-consumption data set/student-por.csv",sep=",",header=TRUE)
View(d2)

help("rowMeans")


data.source=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus",
                             "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
                             "guardian","guardian","traveltime","studytime","failures",
                             "schoolsup","famsup","activities","higher","romantic",
                             "famrel","freetime","goout","Dalc","Walc","health","absences"))

help("merge")

print(nrow(data.source)) #85Rows

data.source$mathgrades=rowMeans(cbind(data.source$G1.x,data.source$G2.x,data.source$G3.x))
data.source$portgrades=rowMeans(cbind(data.source$G1.y,data.source$G2.y,data.source$G3.y))

#data.source$mathgrades
#data.source$portgrades

data.source$Dalc <- as.factor(data.source$Dalc)      

View(data.source)

data.source$Dalc <- as.factor(data.source$Dalc)      
data.source$Dalc <- mapvalues(data.source$Dalc, from = 1:5, to = c("Very Low", "Low", "Medium", "High", "Very High"))

data.source$Dalc


str1 = ggplot(data.source, aes(x=mathgrades, y=portgrades)) + geom_point(aes(colour=factor(Dalc)))+ scale_colour_hue(l=25,c=150)+ geom_smooth(method = "lm", se = FALSE)


data.source$Walc <- as.factor(data.source$Walc)      
data.source$Walc <- mapvalues(data.source$Walc, from = 1:5, to = c("Very Low", "Low", "Medium", "High", "Very High"))

str2=ggplot(data.source, aes(x=mathgrades, y=portgrades))+ geom_point(aes(colour=factor(Walc)))+ scale_colour_hue(l=25,c=150)+ geom_smooth(method = "lm", se = FALSE)

grid.arrange(str1,str2,nrow=2)


d3<-rbind(d1,d2) #combine the two datasets
# and eliminate the repeats:
d3norepeats<-d3 %>% distinct(school,sex,age,address,famsize,Pstatus,
                             Medu,Fedu,Mjob,Fjob,reason,
                             guardian,traveltime,studytime,failures,
                             schoolsup, famsup,activities,nursery,higher,internet,
                             romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)
#add a column with average grades (math or Portuguese, whichever is available)
d3norepeats$avggrades=rowMeans(cbind(d3norepeats$G1,d3norepeats$G2,d3norepeats$G3))
# and drop grades in 3 marking periods.
d3norepeats<-d3norepeats[,-(31:33)]
View(d3norepeats)


ggplot(d3norepeats, aes(x=Dalc, y=avggrades, group=Dalc))+
  geom_boxplot()+
  theme(legend.position="none")+
 # scale_fill_manual(values=waffle.col)+
  xlab("Daily Alcohol consumption")+
  ylab("Average Grades")+
  ggtitle("Average Grade")


failureind<-which(names(d3norepeats)=="failures")
d3norepeats<-d3norepeats[,-failureind]

failureind<-which(names(d3norepeats)=="failures")
d3norepeats<-d3norepeats[,-failureind]

# 1) multiple regression 
lm2<-lm(avggrades~., data=d3norepeats[,1:30])
 summary(lm2)

#2) Regression tree: 
library(rpart)
library(DMwR) 

rt2<-rpart(avggrades~., data=d3norepeats[,1:30])
prettyTree(rt2)

#predictions
lm.predictions<-predict(lm2,d3norepeats)
rt.predictions<-predict(rt2,d3norepeats)
nmse.lm<-mean((lm.predictions-d3norepeats[,"avggrades"])^2)/mean((mean(d3norepeats$avggrades)-d3norepeats[,"avggrades"])^2)
nmse.rt<-mean((rt.predictions-d3norepeats[,"avggrades"])^2)/mean((mean(d3norepeats$avggrades)-d3norepeats[,"avggrades"])^2)
print(nmse.lm) #0.79
print(nmse.rt) #0.85

lmpltdata1=data.frame(cbind(lm.predictions,d3norepeats[,"avggrades"]))
colnames(lmpltdata1)<-c("lm.predictions","avggrades")
rtpltdata1=data.frame(cbind(rt.predictions,d3norepeats[,"avggrades"]))
colnames(rtpltdata1)<-c("rt.predictions","avggrades")

d3norepeats$Dalc<-as.factor(d3norepeats$Dalc)

errplt.lt1=ggplot(lmpltdata1,aes(lm.predictions,avggrades))+
  geom_point(aes(color=d3norepeats[,"Dalc"]))+
  xlab("Predicted Grades (Linear Model)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")

errplt.rt1=ggplot(rtpltdata1,aes(rt.predictions,avggrades))+
  geom_point(aes(color=d3norepeats[,"Dalc"]))+
  xlab("Predicted Grades (Regression Tree)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")

grid.arrange(errplt.lt1,errplt.rt1,nrow=2)

library(randomForest)
set.seed(4543)
rf2<-randomForest(avggrades~., data=d3norepeats[,1:30], ntree=500, importance=T)
rf.predictions<-predict(rf2,d3norepeats)
View(rf.predictions)
nmse.rf<-mean((rf.predictions-d3norepeats[,"avggrades"])^2)/mean((mean(d3norepeats$avggrades)-d3norepeats[,"avggrades"])^2)
print(nmse.rf) #0.2

#combine the rf predictions and actual scores in a single data frame
rfpltdata1=data.frame(cbind(rf.predictions,d3norepeats[,"avggrades"]))
colnames(rfpltdata1)<-c("rf.predictions","avggrades")


#create the error plot.
errplt.rf1<-ggplot(rfpltdata1,aes(rf.predictions,avggrades))+
  geom_point(aes(color=d3norepeats[,"Dalc"]))+
  xlab("Predicted Grades (Random Forest with 500 Trees)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")
#plot the error plot from the random forest with the error plots of the linear and regression tree models.
grid.arrange(errplt.rf1, errplt.lt1,errplt.rt1,nrow=3)
varImpPlot(rf2,type=1)

imp <- importance(rf2)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 3))
for (i in seq_along(impvar)) {
  partialPlot(rf2, d3norepeats[,1:30], impvar[i], ,rug=TRUE, xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
  abline(h=mean(d3norepeats$avggrades),col="red")
}
par(op)


