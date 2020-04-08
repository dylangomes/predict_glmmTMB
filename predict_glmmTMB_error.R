library(glmmTMB)

IN<-read.csv("Predict_dataset.csv")
IN$Year<-as.factor(IN$Year)

scale2<-function(var){
  (var-mean(var))/(2*sd(var))
}

glmmLep<-glmmTMB(Lepidoptera ~ scale2(DayL50)*scale2(Med) +
                   scale2(yday)+ scale2(Veg)+
                   scale2(Elev)+scale2(Moon)+
                   scale2(Temp)+
                   Year, 
                 family=nbinom2(link="log"),
                 data=IN)
summary(glmmLep)

x<-seq(from=min(IN$DayL50),to=max(IN$DayL50),length.out = length(IN$DayL50))
newData<-matrix(nrow=length(IN$DayL50),ncol=8)
newData<-as.data.frame(newData)
names(newData)<-c("DayL50","Med","yday","Veg","Elev","Moon","Temp","Year")
newData$DayL50<-x
newData$Med<-mean(IN$Med)
newData$yday<-mean(IN$yday)
newData$Veg<-mean(IN$Veg)
newData$Elev<-mean(IN$Elev)
newData$Moon<-mean(IN$Moon)
newData$Temp<-mean(IN$Temp)
newData$Year<-"2018"
str(newData)

newData$Year<-as.factor(newData$Year)
na_check<-function(dat){
  length(which(is.na(dat)))
}
sapply(newData,na_check)

y<-predict(glmmLep,newdata=newData,type="response",se.fit = T)
