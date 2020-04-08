Dat<-data.frame(R=rpois(1000,1),C1=rnorm(1000,10,1),C2=rnorm(1000,5,2))

head(Dat)

mod1<-glmmTMB(R~C1*C2,family=poisson,data=Dat)

x<-seq(from=min(Dat$C1),to=max(Dat$C1),length.out = length(Dat$C1))
newData<-matrix(nrow=length(Dat$C1),ncol=2)
newData<-as.data.frame(newData)
names(newData)<-c("C1","C2")
newData$C1<-x
newData$C2<-5


y<-predict(mod1,newData,type="response",se.fit = T)
