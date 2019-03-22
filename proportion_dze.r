##############################################################
###Proportion of double zero events/per MA in ma.re.dz####
##############################################################

setwd("D:/Biostatistics/MSProject/planB/data")
dat <- read.csv("ma.re.dz.csv")
###redefine ma.id for ma.reszonly dataset, ###
dat$ma.id <- rep(1:length(unique(dat$ma.id)), table(dat$ma.id))
View(dat)
N<-length(unique(dat$ma.id))

study.id<-list()
N.study<-NULL
N.dzstudy<-NULL
OR.I.5<-list()

for (i in 1:N)
{
  temp<-dat[dat$ma.id==i,]
  temp.dz<-temp[temp$events_1+temp$events_2==0,]
  N.dzstudy[i]<-nrow(temp.dz)
  study.id[[i]]<-seq(1,nrow(temp),1)
  
  #print(study.id.temp)
  N.study[i]<-length(study.id[[i]])
  
  ###meta-analysis##
  #OR.I.5[[i]]<-summary(metabin(events_2, total_2, events_1, total_1, sm="OR", method="I",data=temp))
  
}
N.study
N.dzstudy
prop.dzstudy<-N.dzstudy/N.study
dat$study.id<-unlist(study.id)
max(dat$study.id)  

plot(prop.dzstudy)
summary(prop.dzstudy)

##exclude 100% of prop.dzstudy## 
ID<-seq(1,384,1)
prop.data<-as.data.frame(cbind(ID,prop.dzstudy,N.study,N.dzstudy))
prop.data<-prop.data[prop.data$prop.dzstudy!=1.00,]

nrow(prop.data) ##368 MAs with dz events less than 100%.
##dataset: mare.dz.final with double zero events proprotion less than 100%, 368MA total##
mare.dz.final<-dat[dat$ma.id%in%prop.data$ID,]
View(mare.dz.final)
length(unique(mare.dz.final$ma.id)) ##368
mare.dz.final$ma.id<-rep(1:length(unique(mare.dz.final$ma.id)), table(mare.dz.final$ma.id))
write.csv(mare.dz.final, "D:/Biostatistics/MSProject/planB/data/mare.dz.final.csv",
          row.names = FALSE)

prop.data$ma.id<-seq(1,368,1)
write.csv(prop.data, "D:/Biostatistics/MSProject/planB/data/propdata.dz.final.csv",
          row.names = FALSE)