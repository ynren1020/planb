##########Exact Meta analysis for mare.dz.final##############
library(gmeta)
setwd("D:/Biostatistics/MSProject/planB/data")
datMA <- read.csv("mare.dz.final.csv")
View(datMA)
N<-length(unique(datMA$ma.id))

OR.exact<-list()
ORCI.exact<-matrix(NA,368,9)

for (i in 1:368) #may need server for a faster speed, otherwise may split this huge task into pieces by 1:5, 5:10 and so on...
{
  temp<-datMA[datMA$ma.id==i,]
  
  OR.exact[[i]] <- summary(gmeta(temp[,c("events_1","total_1","events_2","total_2")], gmi.type='2x2', method='exact1',
                      gmo.xgrid=seq(-5,5,by=0.001), report.error=FALSE)) # log-odd-ratio
  
  ORCI.exact[i,1]<-round(exp(OR.exact[[i]]$cmbd$mean),digits=3)
  ORCI.exact[i,2]<-round(exp(OR.exact[[i]]$cmbd$median),digits=3)
  ORCI.exact[i,3]<-round(exp(OR.exact[[i]]$cmbd$ci.lower),digits=3)
  ORCI.exact[i,4]<-round(exp(OR.exact[[i]]$cmbd$ci.upper),digits=3)
  ORCI.exact[i,5]<-round(OR.exact[[i]]$cmbd$mean,digits=3)
  ORCI.exact[i,6]<-round(OR.exact[[i]]$cmbd$median,digits=3)
  ORCI.exact[i,7]<-round(OR.exact[[i]]$cmbd$stddev,digits=3)
  ORCI.exact[i,8]<-round(OR.exact[[i]]$cmbd$ci.lower,digits=3)
  ORCI.exact[i,9]<-round(OR.exact[[i]]$cmbd$ci.upper,digits=3)
  print(i)
}

nrow(ORCI.exact)
colnames(ORCI.exact) <- c("mean","median","lower","upper","logmean","logmedian","std","loglower","logupper")
write.csv(ORCI.exact, "D:/Biostatistics/MSProject/planB/summary/exact2.csv",
          row.names = FALSE)
