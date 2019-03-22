##########Meta analysis for mare.dz.final##############
library(meta)
setwd("D:/Biostatistics/MSProject/planB/data")
datMA <- read.csv("mare.dz.final.csv")
View(datMA)
N<-length(unique(datMA$ma.id))
prop<- read.csv("propdata.dz.final.csv")
View(prop)
##without double zero events studies##
meta1<-list()
meta2<-list()
meta3<-list()
#meta4<-list()
meta5<-list()
meta6<-list()
#meta7<-list()
meta8<-list()
##with double zero events studies##
#meta1all<-list()
meta2all<-list()
meta3all<-list()
#meta4all<-list()
meta5all<-list()
meta6all<-list()
#meta7all<-list()
#meta8all<-list()

##fixed effect;without double zero events output##
TE.wodz.f<-matrix(NA,368,6)  ##point estimate, log odds ratio
#CI.wodz.f<-matrix(NA,368,12) ##confidence interval,lower and upper
#p.wodz.f<-matrix(NA,368,6)   ## pvalue
#study.wodz.f<-matrix(NA,368,6)##studies numbers
## fixed effect;with double zero events output##
#TE.all.f<-matrix(NA,368,4)   ##logoddsratio
CI.all.f<-matrix(NA,368,8)  ##CI,lower and upper
p.all.f<-matrix(NA,368,4)    ##pvalue
study.all.f<-matrix(NA,368,4) ##studies number

##random effect;without double zero events output##
TE.wodz.r<-matrix(NA,368,6)  ##point estimate, log odds ratio
CI.wodz.r<-matrix(NA,368,12) ##confidence interval,lower and upper
p.wodz.r<-matrix(NA,368,6)   ## pvalue

## random effect;with double zero events output,"I""MH""Peto" without exact1##
TE.all.r<-matrix(NA,368,4)   ##logoddsratio
CI.all.r<-matrix(NA,368,8)  ##CI,lower and upper
p.all.r<-matrix(NA,368,4)    ##pvalue



for (i in 1:368)
{
  temp<-datMA[datMA$ma.id==i,]
  #empirical CC R=Nc/Ne, Kc = R/(R+odds ratio), Ke = odds ratio/(R+odds ratio)#
  #temp$study.id<-seq(1,nrow(temp),1)
  
  
  ###meta-analysis##
  ##without double zero studies##
  meta1[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr=0,method="I",data=temp))
  meta2[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", method="I",data=temp))
  meta3[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr="TACC",method="I",data=temp))
  meta5[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", method="MH",data=temp))
  meta6[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr="TACC",method="MH",data=temp))
  meta8[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR",  incr=0, method="P",data=temp))
  ##with double zero studies##
  #meta1all[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr=0,allstudies=TRUE,method="I",data=temp))
  meta2all[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", allstudies=TRUE,method="I",data=temp))
  meta3all[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr="TACC",allstudies=TRUE,method="I",data=temp))
  meta5all[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", allstudies=TRUE,method="MH",data=temp))
  meta6all[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr="TACC",allstudies=TRUE,method="MH",data=temp))
  
  #meta8all[[i]]<-summary(gmeta(temp[,c("events_1","total_1","events_2","total_2")], gmi.type='2x2', method='exact1',
  #                                              gmo.xgrid=seq(-5,5,by=0.001), report.error=TRUE)) # log-odd-ratio
  
  
  
  TE.wodz.f[i,]<-cbind(round(meta1[[i]]$fixed$TE,3),round(meta2[[i]]$fixed$TE,3),round(meta3[[i]]$fixed$TE,3),round(meta5[[i]]$fixed$TE,3),round(meta6[[i]]$fixed$TE,3),round(meta8[[i]]$fixed$TE,3))
  CI.wodz.f[i,]<-cbind(meta1[[i]]$fixed$lower,meta1[[i]]$fixed$upper,meta2[[i]]$fixed$lower,meta2[[i]]$fixed$upper,meta3[[i]]$fixed$lower,meta3[[i]]$fixed$upper,meta5[[i]]$fixed$lower,meta5[[i]]$fixed$upper,meta6[[i]]$fixed$lower,meta6[[i]]$fixed$upper,meta8[[i]]$fixed$lower,meta8[[i]]$fixed$upper)
  p.wodz.f[i,]<-cbind(round(meta1[[i]]$fixed$p,3),round(meta2[[i]]$fixed$p,3),round(meta3[[i]]$fixed$p,3),round(meta5[[i]]$fixed$p,3),round(meta6[[i]]$fixed$p,3),round(meta8[[i]]$fixed$p,3))
  study.wodz.f[i,]<-cbind(meta1[[i]]$k,meta2[[i]]$k,meta3[[i]]$k,meta5[[i]]$k,meta6[[i]]$k,meta8[[i]]$k)
  
  TE.wodz.r[i,]<-cbind(round(meta1[[i]]$random$TE,3),round(meta2[[i]]$random$TE,3),round(meta3[[i]]$random$TE,3),round(meta5[[i]]$random$TE,3),round(meta6[[i]]$random$TE,3),round(meta8[[i]]$random$TE,3))
  CI.wodz.r[i,]<-cbind(meta1[[i]]$random$lower,meta1[[i]]$random$upper,meta2[[i]]$random$lower,meta2[[i]]$random$upper,meta3[[i]]$random$lower,meta3[[i]]$random$upper,meta5[[i]]$random$lower,meta5[[i]]$random$upper,meta6[[i]]$random$lower,meta6[[i]]$random$upper,meta8[[i]]$random$lower,meta8[[i]]$random$upper)
  p.wodz.r[i,]<-cbind(round(meta1[[i]]$random$p,3),round(meta2[[i]]$random$p,3),round(meta3[[i]]$random$p,3),round(meta5[[i]]$random$p,3),round(meta6[[i]]$random$p,3),round(meta8[[i]]$random$p,3))
  
  TE.all.f[i,]<-cbind(round(meta2all[[i]]$fixed$TE,3),round(meta3all[[i]]$fixed$TE,3),round(meta5all[[i]]$fixed$TE,3),round(meta6all[[i]]$fixed$TE,3))
  CI.all.f[i,]<-cbind(meta2all[[i]]$fixed$lower,meta2all[[i]]$fixed$upper,meta3all[[i]]$fixed$lower,meta3all[[i]]$fixed$upper,meta5all[[i]]$fixed$lower,meta5all[[i]]$fixed$upper,meta6all[[i]]$fixed$lower,meta6all[[i]]$fixed$upper)
  p.all.f[i,]<-cbind(round(meta2all[[i]]$fixed$p,3),round(meta3all[[i]]$fixed$p,3),round(meta5all[[i]]$fixed$p,3),round(meta6all[[i]]$fixed$p,3))
  study.all.f[i,]<-cbind(meta2all[[i]]$k,meta3all[[i]]$k,meta5all[[i]]$k,meta6all[[i]]$k)
  
  TE.all.r[i,]<-cbind(round(meta2all[[i]]$random$TE,3),round(meta3all[[i]]$random$TE,3),round(meta5all[[i]]$random$TE,3),round(meta6all[[i]]$random$TE,3))
  CI.all.r[i,]<-cbind(meta2all[[i]]$random$lower,meta2all[[i]]$random$upper,meta3all[[i]]$random$lower,meta3all[[i]]$random$upper,meta5all[[i]]$random$lower,meta5all[[i]]$random$upper,meta6all[[i]]$random$lower,meta6all[[i]]$random$upper)
  p.all.r[i,]<-cbind(round(meta2all[[i]]$random$p,3),round(meta3all[[i]]$random$p,3),round(meta5all[[i]]$random$p,3),round(meta6all[[i]]$random$p,3))
 
  
  print(i)
  
}
colnames(TE.wodz.f)<-colnames(TE.wodz.r)<-colnames(p.wodz.f)<-colnames(p.wodz.r)<-c("I0wodz","I5wodz","ITACCwodz","MH5wodz","MHTACCwodz","Petowodz")
colnames(CI.wodz.f)<-colnames(CI.wodz.r)<-c("I0wodzlower","I0wodzupper","I5wodzlower","I5wodzupper","ITACClower","ITACCupper","MH5wodzlower","MH5wodzupper","MHTACClower","MHTACCupper","Petolower","Petoupper")

colnames(TE.all.f)<-colnames(TE.all.r)<-colnames(p.all.f)<-colnames(p.all.r)<-paste(c("I5","ITACC","MH5","MHTACC"),"all",sep="")
colnames(CI.all.f)<-colnames(CI.all.r)<-c("I5alllower","I5allupper","ITACCalllower","ITACCallupper","MH5alllower","MH5allupper","MHTACCalllower","MHTACCallupper")

##dataframe with proportion,ma.id##
TE.all.f<-as.data.frame(TE.all.f)
TE.wodz.f<-as.data.frame(TE.wodz.f)
TE.all.r<-as.data.frame(TE.all.r)
TE.wodz.r<-as.data.frame(TE.wodz.r)

CI.all.f<-as.data.frame(CI.all.f)
CI.wodz.f<-as.data.frame(CI.wodz.f)
CI.all.r<-as.data.frame(CI.all.r)
CI.wodz.r<-as.data.frame(CI.wodz.r)

p.all.f<-as.data.frame(p.all.f)
p.wodz.f<-as.data.frame(p.wodz.f)
p.all.r<-as.data.frame(p.all.r)
p.wodz.r<-as.data.frame(p.wodz.r)

########add prop.dzstudy###########################################
TE.all.f$prop.dz<-prop$prop.dzstudy
TE.all.f$ma.id<-prop$ma.id
TE.all.r$prop.dz<-prop$prop.dzstudy
TE.all.r$ma.id<-prop$ma.id

TE.wodz.f$prop.dz<-prop$prop.dzstudy
TE.wodz.f$ma.id<-prop$ma.id
TE.wodz.r$prop.dz<-prop$prop.dzstudy
TE.wodz.r$ma.id<-prop$ma.id

CI.all.f$prop.dz<-prop$prop.dzstudy
CI.all.f$ma.id<-prop$ma.id
CI.all.r$prop.dz<-prop$prop.dzstudy
CI.all.r$ma.id<-prop$ma.id

CI.wodz.f$prop.dz<-prop$prop.dzstudy
CI.wodz.f$ma.id<-prop$ma.id
CI.wodz.r$prop.dz<-prop$prop.dzstudy
CI.wodz.r$ma.id<-prop$ma.id

p.all.f$prop.dz<-prop$prop.dzstudy
p.all.f$ma.id<-prop$ma.id
p.all.r$prop.dz<-prop$prop.dzstudy
p.all.r$ma.id<-prop$ma.id

p.wodz.f$prop.dz<-prop$prop.dzstudy
p.wodz.f$ma.id<-prop$ma.id
p.wodz.r$prop.dz<-prop$prop.dzstudy
p.wodz.r$ma.id<-prop$ma.id

write.csv(TE.all.f, "D:/Biostatistics/MSProject/planB/summary/TE.all.f.csv",
          row.names = FALSE)
write.csv(TE.all.r, "D:/Biostatistics/MSProject/planB/summary/TE.all.r.csv",
          row.names = FALSE)
write.csv(TE.wodz.f, "D:/Biostatistics/MSProject/planB/summary/TE.wodz.f.csv",
          row.names = FALSE)
write.csv(TE.wodz.r, "D:/Biostatistics/MSProject/planB/summary/TE.wodz.r.csv",
          row.names = FALSE)

write.csv(CI.all.f, "D:/Biostatistics/MSProject/planB/summary/CI.all.f.csv",
          row.names = FALSE)
write.csv(CI.all.r, "D:/Biostatistics/MSProject/planB/summary/CI.all.r.csv",
          row.names = FALSE)
write.csv(CI.wodz.f, "D:/Biostatistics/MSProject/planB/summary/CI.wodz.f.csv",
          row.names = FALSE)
write.csv(CI.wodz.r, "D:/Biostatistics/MSProject/planB/summary/CI.wodz.r.csv",
          row.names = FALSE)

write.csv(p.all.f, "D:/Biostatistics/MSProject/planB/summary/p.all.f.csv",
          row.names = FALSE)
write.csv(p.all.r, "D:/Biostatistics/MSProject/planB/summary/p.all.r.csv",
          row.names = FALSE)
write.csv(p.wodz.f, "D:/Biostatistics/MSProject/planB/summary/p.wodz.f.csv",
          row.names = FALSE)
write.csv(p.wodz.r, "D:/Biostatistics/MSProject/planB/summary/p.wodz.r.csv",
          row.names = FALSE)

##########################################################################
