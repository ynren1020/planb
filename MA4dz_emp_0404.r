##################################
#####MA with CC=EMP###############
#####04042017#####################
library(meta)
#library(reshape2)
#library(ggplot2)
#library(BlandAltmanLeh)
setwd("D:/Biostatistics/MSProject/planB/data")
datMA <- read.csv("mare.dz.final.csv")
View(datMA)
N<-length(unique(datMA$ma.id))

##add column R=Nc/Ne##
datMA$R<-datMA$total_2/datMA$total_1

#######################
#datMA.OR<-datMA[datMA$events_1!=0,]
#datMA.OR<-datMA.OR[datMA.OR$events_2!=0,]
#View(datMA.OR)
#N.OR<-length(unique(datMA.OR$ma.id))#325
OR.peto<-list()
OR.wodz<-matrix(NA,368,2)
N.study<-NULL
for (i in 1:368)
{
  temp<-datMA[datMA$ma.id==i,]
  N.study[i]<-nrow(temp)
  ###meta-analysis##
  ##without double zero studies##
  OR.peto[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR",  incr=0,method="P",data=temp))
  
  OR.wodz[i,]<-cbind(round(exp(OR.peto[[i]]$fixed$TE),3),round(exp(OR.peto[[i]]$random$TE),3))
  
  print(i)
}

OR.wodz<-as.data.frame(OR.wodz)
colnames(OR.wodz)<-c("OR.peto.f","OR.peto.r")
head(OR.wodz)
datMA$OR.peto.f<-rep(OR.wodz$OR.peto.f,N.study)
datMA$OR.peto.r<-rep(OR.wodz$OR.peto.r,N.study)
#empirical CC R=Nc/Ne, Kc = R/(R+odds ratio), Ke = odds ratio/(R+odds ratio)#
datMA$Kc.f<-datMA$R/(datMA$R+datMA$OR.peto.f)
datMA$Ke.f<-datMA$OR.peto.f/(datMA$R+datMA$OR.peto.f)
datMA$Kc.r<-datMA$R/(datMA$R+datMA$OR.peto.r)
datMA$Ke.r<-datMA$OR.peto.r/(datMA$R+datMA$OR.peto.r)

###subset datMA without double zero events study##
datMA.wodz<-datMA[datMA$events_1+datMA$events_2!=0,]

N.wodz<-length(unique(datMA.wodz$ma.id))
##Add EMP based on fixed effect model of peto, use Ke.f,kc.f##
for (i in 1:nrow(datMA.wodz))
{
  if (datMA.wodz[i,"events_1"]==0 | datMA.wodz[i,"events_2"]==0)
  {
    datMA.wodz[i,"events_1"]<-datMA.wodz[i,"events_1"]+datMA.wodz[i,"Ke.f"]
    datMA.wodz[i,"events_2"]<-datMA.wodz[i,"events_2"]+datMA.wodz[i,"Kc.f"]
    datMA.wodz[i,"total_1"]<-datMA.wodz[i,"total_1"]+2*datMA.wodz[i,"Ke.f"]
    datMA.wodz[i,"total_2"]<-datMA.wodz[i,"total_2"]+2*datMA.wodz[i,"Kc.f"]
  }
  
  
}
View(datMA.wodz) ##add EMP to single arm zero events studies##

##meta analysis for datMA.wodz with EMP added##
meta1.wodz<-list()
meta2.wodz<-list()
TE.wodzemp.f<-matrix(NA,368,2)
TE.wodzemp.r<-matrix(NA,368,2)
CI.wodzemp.f<-matrix(NA,368,4)
CI.wodzemp.r<-matrix(NA,368,4)
p.wodzemp.f<-matrix(NA,368,2)
p.wodzemp.r<-matrix(NA,368,2)
study.wodzemp.f<-matrix(NA,368,2)
for (i in 1:368)
{
  temp<-datMA.wodz[datMA.wodz$ma.id==i,]
  
  meta1.wodz[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr=0,method="I",data=temp))
  meta2.wodz[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr=0,method="MH",data=temp))
  
  TE.wodzemp.f[i,]<-cbind(round(meta1.wodz[[i]]$fixed$TE,3),round(meta2.wodz[[i]]$fixed$TE,3))
  CI.wodzemp.f[i,]<-cbind(meta1.wodz[[i]]$fixed$lower,meta1.wodz[[i]]$fixed$upper,meta2.wodz[[i]]$fixed$lower,meta2.wodz[[i]]$fixed$upper)
  p.wodzemp.f[i,]<-cbind(round(meta1.wodz[[i]]$fixed$p,3),round(meta2.wodz[[i]]$fixed$p,3))
  study.wodzemp.f[i,]<-cbind(meta1.wodz[[i]]$k,meta2.wodz[[i]]$k)
  
  TE.wodzemp.r[i,]<-cbind(round(meta1.wodz[[i]]$random$TE,3),round(meta2.wodz[[i]]$random$TE,3))
  CI.wodzemp.r[i,]<-cbind(meta1.wodz[[i]]$random$lower,meta1.wodz[[i]]$random$upper,meta2.wodz[[i]]$random$lower,meta2.wodz[[i]]$random$upper)
  p.wodzemp.r[i,]<-cbind(round(meta1.wodz[[i]]$random$p,3),round(meta2.wodz[[i]]$random$p,3))
  
  print(i)
  
}

colnames(TE.wodzemp.f)<-colnames(TE.wodzemp.r)<-colnames(p.wodzemp.f)<-colnames(p.wodzemp.r)<-paste("EMP.wodz",c("Inverse","MH"),sep="")
colnames(CI.wodzemp.f)<-colnames(CI.wodzemp.r)<-c("EMP.wodz.I.lower","EMP.wodz.I.upper","EMP.wodz.MH.lower","EMP.wodz.MH.upper")
TE.wodzemp.f<-as.data.frame(TE.wodzemp.f)
TE.wodzemp.r<-as.data.frame(TE.wodzemp.r)
p.wodzemp.f<-as.data.frame(p.wodzemp.f)
p.wodzemp.r<-as.data.frame(p.wodzemp.r)
CI.wodzemp.f<-as.data.frame(CI.wodzemp.f)
CI.wodzemp.r<-as.data.frame(CI.wodzemp.r)

write.csv(TE.wodzemp.f, "D:/Biostatistics/MSProject/planB/data/TE.wodzemp.f.csv",
          row.names = FALSE)
write.csv(TE.wodzemp.r, "D:/Biostatistics/MSProject/planB/data/TE.wodzemp.r.csv",
          row.names = FALSE)
write.csv(p.wodzemp.f, "D:/Biostatistics/MSProject/planB/data/p.wodzemp.f.csv",
          row.names = FALSE)
write.csv(p.wodzemp.r, "D:/Biostatistics/MSProject/planB/data/p.wodzemp.r.csv",
          row.names = FALSE)
write.csv(CI.wodzemp.f, "D:/Biostatistics/MSProject/planB/data/CI.wodzemp.f.csv",
          row.names = FALSE)
write.csv(CI.wodzemp.r, "D:/Biostatistics/MSProject/planB/data/CI.wodzemp.r.csv",
          row.names = FALSE)


####################################################################
###ADD EMP to datMA with double zero events (include all studies)###
for (i in 1:nrow(datMA))
{
  if (datMA[i,"events_1"]==0 | datMA[i,"events_2"]==0)
  {
    datMA[i,"events_1"]<-datMA[i,"events_1"]+datMA[i,"Ke.f"]
    datMA[i,"events_2"]<-datMA[i,"events_2"]+datMA[i,"Kc.f"]
    datMA[i,"total_1"]<-datMA[i,"total_1"]+2*datMA[i,"Ke.f"]
    datMA[i,"total_2"]<-datMA[i,"total_2"]+2*datMA[i,"Kc.f"]
  }
  
  
}
View(datMA) ##add EMP to single arm zero events studies##

##meta analysis for datMA.wodz with EMP added##
meta1.all<-list()
meta2.all<-list()
TE.allemp.f<-matrix(NA,368,2)
TE.allemp.r<-matrix(NA,368,2)
CI.allemp.f<-matrix(NA,368,4)
CI.allemp.r<-matrix(NA,368,4)
p.allemp.f<-matrix(NA,368,2)
p.allemp.r<-matrix(NA,368,2)
study.allemp.f<-matrix(NA,368,2)
for (i in 1:368)
{
  temp<-datMA[datMA$ma.id==i,]
  
  meta1.all[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr=0,method="I",data=temp))
  meta2.all[[i]]<-summary(metabin(events_1, total_1, events_2, total_2, sm="OR", incr=0,method="MH",data=temp))
  
  TE.allemp.f[i,]<-cbind(round(meta1.all[[i]]$fixed$TE,3),round(meta2.all[[i]]$fixed$TE,3))
  CI.allemp.f[i,]<-cbind(meta1.all[[i]]$fixed$lower,meta1.all[[i]]$fixed$upper,meta2.all[[i]]$fixed$lower,meta2.all[[i]]$fixed$upper)
  p.allemp.f[i,]<-cbind(round(meta1.all[[i]]$fixed$p,3),round(meta2.all[[i]]$fixed$p,3))
  study.allemp.f[i,]<-cbind(meta1.all[[i]]$k,meta2.all[[i]]$k)
  
  TE.allemp.r[i,]<-cbind(round(meta1.all[[i]]$random$TE,3),round(meta2.all[[i]]$random$TE,3))
  CI.allemp.r[i,]<-cbind(meta1.all[[i]]$random$lower,meta1.all[[i]]$random$upper,meta2.all[[i]]$random$lower,meta2.all[[i]]$random$upper)
  p.allemp.r[i,]<-cbind(round(meta1.all[[i]]$random$p,3),round(meta2.all[[i]]$random$p,3))
  
  print(i)
  
}

colnames(TE.allemp.f)<-colnames(TE.allemp.r)<-colnames(p.allemp.f)<-colnames(p.allemp.r)<-paste("EMP.all",c("Inverse","MH"),sep="")
colnames(CI.allemp.f)<-colnames(CI.allemp.r)<-c("EMP.all.I.lower","EMP.all.I.upper","EMP.all.MH.lower","EMP.all.MH.upper")
TE.allemp.f<-as.data.frame(TE.allemp.f)
TE.allemp.r<-as.data.frame(TE.allemp.r)
p.allemp.f<-as.data.frame(p.allemp.f)
p.allemp.r<-as.data.frame(p.allemp.r)
CI.allemp.f<-as.data.frame(CI.allemp.f)
CI.allemp.r<-as.data.frame(CI.allemp.r)
head(CI.allemp.f)

write.csv(TE.allemp.f, "D:/Biostatistics/MSProject/planB/data/TE.allemp.f.csv",
          row.names = FALSE)
write.csv(TE.allemp.r, "D:/Biostatistics/MSProject/planB/data/TE.allemp.r.csv",
          row.names = FALSE)
write.csv(p.allemp.f, "D:/Biostatistics/MSProject/planB/data/p.allemp.f.csv",
          row.names = FALSE)
write.csv(p.allemp.r, "D:/Biostatistics/MSProject/planB/data/p.allemp.r.csv",
          row.names = FALSE)
write.csv(CI.allemp.f, "D:/Biostatistics/MSProject/planB/data/CI.allemp.f.csv",
          row.names = FALSE)
write.csv(CI.allemp.r, "D:/Biostatistics/MSProject/planB/data/CI.allemp.r.csv",
          row.names = FALSE)

