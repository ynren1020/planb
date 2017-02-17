###################################################
####select meta-analyses with rare events (208)####
####from CDSR.dich.largest.csv(1380)###############
####01/17/2017#####################################

setwd("C:/Users/nannan/Dropbox/CDSR")
re <- read.csv("CDSR.dich.largest.csv")
head(re)
colnames(re)
##number of meta analysis##
length(unique(re$ma.id)) #1380
##select rare event meta analysis based on prevalence is less than 5%##

for (i in 1:length(unique(re$ma.id)))
  {
  events_1_total[i]<-sum(re[re$ma.id==i,re$events_1])
  total_1_total[i]<-sum(re[re$ma.id==i,re$total_1])
  events_2_total[i]<-sum(re[re$ma.id==i,re$events_2])
  total_2_total[i]<-sum(re[re$ma.id==i,re$total_2])
  prevalence_1[i]<-events_1_total[i]/total_1_total[i]
  prevalence_2[i]<-events_2_total[i]/total_2_total[i]
  print(cbind(prevalence_1[i],prevalence_2[i]))
  
}

##another way##
for (i in 1:length(unique(re$ma.id)))
  {
 
  total_1[i]<-sum(re[re$ma.id==i,6])
  events_1[i]<-sum(re[re$ma.id==i,7])
  total_2[i]<-sum(re[re$ma.id==i,8])
  events_2[i]<-sum(re[re$ma.id==i,9])

}
prevalence_1<-events_1/total_1
prevalence_2<-events_2/total_2
id<-seq(1,1380,1)
ma.pre<-data.frame(id,prevalence_1,prevalence_2)
head(ma.pre)

##rare event meta-analysis id## 208 meta analyses
resub<-NULL
for (i in 1:nrow(ma.pre))
{if (prevalence_1[i]<0.05|prevalence_2[i]<0.05){resub[i]=ma.pre$id[i]}
  
}
resub<-resub[!is.na(resub)]
resub

##rare event meta-analyssi and create dataset "ma.re" for rare events##
ma.re<-re[re$ma.id%in%resub,]
head(ma.re)
length(unique(ma.re$ma.id))

write.csv(ma.re, "D:/Biostatistics/MSProject/planB/ma.re.csv",
          row.names = FALSE)



