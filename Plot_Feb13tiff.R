##############PLot and exploratory analysis##############
##############04142017###################################
library(reshape2)
library(ggplot2)
#library(BlandAltmanLeh)
library(dplyr)
setwd("D:/Biostatistics/MSProject/planB/summary")
TE.all.f<- read.csv("TE.all.f.csv")
TE.wodz.f<-read.csv("TE.wodz.f.csv")
#TE.all.r<- read.csv("TE.all.r.csv")
#TE.wodz.r<-read.csv("TE.wodz.r.csv")

CL.all.f<- read.csv("CL.all.f.csv")
CL.wodz.f<-read.csv("CL.wodz.f.csv")
#CL.all.r<- read.csv("CL.all.r.csv")
#CL.wodz.r<-read.csv("CL.wodz.r.csv")

p.all.f<- read.csv("p.all.f.csv")
p.wodz.f<-read.csv("p.wodz.f.csv")
#p.all.r<- read.csv("p.all.r.csv")
#p.wodz.r<-read.csv("p.wodz.r.csv")

exact1<-read.csv("exact1.csv")
View(exact1)
CI.bayesian.f<-read.csv("CI.bayesian.f.csv")
#CI.bayesian.r<-read.csv("CI.bayesian.r.csv")

View(TE.all.f)
View(TE.wodz.f)
#View(TE.all.r)
#View(TE.wodz.r)

View(CL.all.f)
View(CL.wodz.f)
#View(CL.all.r)
#View(CL.wodz.r)

CL.all.fsub<- select(CL.all.f,ends_with(".CL"))
CL.wodz.fsub<-select(CL.wodz.f,ends_with(".CL"))
#CL.all.rsub<- select(CL.all.r,ends_with(".CL"))
#CL.wodz.rsub<-select(CL.wodz.r,ends_with(".CL"))

View(CL.all.fsub)
View(CL.wodz.fsub)
#View(CL.all.rsub)
#View(CL.wodz.rsub)
write.csv(CL.all.fsub, "D:/Biostatistics/MSProject/planB/summary/CL.all.fsub.csv",
          row.names = FALSE)
write.csv(CL.wodz.fsub, "D:/Biostatistics/MSProject/planB/summary/CL.wodz.fsub.csv",
          row.names = FALSE)
#####pval categorize by 0.05##
####p.all.f#########
for (i in 1:nrow(p.all.f))
  {
  ifelse(p.all.f[i,3]<=0.05,p.all.f[i,3]<-1,p.all.f[i,3]<-0)
  ifelse(p.all.f[i,4]<=0.05,p.all.f[i,4]<-1,p.all.f[i,4]<-0)
  ifelse(p.all.f[i,5]<=0.05,p.all.f[i,5]<-1,p.all.f[i,5]<-0)
  ifelse(p.all.f[i,6]<=0.05,p.all.f[i,6]<-1,p.all.f[i,6]<-0)
  ifelse(p.all.f[i,7]<=0.05,p.all.f[i,7]<-1,p.all.f[i,7]<-0)
  ifelse(p.all.f[i,8]<=0.05,p.all.f[i,8]<-1,p.all.f[i,8]<-0)
  
}
View(p.all.f)
write.csv(p.all.f, "D:/Biostatistics/MSProject/planB/summary/p.all.f.cat.csv",
          row.names = FALSE)
###p.all.random pval categorization####
for (i in 1:nrow(p.all.r))
{
  ifelse(p.all.r[i,3]<=0.05,p.all.r[i,3]<-1,p.all.r[i,3]<-0)
  ifelse(p.all.r[i,4]<=0.05,p.all.r[i,4]<-1,p.all.r[i,4]<-0)
  ifelse(p.all.r[i,5]<=0.05,p.all.r[i,5]<-1,p.all.r[i,5]<-0)
  ifelse(p.all.r[i,6]<=0.05,p.all.r[i,6]<-1,p.all.r[i,6]<-0)
  ifelse(p.all.r[i,7]<=0.05,p.all.r[i,7]<-1,p.all.r[i,7]<-0)
  ifelse(p.all.r[i,8]<=0.05,p.all.r[i,8]<-1,p.all.r[i,8]<-0)
  
}
View(p.all.r)
write.csv(p.all.r, "D:/Biostatistics/MSProject/planB/summary/p.all.r.cat.csv",
          row.names = FALSE)
############wodz##############3
####p.wodz.f#########
for (i in 1:nrow(p.wodz.f))
{
  ifelse(p.wodz.f[i,3]<=0.05,p.wodz.f[i,3]<-1,p.wodz.f[i,3]<-0)
  ifelse(p.wodz.f[i,4]<=0.05,p.wodz.f[i,4]<-1,p.wodz.f[i,4]<-0)
  ifelse(p.wodz.f[i,5]<=0.05,p.wodz.f[i,5]<-1,p.wodz.f[i,5]<-0)
  ifelse(p.wodz.f[i,6]<=0.05,p.wodz.f[i,6]<-1,p.wodz.f[i,6]<-0)
  ifelse(p.wodz.f[i,7]<=0.05,p.wodz.f[i,7]<-1,p.wodz.f[i,7]<-0)
  ifelse(p.wodz.f[i,8]<=0.05,p.wodz.f[i,8]<-1,p.wodz.f[i,8]<-0)
  ifelse(p.wodz.f[i,9]<=0.05,p.wodz.f[i,9]<-1,p.wodz.f[i,9]<-0)
  ifelse(p.wodz.f[i,10]<=0.05,p.wodz.f[i,10]<-1,p.wodz.f[i,10]<-0)
}
View(p.wodz.f)
write.csv(p.wodz.f, "D:/Biostatistics/MSProject/planB/summary/p.wodz.f.cat.csv",
          row.names = FALSE)
###p.wodz.random pval categorization####
for (i in 1:nrow(p.wodz.r))
{
  ifelse(p.wodz.r[i,3]<=0.05,p.wodz.r[i,3]<-1,p.wodz.r[i,3]<-0)
  ifelse(p.wodz.r[i,4]<=0.05,p.wodz.r[i,4]<-1,p.wodz.r[i,4]<-0)
  ifelse(p.wodz.r[i,5]<=0.05,p.wodz.r[i,5]<-1,p.wodz.r[i,5]<-0)
  ifelse(p.wodz.r[i,6]<=0.05,p.wodz.r[i,6]<-1,p.wodz.r[i,6]<-0)
  ifelse(p.wodz.r[i,7]<=0.05,p.wodz.r[i,7]<-1,p.wodz.r[i,7]<-0)
  ifelse(p.wodz.r[i,8]<=0.05,p.wodz.r[i,8]<-1,p.wodz.r[i,8]<-0)
  ifelse(p.wodz.r[i,9]<=0.05,p.wodz.r[i,9]<-1,p.wodz.r[i,9]<-0)
  ifelse(p.wodz.r[i,10]<=0.05,p.wodz.r[i,10]<-1,p.wodz.r[i,10]<-0)
}
View(p.wodz.r)
write.csv(p.wodz.r, "D:/Biostatistics/MSProject/planB/summary/p.wodz.r.cat.csv",
          row.names = FALSE)

##########define exact1-pval by confidence interval######
for (i in 1:nrow(exact1))
  
{ ifelse(is.na(as.numeric(as.character(exact1$lower[i]))) | is.na(as.numeric(as.character(exact1$upper[i]))),exact1$pval[i]<-NA,ifelse(as.numeric(as.character(exact1$lower[i]))>1 | as.numeric(as.character(exact1$upper[i]))<1,exact1$pval[i]<-1,exact1$pval[i]<-0))
  
}
View(exact1)
class(exact1$loglower)
###########define pval for bayesian.f##################
for (i in 1:nrow(CI.bayesian.f))
{
  ifelse(CI.bayesian.f[i,1]>0 | CI.bayesian.f[i,2]<0,CI.bayesian.f$pval[i]<-1,CI.bayesian.f$pval[i]<-0)
}
View(CI.bayesian.f)

###########define pval for bayesian.r##################
for (i in 1:nrow(CI.bayesian.r))
{
  ifelse(CI.bayesian.r[i,1]>0 | CI.bayesian.r[i,2]<0,CI.bayesian.r$pval[i]<-1,CI.bayesian.r$pval[i]<-0)
}
View(CI.bayesian.r)

########p.all.f with pval from all methods######
p.all.f$exact.pval<-exact1$pval
p.all.f$bayesianF.pval<-CI.bayesian.f$pval
View(p.all.f)  ####To be used for calculating kappa statistics#####

########p.all.r with pval from all methods######
p.all.r$bayesianR.pval<-CI.bayesian.r$pval
View(p.all.r)  ##

##########scatter matrix plot in tiff format for RSM paper#####
##########2018 Feb 13 by Yanan#################################
#my_line <- function(x,y,...){
#  points(x,y,...)
#  segments(min(x), min(y), max(x), max(y),...)
#}
#pairs(TE.all.f[,3:10], lower.panel = my_line, upper.panel = my_line)
#pairs(TE.wodz.f[,3:10], lower.panel = my_line, upper.panel = my_line)
#pairs(TE.all.r[,3:9], lower.panel = my_line, upper.panel = my_line)
#pairs(TE.wodz.r[,3:10], lower.panel = my_line, upper.panel = my_line)


############intercept=0,slope=1####################USE THIS!!!!
my_line2 <- function(x,y,...){
  points(x,y,...)
 abline(a = 0,b = 1,...)
}
#pdf("TEallf.scatterplot_final.pdf")#thesis
#pdf("TEallf.scatterplot_0906.pdf")#paper JCE
#pdf("TEwodzf.scatterplot_0906.pdf") #paper JCE
tiff(filename = "TEallf.scatterplot_Feb13.tiff",
     width = 1200, height = 1200, units = "px", pointsize = 6,res=300)
tiff(filename = "TEwodzf.scatterplot_Feb13.tiff",
     width = 1200, height = 1200, units = "px", pointsize = 6,res=300)

#tiff("TEallfScatterFeb13.tiff", width = 4, height = 4,  pointsize=12, units = 'in', res = 300)

pairs(TE.all.f[,3:10], lower.panel = my_line2, upper.panel = my_line2,pch=10,cex=0.2,font.labels = 2) ##exclude bayesian cex=0.2
pairs(TE.wodz.f[,3:10], lower.panel = my_line2, upper.panel = my_line2,pch=10,cex=0.2)
#pairs(TE.all.r[,3:9], lower.panel = my_line2, upper.panel = my_line2)
#pairs(TE.wodz.r[,3:10], lower.panel = my_line2, upper.panel = my_line2)
dev.off()
##################

############intercept=0,slope=1#################### USE THIS!!!!!
CL.all.fsub<- read.csv("CL.all.fsub.csv")
CL.wodz.fsub<-read.csv("CL.wodz.fsub.csv")
tiff(filename = "CLallf.scatterplot_Feb13.tiff",
     width = 1200, height = 1200, units = "px", pointsize = 6,res=300)
tiff(filename = "CLwodzf.scatterplot_Feb13.tiff",
     width = 1200, height = 1200, units = "px", pointsize = 6,res=300)

#pdf("CLallf.scatterplot_0906.pdf")#paper
#pdf("CLwodzf.scatterplot_0906.pdf")
pairs(CL.all.fsub, lower.panel = my_line2, upper.panel = my_line2,pch=10,cex=0.2) ##exclude bayesian
pairs(CL.wodz.fsub, lower.panel = my_line2, upper.panel = my_line2,pch=10,cex=0.2)
#pairs(CL.all.rsub, lower.panel = my_line2, upper.panel = my_line2)
#pairs(CL.wodz.rsub, lower.panel = my_line2, upper.panel = my_line2)
dev.off()
##################

####################
##############plot TE all boxplot######
graph.all.TE<- read.csv("graph_TEall.csv")
head(graph.all.TE)
colnames(graph.all.TE)

TE.all.long <- reshape(graph.all.TE, 
             varying = c("I5all", "ITACCall", "IEMPall", "MH5all","MHTACCall","MHEMPall","Exact1","Bayesian.f","I5all.r","ITACCall.r","IEMPall.r","MH5all.r","MHTACCall.r","MHEMPall.r","Bayesian.r"), 
             v.names = "LogOddsRatio",
             timevar = "ma.method", 
             times = c("I5all", "ITACCall", "IEMPall", "MH5all","MHTACCall","MHEMPall","Exact1","Bayesian.f","I5all.r","ITACCall.r","IEMPall.r","MH5all.r","MHTACCall.r","MHEMPall.r","Bayesian.r"), 
             direction = "long")

TE.all.long.sort <- TE.all.long[order(TE.all.long$ma.id),]
TE.all.long.sort[1:10,]
names(TE.all.long.sort)
nrow(TE.all.long.sort)
effect<-rep(NA,5520)
for (i in 1:nrow(TE.all.long.sort))
{if(endsWith(TE.all.long.sort$ma.method[i],".r"))
{effect[i]<-"random"}
  else {effect[i]<-"fix"}
}

TE.all.long.sort$effect<-effect

###modify the name and lab for paper aje###

bp<-ggplot(data=TE.all.long.sort, aes(x=ma.method, y=LogOddsRatio)) + 
  geom_boxplot(aes(fill = effect), width = 0.6) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

bp2 <- bp + scale_y_continuous(breaks=seq(-6,6,by=2)) +
  coord_cartesian(ylim=c(-6.5, 6.5))
bp3<-bp2+labs(y = "Log(OR)") 
last_plot()
ggsave("TE.all.boxplot_final.png", width = 5, height = 5)

##############plot CL all boxplot######
graph.all.CL<- read.csv("graph_CLall.csv")
head(graph.all.CL)
colnames(graph.all.CL)

CL.all.long <- reshape(graph.all.CL, 
                       varying = c("I5all.CL", "ITACCall.CL", "IEMPall.CL", "MH5all.CL","MHTACCall.CL","MHEMPall.CL","exact.CL","Bayesian.CL","I5all.CL.r","ITACCall.CL.r","IEMPall.CL.r","MH5all.CL.r","MHTACCall.CL.r","MHEMPall.CL.r","Bayesian.CL.r"), 
                       v.names = "CI_Length",
                       timevar = "ma.method", 
                       times = c("I5all.CL", "ITACCall.CL", "IEMPall.CL", "MH5all.CL","MHTACCall.CL","MHEMPall.CL","exact.CL","Bayesian.CL","I5all.CL.r","ITACCall.CL.r","IEMPall.CL.r","MH5all.CL.r","MHTACCall.CL.r","MHEMPall.CL.r","Bayesian.CL.r"), 
                       direction = "long")

CL.all.long.sort <- CL.all.long[order(CL.all.long$ma.id),]
CL.all.long.sort[1:10,]
names(CL.all.long.sort)
nrow(CL.all.long.sort)
CL.effect<-rep(NA,5520)
for (i in 1:nrow(CL.all.long.sort))
{if(endsWith(CL.all.long.sort$ma.method[i],".r"))
{CL.effect[i]<-"random"}
  else {CL.effect[i]<-"fix"}
}

CL.all.long.sort$effect<-CL.effect

CL.all.long.sort$CI_Length<-as.numeric(CL.all.long.sort$CI_Length)


CL<-ggplot(data=CL.all.long.sort, aes(x=ma.method, y=CI_Length)) + 
  geom_boxplot(aes(fill = effect), width = 0.6) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

CL2 <- CL + scale_y_continuous(breaks=seq(0,14,by=2)) +
  coord_cartesian(ylim=c(-0.5, 14.5))
CL3<-CL2+labs(x = "Pooling Methods", y = "CIL") 
last_plot()
ggsave("CL.all.boxplot.png", width = 5, height = 5)

##############plot TE wodz boxplot##########################################
graph.wodz.TE<- read.csv("graph_TEwodz.csv")
head(graph.wodz.TE)
colnames(graph.wodz.TE)

TE.wodz.long <- reshape(graph.wodz.TE, 
                       varying = c("I0wodz", "I5wodz", "ITACCwodz", "IEMPwodz", "MH5wodz","MHTACCwodz","MHEMPwodz","Petowodz","I0wodz.r","I5wodz.r","ITACCwodz.r","IEMPwodz.r","MH5wodz.r","MHTACCwodz.r","MHEMPwodz.r","Petowodz.r"), 
                       v.names = "LogOddsRatio",
                       timevar = "ma.method", 
                       times = c("I0wodz", "I5wodz", "ITACCwodz", "IEMPwodz", "MH5wodz","MHTACCwodz","MHEMPwodz","Petowodz","I0wodz.r","I5wodz.r","ITACCwodz.r","IEMPwodz.r","MH5wodz.r","MHTACCwodz.r","MHEMPwodz.r","Petowodz.r"), 
                       direction = "long")

TE.wodz.long.sort <- TE.wodz.long[order(TE.wodz.long$ma.id),]
TE.wodz.long.sort[1:10,]
names(TE.wodz.long.sort)
nrow(TE.wodz.long.sort)
wodz.effect<-rep(NA,5888)
for (i in 1:nrow(TE.wodz.long.sort))
{if(endsWith(TE.wodz.long.sort$ma.method[i],".r"))
{wodz.effect[i]<-"random"}
  else {wodz.effect[i]<-"fix"}
}

TE.wodz.long.sort$effect<-wodz.effect


wodz<-ggplot(data=TE.wodz.long.sort, aes(x=ma.method, y=LogOddsRatio)) + 
  geom_boxplot(aes(fill = effect), width = 0.6) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

wodz2 <- wodz + scale_y_continuous(breaks=seq(-6,6,by=2)) +
  coord_cartesian(ylim=c(-6.5, 6.5))
wodz3<-wodz2+labs(x = "Pooling Methods", y = "Log(OR)") 
last_plot()
ggsave("TE.wodz.boxplot.png", width = 5, height = 5)

##############plot CL wodz boxplot######
graph.wodz.CL<- read.csv("graph_CLwodz.csv")
head(graph.wodz.CL)
colnames(graph.wodz.CL)

CL.wodz.long <- reshape(graph.wodz.CL, 
                       varying = c("I0wodz.CL","I5wodz.CL", "ITACCwodz.CL", "IEMPwodz.CL", "MH5wodz.CL","MHTACCwodz.CL","MHEMPwodz.CL","Petowodz.CL","I0wodz.CL.r","I5wodz.CL.r","ITACCwodz.CL.r","IEMPwodz.CL.r","MH5wodz.CL.r","MHTACCwodz.CL.r","MHEMPwodz.CL.r","Petowodz.CL.r"), 
                       v.names = "CI_Length",
                       timevar = "ma.method", 
                       times = c("I0wodz.CL","I5wodz.CL", "ITACCwodz.CL", "IEMPwodz.CL", "MH5wodz.CL","MHTACCwodz.CL","MHEMPwodz.CL","Petowodz.CL","I0wodz.CL.r","I5wodz.CL.r","ITACCwodz.CL.r","IEMPwodz.CL.r","MH5wodz.CL.r","MHTACCwodz.CL.r","MHEMPwodz.CL.r","Petowodz.CL.r"), 
                       direction = "long")

CL.wodz.long.sort <- CL.wodz.long[order(CL.wodz.long$ma.id),]
CL.wodz.long.sort[1:10,]
names(CL.wodz.long.sort)
nrow(CL.wodz.long.sort)
CLwodz.effect<-rep(NA,5888)
for (i in 1:nrow(CL.wodz.long.sort))
{if(endsWith(CL.wodz.long.sort$ma.method[i],".r"))
{CLwodz.effect[i]<-"random"}
  else {CLwodz.effect[i]<-"fix"}
}

CL.wodz.long.sort$effect<-CLwodz.effect

CL.wodz.long.sort$CI_Length<-as.numeric(CL.wodz.long.sort$CI_Length)


CL.wodz<-ggplot(data=CL.wodz.long.sort, aes(x=ma.method, y=CI_Length)) + 
  geom_boxplot(aes(fill = effect), width = 0.6) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
CL.wodz.error<-CL.wodz+stat_summary(fun.y=mean, geom="point", size=2) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")

CL2 <- CL.wodz + scale_y_continuous(breaks=seq(0,14,by=2)) +
  coord_cartesian(ylim=c(-0.5, 14.5))
CL3<-CL2+labs(x = "Pooling Methods", y = "CIL") 
last_plot()
ggsave("CL.wodz.boxplot.png", width = 5, height = 5)


###########################compare wodz and all fixed effect########33
graph.allwodz.TE<- read.csv("graph_TEallwodzf.csv")
head(graph.allwodz.TE)
colnames(graph.allwodz.TE)

TE.allwodz.long <- reshape(graph.allwodz.TE, 
                        varying = c("I5.Inc","ITACC.Inc","IEMP.Inc","MH5.Inc","MHTACC.Inc","MHEMP.Inc","Exact","Bayesian","I0.Exc", "I5.Exc", "ITACC.Exc", "IEMP.Exc", "MH5.Exc","MHTACC.Exc","MHEMP.Exc","Peto.Exc"), 
                        v.names = "LogOddsRatio",
                        timevar = "ma.method", 
                        times = c("I5.Inc","ITACC.Inc","IEMP.Inc","MH5.Inc","MHTACC.Inc","MHEMP.Inc","Exact","Bayesian","I0.Exc", "I5.Exc", "ITACC.Exc", "IEMP.Exc", "MH5.Exc","MHTACC.Exc","MHEMP.Exc","Peto.Exc"), 
                        direction = "long")

TE.allwodz.long.sort <- TE.allwodz.long[order(TE.allwodz.long$ma.id),]
TE.allwodz.long.sort[1:10,]
names(TE.allwodz.long.sort)
nrow(TE.allwodz.long.sort)
allwodz.effect<-rep(NA,5888)
for (i in 1:nrow(TE.allwodz.long.sort))
{if(endsWith(TE.allwodz.long.sort$ma.method[i],".Exc"))
{allwodz.effect[i]<-"Exclude"}
  else {allwodz.effect[i]<-"Include"}
}

TE.allwodz.long.sort$DZS<-allwodz.effect


allwodz<-ggplot(data=TE.allwodz.long.sort, aes(x=ma.method, y=LogOddsRatio)) + 
  geom_boxplot(aes(fill=DZS),width = 0.6) +
  scale_fill_manual("DZS",values=c("Grey","White"))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

allwodz2 <- allwodz + scale_y_continuous(breaks=seq(-6,6,by=2)) +
  coord_cartesian(ylim=c(-6.5, 6.5))
allwodz3<-allwodz2+labs(x = "Methods Include/Exclude DZS", y = "Log(OR)") 
last_plot()
ggsave("TE.allwodz.boxplot_paper.pdf", width = 5, height = 5)

#############subset by proportion for TE.allwodzf to look into details#######
#######about the proportion of DZS effect############
group<-rep(NA,5888)
for (i in 1:nrow(TE.allwodz.long.sort))
{
  ifelse(TE.allwodz.long.sort$prop.dz[i]<0.25,group[i]<-paste(0,"%"," ~ ",25,"%",sep=""),ifelse(0.25<=TE.allwodz.long.sort$prop.dz[i]&TE.allwodz.long.sort$prop.dz[i]<0.5,group[i]<-paste(25,"%"," ~ ",50,"%",sep=""),ifelse(0.5<=TE.allwodz.long.sort$prop.dz[i]&TE.allwodz.long.sort$prop.dz[i]<0.75,group[i]<-paste(50,"%"," ~ ",75,"%",sep=""),group[i]<-paste(75,"%"," ~ ",100,"%",sep=""))))
}
group  #######25 50 75 >75##########

TE.allwodz.long.sort$Group<-group
head(TE.allwodz.long.sort)

TEallwodz<-ggplot(data=TE.allwodz.long.sort, aes(x=ma.method, y=LogOddsRatio)) + 
  geom_boxplot(aes(fill = DZS), width = 0.6) + 
  scale_fill_manual("DZS",values=c("Grey","White"))+
  facet_wrap(~Group)+
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

TEallwodz2 <- TEallwodz + scale_y_continuous(breaks=seq(-6,6,by=2)) +
  coord_cartesian(ylim=c(-6.5, 6.5))
TEallwodz3<-TEallwodz2+labs(x = "Methods Include/Exclude DZS", y = "Log(OR)") 
last_plot()
ggsave("TE.allwodz.boxplot.proportion_papercolor.pdf", width = 7, height = 7)

###############################Confidence interval length#############
graph.allwodz.CL<- read.csv("graph_CLallwodzf.csv")
head(graph.allwodz.CL)
colnames(graph.allwodz.CL)

CL.allwodz.long <- reshape(graph.allwodz.CL, 
                           varying = c("I5.Inc","ITACC.Inc","IEMP.Inc","MH5.Inc","MHTACC.Inc","MHEMP.Inc","Exact","Bayesian","I0.Exc", "I5.Exc", "ITACC.Exc", "IEMP.Exc", "MH5.Exc","MHTACC.Exc","MHEMP.Exc","Peto.Exc"), 
                           v.names = "CIL",
                           timevar = "ma.method", 
                           times = c("I5.Inc","ITACC.Inc","IEMP.Inc","MH5.Inc","MHTACC.Inc","MHEMP.Inc","Exact","Bayesian","I0..Exc", "I5..Exc", "ITACC..Exc", "IEMP..Exc", "MH5..Exc","MHTACC..Exc","MHEMP..Exc","Peto..Exc"),
                           direction = "long")

CL.allwodz.long.sort <- CL.allwodz.long[order(CL.allwodz.long$ma.id),]
CL.allwodz.long.sort[1:10,]
names(CL.allwodz.long.sort)
nrow(CL.allwodz.long.sort)
CLallwodz.effect<-rep(NA,5888)
for (i in 1:nrow(CL.allwodz.long.sort))
{if(endsWith(CL.allwodz.long.sort$ma.method[i],".Exc"))
{CLallwodz.effect[i]<-"Exclude"}
  else {CLallwodz.effect[i]<-"Include"}
}

CL.allwodz.long.sort$DZS<-CLallwodz.effect


CLallwodz<-ggplot(data=CL.allwodz.long.sort, aes(x=ma.method, y=CIL)) + 
  geom_boxplot(aes(fill = DZS), width = 0.6) + 
  scale_fill_manual("DZS",values=c("Grey","White"))+
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

CLallwodz2 <- CLallwodz + scale_y_continuous(breaks=seq(0,13,by=2)) +
  coord_cartesian(ylim=c(-0.5, 13.5))
CLallwodz3<-CLallwodz2+labs(x = "Methods Include/Exclude DZS", y = "Confidence Interval Length") 
last_plot()
ggsave("CL.allwodz.boxplot.papercolor.pdf", width = 5, height = 5)

#############subset by proportion for CL.allwodzf to look into details#######
#######about the proportion of DZS effect############
group<-rep(NA,5888)
for (i in 1:nrow(CL.allwodz.long.sort))
{
  ifelse(CL.allwodz.long.sort$prop.dz[i]<0.25,group[i]<-paste(0,"%", " ~ ",25,"%",sep=""),ifelse(0.25<=CL.allwodz.long.sort$prop.dz[i]&CL.allwodz.long.sort$prop.dz[i]<0.5,group[i]<-paste(25,"%"," ~ ",50,"%",sep=""),ifelse(0.5<=CL.allwodz.long.sort$prop.dz[i]&CL.allwodz.long.sort$prop.dz[i]<0.75,group[i]<-paste(50,"%"," ~ ",75,"%",sep=""),group[i]<-paste(75,"%"," ~ ", 100,"%",sep=""))))
}
group  #######25 50 75 >75##########

CL.allwodz.long.sort$Group<-group

CLallwodz<-ggplot(data=CL.allwodz.long.sort, aes(x=ma.method, y=CIL)) + 
  geom_boxplot(aes(fill = DZS), width = 0.6) + 
  scale_fill_manual("DZS",values=c("Grey","White"))+
  facet_wrap(~Group)+
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

CLallwodz2 <- CLallwodz + scale_y_continuous(breaks=seq(0,13,by=2)) +
  coord_cartesian(ylim=c(-0.5, 13.5))
CLallwodz3<-CLallwodz2+labs(x = "Methods Include/Exclude DZS", y = "Confidence Interval Length") 
last_plot()
ggsave("CL.allwodz.boxplot.proportion2color.pdf", width = 7, height = 7)
#######################proportion plot#################################







