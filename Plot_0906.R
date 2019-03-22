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

##########scatter matrix plot#####
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
pdf("TEallf.scatterplot_0906.pdf")#paper
pdf("TEwodzf.scatterplot_0906.pdf")
#tiff(filename = "TEallf.scatterplot_0906.tiff",
 #    width = 480, height = 480, units = "px", pointsize = 12)

pairs(TE.all.f[,3:10], lower.panel = my_line2, upper.panel = my_line2,pch=10,cex=0.2) ##exclude bayesian cex=0.2
pairs(TE.wodz.f[,3:10], lower.panel = my_line2, upper.panel = my_line2,pch=10,cex=0.2)
#pairs(TE.all.r[,3:9], lower.panel = my_line2, upper.panel = my_line2)
#pairs(TE.wodz.r[,3:10], lower.panel = my_line2, upper.panel = my_line2)
dev.off()
##################

############intercept=0,slope=1#################### USE THIS!!!!!
CL.all.fsub<- read.csv("CL.all.fsub.csv")
CL.wodz.fsub<-read.csv("CL.wodz.fsub.csv")

pdf("CLallf.scatterplot_0906.pdf")#paper
pdf("CLwodzf.scatterplot_0906.pdf")
pairs(CL.all.fsub, lower.panel = my_line2, upper.panel = my_line2,pch=10,cex=0.2) ##exclude bayesian
pairs(CL.wodz.fsub, lower.panel = my_line2, upper.panel = my_line2,pch=10,cex=0.2)
#pairs(CL.all.rsub, lower.panel = my_line2, upper.panel = my_line2)
#pairs(CL.wodz.rsub, lower.panel = my_line2, upper.panel = my_line2)
dev.off()
##################
##TE agreement between methods#### 
library(psy)
y<-cbind(TE.all.r[,9],TE.wodz.r[,10])
wkappa(y)
###confidence interval length agreement between methods#####




###pval agreement between methods############
###p.all.f (graph matrix, bottom, first column)
ckappa(p.all.f[,c(3,4)]) #0.975
ckappa(p.all.f[,c(3,5)]) #0.792
ckappa(p.all.f[,c(3,6)]) #0.837
ckappa(p.all.f[,c(3,7)]) #0.865
ckappa(p.all.f[,c(3,8)]) #0.658
ckappa(p.all.f[,c(3,10)]) #0.667
ckappa(p.all.f[,c(3,9)]) #0.854
###p.all.f (graph matrix, bottom, second column)
ckappa(p.all.f[,c(4,5)]) #0.767
ckappa(p.all.f[,c(4,6)]) #0.813
ckappa(p.all.f[,c(4,7)]) #0.841
ckappa(p.all.f[,c(4,8)]) #0.636
ckappa(p.all.f[,c(4,10)]) #0.645
ckappa(p.all.f[,c(4,9)]) #0.841
###p.all.f (graph matrix, bottom, third column)
ckappa(p.all.f[,c(5,6)]) #0.741
ckappa(p.all.f[,c(5,7)]) #0.734
ckappa(p.all.f[,c(5,8)]) #0.821
ckappa(p.all.f[,c(5,10)]) #0.788
ckappa(p.all.f[,c(5,9)]) #0.795
###p.all.f (graph matrix, bottom, Fourth column)
ckappa(p.all.f[,c(6,7)]) #0.949
ckappa(p.all.f[,c(6,8)]) #0.81
ckappa(p.all.f[,c(6,10)]) #0.82
ckappa(p.all.f[,c(6,9)]) #0.938
###p.all.f (graph matrix, bottom, Fifth column)
ckappa(p.all.f[,c(7,8)]) #0.783
ckappa(p.all.f[,c(7,10)]) #0.793
ckappa(p.all.f[,c(7,9)]) #0.988
###p.all.f (graph matrix, bottom, Six column)
ckappa(p.all.f[,c(8,10)]) #0.946
ckappa(p.all.f[,c(8,9)]) #0.85
###p.all.f (graph matrix, bottom, Seven column)
ckappa(p.all.f[,c(10,9)]) #0.88

###p.all.r (graph matrix, bottom, first column,from right to left)
ckappa(p.all.r[,c(9,8)]) #0.691
ckappa(p.all.r[,c(9,7)]) #0.637
ckappa(p.all.r[,c(9,6)]) #0.657
ckappa(p.all.r[,c(9,5)]) #0.677
ckappa(p.all.r[,c(9,4)]) #0.646
ckappa(p.all.r[,c(9,3)]) #0.676

###p.all.r (graph matrix, bottom, second column)
ckappa(p.all.r[,c(8,7)]) #0.757
ckappa(p.all.r[,c(8,6)]) #0.775
ckappa(p.all.r[,c(8,5)]) #0.989
ckappa(p.all.r[,c(8,4)]) #0.776
ckappa(p.all.r[,c(8,3)]) #0.800

###p.all.r (graph matrix, bottom, third column)
ckappa(p.all.r[,c(7,6)]) #0.980
ckappa(p.all.r[,c(7,5)]) #0.768
ckappa(p.all.r[,c(7,4)]) #0.981
ckappa(p.all.r[,c(7,3)]) #0.955

###p.all.r (graph matrix, bottom, Fourth column)
ckappa(p.all.r[,c(6,5)]) #0.786
ckappa(p.all.r[,c(6,4)]) #0.961
ckappa(p.all.r[,c(6,3)]) #0.962

###p.all.r (graph matrix, bottom, Fifth column)
ckappa(p.all.r[,c(5,4)]) #0.763
ckappa(p.all.r[,c(5,3)]) #0.787
###p.all.r (graph matrix, bottom, Seven column)
ckappa(p.all.r[,c(4,3)]) #0.974
#############################Wodz  Kappa#######
###p.wodz.f (graph matrix, bottom, first column)
ckappa(p.wodz.f[,c(3,4)]) #0.773
ckappa(p.wodz.f[,c(3,5)]) #0.786
ckappa(p.wodz.f[,c(3,6)]) #0.779
ckappa(p.wodz.f[,c(3,7)]) #0.668
ckappa(p.wodz.f[,c(3,8)]) #0.668
ckappa(p.wodz.f[,c(3,9)]) #0.641
ckappa(p.wodz.f[,c(3,10)]) #0.641
###p.wodz.f (graph matrix, bottom, second column)
ckappa(p.wodz.f[,c(4,5)]) #0.988
ckappa(p.wodz.f[,c(4,6)]) #0.935
ckappa(p.wodz.f[,c(4,7)]) #0.842
ckappa(p.wodz.f[,c(4,8)]) #0.842
ckappa(p.wodz.f[,c(4,9)]) #0.81
ckappa(p.wodz.f[,c(4,10)]) #0.784
###p.wodz.f (graph matrix, bottom, third column)
ckappa(p.wodz.f[,c(5,6)]) #0.947
ckappa(p.wodz.f[,c(5,7)]) #0.831
ckappa(p.wodz.f[,c(5,8)]) #0.831
ckappa(p.wodz.f[,c(5,9)]) #0.799
ckappa(p.wodz.f[,c(5,10)]) #0.773
###p.wodz.f (graph matrix, bottom, Fourth column)
ckappa(p.wodz.f[,c(6,7)]) #0.803
ckappa(p.wodz.f[,c(6,8)]) #0.803
ckappa(p.wodz.f[,c(6,9)]) #0.805
ckappa(p.wodz.f[,c(6,10)]) #0.779
###p.wodz.f (graph matrix, bottom, Fifth column)
ckappa(p.wodz.f[,c(7,8)]) #1
ckappa(p.wodz.f[,c(7,9)]) #0.967
ckappa(p.wodz.f[,c(7,10)]) #0.94
###p.wodz.f (graph matrix, bottom, Six column)
ckappa(p.wodz.f[,c(8,9)]) #0.967
ckappa(p.wodz.f[,c(8,10)]) #0.94
###p.wodz.f (graph matrix, bottom, Seven column)
ckappa(p.wodz.f[,c(9,10)]) #0.973

####p.wodz.r kappa####
###p.wodz.r (graph matrix, top, first column, from right)
ckappa(p.wodz.r[,c(10,9)]) #0.854
ckappa(p.wodz.r[,c(10,8)]) #0.836
ckappa(p.wodz.r[,c(10,7)]) #0.848
ckappa(p.wodz.r[,c(10,6)]) #0.854
ckappa(p.wodz.r[,c(10,5)]) #0.848
ckappa(p.wodz.r[,c(10,4)]) #0.86
ckappa(p.wodz.r[,c(10,3)]) #0.658
###p.wodz.r (graph matrix, bottom, second column)
ckappa(p.wodz.r[,c(9,8)]) #0.957
ckappa(p.wodz.r[,c(9,7)]) #0.945
ckappa(p.wodz.r[,c(9,6)]) #1
ckappa(p.wodz.r[,c(9,5)]) #0.945
ckappa(p.wodz.r[,c(9,4)]) #0.933
ckappa(p.wodz.r[,c(9,3)]) #0.777
###p.wodz.f (graph matrix, bottom, third column)
ckappa(p.wodz.r[,c(8,7)]) #0.988
ckappa(p.wodz.r[,c(8,6)]) #0.957
ckappa(p.wodz.r[,c(8,5)]) #0.988
ckappa(p.wodz.r[,c(8,4)]) #0.976
ckappa(p.wodz.r[,c(8,3)]) #0.797
###p.wodz.f (graph matrix, bottom, Fourth column)
ckappa(p.wodz.r[,c(7,6)]) #0.945
ckappa(p.wodz.r[,c(7,5)]) #0.976
ckappa(p.wodz.r[,c(7,4)]) #0.988
ckappa(p.wodz.r[,c(7,3)]) #0.784
###p.wodz.f (graph matrix, bottom, Fifth column)
ckappa(p.wodz.r[,c(6,5)]) #0.945
ckappa(p.wodz.r[,c(6,4)]) #0.933
ckappa(p.wodz.r[,c(6,3)]) #0.777
###p.wodz.f (graph matrix, bottom, Six column)
ckappa(p.wodz.r[,c(5,4)]) #0.988
ckappa(p.wodz.r[,c(5,3)]) #0.784
###p.wodz.f (graph matrix, bottom, Seven column)
ckappa(p.wodz.r[,c(4,3)]) #0.771

######################################################################################
############################prop.dz>50% (subh) <=50% (subl) method agreement ###########################3
###pval agreement between methods############
###p.all.fsubh (graph matrix, bottom, first column)
ckappa(p.all.fsubh[,c(3,4)]) #0.975  ##0.485
ckappa(p.all.fsubh[,c(3,5)]) #0.792  ##0.25
ckappa(p.all.fsubh[,c(3,6)]) #0.837  ##0.79
ckappa(p.all.fsubh[,c(3,7)]) #0.865  ##0.485
ckappa(p.all.fsubh[,c(3,8)]) #0.658  ##0.25
ckappa(p.all.fsubh[,c(3,10)]) #0.667 ##0.206
ckappa(p.all.fsubh[,c(3,9)]) #0.854  ##0.304
###p.all.fsubh (graph matrix, bottom, second column)
ckappa(p.all.fsubh[,c(4,5)]) #0.767  ##0.088
ckappa(p.all.fsubh[,c(4,6)]) #0.813  ##0.658
ckappa(p.all.fsubh[,c(4,7)]) #0.841  ##1.00
ckappa(p.all.fsubh[,c(4,8)]) #0.636  ##0.0876
ckappa(p.all.fsubh[,c(4,10)]) #0.645 ##0.0707
ckappa(p.all.fsubh[,c(4,9)]) #0.841  ##0.0
###p.all.fsubh (graph matrix, bottom, third column)
ckappa(p.all.fsubh[,c(5,6)]) #0.741  ##0.171
ckappa(p.all.fsubh[,c(5,7)]) #0.734  ##0.088
ckappa(p.all.fsubh[,c(5,8)]) #0.821  ##1.0
ckappa(p.all.fsubh[,c(5,10)]) #0.788 ##0.712
ckappa(p.all.fsubh[,c(5,9)]) #0.795  ##0.576
###p.all.fsubh (graph matrix, bottom, Fourth column)
ckappa(p.all.fsubh[,c(6,7)]) #0.949  ##0.658
ckappa(p.all.fsubh[,c(6,8)]) #0.81   ##0.171
ckappa(p.all.fsubh[,c(6,10)]) #0.82  ##0.139
ckappa(p.all.fsubh[,c(6,9)]) #0.938  ##0.304
###p.all.fsubh (graph matrix, bottom, Fifth column)
ckappa(p.all.fsubh[,c(7,8)]) #0.783  ##0.088
ckappa(p.all.fsubh[,c(7,10)]) #0.793 ##0.0707
ckappa(p.all.fsubh[,c(7,9)]) #0.988  ##0.0
###p.all.fsubh (graph matrix, bottom, Six column)
ckappa(p.all.fsubh[,c(8,10)]) #0.946 ##0.712
ckappa(p.all.fsubh[,c(8,9)]) #0.85   ##0.576
###p.all.fsubh (graph matrix, bottom, Seven column)
ckappa(p.all.fsubh[,c(10,9)]) #0.88  ##0.727

############<=50%#################Note:prop.dz<=50% method agreement fixed effect including dzesutides
###p.all.fsubl (graph matrix, bottom, first column)
ckappa(p.all.fsubl[,c(3,4)]) #0.975  ##0.485 ##1.00 (<=50%)
ckappa(p.all.fsubl[,c(3,5)]) #0.792  ##0.25  ##0.915
ckappa(p.all.fsubl[,c(3,6)]) #0.837  ##0.79  ##0.969
ckappa(p.all.fsubl[,c(3,7)]) #0.865  ##0.485 ##0.977
ckappa(p.all.fsubl[,c(3,8)]) #0.658  ##0.25  ##0.931
ckappa(p.all.fsubl[,c(3,10)]) #0.667 ##0.206 ##0.727
ckappa(p.all.fsubl[,c(3,9)]) #0.854  ##0.304 ##0.837
###p.all.fsubl (graph matrix, bottom, second column)
ckappa(p.all.fsubl[,c(4,5)]) #0.767  ##0.088 ##0.915
ckappa(p.all.fsubl[,c(4,6)]) #0.813  ##0.658 ##0.969
ckappa(p.all.fsubl[,c(4,7)]) #0.841  ##1.00  ##0.977
ckappa(p.all.fsubl[,c(4,8)]) #0.636  ##0.0876 ##0.931
ckappa(p.all.fsubl[,c(4,10)]) #0.645 ##0.0707 ##0.727
ckappa(p.all.fsubl[,c(4,9)]) #0.841  ##0.0    ##0.837
###p.all.fsubl (graph matrix, bottom, third column)
ckappa(p.all.fsubl[,c(5,6)]) #0.741  ##0.171  ##0.93
ckappa(p.all.fsubl[,c(5,7)]) #0.734  ##0.088  ##0.92
ckappa(p.all.fsubl[,c(5,8)]) #0.821  ##1.0    ##0.99
ckappa(p.all.fsubl[,c(5,10)]) #0.788 ##0.712  ##0.763
ckappa(p.all.fsubl[,c(5,9)]) #0.795  ##0.576  ##0.767
###p.all.fsubl (graph matrix, bottom, Fourth column)
ckappa(p.all.fsubl[,c(6,7)]) #0.949  ##0.658  ##0.99
ckappa(p.all.fsubl[,c(6,8)]) #0.81   ##0.171  ##0.92
ckappa(p.all.fsubl[,c(6,10)]) #0.82  ##0.139  ##0.70
ckappa(p.all.fsubl[,c(6,9)]) #0.938  ##0.304  ##0.81
###p.all.fsubl (graph matrix, bottom, Fifth column)
ckappa(p.all.fsubl[,c(7,8)]) #0.783  ##0.088  ##0.907
ckappa(p.all.fsubl[,c(7,10)]) #0.793 ##0.0707 ##0.705
ckappa(p.all.fsubl[,c(7,9)]) #0.988  ##0.0    ##0.813
###p.all.fsubl (graph matrix, bottom, Six column)
ckappa(p.all.fsubl[,c(8,10)]) #0.946 ##0.712  ##0.777
ckappa(p.all.fsubl[,c(8,9)]) #0.85   ##0.576  ##0.783
###p.all.fsubl (graph matrix, bottom, Seven column)
ckappa(p.all.fsubl[,c(10,9)]) #0.88  ##0.727  ##0.892

###p.all.r (graph matrix, bottom, first column,from right to left)
ckappa(p.all.r[,c(9,8)]) #0.691
ckappa(p.all.r[,c(9,7)]) #0.637
ckappa(p.all.r[,c(9,6)]) #0.657
ckappa(p.all.r[,c(9,5)]) #0.677
ckappa(p.all.r[,c(9,4)]) #0.646
ckappa(p.all.r[,c(9,3)]) #0.676

###p.all.r (graph matrix, bottom, second column)
ckappa(p.all.r[,c(8,7)]) #0.757
ckappa(p.all.r[,c(8,6)]) #0.775
ckappa(p.all.r[,c(8,5)]) #0.989
ckappa(p.all.r[,c(8,4)]) #0.776
ckappa(p.all.r[,c(8,3)]) #0.800

###p.all.r (graph matrix, bottom, third column)
ckappa(p.all.r[,c(7,6)]) #0.980
ckappa(p.all.r[,c(7,5)]) #0.768
ckappa(p.all.r[,c(7,4)]) #0.981
ckappa(p.all.r[,c(7,3)]) #0.955

###p.all.r (graph matrix, bottom, Fourth column)
ckappa(p.all.r[,c(6,5)]) #0.786
ckappa(p.all.r[,c(6,4)]) #0.961
ckappa(p.all.r[,c(6,3)]) #0.962

###p.all.r (graph matrix, bottom, Fifth column)
ckappa(p.all.r[,c(5,4)]) #0.763
ckappa(p.all.r[,c(5,3)]) #0.787
###p.all.r (graph matrix, bottom, Seven column)
ckappa(p.all.r[,c(4,3)]) #0.974
#############################Wodz  Kappa#######
###p.wodz.f (graph matrix, bottom, first column)
ckappa(p.wodz.f[,c(3,4)]) #0.773
ckappa(p.wodz.f[,c(3,5)]) #0.786
ckappa(p.wodz.f[,c(3,6)]) #0.779
ckappa(p.wodz.f[,c(3,7)]) #0.668
ckappa(p.wodz.f[,c(3,8)]) #0.668
ckappa(p.wodz.f[,c(3,9)]) #0.641
ckappa(p.wodz.f[,c(3,10)]) #0.641
###p.wodz.f (graph matrix, bottom, second column)
ckappa(p.wodz.f[,c(4,5)]) #0.988
ckappa(p.wodz.f[,c(4,6)]) #0.935
ckappa(p.wodz.f[,c(4,7)]) #0.842
ckappa(p.wodz.f[,c(4,8)]) #0.842
ckappa(p.wodz.f[,c(4,9)]) #0.81
ckappa(p.wodz.f[,c(4,10)]) #0.784
###p.wodz.f (graph matrix, bottom, third column)
ckappa(p.wodz.f[,c(5,6)]) #0.947
ckappa(p.wodz.f[,c(5,7)]) #0.831
ckappa(p.wodz.f[,c(5,8)]) #0.831
ckappa(p.wodz.f[,c(5,9)]) #0.799
ckappa(p.wodz.f[,c(5,10)]) #0.773
###p.wodz.f (graph matrix, bottom, Fourth column)
ckappa(p.wodz.f[,c(6,7)]) #0.803
ckappa(p.wodz.f[,c(6,8)]) #0.803
ckappa(p.wodz.f[,c(6,9)]) #0.805
ckappa(p.wodz.f[,c(6,10)]) #0.779
###p.wodz.f (graph matrix, bottom, Fifth column)
ckappa(p.wodz.f[,c(7,8)]) #1
ckappa(p.wodz.f[,c(7,9)]) #0.967
ckappa(p.wodz.f[,c(7,10)]) #0.94
###p.wodz.f (graph matrix, bottom, Six column)
ckappa(p.wodz.f[,c(8,9)]) #0.967
ckappa(p.wodz.f[,c(8,10)]) #0.94
###p.wodz.f (graph matrix, bottom, Seven column)
ckappa(p.wodz.f[,c(9,10)]) #0.973

####p.wodz.r kappa####
###p.wodz.r (graph matrix, top, first column, from right)
ckappa(p.wodz.r[,c(10,9)]) #0.854
ckappa(p.wodz.r[,c(10,8)]) #0.836
ckappa(p.wodz.r[,c(10,7)]) #0.848
ckappa(p.wodz.r[,c(10,6)]) #0.854
ckappa(p.wodz.r[,c(10,5)]) #0.848
ckappa(p.wodz.r[,c(10,4)]) #0.86
ckappa(p.wodz.r[,c(10,3)]) #0.658
###p.wodz.r (graph matrix, bottom, second column)
ckappa(p.wodz.r[,c(9,8)]) #0.957
ckappa(p.wodz.r[,c(9,7)]) #0.945
ckappa(p.wodz.r[,c(9,6)]) #1
ckappa(p.wodz.r[,c(9,5)]) #0.945
ckappa(p.wodz.r[,c(9,4)]) #0.933
ckappa(p.wodz.r[,c(9,3)]) #0.777
###p.wodz.f (graph matrix, bottom, third column)
ckappa(p.wodz.r[,c(8,7)]) #0.988
ckappa(p.wodz.r[,c(8,6)]) #0.957
ckappa(p.wodz.r[,c(8,5)]) #0.988
ckappa(p.wodz.r[,c(8,4)]) #0.976
ckappa(p.wodz.r[,c(8,3)]) #0.797
###p.wodz.f (graph matrix, bottom, Fourth column)
ckappa(p.wodz.r[,c(7,6)]) #0.945
ckappa(p.wodz.r[,c(7,5)]) #0.976
ckappa(p.wodz.r[,c(7,4)]) #0.988
ckappa(p.wodz.r[,c(7,3)]) #0.784
###p.wodz.f (graph matrix, bottom, Fifth column)
ckappa(p.wodz.r[,c(6,5)]) #0.945
ckappa(p.wodz.r[,c(6,4)]) #0.933
ckappa(p.wodz.r[,c(6,3)]) #0.777
###p.wodz.f (graph matrix, bottom, Six column)
ckappa(p.wodz.r[,c(5,4)]) #0.988
ckappa(p.wodz.r[,c(5,3)]) #0.784
###p.wodz.f (graph matrix, bottom, Seven column)
ckappa(p.wodz.r[,c(4,3)]) #0.771


##############kappa for wodz and all studies included##############
#########fixed effect############
y<-cbind(p.wodz.f$Petowodz,p.all.f$exact.pval)
ckappa(y)  ####0.891
yb<-cbind(p.wodz.f$Petowodz,p.all.f$bayesianF.pval)
ckappa(yb) #####0.946  
yI<-cbind(p.wodz.f$I0wodz,p.all.f$exact.pval)
ckappa(yI)  ####0.713
yIb<-cbind(p.wodz.f$I0wodz,p.all.f$bayesianF.pval)
ckappa(yIb) #####0.63  


y0<-cbind(p.wodz.f$I5wodz,p.all.f$I5all)
ckappa(y0) ####0.891
y1<-cbind(p.wodz.f$ITACCwodz,p.all.f$ITACCall)
ckappa(y1) ####0.877
y2<-cbind(p.wodz.f$EMP.wodzInverse,p.all.f$EMP.allInverse)
ckappa(y2) ####0.855
y3<-cbind(p.wodz.f$MH5wodz,p.all.f$MH5all)
ckappa(y3) ####0.702
y4<-cbind(p.wodz.f$MHTACCwodz,p.all.f$MHTACCall)
ckappa(y4) ####0.696
y5<-cbind(p.wodz.f$EMP.wodzMH,p.all.f$EMP.allMH)
ckappa(y5) ####0.734


#########random effect############

yr<-cbind(p.wodz.r$Petowodz,p.all.r$bayesianR.pval)
ckappa(yr) #####0.684  

yrb<-cbind(p.wodz.r$I0wodz,p.all.r$bayesianR.pval)
ckappa(yrb) #####0.586 


y0r<-cbind(p.wodz.r$I5wodz,p.all.r$I5all)
ckappa(y0r) ####0.92
y1r<-cbind(p.wodz.r$ITACCwodz,p.all.r$ITACCall)
ckappa(y1r) ####0.906
y2r<-cbind(p.wodz.r$EMP.wodzInverse,p.all.r$EMP.allInverse)
ckappa(y2r) ####0.883
y3r<-cbind(p.wodz.r$MH5wodz,p.all.r$MH5all)
ckappa(y3r) ####0.893
y4r<-cbind(p.wodz.r$MHTACCwodz,p.all.r$MHTACCall)
ckappa(y4r) ####0.899
y5r<-cbind(p.wodz.r$EMP.wodzMH,p.all.r$EMP.allMH)
ckappa(y5r) ####0.872

##############kappa for wodz and all studies included Categorized by prop.dz=50% ##############
#########fixed effect prop.dz>50%############
p.wodz.fsubh<-filter(p.wodz.f,prop.dz>0.5)
nrow(p.wodz.fsubh) ##102
p.wodz.fsubl<-filter(p.wodz.f,prop.dz<=0.5)
nrow(p.wodz.fsubl) ##266
p.all.fsubh<-filter(p.all.f,prop.dz>0.5)
p.all.fsubl<-filter(p.all.f,prop.dz<=0.5)

h1<-cbind(p.wodz.fsubh$Petowodz,p.all.fsubh$exact.pval)
ckappa(h1)  ####0.891  ##0.804 (50%) 
h2<-cbind(p.wodz.fsubh$Petowodz,p.all.fsubh$bayesianF.pval)
ckappa(h2) #####0.946  ##0.771 (50%)
h3<-cbind(p.wodz.fsubh$I0wodz,p.all.fsubh$exact.pval)
ckappa(h3)  ####0.713  ##0    (50%)
h4<-cbind(p.wodz.fsubh$I0wodz,p.all.fsubh$bayesianF.pval)
ckappa(h4) #####0.63   ##0    (50%)


h5<-cbind(p.wodz.fsubh$I5wodz,p.all.fsubh$I5all)
ckappa(h5) ####0.891   ##0.452
h6<-cbind(p.wodz.fsubh$ITACCwodz,p.all.fsubh$ITACCall)
ckappa(h6) ####0.877   ##0.194
h7<-cbind(p.wodz.fsubh$EMP.wodzInverse,p.all.fsubh$EMP.allInverse)
ckappa(h7) ####0.855   ##0.598
h8<-cbind(p.wodz.fsubh$MH5wodz,p.all.fsubh$MH5all)
ckappa(h8) ####0.702   ##0.258
h9<-cbind(p.wodz.fsubh$MHTACCwodz,p.all.fsubh$MHTACCall)
ckappa(h9) ####0.696   ##0.136
h10<-cbind(p.wodz.fsubh$EMP.wodzMH,p.all.fsubh$EMP.allMH)
ckappa(h10) ####0.734  ##0.571


#########random effect,prop.dz>50%############
p.wodz.rsubh<-filter(p.wodz.r,prop.dz>0.5)
 ##102
p.wodz.rsubl<-filter(p.wodz.r,prop.dz<=0.5)
 ##266
p.all.rsubh<-filter(p.all.r,prop.dz>0.5)
p.all.rsubl<-filter(p.all.r,prop.dz<=0.5)


h11<-cbind(p.wodz.rsubh$Petowodz,p.all.rsubh$bayesianR.pval)
ckappa(h11) #####0.684 ##0.607(>50%) 

h12<-cbind(p.wodz.rsubh$I0wodz,p.all.rsubh$bayesianR.pval)
ckappa(h12) #####0.586 ##0 (>50%)

h13<-cbind(p.wodz.rsubh$I5wodz,p.all.rsubh$I5all)
ckappa(h13) ####0.92   ##0.452(>50%)
h14<-cbind(p.wodz.rsubh$ITACCwodz,p.all.rsubh$ITACCall)
ckappa(h14) ####0.906  ##0.194(>50%)
h15<-cbind(p.wodz.rsubh$EMP.wodzInverse,p.all.rsubh$EMP.allInverse)
ckappa(h15) ####0.883  ##0.598(>50%)
h16<-cbind(p.wodz.rsubh$MH5wodz,p.all.rsubh$MH5all)
ckappa(h16) ####0.893  ##0.32
h17<-cbind(p.wodz.rsubh$MHTACCwodz,p.all.rsubh$MHTACCall)
ckappa(h17) ####0.899  ##0.194
h18<-cbind(p.wodz.rsubh$EMP.wodzMH,p.all.rsubh$EMP.allMH)
ckappa(h18) ####0.872  ##0.598

###############kappa statistics for prop.dz<=50%#######################
#########fixed effect prop.dz<=50%############
p.wodz.fsubl<-filter(p.wodz.f,prop.dz<=0.5)
p.all.fsubl<-filter(p.all.f,prop.dz<=0.5)

l1<-cbind(p.wodz.fsubl$Petowodz,p.all.fsubl$exact.pval)
ckappa(l1)  ####0.891  ##0.892 (<50%) ##0.804 (50%) 
l2<-cbind(p.wodz.fsubl$Petowodz,p.all.fsubl$bayesianF.pval)
ckappa(l2) #####0.946  ##1.0 (<50%) ##0.771 (50%)
l3<-cbind(p.wodz.fsubl$I0wodz,p.all.fsubl$exact.pval)
ckappa(l3)  ####0.713  ##0.738   ##0    (50%)
l4<-cbind(p.wodz.fsubl$I0wodz,p.all.fsubl$bayesianF.pval)
ckappa(l4) #####0.63   ##0.671   ##0    (50%)


l5<-cbind(p.wodz.fsubl$I5wodz,p.all.fsubl$I5all)
ckappa(l5) ####0.891   ##0.954  ##0.452
l6<-cbind(p.wodz.fsubl$ITACCwodz,p.all.fsubl$ITACCall)
ckappa(l6) ####0.877   ##0.954  ##0.194
l7<-cbind(p.wodz.fsubl$EMP.wodzInverse,p.all.fsubl$EMP.allInverse)
ckappa(l7) ####0.855   ##0.923  ##0.598
l8<-cbind(p.wodz.fsubl$MH5wodz,p.all.fsubl$MH5all)
ckappa(l8) ####0.702   ##0.747  ##0.258
l9<-cbind(p.wodz.fsubl$MHTACCwodz,p.all.fsubl$MHTACCall)
ckappa(l9) ####0.696   ##0.754  ##0.136
l10<-cbind(p.wodz.fsubl$EMP.wodzMH,p.all.fsubl$EMP.allMH)
ckappa(l10) ####0.734  ##0.77   ##0.571


#########random effect,prop.dz<=50%############
p.wodz.rsubl<-filter(p.wodz.r,prop.dz<=0.5)
##266
p.all.rsubl<-filter(p.all.r,prop.dz<=0.5)


l11<-cbind(p.wodz.rsubl$Petowodz,p.all.rsubl$bayesianR.pval)
ckappa(l11) #####0.684 ##0.697 (<50%) ##0.607(>50%) 

l12<-cbind(p.wodz.rsubl$I0wodz,p.all.rsubl$bayesianR.pval)
ckappa(l12) #####0.586 ##0.588 (<50%) ##0 (>50%)

l13<-cbind(p.wodz.rsubl$I5wodz,p.all.rsubl$I5all)
ckappa(l13) ####0.92   ##0.992 ##0.452(>50%)
l14<-cbind(p.wodz.rsubl$ITACCwodz,p.all.rsubl$ITACCall)
ckappa(l14) ####0.906  ##0.992 ##0.194(>50%)
l15<-cbind(p.wodz.rsubl$EMP.wodzInverse,p.all.rsubl$EMP.allInverse)
ckappa(l15) ####0.883  ##0.961 ##0.598(>50%)
l16<-cbind(p.wodz.rsubl$MH5wodz,p.all.rsubl$MH5all)
ckappa(l16) ####0.893  ##0.976 ##0.32
l17<-cbind(p.wodz.rsubl$MHTACCwodz,p.all.rsubl$MHTACCall)
ckappa(l17) ####0.899  ##0.984 ##0.194
l18<-cbind(p.wodz.rsubl$EMP.wodzMH,p.all.rsubl$EMP.allMH)
ckappa(l18) ####0.872  ##0.95  ##0.598


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
#NOT USED FOR THESIS OR PAPER
#######################################################################
# an example with many ties and the 'sunflower'-option
a <- TE.all.f$meta2all
b <- TE.all.f$meta3all
bland.altman.plot(a,b,sunflower=FALSE, xlab="Mean",ylab="Difference",
                  main="discrete values lead to ties")

c <- TE.all.f$meta2all
d <- TE.wodz.f$meta2
bland.altman.plot(c,d,sunflower=FALSE, xlab="Mean",ylab="Difference",
                  main="discrete values lead to ties")


############################proportion of double zero events studies######################################
TE.all.f25<-filter(TE.all.f,TE.all.f$prop.dz<0.25)
nrow(TE.all.f25) ##144  39.1%

TE.all.f50<-filter(TE.all.f,TE.all.f$prop.dz<0.5 & TE.all.f$prop.dz>=0.25)
nrow(TE.all.f50) ##109   29.6%

TE.all.f75<-filter(TE.all.f,TE.all.f$prop.dz<0.75 & TE.all.f$prop.dz>=0.5)
nrow(TE.all.f75) ##80    21.7%

TE.all.fgt75<-filter(TE.all.f, TE.all.f$prop.dz>0.7499)
nrow(TE.all.fgt75) ##35   9.5%

###################subset by nonagreement between methods:0<p.sum<16#########################
p.catsub<- read.csv("p.cat.allpooling.sub.csv")
View(p.catsub)
p.catsubnon<-filter(p.catsub,p.catsub$p.sum<16 & p.catsub$p.sum>0)
View(p.catsubnon) ##99
p.catsubnonsub<-filter(p.catsubnon, p.catsubnon$prop.dz>0.5)
View(p.catsubnonsub) ##39
p.catsubnonsubNA<-na.omit(p.catsubnonsub)
View(p.catsubnonsubNA) ##19

