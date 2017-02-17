###########################################
##Methods of meta-analysis with Rare event#
##data:Rosiglitazone, dat4Meta)############
##01/24/2017###############################

##read dataset##
setwd("D:/Biostatistics/MSProject/planB/data")
dat <- read.csv("dat4Meta.csv")
# common
head(dat)
##apply r package gmeta and meta;odds ratio as effect measurement##
datMI=dat[,c("MI.TRT","n.TRT","MI.CTRL","n.CTRL")]
library(gmeta)
##call "gmeta" for "exact1" on OR, Liu method##
MI.exactLiuOR=gmeta(datMI,gmi.type = "2x2",method="exact1",ci.level=0.95,mc.iteration=2000)
summary(MI.exactLiuOR)
names(summary(MI.exactLiuOR))
summary(MI.exactLiuOR)$cmbd
exp(summary(MI.exactLiuOR)$cmbd)
##call "gmeta" for "exact2" on RD, Tian method##
MI.exactTianRD=gmeta(datMI,gmi.type = "2x2",method="exact2",ci.level=0.95,mc.iteration=2000)
summary(MI.exactTianRD)
names(summary(MI.exactTianRD))
summary(MI.exactTianRD)$cmbd

##meta,OR##
library(meta)
MI.OR.wo=metabin(MI.TRT,n.TRT,MI.CTRL,n.CTRL,data=datMI,incr=0,method="Inverse",sm="OR")
summary(MI.OR.wo)

MI.OR.w.5=metabin(MI.TRT,n.TRT,MI.CTRL,n.CTRL,data=datMI,method="Inverse",sm="OR")
summary(MI.OR.w.5)

MI.OR.P=metabin(MI.TRT,n.TRT,MI.CTRL,n.CTRL,data=datMI[1:42,],method="P",sm="OR")
summary(MI.OR.P)

MI.OR.MH=metabin(MI.TRT,n.TRT,MI.CTRL,n.CTRL,data=datMI,method="MH",sm="OR")
summary(MI.OR.MH)
