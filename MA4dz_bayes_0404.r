#################################
###Bayesian-model################
#########04/04/2017##############
library(rjags)
options(digits = 3)
setwd("D:/Biostatistics/MSProject/planB/data")
datMA <- read.csv("mare.dz.final.csv")
View(datMA)
N<-length(unique(datMA$ma.id))
######fixed effect model#################################
CI<-matrix(NA,368,3)
TE<-NULL
for (i in 1:368)
{
  temp<-datMA[datMA$ma.id==i,]
  
  y0<-temp$events_2
  n0<-temp$total_2
  y1<-temp$events_1
  n1<-temp$total_1

jags <- jags.model(file = "D:/Biostatistics/MSProject/planB/R/model_r.txt", data = list("S" = length(y0), "y0" = y0, "n0" = n0,"y1"=y1,"n1"=n1),
                   init = list(delta = 0),
                   quiet = TRUE)
burn_in <- 1000
update(jags, burn_in)
n_s <- 100000
pars <- c("delta")
msamp <- coda.samples(jags, variable.names = pars, n.iter = n_s)
samp <- msamp[[1]]
CI[i,]<-t(apply(samp, 2, quantile, c(0.025, 0.5, 0.975)))
TE[i]<-mean(samp[, "delta"])
print(i)

}
TE.bayesian.f<-TE
CI.bayesian.f<-CI
colnames(CI.bayesian.f)<-c("2.5%","50%","97.5%")


####random effect model##########################################
CI.r<-matrix(NA,368,3)
TE.r<-NULL
for (i in 1:368)
{
  temp<-datMA[datMA$ma.id==i,]
  
  y0<-temp$events_2
  n0<-temp$total_2
  y1<-temp$events_1
  n1<-temp$total_1

jags <- jags.model(file = "D:/Biostatistics/MSProject/planB/R/model_rr.txt", data = list("S" = length(y0), "y0" = y0, "n0" = n0,"y1"=y1,"n1"=n1),
                   init = list(d= 0,sigma=1),
                   quiet = TRUE)
burn_in <- 1000
update(jags, burn_in)
n_s <- 100000
#pars <- c("d", "tau")
pars <- c("d")
msamp <- coda.samples(jags, variable.names = pars, n.iter = n_s)
samp <- msamp[[1]]
CI.r[i,]<-t(apply(samp, 2, quantile, c(0.025, 0.5, 0.975)))
TE.r[i]<-mean(samp[, "d"])
print(i)
}

CI.r
TE.r

TE.bayesian.r<-TE.r
CI.bayesian.r<-CI.r
colnames(CI.bayesian.r)<-c("2.5%","50%","97.5%")

TE.bayesian<-as.data.frame(cbind(TE.bayesian.f,TE.bayesian.r))
TE.bayesian
CI.bayesian.r<-as.data.frame(CI.bayesian.r)
CI.bayesian.f<-as.data.frame(CI.bayesian.f)

write.csv(TE.bayesian, "D:/Biostatistics/MSProject/planB/summary/TE.bayesian.csv",
          row.names = FALSE)
write.csv(CI.bayesian.r, "D:/Biostatistics/MSProject/planB/summary/CI.bayesian.r.csv",
          row.names = FALSE)
write.csv(CI.bayesian.f, "D:/Biostatistics/MSProject/planB/summary/CI.bayesian.f.csv",
          row.names = FALSE)


