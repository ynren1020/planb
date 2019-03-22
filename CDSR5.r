####1.select MA studies >5####
####copy from SelectMA_all_per_sr_Modify.r###

#setwd("E:/Dropbox/RA/ChuH/CDSR")
setwd("D:/Biostatistics/MSProject/planB/data")
CDSR.dat <- read.csv("CDSR_clean.csv")
CDSR.id <- CDSR.dat$CDSR.id
CDSR.id <- sort(unique(CDSR.id))
N <- length(CDSR.id)

## chose meta-analyses >= 5
ma.dat.c <- list(NULL)
listc.id <- 1

for(i in 1:N){
  sr.id <- CDSR.id[i]
  sr.temp <- CDSR.dat[CDSR.dat$CDSR.id == sr.id,]
  cprs <- sr.temp[, "comparison"]
  study.id.temp <- sr.temp[, "study"]
  study.id.temp <- c(study.id.temp, 1)
  diff.id <- diff(study.id.temp)
  eli.id <- which(diff.id <= -4)
  if(length(eli.id) > 0){
    for(j in 1:length(eli.id)){
      max.id <- eli.id[j]
      min.id <- max.id - (study.id.temp[max.id] - 1)
      data_type.temp <- sr.temp[min.id:max.id, "data_type"]
      if(length(unique(data_type.temp)) != 1) stop("check!")
      y.temp <- sr.temp[min.id:max.id, "y"]
      s2.temp <- sr.temp[min.id:max.id, "s2"]
      total_1.temp <- sr.temp[min.id:max.id, "total_1"]
      events_1.temp <- sr.temp[min.id:max.id, "events_1"]
      total_2.temp <- sr.temp[min.id:max.id, "total_2"]
      events_2.temp <- sr.temp[min.id:max.id, "events_2"]
      
      if(study.id.temp[max.id] >=5){
        ma.temp <- data.frame(data_type = data_type.temp, y = y.temp, s2 = s2.temp,
                              total_1 = total_1.temp, events_1 = events_1.temp,
                              total_2 = total_2.temp, events_2 = events_2.temp)
        out <- NULL
        out$CDSR.id <- sr.id
        out$ma.dat <- ma.temp
        ma.dat.c[[listc.id]] <- out
        listc.id <- listc.id + 1
      }
    }
  }
}

CDSR5 <- NULL
#for(i in 1:length(ma.dat.c)){
#  temp <- cbind(CDSR.id = ma.dat.c[[i]]$CDSR.id, ma.dat.c[[i]]$ma.dat)
#  CDSR5 <- rbind(CDSR5, temp)
#  print(i)
#}

for(i in 1:20000){
  temp <- cbind(CDSR.id = ma.dat.c[[i]]$CDSR.id, ma.id = i, ma.dat.c[[i]]$ma.dat)
  CDSR5 <- rbind(CDSR5, temp)
  print(i)
}
nrow(CDSR5) ##260876

for(i in 20001:22439){
  temp <- cbind(CDSR.id = ma.dat.c[[i]]$CDSR.id, ma.id = i, ma.dat.c[[i]]$ma.dat)
  CDSR5 <- rbind(CDSR5, temp)
  print(i)
}
nrow(CDSR5) ##281941


write.csv(CDSR5, "D:/Biostatistics/MSProject/planB/data/CDSR5.csv",
          row.names = FALSE)

length(unique(CDSR5$ma.id)) ##22439


