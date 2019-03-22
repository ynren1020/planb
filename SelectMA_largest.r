############Choose largest MA(3024 MAs), including 2393 for DICH##
######check CDSR.id for flow chart#####
######Date:6/22/2017################

setwd("D:/Biostatistics/MSProject/planB/data")
CDSR5 <- read.csv("CDSR5.csv")
CDSR5_old <- read.csv("CDSR5.old.csv")
CDSR.RE<-read.csv("ma.redz.csv") #583MA #MAs with rare events
CDSR.RE.DZ<-read.csv("mare.dz.final.csv")
length(unique(CDSR5_old$ma.id)) ##22439 #MAs which have at least 5 studies(CONT and DICH)
length(unique(CDSR5_old$CDSR.id)) ##1948
length(unique(CDSR.RE$CDSR.id)) #371
length(unique(CDSR.RE.DZ$CDSR.id))#237

CDSR5.largest <- NULL
CDSR5.largest.id <- 1
N<-length(unique(CDSR5$CDSR.id))
for(i in 1:N){
	sr.id <- unique(CDSR5$CDSR.id)[i]
	dat.temp <- CDSR5[CDSR5$CDSR.id == sr.id,]
	ns.studies <- table(dat.temp$ma.id)
	maxma.ids <- names(ns.studies)[ns.studies == max(ns.studies)]
	maxma.ids <- as.numeric(maxma.ids)
	if(length(maxma.ids) == 1){
		maxma.id <- maxma.ids
	}
	if(length(maxma.ids) > 1){
		totals <- numeric(length(maxma.ids))
		for(j in 1:length(maxma.ids)){
			dat.tt <- dat.temp[dat.temp$ma.id == maxma.ids[j],]
			totals[j] <- sum(dat.tt[, "total_1"] + dat.tt[, "total_2"])
		}
		totals.max.id <- which(totals == max(totals))
		if(length(totals.max.id) == 1) maxma.id <- maxma.ids[totals.max.id]
		if(length(totals.max.id) > 1){
			maxma.id <- maxma.ids[totals.max.id]
			maxma.id <- maxma.id[1]
		}
	}
	dat.temp <- dat.temp[dat.temp$ma.id == maxma.id,]
	if(any(is.na(dat.temp$y))) cat("Error 0!\n")
	dat.temp$ma.id <- CDSR5.largest.id
	CDSR5.largest <- rbind(CDSR5.largest, dat.temp)
	CDSR5.largest.id <- CDSR5.largest.id + 1
	print(i)
}

write.csv(CDSR5.largest, "D:/Biostatistics/MSProject/planB/data/CDSR5.largest_622.csv",
	row.names = FALSE)
write.csv(CDSR5.largest, "D:/Biostatistics/MSProject/planB/data/CDSR5.largest.csv",
          row.names = FALSE)

CDSR5.largest <- read.csv("CDSR5.largest.csv") 

CDSR.cont.largest <- CDSR5.largest[CDSR5.largest$data_type == "CONT",]
CDSR.cont.largest$ma.id <- rep(1:length(unique(CDSR.cont.largest$ma.id)), table(CDSR.cont.largest$ma.id))
write.csv(CDSR.cont.largest, "D:/Biostatistics/MSProject/planB/data/CDSR.cont.largest.csv",
	row.names = FALSE)



CDSR.dich.largest <- CDSR5.largest[CDSR5.largest$data_type == "DICH",]
CDSR.dich.largest$ma.id <- rep(1:length(unique(CDSR.dich.largest$ma.id)), table(CDSR.dich.largest$ma.id))
write.csv(CDSR.dich.largest, "D:/Biostatistics/MSProject/planB/data/CDSR.dich.largest.csv",
	row.names = FALSE)

length(unique(CDSR5.largest$ma.id)) #3024  #MAs with the largest studies (CONT and DICH)#
length(unique(CDSR5.largest$CDSR.id))

length(unique(CDSR.cont.largest$ma.id))

length(unique(CDSR.dich.largest$ma.id)) #2393 #MAs with the largest studies (DICH)#
length(unique(CDSR.dich.largest$CDSR.id))


