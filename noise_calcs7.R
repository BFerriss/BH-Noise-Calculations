##########################################
# Process noise data for Beacon Hill
# 
# Edmund Seto
# 3/7/2019
 
# & Bridget Ferriss

##########################################

library(ggplot2)
library(ggmap)   # for maps
library(gstat)   # for IDW on the map
library(sp)   # for spatial object



getwd()
setwd("~/Desktop/mbair Desktop/UW EHS Position/Roseanne Lorenzana/Beacon Hill Noise/Analysis/")

####################
## read in the whole dataset from the Access database
####################

## this will take a long time because of the amount of data
dat<- read.table("Noise_Logs_20190307.txt",header=TRUE, sep = ',')   
head(dat)

## read in site info
SiteDat=read.csv("Site_Information_20190307.txt")
head(SiteDat)

# drop all but Site.ID, Latitude and Longitude fields
SiteDat <- data.frame(Site.ID = SiteDat$Site.ID, Latitude = SiteDat$Latitude, Longitude = SiteDat$Longitude)

head(SiteDat)


## donut (actually, a hollow square) jittering of the lat long coordinates
#  see this website on finding the right precision:  https://en.wikipedia.org/wiki/Decimal_degrees

#           Degree precision versus length
#
# decimal   decimal     DMS     qualitative scale    N/S or E/W    E/W at     E/W at   E/W at
# places	   degrees                                 at equator     23N/S      45N/S    67N/S
#	
# 0	1.0	1° 00′ 0″	country or large region	111.32 km	102.47 km	78.71 km	43.496 km
# 1	0.1	0° 06′ 0″	large city or district	11.132 km	10.247 km	7.871 km	4.3496 km
# 2	0.01	0° 00′ 36″	town or village	1.1132 km	1.0247 km	787.1 m	434.96 m
# 3	0.001	0° 00′ 3.6″	neighborhood, street	111.32 m	102.47 m	78.71 m	43.496 m
# 4	0.0001	0° 00′ 0.36″	individual street, land parcel	11.132 m	10.247 m	7.871 m	4.3496 m
# 5	0.00001	0° 00′ 0.036″	individual trees, door entrance	1.1132 m	1.0247 m	787.1 mm	434.96 mm
# 6	0.000001	0° 00′ 0.0036″	individual humans	111.32 mm	102.47 mm	78.71 mm	43.496 mm
# 7	0.0000001	0° 00′ 0.00036″	practical limit of commercial surveying	11.132 mm	10.247 mm	7.871 mm	4.3496 mm
# 8	0.00000001	0° 00′ 0.000036″	specialized surveying (e.g. tectonic plate mapping)	1.1132 mm	1.0247 mm	787.1 µm	434.96 µm

# level 3 seems appropriate (~80m)  ## Seattle is at 47N
# so 0.001 decimal degrees ~= 80m   Do a donut that will shift coords between 40 to 120 m.

SiteDat$latshift <- runif(nrow(SiteDat), min = 0.0005, max = 0.0015)
SiteDat$lonshift <- runif(nrow(SiteDat), min = 0.0005, max = 0.0015)
# figure out if +/- shift on lat and lon
SiteDat$latdir <- runif(nrow(SiteDat), min = 0, max = 1) > 0.5
SiteDat$londir <- runif(nrow(SiteDat), min = 0, max = 1) > 0.5

SiteDat$maskedLatitude <- SiteDat$Latitude + SiteDat$latshift
SiteDat[SiteDat$latdir,]$maskedLatitude <- SiteDat[SiteDat$latdir,"Latitude"] - SiteDat[SiteDat$latdir,"latshift"]

SiteDat$maskedLongitude <- SiteDat$Longitude + SiteDat$lonshift
SiteDat[SiteDat$londir,]$maskedLongitude <- SiteDat[SiteDat$londir,"Longitude"] - SiteDat[SiteDat$londir,"lonshift"]

## drop the original and temp fields
SiteDat$Latitude <- NULL
SiteDat$Longitude <- NULL
SiteDat$latshift <- NULL
SiteDat$lonshift <- NULL
SiteDat$latdir <- NULL
SiteDat$londir <- NULL

head(SiteDat)

# quick and dirty check of the new lat lon coords
plot(SiteDat$maskedLongitude, SiteDat$maskedLatitude)

write.csv(SiteDat, "masked_site_information_20190307.csv", row.names = FALSE);





# need to fix the date and time and create a POSIX object
dat$datetime_tmp1 <- as.POSIXct(dat$Date, format = "%m/%d/%Y %H:%M:%S")    
dat$datetime_tmp2 <- as.POSIXct(dat$Time, format = "%m/%d/%Y %H:%M:%S")    
dat$datetime <- as.POSIXct(paste(strftime(dat$datetime_tmp1, format="%m/%d/%Y"), strftime(dat$datetime_tmp2, format="%H:%M:%S")), format = "%m/%d/%Y %H:%M:%S")

dat$datetime_tmp1 <- NULL
dat$datetime_tmp2 <- NULL

## save a temp copy
save(dat, file = "dat.RData")


#remove siteID=NA and DB=99999 values
datTemp=dat[which(dat$Site.ID!="NA"),] #remove NA from allsites variable
which(datTemp$Site.ID=="NA") #double check all NA are removed from Site ID

max(datTemp$DB) #max is 99999, produces "Inf" when reverse log
datTemp2=datTemp[which(datTemp$DB!=max(datTemp$DB)),] #remove the 99999 values
max(datTemp2$DB) #double check all 99999 are removed from DB
min(datTemp2$DB)
dat=datTemp2

rm(datTemp)
rm(datTemp2)

# what are the sites in the noise log data?
allsites <- unique(dat$Site.ID)
allsites

# what are the unique monitors?
allmonitors <- unique(dat$Monitor)
allmonitors 

## create a column that has "site-monitor" in order to be able to identify doubles
dat$siteMonitor <- paste(dat$Site.ID, dat$Monitor, sep="-")

allsitemonitors <- unique(dat$siteMonitor)
allsitemonitors 



head(dat)


############################################
## Seattle Seafair (Blue Angels) Exclusion
############################################

# Data were collected during Seafair, we should put these in a separate dataset for comparison
# 8/3/2018 - 8/6/2018 
# these are the site-monitors
# 38-720 (Paul)   [1] start time: 2018-08-03 10:43:00   [1] end time: 2018-08-06 09:59:35
# 60-858 (Roseanne) [1] start time: 2018-08-03 09:58:05   [1] end time: 2018-08-05 19:31:56
# 64-301 (Ariana)  <-- But, also includes normal conditions  (there's a break in the timeseries right before 8/6, and then continues later in 8/6

## SFdat will hold the Seafair data
SFdat <- dat[which(dat$datetime > as.POSIXct("8/3/2018 00:00:01", format = "%m/%d/%Y %H:%M:%S") & dat$datetime < as.POSIXct("8/6/2018 10:00:00", format = "%m/%d/%Y %H:%M:%S") ),] 
nrow(SFdat)

## dat will now have the Seafair data removed from it.
nrow(dat)
dat <- dat[which(dat$datetime <= as.POSIXct("8/3/2018 00:00:01", format = "%m/%d/%Y %H:%M:%S") | dat$datetime >= as.POSIXct("8/6/2018 10:00:00", format = "%m/%d/%Y %H:%M:%S") ),] 
nrow(dat)

## also drop the two SF monitors (not Ariana's though) from allsitemonitors
allsitemonitors <- unique(dat$siteMonitor)



####################
## a timeseries
####################

plot_site_timeseries <- function (data, site) {
	p <- ggplot(data[data$Site.ID ==site,], aes(datetime, DB)) + ylab("Noise, dB") + xlab("Date") + ggtitle("Noise Levels")
	p + geom_point(alpha=0.2)
}

plot_site_timeseries(dat, 1)
plot_site_timeseries(dat, 2)
plot_site_timeseries(dat, 10)
plot_site_timeseries(dat, 11)

plot_site_timeseries(dat, 12)
plot_site_timeseries(dat, 64)

# these sites had a lot of variation
plot_site_timeseries(dat, 9)
plot_site_timeseries(dat, 37)
plot_site_timeseries(dat, 47)
plot_site_timeseries(dat, 50)
plot_site_timeseries(dat, 55)
plot_site_timeseries(dat, 62)
plot_site_timeseries(dat, 70)
plot_site_timeseries(dat, 72)




####################
## a histogram
####################

plot_site_histogram <- function (data, site) {
	p <- ggplot(data[data$Site.ID ==site,], aes(DB)) + xlab("Noise, dB") + ggtitle("Noise Levels")
	p + geom_histogram()
}

plot_site_histogram(dat, 1)
plot_site_histogram(dat, 2)
plot_site_histogram(dat, 10)
plot_site_histogram(dat, 11)

###################
## plot dB by time of day
###################

# 1. Prepare data

#separate time from datetime column
dat$Date2 <- sapply(strsplit(as.character(dat$datetime), " "), "[", 1) #separate date from date time column
dat$Time2 <- sapply(strsplit(as.character(dat$datetime), " "), "[", 2) #separate time from date time column

#separate hour only for each data point
dat$Hr=format(strptime(dat$Time2,"%H:%M:%S"),'%H')
dat$Hr=as.numeric(as.character(dat$Hr)) #convert from character to numeric



######################################
##  Boxplots by hour (all sites)

p <- ggplot(dat, aes(factor(Hr), DB)) + ylab("Noise (dB)") + xlab("Hour of day") + ggtitle("Beacon Hill Noise(dB) by Hour")
p + geom_boxplot()+
geom_hline(yintercept=65, lty=2)+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(color="black",fill=NA)) +
  theme(plot.margin=unit(c(0,3,0,2), "cm")) 



######################################
##  Boxplots by day of week (all sites)

dat$dayofweek <- strftime(dat$datetime, format="%a")

dat$dayofweek <- ordered(dat$dayofweek, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


p <- ggplot(dat, aes(factor(dayofweek), DB)) + ylab("Noise (dB)") + xlab("Day of week") + ggtitle("Beacon Hill Noise(dB) by Day of Week")
p + geom_boxplot()+
geom_hline(yintercept=65, lty=2)+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(color="black",fill=NA)) +
  theme(plot.margin=unit(c(0,3,0,2), "cm")) 



############################################
## Site Summary function
#
## modified because we may have multiple monitors per site.
############################################

summary_site <- function(data, sitemon) {
	
	sitedata <- data[data$siteMonitor == sitemon,]
	number <- nrow(sitedata)
	start <- sitedata$datetime[1]
	end <- sitedata$datetime[number]
	hours <- strftime(sitedata$datetime, format="%H")

	print(paste("Summary for Site-Monitor:", sitemon), quote=FALSE)
	print(" ", quote=FALSE)
	print(paste("observations:", number), quote=FALSE)
	print(paste("start time:", start), quote=FALSE)
	print(paste("end time:", end), quote=FALSE)

	print(paste("start time:", start), quote=FALSE)
	print(paste("end time:", end), quote=FALSE)
	print(end-start, quote=FALSE)
}


for (i in 1:length(allsitemonitors)) {
	summary_site(dat, allsitemonitors[i])
}





######################
## Noise average (LEQ)  
######################

# see https://www.cirrusresearch.co.uk/blog/2013/01/noise-data-averaging-how-do-i-average-noise-measurements/
# In math...  mean = 10*log10( 10^(L1/10) + 10^(L2/10) + ... + 10^(Ln/10) / n)


############################################
## LEQ function
#
## modified because we may have multiple monitors per site.
############################################

## this function computes LEQ across all the data collected for a site
LEQ_site <- function(dat, sitemon) {
	spl <- 10^(dat[dat$siteMonitor == sitemon,"DB"]/10)
	avespl <- sum(spl)/nrow(dat[dat$siteMonitor == sitemon,])
	LEQ <- 10*log10(avespl)
	LEQ
}


LEQlist <- data.frame()
for (i in 1:length(allsitemonitors)) {
	test <- data.frame(siteMonitor = allsitemonitors[i], LEQ = LEQ_site(dat, allsitemonitors[i]))
	LEQlist <- rbind(LEQlist, test)
}
LEQlist



############################################
## this function computes daily 24-hour LEQs for the specified site
#
## modified because we may have multiple monitors per site.
############################################

LEQ24_site <- function(dat, sitemon) {
	sitedat <- dat[dat$siteMonitor == sitemon,]
	res <- data.frame()
	
	if (nrow(sitedat) < (60*60*24)) {
		s <- sitedat$datetime[1]
		e <- sitedat$datetime[nrow(sitedat)]
		l <- LEQ_site(sitedat, sitemon)
		res <- rbind(res, data.frame(siteMonitor=sitemon, start=s, end=e, LEQ=l))
	} else {

		for(i in 0:(floor(nrow(sitedat)/(60*60*24))-1)) {
			s <- sitedat$datetime[i*(60*60*24)+1]
			e <- sitedat$datetime[(i+1)*(60*60*24)]
			l <- LEQ_site(sitedat[ (i*(60*60*24)+1): ((i+1)*(60*60*24)),], sitemon) 
			res <- rbind(res, data.frame(siteMonitor=sitemon, start=s, end=e, LEQ=l))
			#med=median(l)
			#medLEQ24=cbind(site, med)
		}
	}
	res
	#aveLEQ24
}

LEQ24list <- data.frame()
for (i in 1:length(allsitemonitors)) {
	test <- LEQ24_site(dat, allsitemonitors[i])
	LEQ24list <- rbind(LEQ24list, test)
}
LEQ24list





##############################################################
## Penalized Day Evening Night Noise Levels (LDN and LDEN) 
##############################################################

# See https://www.noisemeters.com/apps/ldn-calculator.asp
# the LDEN is also called a CNEL.

# basically compute 1-hour LEQs for each hour in a 24-hour period.
# add 5 dB to hours between 19:00 - 22:00
# add 10 dB to the hours between 22:00 - 07:00

# Then do the averaging math equation shown in the previous section.
# You can check your result against the website calculator.




############################################
## LDN function
#
## modified because we may have multiple monitors per site.
############################################

LDN_site <- function(dat, sitemon) {
	dat <- dat[dat$siteMonitor == sitemon,]
	df <- data.frame("datetime" = dat$datetime, "DB" = dat$DB)
	df$hod <- as.numeric(strftime(df$datetime, format="%H"))
	df$penaltyValue <- df$DB
	df$penaltyValue[df$hod >= 22 | df$hod < 7] <- df$DB[df$hod >= 22 | df$hod < 7]+ 10			# 22:00 to 07:00
	
	spl <- 10^(df$penaltyValue/10)
	avespl <- sum(spl)/nrow(dat)
	Ldn <- 10*log10(avespl)

	Ldn
}



############################################
## this function computes daily 24-hour LDNs for the specified site
#
## modified because we may have multiple monitors per site.
############################################

LDN24_site <- function(dat, sitemon) {
	sitedat <- dat[dat$siteMonitor == sitemon,]
	res <- data.frame()

	if (nrow(sitedat) < (60*60*24)) {
		s <- sitedat$datetime[1]
		e <- sitedat$datetime[nrow(sitedat)]
		l <- LDN_site(sitedat, sitemon)
		res <- rbind(res, data.frame(siteMonitor=sitemon, start=s, end=e, LDN=l))
	} else {

		for(i in 0:(floor(nrow(sitedat)/(60*60*24))-1)) {
			s <- sitedat$datetime[i*(60*60*24)+1]
			e <- sitedat$datetime[(i+1)*(60*60*24)]
			l <- LDN_site(sitedat[ (i*(60*60*24)+1): ((i+1)*(60*60*24)),], sitemon) 
			res <- rbind(res, data.frame(siteMonitor=sitemon, start=s, end=e, LDN=l))
		}
	}
	res
}


LDN24list <- data.frame()
for (i in 1:length(allsitemonitors)) {
	test <- LDN24_site(dat, allsitemonitors[i])
	LDN24list <- rbind(LDN24list, test)
}
LDN24list

write.csv(LDN24list, "LDN24list.csv")


############################################
## LDEN function
#
## modified because we may have multiple monitors per site.
############################################

LDEN_site <- function(dat, sitemon) {
	dat <- dat[dat$siteMonitor == sitemon,]
	df <- data.frame("datetime" = dat$datetime, "DB" = dat$DB)
	df$hod <- as.numeric(strftime(df$datetime, format="%H"))
	df$penaltyValue <- df$DB
	df$penaltyValue[df$hod >= 19 & df$hod < 22] <- df$DB[df$hod >= 19 & df$hod < 22]+ 5			# 19:00 to 22:00
	df$penaltyValue[df$hod >= 22 | df$hod < 7] <- df$DB[df$hod >= 22 | df$hod < 7]+ 10			# 22:00 to 07:00
	
	spl <- 10^(df$penaltyValue/10)
	avespl <- sum(spl)/nrow(dat)
	Lden <- 10*log10(avespl)

	Lden
}



############################################
## this function computes daily 24-hour LDENs for the specified site
#
## modified because we may have multiple monitors per site.
############################################

LDEN24_site <- function(dat, sitemon) {
	sitedat <- dat[dat$siteMonitor == sitemon,]
	res <- data.frame()

	if (nrow(sitedat) < (60*60*24)) {
		s <- sitedat$datetime[1]
		e <- sitedat$datetime[nrow(sitedat)]
		l <- LDEN_site(sitedat, sitemon)
		res <- rbind(res, data.frame(siteMonitor=sitemon, start=s, end=e, LDEN=l))
	} else {

		for(i in 0:(floor(nrow(sitedat)/(60*60*24))-1)) {
			s <- sitedat$datetime[i*(60*60*24)+1]
			e <- sitedat$datetime[(i+1)*(60*60*24)]
			l <- LDEN_site(sitedat[ (i*(60*60*24)+1): ((i+1)*(60*60*24)),], sitemon) 
			res <- rbind(res, data.frame(siteMonitor=sitemon, start=s, end=e, LDEN=l))
		}
	}
	res
}

LDEN24list <- data.frame()
for (i in 1:length(allsitemonitors)) {
	test <- LDEN24_site(dat, allsitemonitors[i])
	LDEN24list <- rbind(LDEN24list, test)
}
LDEN24list





############################################
## QA/QC of the data by checking "doubles" (sites with two simulatenous monitors)
############################################

# Example
#19      63-858 70.88196
#20      63-720 70.95111

datpair <- dat[which(dat$siteMonitor=="63-858"),]
datpair2 <- dat[which(dat$siteMonitor=="63-720"),]
# merge them on timestamp
datpair <- merge(datpair, datpair2, by="datetime", all=FALSE)  # keep only matching times
head(datpair)

cor(datpair$DB.x, datpair$DB.y)
# [1] 0.9812812
summary(lm(datpair$DB.x ~ datpair$DB.y))
# Multiple R-squared:  0.9629


# Example
#27      64-301 71.32073
#28      64-720 67.96259

datpair <- dat[which(dat$siteMonitor=="64-301"),]
datpair2 <- dat[which(dat$siteMonitor=="64-720"),]
# merge them on timestamp
datpair <- merge(datpair, datpair2, by="datetime", all=FALSE)  # keep only matching times
head(datpair)

cor(datpair$DB.x, datpair$DB.y)
# 0.98246
summary(lm(datpair$DB.x ~ datpair$DB.y))
# Multiple R-squared:  0.9652


# Example
#37      53-720 70.28411
#38      53-301 70.47309

datpair <- dat[which(dat$siteMonitor=="53-720"),]
datpair2 <- dat[which(dat$siteMonitor=="53-301"),]
# merge them on timestamp
datpair <- merge(datpair, datpair2, by="datetime", all=FALSE)  # keep only matching times
head(datpair)

cor(datpair$DB.x, datpair$DB.y)
# [1] 0.9362993
summary(lm(datpair$DB.x ~ datpair$DB.y))
# Multiple R-squared:  0.8767



# Example
#47      70-301 77.17996
#48      70-858 81.24047

datpair <- dat[which(dat$siteMonitor=="70-301"),]
datpair2 <- dat[which(dat$siteMonitor=="70-858"),]
# merge them on timestamp
datpair <- merge(datpair, datpair2, by="datetime", all=FALSE)  # keep only matching times
head(datpair)

cor(datpair$DB.x, datpair$DB.y)
# 0.9595316
summary(lm(datpair$DB.x ~ datpair$DB.y))
# Multiple R-squared:  0.9207



# Example
#51  66-858 58.50689
#52  66-301 57.44846

datpair <- dat[which(dat$siteMonitor=="66-858"),]
datpair2 <- dat[which(dat$siteMonitor=="66-301"),]
# merge them on timestamp
datpair <- merge(datpair, datpair2, by="datetime", all=FALSE)  # keep only matching times
head(datpair)

cor(datpair$DB.x, datpair$DB.y)
# [1] 0.9828127
summary(lm(datpair$DB.x ~ datpair$DB.y))
# Multiple R-squared:  0.9659



# Example
#59  12-858 68.98107
#60  12-720 70.79685

datpair <- dat[which(dat$siteMonitor=="12-858"),]
datpair2 <- dat[which(dat$siteMonitor=="12-720"),]
# merge them on timestamp
datpair <- merge(datpair, datpair2, by="datetime", all=FALSE)  # keep only matching times
head(datpair)

cor(datpair$DB.x, datpair$DB.y)
# 0.9862473
summary(lm(datpair$DB.x ~ datpair$DB.y))
# Multiple R-squared:  0.9727





####################
## a histogram
####################

#LDN for all 24-hour measurements
p <- ggplot(LDN24list, aes(LDN)) + xlab("LDN (dB)") + ggtitle("Day-Night Average Noise Levels") + ylab("24-hour measurements")
  p + geom_histogram(color="black", fill="white") + geom_vline(xintercept=median(LDN24list$LDN), lty=2) 



####################
## descriptive stats
####################

#LDN for all 24-hour measurements

# Create summary table 
DBbyHR=as.data.frame(cbind(nrow(LDN24list), median(LDN24list$LDN), sd(LDN24list$LDN), min(LDN24list$LDN), max(LDN24list$LDN)))
colnames(DBbyHR)=c("N", "Median", "SD", "Min", "Max")
DBbyHR





####################
## collapse 24-hour noise measurements by site
####################

strsplit(LDN24list$siteMonitor, split="-")

LDN24list$Site.ID <- sapply(strsplit(as.character(LDN24list$siteMonitor), "-"), "[", 1)

LDN24list$RevLogDB <- 10^(LDN24list$LDN/10)
ag <- aggregate(LDN24list, list(Site.ID = LDN24list$Site.ID), mean)
LDNsitelist <- data.frame(cbind(Site.ID=ag[,1], LDN=10*log10(ag[,7])))
LDNsitelist$LDN <- as.numeric(as.character(LDNsitelist$LDN))
LDNsitelist



strsplit(LEQ24list$siteMonitor, split="-")

LEQ24list$Site.ID <- sapply(strsplit(as.character(LEQ24list$siteMonitor), "-"), "[", 1)

LEQ24list$RevLogDB <- 10^(LEQ24list$LEQ/10)
ag <- aggregate(LEQ24list, list(Site.ID = LEQ24list$Site.ID), mean)
LEQsitelist <- data.frame(cbind(Site.ID=ag[,1], LEQ=10*log10(ag[,7])))
LEQsitelist$LEQ <- as.numeric(as.character(LEQsitelist$LEQ))
LEQsitelist




####################
## descriptive stats
####################

#LDN for sites

# Create summary table 
DBbyHR=as.data.frame(cbind(nrow(LDNsitelist), median(LDNsitelist$LDN), sd(LDNsitelist$LDN), min(LDNsitelist$LDN), max(LDNsitelist$LDN)))
colnames(DBbyHR)=c("N", "Median", "SD", "Min", "Max")
DBbyHR



####################
## Appendix sites with LDNs, and LEQs
####################

SiteDat_LDN <- merge (SiteDat, LDNsitelist, by="Site.ID")
SiteDat_LDN <- merge (SiteDat_LDN, LEQsitelist, by="Site.ID")

head(SiteDat_LDN)

SiteDat_LDN$LDN <- format(round(SiteDat_LDN$LDN, 2), nsmall = 2)
SiteDat_LDN$LEQ <- format(round(SiteDat_LDN$LEQ, 2), nsmall = 2)
SiteDat_LDN$maskedLatitude <- format(round(SiteDat_LDN$maskedLatitude, 6), nsmall = 6)
SiteDat_LDN$maskedLongitude <- format(round(SiteDat_LDN$maskedLongitude, 6), nsmall = 6)


write.csv(SiteDat_LDN, "masked_SiteDat_LEQLDN_20190307.csv", row.names = FALSE)




####################
## Maps!
####################

##################
## Step 1: read in home location & LDEN data
###################

#get lat&lon of volunteer homes (previously converted from street addresses)
#data=read.csv("MapDatComb_20180920.csv") # Note I created this csv file in Excel by combining outputs from "noise_calcs.R" for LDN, LDEN, LEQ, %AboveThreshold, with lat and lon from Site ID file. [this could be coded in R]
head(SiteDat_LDN)

vol=as.data.frame(SiteDat_LDN)
lat=vol$Latitude
lon=vol$Longitude
LDN=vol$LDN
Site=vol$Site.ID

##################
## Step 2: Make maps varying scales
###################
#create breaks similar to scale from CONUS Road and Aviation Noise ( https://maps.bts.dot.gov/arcgis/apps/webappviewer/index.html?id=a303ff5924c9474790464cc0e9d5c9fb)

bks=c(35,40.01,45.01,50.01,55.01,60.01,65.01,70.01,75.01,80.01,85.01,90.01,95.01,100)

####LDN
BHmap= get_stamenmap(bbox = c(-122.35,47.49,-122.25,47.62), zoom=13, maptype = "toner-lite") 
ggmap(BHmap,  legend = "topleft", data=vol)+ 
  geom_point(data=vol,aes(lon,lat, fill=LDN),shape=21, cex=3)+ #add houses
  ggtitle("Beacon Hill, WA  Day-Night Average Noise Levels (LDN, dB)  Summer 2018") + 
  scale_fill_gradientn( limits = c(35,100),
                        colours=c("yellow", "red", "blue"), 
                        guide="legend",breaks=bks)+
  labs(x="Longitude °W",y="Latitude °N") 


# different scale
ggmap(BHmap,  legend = "topleft", data=vol)+ 
  geom_point(data=vol,aes(lon,lat, fill=LDN),shape=21, cex=3)+ #add houses
  ggtitle("Beacon Hill, WA  Day-Night Average Noise Levels (LDN, dB)  Summer 2018") + 
  scale_fill_gradientn( limits = c(50,90),
                        colours=c("green", "red"), 
                        guide="legend")+
  labs(x="Longitude °W",y="Latitude °N") 


# different scale
ggmap(BHmap,  legend = "topleft", data=vol)+ 
  geom_point(data=vol,aes(lon,lat, fill=LDN),shape=21, cex=3)+ #add houses
  ggtitle("Beacon Hill, WA  Day-Night Average Noise Levels (LDN, dB)  Summer 2018") + 
  scale_fill_gradientn(                         colours=c("green", "red"), 
                        guide="legend")+
  labs(x="Longitude °W",y="Latitude °N") 



######################################
## Try IDW model on the map
######################################

## Build a grid to the geographic limits of the data
# get the min/max range for lat/long to make an empty grid 
x.range <- as.numeric(c(min(lon), max(lon)))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(min(lat), max(lat)))  # min/max latitude of the interpolation area  
# from the range, exapnd the coordinates to make a regular grid
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00075), 
                   y = seq(from = y.range[1], to = y.range[2], by = 0.00075))  # expand 

## make the grid into spatial coords, grid, assign WGS84 CRS
# World Geographic System 1984 (lat/long) - mapping
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
coordinates(grd) <- ~x + y
sp::gridded(grd) <- TRUE
proj4string(grd) <- WGS84
# quick plot to see what we have
plot(grd, cex = 1.5, col = "grey")
vol_spat <-  SpatialPointsDataFrame(coords = vol[,c("Longitude", "Latitude")], 
                                   data = vol, 
                                   proj4string = WGS84)
points(vol_spat, pch = 1, col = "red", cex = 1)

### IDW interpolation of 'new_type' using gstat package
idw <- gstat::idw(formula = LDN ~ 1, locations = vol_spat, newdata = grd)  # apply idw model for the data

# grab output of IDW for plotting
idw.output = as.data.frame(idw)  # output is defined as a data table
# set the names of the idw.output columns
# basic ggplot using geom_tile to display our interpolated grid within no map
ggplot() + 
  geom_tile(data = idw.output, aes(x = x, y = y, fill = var1.pred)) + 
  geom_point(data = vol, aes(x = Longitude, y = Latitude), shape = 21, color = "red") +
  scale_fill_distiller(palette = "PuOr", direction = 1) +
  theme_bw() 


# use ggmap as earlier to display interpolation in geographic space
# Can take a LONG time to render (~1 minute)
ggmap(BHmap, extent = "normal") +
  geom_tile(data = idw.output, aes(x = x, y = y, fill = var1.pred), alpha = 0.75) + 
  geom_point(data = vol, aes(x = Longitude, y = Latitude), shape = 1, color = "blue") +
  ggtitle("Beacon Hill, WA  Interpolated Noise Levels (LDN, dB)  Summer 2018") +
  scale_fill_gradientn(                         colours=c("green", "red"), 
                        guide="legend", name="LDN")+
  labs(x="Longitude °W",y="Latitude °N")





######################################
## Options for changing scale in maps

#Map1: zoomed out (no houses missing)
#BHmap=get_map(location = c(-122.40,47.5,-122.22,47.59)) # left/bottom/right/top bounding box

##Map 2: most zoomed in with least houses missing  (1 house missing)
#BHmap=get_map(location = c(-122.40,47.5,-122.25,47.60))

#Map 3: Medium zoomed in (10 houses missing)

#Map 4: most zoomed in around Jefferson Park (30 houses missing)
#BHmap=get_map(location = c(-122.33,47.555,-122.285,47.575)) 

#note Site # 45, 64 are close to Port Authority JEfferson Park Monitor









######################################
######################################
######################################
######################################
OLD STUFF BELOW THIS LINE
######################################
######################################
######################################
######################################







######################################
# 1. Barplot with Sites with highest LDEN

#create new dataset with only higher LDEN sites
LDENHi=vol[which(vol$LDEN>70),]
length(LDENHi[,1])
colnames(LDENHi)[1]="Site.ID"
LDENHiDat=subset(dat, Site.ID %in% LDENHi$Site.ID)

#plot higher LDEN sites
p <- ggplot(LDENHiDat, aes(factor(Hr), DB)) + ylab("Noise (dB)") + xlab("Hour of day") + ggtitle("Beacon Hill Noise(dB) by Hour (LDEN>70, #sites=21)")
p + geom_boxplot()+
  geom_hline(yintercept=65, lty=2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(color="black",fill=NA)) +#,axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(plot.margin=unit(c(0,3,0,2), "cm")) #top, right, bottom, left

####################
## a histogram
####################

#LDEN
plot_site_histogram <- function (data, site) {
  p <- ggplot(vol[vol$Site.ID ==site,], aes(LDEN)) + xlab("LDEN (dB)") + ggtitle("LDEN")
  p + geom_histogram()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border =         element_rect(color="black",fill=NA)) +
    theme(plot.margin=unit(c(0,3,0,2), "cm")) 
}

plot_site_histogram(vol, allsites)

#LEQ
plot_site_histogram <- function (data, site) {
  p <- ggplot(vol[vol$Site.ID ==site,], aes(LEQ)) + xlab("LEQ (dB)") + ggtitle("LEQ")
  p + geom_histogram()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border =         element_rect(color="black",fill=NA)) +
    theme(plot.margin=unit(c(0,3,0,2), "cm")) 
  
}

plot_site_histogram(vol, allsites)

#LDN
plot_site_histogram <- function (data, site) {
  p <- ggplot(vol[vol$Site.ID ==site,], aes(LDN)) + xlab("LDN (dB)") + ggtitle("LDN")
  p + geom_histogram()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border =         element_rect(color="black",fill=NA)) +
    theme(plot.margin=unit(c(0,3,0,2), "cm")) 
  
}

plot_site_histogram(vol, allsites)

#Percent time above threshold
plot_site_histogram <- function (data, site) {
  p <- ggplot(vol[vol$Site.ID ==site,], aes(PercTimeAboveThreshold )) + xlab("Percent Time Above 65dB Threshold") + ggtitle("Percent Time Above 65dB Threshold")
  p + geom_histogram()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border =         element_rect(color="black",fill=NA)) +
    theme(plot.margin=unit(c(0,3,0,2), "cm")) 
  
}

plot_site_histogram(vol, allsites)


#################################################################OLD REMOVE
#reverse log to calculate summary stats
dat$RevLogDB <- 10^(dat[,"DB"]/10)
max(dat$DB) #check no "Inf" (produces from reverse log of "99999")

# calculate Summary statistics (mean, SD,n, CI)
RevDBmnAll=aggregate(RevLogDB~Hr, data=dat, FUN=mean, na.omit=T)
RevDBsdAll=aggregate(RevLogDB~Hr, data=dat, FUN=sd, na.rm=T)
RevDBnAll=aggregate(RevLogDB~Hr, data=dat, FUN=length)
RevDBciAll<-qnorm(0.975)*RevDBsdAll/sqrt(RevDBnAll) # 95% confidence interval

#log transform summary stats back to dB
aveDBbyHr <- 10*log10(RevDBmnAll)
sdDBbyHr <- 10*log10(RevDBsdAll)
ciDBbyHr <- 10*log10(RevDBciAll)

# Create summary table (mean, standard deviation, number of observations)
hrs=as.data.frame(sort(unique(dat$Hr)))
DBbyHR=as.data.frame(cbind(hrs,aveDBbyHr[,2], sdDBbyHr[,2], RevDBn[,2], ciDBbyHr[,2]))
colnames(DBbyHR)=c("Hr", "Mean", "SD", "n", "ci")
DBbyHR
 
#  2. Plot Mean Noise (dB) by hour for all site, all recorded days

# Plot mean, CI, dashed line at 65dB
ggplot(DBbyHR, aes(x=Hr, y=Mean)) + 
  geom_errorbar(width=.1, aes(ymin=Mean-ci, ymax=Mean+ci)) +
  geom_point()+
  geom_hline(yintercept=65, lty=2)+
  xlab("Hour of Day")+
  ylab("Mean Noise Level (dB)")+
  ggtitle("Hourly Levels of Noise")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border      =element_rect(color="black",fill=NA))#+

#################################################################







############################################
## Noise above a threshold level 
############################################

# while not technically a standard noise metric, it could be useful to calculate the amount of time above a particular threshold
# Assuming we have 1-sec measurements, a simple counting of the dat above the threshold gives the number of secs, or in this case, a percentage:

Threshold_site <- function(dat, site, threshold) {
	dat <- dat[dat$Site.ID == site,]
	nrow(dat[dat$DB>=threshold,])/nrow(dat) * 100  # as a percentage of time
}


for (i in 1:length(allsites)) {
	print(paste(allsites[i], Threshold_site(dat, allsites[i], 65)))
}



## this function computes daily 24-hour thresholds for the specified site
Threshold24_site <- function(dat, site, threshold) {
	sitedat <- dat[dat$Site.ID == site,]
	res <- data.frame()

	if (nrow(sitedat) < (60*60*24)) {
		s <- sitedat$datetime[1]
		e <- sitedat$datetime[nrow(sitedat)]
		l <- Threshold_site(sitedat, site, threshold)
		res <- rbind(res, data.frame(start=s, end=e, threshold=l))
	} else {

		for(i in 0:(floor(nrow(sitedat)/(60*60*24))-1)) {
			s <- sitedat$datetime[i*(60*60*24)+1]
			e <- sitedat$datetime[(i+1)*(60*60*24)]
			l <- Threshold_site(sitedat[ (i*(60*60*24)+1): ((i+1)*(60*60*24)),], site, threshold) 
			res <- rbind(res, data.frame(start=s, end=e, threshold=l))
		}
	}
	res
}

Threshold24_site(dat, 1, 60)
Threshold24_site(dat, 1, 65)
Threshold24_site(dat, 1, 70)

Threshold24_site(dat, 2, 60)
Threshold24_site(dat, 2, 65)
Threshold24_site(dat, 2, 70)

Threshold24_site(dat, 10, 60)
Threshold24_site(dat, 10, 65)
Threshold24_site(dat, 10, 70)

Threshold24_site(dat, 11, 60)
Threshold24_site(dat, 11, 65)
Threshold24_site(dat, 11, 70)


