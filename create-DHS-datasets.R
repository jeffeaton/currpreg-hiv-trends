## variables needed:
## 1. country
## 2. region
## 3. survyear
## 4. period
## 5. psu
## 6. stratum
## 7. pophivweight
## 8. sex
## 9. age
## 10. agegroup
## 11. hivres
## 12. currpreg


library(foreign)

####################################
####  Burkina Faso 2003 (BF43)  ####
####################################

dirData <- "~/Documents/Data/DHS/Burkina Faso/2003/"
bfar41 <- read.dta(paste(dirData, "BFAR41FL.DTA", sep=""))
bfir43 <- read.dta(paste(dirData, "BFIR43FL.DTA", sep=""))

## Recode HIV dataset
names(bfar41) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
bfar41 <- bfar41[,c("cluster", "household", "line", "hivres", "hivweight")]
table(bfar41$hivres) # check any other codes
bfar41$hivres[bfar41$hivres %in% c("hiv  positive", "hiv2 positive", "hiv1 & hiv2 positive")] <- "hiv  positive"
bfar41$hivres <- bfar41$hivres=="hiv  positive"

## Extract female dataset
bf03 <- with(bfir43, data.frame(country   = "Burkina Faso",
                                region    = "Western",
                                survyear  = "2003",
                                period    = "per1",
                                psu       = v021,
                                stratum   = factor(paste(v023, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

bf03 <- merge(bf03, bfar41)
bf03 <- subset(bf03, age %in% 15:49 & !is.na(hivres))

####################################
####  Burkina Faso 2010 (BF61)  ####
####################################

dirData <- "~/Documents/Data/DHS/Burkina Faso/2010/"
bfar61 <- read.dta(paste(dirData, "BFAR61FL.DTA", sep=""))
bfir61 <- read.dta(paste(dirData, "BFIR61FL.DTA", sep=""))

## Recode HIV dataset
names(bfar61) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
bfar61 <- bfar61[,c("cluster", "household", "line", "hivres", "hivweight")]
table(bfar61$hivres) # check any other codes
bfar61$hivres[bfar61$hivres == "indeterminant"] <- NA
bfar61$hivres[bfar61$hivres %in% c("hiv  positive", "hiv2 positive", "hiv1 & hiv2 positive")] <- "hiv  positive"
bfar61$hivres <- bfar61$hivres=="hiv  positive"

## Extract female dataset
bf10 <- with(bfir61, data.frame(country   = "Burkina Faso",
                                region    = "Western",
                                survyear  = "2010",
                                period    = "per2",
                                psu       = v021,
                                stratum   = factor(paste(v024, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

bf10 <- merge(bf10, bfar61)
bf10 <- subset(bf10, age %in% 15:49 & !is.na(hivres))

################################
####  Cameroon 2004 (CM44)  ####
################################

dirData <- "~/Documents/Data/DHS/Cameroon/2004/"
cmar42 <- read.dta(paste(dirData, "CMAR42FL.DTA", sep=""))
cmir44 <- read.dta(paste(dirData, "CMIR44FL.DTA", sep=""))

## Recode HIV dataset
names(cmar42) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
cmar42 <- cmar42[,c("cluster", "household", "line", "hivres", "hivweight")]
table(cmar42$hivres) # check any other codes
cmar42$hivres[cmar42$hivres == "indeterminant"] <- NA
cmar42$hivres <- cmar42$hivres == "hiv  positive"

## Extract female dataset
cm04 <- with(cmir44, data.frame(country   = "Cameroon",
                                region    = "Western",
                                survyear  = "2004",
                                period    = "per1",
                                psu       = v021,
                                stratum   = v023,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

cm04 <- merge(cm04, cmar42)
cm04 <- subset(cm04, age %in% 15:49 & !is.na(hivres))

################################
####  Cameroon 2011 (CM60)  ####
################################

dirData <- "~/Documents/Data/DHS/Cameroon/2011/"
cmar61 <- read.dta(paste(dirData, "CMAR61FL.DTA", sep=""))
cmir60 <- read.dta(paste(dirData, "CMIR60FL.DTA", sep=""))

## Recode HIV dataset
names(cmar61) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
cmar61 <- cmar61[,c("cluster", "household", "line", "hivres", "hivweight")]
table(cmar61$hivres) # check any other codes
cmar61$hivres[cmar61$hivres == "indeterminant"] <- NA
cmar61$hivres <- cmar61$hivres == "hiv  positive"

## Extract female dataset
cm11 <- with(cmir60, data.frame(country   = "Cameroon",
                                region    = "Western",
                                survyear  = "2011",
                                period    = "per2",
                                psu       = v021,
                                stratum   = factor(v022),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

cm11 <- merge(cm11, cmar61)
cm11 <- subset(cm11, age %in% 15:49 & !is.na(hivres))


#####################################
####  Cote d'Ivoire 2005 (CI50)  ####
#####################################

## Note: Both male and female data are in individual recode data set

dirData <- "~/Documents/Data/DHS/Cote d'Ivoire/2005/"
ciar50 <- read.dta(paste(dirData, "CIAR50FL.DTA", sep=""))
ciir50 <- read.dta(paste(dirData, "CIIR50FL.DTA", sep=""))

## Recode HIV dataset
names(ciar50) <- c("cluster", "structure", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
ciar50 <- ciar50[,c("cluster", "household", "line", "hivres", "hivweight")]
table(ciar50$hivres) # check any other codes
ciar50$hivres[ciar50$hivres == "indeterminant"] <- NA
ciar50$hivres <- ciar50$hivres == "hiv  positive"

## Extract female dataset
ci05 <- with(ciir50, data.frame(country   = "Cote d'Ivoire",
                                region    = "Western",
                                survyear  = "2005",
                                period    = "per1",
                                psu       = v021,
                                stratum   = factor(paste(v024, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = aidsex,
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

ci05 <- merge(ci05, ciar50)
ci05 <- subset(ci05, sex=="female" & age %in% 15:49 & !is.na(hivres) & hivweight != 0)


########################################
####  Cote d'Ivoire 2010-11 (CI50)  ####
########################################

dirData <- "~/Documents/Data/DHS/Cote d'Ivoire/2010-11/"
ciar61 <- read.dta(paste(dirData, "CIAR61FL.DTA", sep=""))
ciir61 <- read.dta(paste(dirData, "CIIR61FL.DTA", sep=""))

## Recode HIV dataset
names(ciar61) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
ciar61 <- ciar61[,c("cluster", "household", "line", "hivres", "hivweight")]
table(ciar61$hivres) # check any other codes
ciar61$hivres[ciar61$hivres == "indeterminant"] <- NA
ciar61$hivres <- ciar61$hivres == "hiv  positive"

## Extract female dataset
ci11 <- with(ciir61, data.frame(country   = "Cote d'Ivoire",
                                region    = "Western",
                                survyear  = "2010-11",
                                period    = "per2",
                                psu       = v021,
                                stratum   = v022,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = factor(v213, labels=c("no or unsure", "yes"))))

ci11 <- merge(ci11, ciar61)
ci11 <- subset(ci11, age %in% 15:49 & !is.na(hivres))


################################
####  Ethiopia 2005 (ET51)  ####
################################

dirData <- "~/Documents/Data/DHS/Ethiopia/2005/"
etar51 <- read.dta(paste(dirData, "ETAR51FL.DTA", sep=""))
etir51 <- read.dta(paste(dirData, "ETIR51FL.DTA", sep=""))

## Recode HIV dataset
names(etar51) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
etar51 <- etar51[,c("cluster", "household", "line", "hivres", "hivweight")]
table(etar51$hivres) # check any other codes
etar51$hivres[etar51$hivres == 8] <- NA
etar51$hivres <- etar51$hivres == 1

## Extract female dataset
et05 <- with(etir51, data.frame(country   = "Ethiopia",
                                region    = "Eastern",
                                survyear  = "2005",
                                period    = "per1",
                                psu       = v021,
                                stratum   = v023,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

et05 <- merge(et05, etar51)
et05 <- subset(et05, age %in% 15:49 & !is.na(hivres))


################################
####  Ethiopia 2011 (ET61)  ####
################################

dirData <- "~/Documents/Data/DHS/Ethiopia/2011/"
etar61 <- read.dta(paste(dirData, "ETAR61FL.DTA", sep=""))
etir61 <- read.dta(paste(dirData, "ETIR61FL.DTA", sep=""))

## Recode HIV dataset
names(etar61) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
etar61 <- etar61[,c("cluster", "household", "line", "hivres", "hivweight")]
table(etar61$hivres) # check any other codes
etar61$hivres[etar61$hivres %in% 7:8] <- NA
etar61$hivres <- etar61$hivres == 1

## Extract female dataset
et11 <- with(etir61, data.frame(country   = "Ethiopia",
                                region    = "Eastern",
                                survyear  = "2011",
                                period    = "per2",
                                psu       = v021,
                                stratum   = v022,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

et11 <- merge(et11, etar61)
et11 <- subset(et11, age %in% 15:49 & !is.na(hivres))


#############################
####  Kenya 2003 (KE42)  ####
#############################

dirData <- "~/Documents/Data/DHS/Kenya/2003/"
kear42 <- read.dta(paste(dirData, "KEar42fl.DTA", sep=""))
keir42 <- read.dta(paste(dirData, "KEIR42FL.DTA", sep=""))

## Recode HIV dataset
names(kear42) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
kear42 <- kear42[,c("cluster", "household", "line", "hivres", "hivweight")]
table(kear42$hivres) # check any other codes
kear42$hivres[kear42$hivres == "indeterminant"] <- NA
kear42$hivres <- kear42$hivres == "hiv  positive"

## Extract female dataset
ke03 <- with(keir42, data.frame(country   = "Kenya",
                                region    = "Eastern",
                                survyear  = "2003",
                                period    = "per1",
                                psu       = v021,
                                stratum   = factor(paste(v024, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

ke03 <- merge(ke03, kear42)
ke03 <- subset(ke03, age %in% 15:49 & !is.na(hivres))


################################
####  Kenya 2008-09 (KE52)  ####
################################

dirData <- "~/Documents/Data/DHS/Kenya/2008-09/"
kear51 <- read.dta(paste(dirData, "KEar51fl.DTA", sep=""))
keir52 <- read.dta(paste(dirData, "KEIR52FL.DTA", sep=""))

## Recode HIV dataset
names(kear51) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
kear51 <- kear51[,c("cluster", "household", "line", "hivres", "hivweight")]
table(kear51$hivres) # check any other codes
kear51$hivres[kear51$hivres == "indeterminant"] <- NA
kear51$hivres <- kear51$hivres == "hiv  positive"

## Extract female dataset
ke09 <- with(keir52, data.frame(country   = "Kenya",
                                region    = "Eastern",
                                survyear  = "2008-09",
                                period    = "per2",
                                psu       = v021,
                                stratum   = v022,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

ke09 <- merge(ke09, kear51)
ke09 <- subset(ke09, age %in% 15:49 & !is.na(hivres))


###############################
####  Lesotho 2004 (LS41)  ####
###############################

dirData <- "~/Documents/Data/DHS/Lesotho/2004/"
lsar41 <- read.dta(paste(dirData, "LSAR41FL.DTA", sep=""))
lsir41 <- read.dta(paste(dirData, "LSIR41FL.DTA", sep=""))

## Recode HIV dataset
names(lsar41) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
lsar41 <- lsar41[,c("cluster", "household", "line", "hivres", "hivweight")]
table(lsar41$hivres) # check any other codes
lsar41$hivres[lsar41$hivres == "indeterminant"] <- NA
lsar41$hivres <- lsar41$hivres == "hiv  positive"

## Extract female dataset
ls04 <- with(lsir41, data.frame(country   = "Lesotho",
                                region    = "Southern",
                                survyear  = "2004",
                                period    = "per1",
                                psu       = v021,
                                stratum   = factor(paste(v024, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

ls04 <- merge(ls04, lsar41)
ls04 <- subset(ls04, age %in% 15:49 & !is.na(hivres))


###############################
####  Lesotho 2009 (LS60)  ####
###############################

dirData <- "~/Documents/Data/DHS/Lesotho/2009/"
lsar60 <- read.dta(paste(dirData, "LSAR60FL.DTA", sep=""))
lsir60 <- read.dta(paste(dirData, "LSIR60FL.DTA", sep=""))

## Recode HIV dataset
names(lsar60) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
lsar60 <- lsar60[,c("cluster", "household", "line", "hivres", "hivweight")]
table(lsar60$hivres) # check any other codes
lsar60$hivres <- lsar60$hivres == "hiv  positive"

## Extract female dataset
ls09 <- with(lsir60, data.frame(country   = "Lesotho",
                                region    = "Southern",
                                survyear  = "2009",
                                period    = "per2",
                                psu       = v021,
                                stratum   = v022,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

ls09 <- merge(ls09, lsar60)
ls09 <- subset(ls09, age %in% 15:49 & !is.na(hivres))


##############################
####  Malawi 2004 (MW4D)  ####
##############################

## !!!! Stratum are possibly incorrect -- nead clarifaction about stratum

dirData <- "~/Documents/Data/DHS/Malawi/2004/"
mwar4a <- read.dta(paste(dirData, "MWAR4AFL.DTA", sep=""))
mwir4d <- read.dta(paste(dirData, "MWIR4dFL.DTA", sep=""))

## Recode HIV dataset
names(mwar4a) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
mwar4a <- mwar4a[,c("cluster", "household", "line", "hivres", "hivweight")]
table(mwar4a$hivres) # check any other codes
mwar4a$hivres <- mwar4a$hivres == "hiv  positive"

## Extract female dataset
mw04 <- with(mwir4d, data.frame(country   = "Malawi",
                                region    = "Southern",
                                survyear  = "2004",
                                period    = "per1",
                                psu       = v021,
                                stratum   = factor(paste(sdist, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

mw04 <- merge(mw04, mwar4a)
mw04 <- subset(mw04, age %in% 15:49 & !is.na(hivres))


##############################
####  Malawi 2010 (MW61)  ####
##############################

dirData <- "~/Documents/Data/DHS/Malawi/2010/"
mwar61 <- read.dta(paste(dirData, "MWAR61FL.DTA", sep=""))
mwir61 <- read.dta(paste(dirData, "MWIR61FL.DTA", sep=""))

## Recode HIV dataset
names(mwar61) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
mwar61 <- mwar61[,c("cluster", "household", "line", "hivres", "hivweight")]
table(mwar61$hivres) # check any other codes
mwar61$hivres[mwar61$hivres == "indeterminant"] <- NA
mwar61$hivres <- mwar61$hivres == "hiv  positive"

## Extract female dataset
mw10 <- with(mwir61, data.frame(country   = "Malawi",
                                region    = "Southern",
                                survyear  = "2010",
                                period    = "per2",
                                psu       = v021,
                                stratum   = factor(v022),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

mw10 <- merge(mw10, mwar61)
mw10 <- subset(mw10, age %in% 15:49 & !is.na(hivres))


##############################
####  Rwanda 2005 (RW53)  ####
##############################

dirData <- "~/Documents/Data/DHS/Rwanda/2005/"
rwar51 <- read.dta(paste(dirData, "RWAR51FL.DTA", sep=""))
rwir53 <- read.dta(paste(dirData, "RWIR53FL.DTA", sep=""))

## Recode HIV dataset
names(rwar51) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
rwar51 <- rwar51[,c("cluster", "household", "line", "hivres", "hivweight")]
table(rwar51$hivres) # check any other codes
rwar51$hivres <- rwar51$hivres == "hiv1  positive"

## Extract female dataset
rw05 <- with(rwir53, data.frame(country   = "Rwanda",
                                region    = "Eastern",
                                survyear  = "2005",
                                period    = "per1",
                                psu       = v021,
                                stratum   = factor(paste(v023, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

rw05 <- merge(rw05, rwar51)
rw05 <- subset(rw05, age %in% 15:49 & !is.na(hivres))


##############################
####  Rwanda 2010 (RW61)  ####
##############################

dirData <- "~/Documents/Data/DHS/Rwanda/2010/"
rwar61 <- read.dta(paste(dirData, "RWAR61FL.DTA", sep=""))
rwir61 <- read.dta(paste(dirData, "RWIR61FL.DTA", sep=""))

## Recode HIV dataset
names(rwar61) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
rwar61 <- rwar61[,c("cluster", "household", "line", "hivres", "hivweight")]
table(rwar61$hivres) # check any other codes
rwar61$hivres <- rwar61$hivres == "hiv  positive"

## Extract female dataset
rw10 <- with(rwir61, data.frame(country   = "Rwanda",
                                region    = "Eastern",
                                survyear  = "2010",
                                period    = "per2",
                                psu       = v021,
                                stratum   = v023,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

rw10 <- merge(rw10, rwar61)
rw10 <- subset(rw10, age %in% 15:49 & !is.na(hivres))


###############################
####  Senegal 2005 (SN4H)  ####
###############################

dirData <- "~/Documents/Data/DHS/Senegal/2005/"
snar4a <- read.dta(paste(dirData, "SNAR4aFL.DTA", sep=""))
snir4h <- read.dta(paste(dirData, "SNIR4HFL.DTA", sep=""))

## Recode HIV dataset
names(snar4a) <- c("cluster", "structure", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
snar4a <- snar4a[,c("cluster", "household", "line", "hivres", "hivweight")]
table(snar4a$hivres) # check any other codes
snar4a$hivres[snar4a$hivres %in% c("hiv  positive", "hiv2 positive")] <- "hiv  positive"
snar4a$hivres <- snar4a$hivres=="hiv  positive"

## Extract female dataset
sn05 <- with(snir4h, data.frame(country   = "Senegal",
                                region    = "Western",
                                survyear  = "2005",
                                period    = "per1",
                                psu       = v021,
                                stratum   = factor(paste(v024, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

sn05 <- merge(sn05, snar4a)
sn05 <- subset(sn05, age %in% 15:49 & !is.na(hivres))


##################################
####  Senegal 2010-11 (SN60)  ####
##################################

dirData <- "~/Documents/Data/DHS/Senegal/2010-11/"
snar60 <- read.dta(paste(dirData, "SNAR60FL.DTA", sep=""))
snir60 <- read.dta(paste(dirData, "SNIR60FL.DTA", sep=""))

## Recode HIV dataset
names(snar60) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
snar60 <- snar60[,c("cluster", "household", "line", "hivres", "hivweight")]
table(snar60$hivres) # check any other codes
snar60$hivres[snar60$hivres %in% c("hiv  positive", "hiv2 positive")] <- "hiv  positive"
snar60$hivres <- snar60$hivres=="hiv  positive"

## Extract female dataset
sn11 <- with(snir60, data.frame(country   = "Senegal",
                                region    = "Western",
                                survyear  = "2010-11",
                                period    = "per2",
                                psu       = v021,
                                stratum   = factor(paste(v024, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

sn11 <- merge(sn11, snar60)
sn11 <- subset(sn11, age %in% 15:49 & !is.na(hivres))


##################################
####  Tanzania 2003-04 (TZ4A) ####
##################################

## Note: both male and female data are in individual recode file

dirData <- "~/Documents/Data/DHS/Tanzania/2003-04/"
tzar4a <- read.dta(paste(dirData, "TZAR4AFL.DTA", sep=""))
tzir4a <- read.dta(paste(dirData, "TZIR4AFL.DTA", sep=""))

## Recode HIV dataset
names(tzar4a) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
tzar4a <- tzar4a[,c("cluster", "household", "line", "hivres", "hivweight")]
table(tzar4a$hivres) # check any other codes
tzar4a$hivres <- tzar4a$hivres == "hiv  positive"

## Extract female dataset
tz04 <- with(tzir4a, data.frame(country   = "Tanzania",
                                region    = "Eastern",
                                survyear  = "2003-04",
                                period    = NA,
                                psu       = v021,
                                stratum   = factor(paste(v024, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = factor(aidsex, c("men", "women"), c("male", "female")),
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = factor(v213, c("no or unsure", "yes"))))

tz04 <- merge(tz04, tzar4a)
tz04 <- subset(tz04, sex=="female" & age %in% 15:49 & !is.na(hivres))


##################################
####  Tanzania 2007-08 (TZ51) ####
##################################

dirData <- "~/Documents/Data/DHS/Tanzania/2007-08/"
tzar51 <- read.dta(paste(dirData, "TZAR51FL.DTA", sep=""))
tzir51 <- read.dta(paste(dirData, "TZIR51FL.DTA", sep=""))

## Recode HIV dataset
names(tzar51) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
tzar51 <- tzar51[,c("cluster", "household", "line", "hivres", "hivweight")]
table(tzar51$hivres) # check any other codes
tzar51$hivres <- tzar51$hivres == "hiv  positive"

## Extract female dataset
tz08 <- with(tzir51, data.frame(country   = "Tanzania",
                                region    = "Eastern",
                                survyear  = "2007-08",
                                period    = "per1",
                                psu       = v021,
                                stratum   = v023,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = factor(aidsex, c("men", "women"), c("male", "female")),
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

tz08 <- merge(tz08, tzar51)
tz08 <- subset(tz08, sex=="female" & age %in% 15:49 & !is.na(hivres))

tz08$stratum[tz08$stratum == "kagera:urban"] <- "kagera:rural"
tz08$stratum[tz08$stratum == "manyara:urban"] <- "manyara:rural"
tz08$stratum[tz08$stratum == "mara:urban"] <- "mara:rural"
tz08$stratum[tz08$stratum == "unguja 2:urban"] <- "unguja 2:rural"


##################################
####  Tanzania 2011-12 (TZ6A) ####
##################################

## Note: both male and female data are in individual recode file

dirData <- "~/Documents/Data/DHS/Tanzania/2011-12/"
tzar6a <- read.dta(paste(dirData, "TZAR6AFL.DTA", sep=""))
tzir6a <- read.dta(paste(dirData, "TZIR6AFL.DTA", sep=""))

## Recode HIV dataset
names(tzar6a) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
tzar6a <- tzar6a[,c("cluster", "household", "line", "hivres", "hivweight")]
table(tzar6a$hivres) # check any other codes
tzar6a$hivres <- tzar6a$hivres == "hiv  positive"

## Extract female dataset
tz12 <- with(tzir6a, data.frame(country   = "Tanzania",
                                region    = "Eastern",
                                survyear  = "2011-12",
                                period    = "per2",
                                psu       = v021,
                                stratum   = v023,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = aidsex,
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

tz12 <- merge(tz12, tzar6a)
tz12 <- subset(tz12, sex=="female" & age %in% 15:49 & !is.na(hivres))

tz12$stratum[tz12$stratum == "kagera urban"] <- "kagera rural"
tz12$stratum[tz12$stratum == "katavi urban"] <- "katavi rural"
tz12$stratum[tz12$stratum == "katavi urban"] <- "katavi rural"
tz12$stratum[tz12$stratum == "kusini urban"] <- "kusini rural"


###################################
####  Zimbabwe 2005-06 (ZW51)  ####
###################################

dirData <- "~/Documents/Data/DHS/Zimbabwe/2005-06/"
zwar51 <- read.dta(paste(dirData, "ZWAR51FL.DTA", sep=""))
zwir51 <- read.dta(paste(dirData, "ZWIR51FL.DTA", sep=""))

## Recode HIV dataset
names(zwar51) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
zwar51 <- zwar51[,c("cluster", "household", "line", "hivres", "hivweight")]
table(zwar51$hivres) # check any other codes
zwar51$hivres <- zwar51$hivres == "hiv  positive"

## Extract female dataset
zw06 <- with(zwir51, data.frame(country   = "Zimbabwe",
                                region    = "Southern",
                                survyear  = "2005-06",
                                period    = "per1",
                                psu       = v021,
                                stratum   = factor(paste(v023, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

zw06 <- merge(zw06, zwar51)
zw06 <- subset(zw06, age %in% 15:49 & !is.na(hivres))


###################################
####  Zimbabwe 2010-11 (ZW62)  ####
###################################

dirData <- "~/Documents/Data/DHS/Zimbabwe/2010-11/"
zwar61 <- read.dta(paste(dirData, "ZWAR61FL.DTA", sep=""))
zwir62 <- read.dta(paste(dirData, "ZWIR62FL.DTA", sep=""))

## Recode HIV dataset
names(zwar61) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
zwar61 <- zwar61[,c("cluster", "household", "line", "hivres", "hivweight")]
table(zwar61$hivres) # check any other codes
zwar61$hivres <- zwar61$hivres == "hiv  positive"

## Extract female dataset
zw11 <- with(zwir62, data.frame(country   = "Zimbabwe",
                                region    = "Southern",
                                survyear  = "2010-11",
                                period    = "per2",
                                psu       = v021,
                                stratum   = v022,
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

zw11 <- merge(zw11, zwar61)
zw11 <- subset(zw11, age %in% 15:49 & !is.na(hivres))


####################################
####  Swaziland 2006-07 (SZ51)  ####
####################################

dirData <- "~/Documents/Data/DHS/Swaziland/2006-07/"
szar51 <- read.dta(paste(dirData, "SZAR51FL.DTA", sep=""))
szir51 <- read.dta(paste(dirData, "SZIR51FL.DTA", sep=""))

## Recode HIV dataset
names(szar51) <- c("cluster", "household", "line", "hiv01", "hiv02", "hivres", "hivweight")
szar51 <- szar51[,c("cluster", "household", "line", "hivres", "hivweight")]
table(szar51$hivres) # check any other codes
szar51$hivres <- szar51$hivres == "hiv  positive"

## Extract female dataset
sz07 <- with(szir51, data.frame(country   = "Swaziland",
                                region    = "Southern",
                                survyear  = "2006-07",
                                period    = "per1",
                                psu       = v021,
                                stratum   = factor(paste(v024, v025)),
                                cluster   = v001,
                                household = v002,
                                line      = v003,                                
                                sex       = "female",
                                age       = v012,
                                agegroup  = v013,
                                currpreg  = v213))

sz07 <- merge(sz07, szar51)
sz07 <- subset(sz07, age %in% 15:49 & !is.na(hivres))


######################################################
####  Denormalize weights to population weights  ####
######################################################

## UN WPP 2012 data from http://esa.un.org/wpp/ASCII-Data/DISK_NAVIGATION_ASCII.htm. Accessed 17 March 2014

wpp <- read.fwf("~/Documents/Data/UN Population Prospects/WPP2012_DB04_POPULATION_ANNUAL/WPP2012_DB04_Population_Annual.dat", c(4, 59, 1, 6, 4, 6, 1, 6, 5, 3, 2, 20))
colnames(wpp) <- c("LocId", "Location", "VarID", "Variant", "Time", "MidPeriod", "SexID", "Sex", "AgeGrp", "AgeGrpStart", "AgeGrpSpan", "Value")
wpp$Location <- sub(" +$", "", wpp$Location)
wpp$Sex <- sub(" +$", "", wpp$Sex)
wpp$Variant <- sub(" +$", "", wpp$Variant)
wpp <- subset(wpp, Variant == "Medium" & AgeGrpStart %in% 15:45 & Time %in% 2001:2012)

bfpop <- 1e3*with(subset(wpp, Location=="Burkina Faso" & Time==2012), tapply(Value, Sex, sum))
cmpop <- 1e3*with(subset(wpp, Location=="Cameroon" & Time==2012), tapply(Value, Sex, sum))
cipop <- 1e3*with(subset(wpp, Location=="C\364te d'Ivoire" & Time==2012), tapply(Value, Sex, sum))
etpop <- 1e3*with(subset(wpp, Location=="Ethiopia" & Time==2012), tapply(Value, Sex, sum))
kepop <- 1e3*with(subset(wpp, Location=="Kenya" & Time==2012), tapply(Value, Sex, sum))
lspop <- 1e3*with(subset(wpp, Location=="Lesotho" & Time==2012), tapply(Value, Sex, sum))
mwpop <- 1e3*with(subset(wpp, Location=="Malawi" & Time==2012), tapply(Value, Sex, sum))
rwpop <- 1e3*with(subset(wpp, Location=="Rwanda" & Time==2012), tapply(Value, Sex, sum))
snpop <- 1e3*with(subset(wpp, Location=="Senegal" & Time==2012), tapply(Value, Sex, sum))
tzpop <- 1e3*with(subset(wpp, Location=="United Republic of Tanzania" & Time==2012), tapply(Value, Sex, sum))
zwpop <- 1e3*with(subset(wpp, Location=="Zimbabwe" & Time==2012), tapply(Value, Sex, sum))
szpop <- 1e3*with(subset(wpp, Location=="Swaziland" & Time==2012 & AgeGrpStart%in%15:45), tapply(Value, Sex, sum))

bf03$pophivweight <- bf03$hivweight/1e6 * bfpop["Female"]/sum(bf03$hivweight/1e6, na.rm=TRUE)
bf10$pophivweight <- bf10$hivweight/1e6 * bfpop["Female"]/sum(bf10$hivweight/1e6, na.rm=TRUE)
cm04$pophivweight <- cm04$hivweight/1e6 * cmpop["Female"]/sum(cm04$hivweight/1e6, na.rm=TRUE)
cm11$pophivweight <- cm11$hivweight/1e6 * cmpop["Female"]/sum(cm11$hivweight/1e6, na.rm=TRUE)
ci05$pophivweight <- ci05$hivweight/1e6 * cipop["Female"]/sum(ci05$hivweight/1e6, na.rm=TRUE)
ci11$pophivweight <- ci11$hivweight/1e6 * cipop["Female"]/sum(ci11$hivweight/1e6, na.rm=TRUE)
et05$pophivweight <- et05$hivweight/1e6 * etpop["Female"]/sum(et05$hivweight/1e6, na.rm=TRUE)
et11$pophivweight <- et11$hivweight/1e6 * etpop["Female"]/sum(et11$hivweight/1e6, na.rm=TRUE)
ke03$pophivweight <- ke03$hivweight/1e6 * kepop["Female"]/sum(ke03$hivweight/1e6, na.rm=TRUE)
ke09$pophivweight <- ke09$hivweight/1e6 * kepop["Female"]/sum(ke09$hivweight/1e6, na.rm=TRUE)
ls04$pophivweight <- ls04$hivweight/1e6 * lspop["Female"]/sum(ls04$hivweight/1e6, na.rm=TRUE)
ls09$pophivweight <- ls09$hivweight/1e6 * lspop["Female"]/sum(ls09$hivweight/1e6, na.rm=TRUE)
mw04$pophivweight <- mw04$hivweight/1e6 * mwpop["Female"]/sum(mw04$hivweight/1e6, na.rm=TRUE)
mw10$pophivweight <- mw10$hivweight/1e6 * mwpop["Female"]/sum(mw10$hivweight/1e6, na.rm=TRUE)
rw05$pophivweight <- rw05$hivweight/1e6 * rwpop["Female"]/sum(rw05$hivweight/1e6, na.rm=TRUE)
rw10$pophivweight <- rw10$hivweight/1e6 * rwpop["Female"]/sum(rw10$hivweight/1e6, na.rm=TRUE)
sn05$pophivweight <- sn05$hivweight/1e6 * snpop["Female"]/sum(sn05$hivweight/1e6, na.rm=TRUE)
sn11$pophivweight <- sn11$hivweight/1e6 * snpop["Female"]/sum(sn11$hivweight/1e6, na.rm=TRUE)
tz04$pophivweight <- tz04$hivweight/1e6 * tzpop["Female"]/sum(tz04$hivweight/1e6, na.rm=TRUE)
tz08$pophivweight <- tz08$hivweight/1e6 * tzpop["Female"]/sum(tz08$hivweight/1e6, na.rm=TRUE)
tz12$pophivweight <- tz12$hivweight/1e6 * tzpop["Female"]/sum(tz12$hivweight/1e6, na.rm=TRUE)
zw06$pophivweight <- zw06$hivweight/1e6 * zwpop["Female"]/sum(zw06$hivweight/1e6, na.rm=TRUE)
zw11$pophivweight <- zw11$hivweight/1e6 * zwpop["Female"]/sum(zw11$hivweight/1e6, na.rm=TRUE)
sz07$pophivweight <- sz07$hivweight/1e6 * szpop["Female"]/sum(sz07$hivweight/1e6, na.rm=TRUE)


#############################
####  Save the datasets  ####
#############################

save(bf03, bf10, cm04, cm11, ci05, ci11, et05, et11, ke03, ke09, ls04, ls09, mw04, mw10, rw05, rw10, sn05, sn11, tz04, tz08, tz12, zw06, zw11, sz07,
     file="currpreg-dhs-datasets.RData")
