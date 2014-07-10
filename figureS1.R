rm(list=ls())
setwd("./")

####################
####  DHS data  ####
####################

load("currpreg-dhs-datasets-incl-men.RData")

######################
####  Kenya data  ####
######################

## update period for ke03 and ke09

ke03$period <- NA
ke09$period <- "per1"


## KAIS 2007 and 2012

library(sas7bdat)
ke07 <- read.sas7bdat("~/Documents/Data/Kenya/KAIS/surveymodel_kais2007.sas7bdat")
ke12 <- read.sas7bdat("~/Documents/Data/Kenya/KAIS/survemodel_kais2012.sas7bdat")

ke07$age <- ke07$Q103
ke07$sex <- factor(is.na(ke07$currentpreg), c(TRUE, FALSE), c("male", "female")) # this is wrong for age 50+

ke07 <- with(subset(ke07, age %in% 15:49),
             data.frame(cluster        = QCLUST,
                        household      = NA,
                        line           = NA,
                        country        = "Kenya",
                        region         = "Eastern",
                        survyear       = "2007",
                        period         = NA,
                        psu            = QCLUST,
                        stratum        = factor(STRATA1),
                        sex            = sex,
                        age            = age,
                        agegroup       = cut(age, 3:10*5, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), right=FALSE),
                        currpreg       = factor(currentpreg, 1:2, c("yes", "no or unsure")),
                        hivres         = hiv3 == 1,
                        hivweight      = BL_WEIGHT,
                        pophivweight   = sum(ke09$pophivweight)*BL_WEIGHT/sum(BL_WEIGHT)))

ke12$age <- ke12$q_102
ke12$sex <- factor(ke12$sex, 1:2, c("male", "female"))

ke12 <- with(subset(ke12, age %in% 15:49 & !is.na(abweight)),
             data.frame(cluster        = qclust,
                        household      = NA,
                        line           = NA,
                        country        = "Kenya",
                        region         = "Eastern",
                        survyear       = "2012",
                        period         = "per2",
                        psu            = qclust,
                        stratum        = factor(Strata2),
                        sex            = sex,
                        age            = age,
                        agegroup       = cut(age, 3:10*5, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), right=FALSE),
                        currpreg       = factor(currentpreg, 1:2, c("yes", "no or unsure")),
                        hivres         = hiv == 1,
                        hivweight      = abweight,
                        pophivweight   = sum(ke09$pophivweight)*abweight/sum(abweight)))


#############################
####  South Africa data  ####
#############################

## UN WPP 2012 -- female 15-49 population size, 2012: 14095144
sa.wpp2012.fempopsize <- 14095144
sa.wpp2012.malepopsize <- 14055684
sa.wpp2012.bothpopsize <- 28151302


library(foreign)

sa05 <- read.dta("~/Documents/Data/South Africa/HSRC surveys/currpreg-hiv-trends/ANC vs population prevalence2005.dta")
sa08 <- read.dta("~/Documents/Data/South Africa/HSRC surveys/currpreg-hiv-trends/ANC vs population prevalence2008.dta")
sa12 <- read.dta("~/Documents/Data/South Africa/HSRC surveys/currpreg-hiv-trends/ANC vs population prevalence2012.dta")

sa05$sex <- factor(sa05$sex, c("Male", "Female"), c("male", "female"))
sa08$sex <- factor(sa08$sex, c("male", "female"), c("male", "female"))
sa12$sex <- factor(sa12$sex, c("Male", "Female"), c("male", "female"))

sa05$age <- floor(sa05$age)
sa08$age <- floor(sa08$age)
sa12$age <- floor(sa12$age)

sa05 <- with(subset(sa05, age %in% 15:49 & !is.na(hivweight)),
             data.frame(cluster        = psu,
                        household      = household,
                        line           = line,
                        country        = "South Africa",
                        region         = "Southern",
                        survyear       = "2005",
                        period         = NA,
                        psu            = psu,
                        stratum        = factor(stratum),
                        sex            = sex,
                        age            = age,
                        agegroup       = cut(age, 3:10*5, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), right=FALSE),
                        currpreg       = factor(currpreg, c("No or unsure", "Yes"), c("no or unsure", "yes")),
                        hivres         = hivres== "HIV+",
                        hivweight      = hivweight,
                        pophivweight   = hivweight*sa.wpp2012.bothpopsize/sum(hivweight)))

sa08 <- with(subset(sa08, age %in% 15:49 & !is.na(hivweight)),
             data.frame(cluster        = psu,
                        household      = household,
                        line           = line,
                        country        = "South Africa",
                        region         = "Southern",
                        survyear       = "2008",
                        period         = "per1",
                        psu            = psu,
                        stratum        = factor(stratum),
                        sex            = sex,
                        age            = age,
                        agegroup       = cut(age, 3:10*5, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), right=FALSE),
                        currpreg       = factor(currpreg, c("No or unsure", "Yes"), c("no or unsure", "yes")),
                        hivres         = hivres== "HIV+",
                        hivweight      = hivweight,
                        pophivweight   = hivweight*sa.wpp2012.bothpopsize/sum(hivweight)))

sa12 <- with(subset(sa12, age %in% 15:49 & !is.na(hivweight)),
             data.frame(cluster        = psu,
                        household      = household,
                        line           = line,
                        country        = "South Africa",
                        region         = "Southern",
                        survyear       = "2012",
                        period         = "per2",
                        psu            = psu,
                        stratum        = factor(stratum),
                        sex            = sex,
                        age            = age,
                        agegroup       = cut(age, 3:10*5, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), right=FALSE),
                        currpreg       = factor(currpreg, c("No or unsure", "Yes"), c("no or unsure", "yes")),
                        hivres         = hivres== "HIV+",
                        hivweight      = hivweight,
                        pophivweight   = hivweight*sa.wpp2012.bothpopsize/sum(hivweight)))


##########################
####  Swaziland data  ####
##########################

sz07 <- subset(sz07, age %in% 18:49)   # restrict to 18-49 to be comparable with SHIMS

## SZ 2011 from SHIMS survey (18-49 only)
sz11 <- read.csv("~/Documents/Data/Swaziland/SHIMS/Eaton_DURDC_request_12June2014.csv")
sz11 <- rbind(with(sz11, data.frame(cluster        = shimsea,
                              household      = uhhid,
                              line           = NA,
                              country        = "Swaziland",
                              region         = "Southern",
                              survyear       = "2011",
                              period         = "per2",
                              psu            = shimsea,
                              stratum        = factor(paste(urban, region)),
                              sex            = factor(gender, c("Male", "Female"), c("male", "female")),
                              age            = age,
                              agegroup       = cut(age, 3:10*5, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), right=FALSE),
                              currpreg       = factor(preg, c("No", "Yes"), c("no or unsure", "yes")),
                              hivres         = hivstatust1 == "Pos",
                              hivweight      = wtcohort,
                              pophivweight   = sum(subset(sz07, sex=="female")$pophivweight)*wtcohort/sum(wtcohort))),
              data.frame(cluster        = 999,
                         household      = NA,
                         line           = NA,
                         country        = "Swaziland",
                         region         = "Southern",
                         survyear       = "2011",
                         period         = "per2",
                         psu            = 999,
                         stratum        = factor(1),
                         sex            = "male",
                         age            = NA,
                         agegroup       = rep(c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), c(995, 2093, 1682, 1267, 993, 728, 570)),
                         currpreg       = NA,
                         hivres         = rep(rep(c(TRUE, FALSE), 7), round(c(rbind(c(0.008, 0.066, 0.213, 0.366, 0.470, 0.455, 0.425),
                           1-c(0.008, 0.066, 0.213, 0.366, 0.470, 0.455, 0.425))) * 
                           rep(c(995, 2093, 1682, 1267, 993, 728, 570), each=2))),
                         hivweight      = 1.2,
                         pophivweight   = sum(subset(sz07, sex=="male")$pophivweight)/8328))


##############################
####  Create pooled data  ####
##############################

library(survey)

pooldat <- rbind(bf03, bf10,
                 cm04, cm11,
                 ci05, ci11,
                 et05, et11,
                 ke03, ke07, ke09, ke12,
                 ls04, ls09,
                 mw04, mw10,
                 rw05, rw10,
                 sn05, sn11,
                 tz08, tz12, tz04,
                 zw06, zw11,                  
                 sa08, sa12, sa05,
                 sz07, sz11)

pooldat$country <- factor(pooldat$country, c("Burkina Faso", "Cameroon", "Cote d'Ivoire", "Senegal",
                                             "Ethiopia", "Kenya", "Rwanda", "Tanzania",
                                             "Lesotho", "Malawi", "South Africa", "Swaziland", "Zimbabwe"))
pooldat$agegroup <- factor(pooldat$agegroup)
pooldat$hivres <- as.numeric(pooldat$hivres)
pooldat$currpreg[pooldat$sex=="male"] <- "no or unsure"

pooldes <- svydesign(ids=~psu, strata=~stratum+country+survyear, weights=~pophivweight, nest=TRUE, data=pooldat)
femdes <- subset(pooldes, sex=="female")
maledes <- subset(pooldes, sex=="male")


####################
####  Analysis  ####
####################

fem.prev.country <- svyby(~hivres, ~country+period, femdes, svyciprop, vartype=c("ci", "var", "se"))
fem.prev.region <- svyby(~hivres, ~region+period, femdes, svyciprop, vartype=c("ci", "var", "se"))
fem.prev.all <- svyby(~hivres, ~period, femdes, svyciprop, vartype=c("ci", "var", "se"))
fem.prev <- rbind(setNames(fem.prev.country, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                  setNames(fem.prev.region, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                  setNames(cbind("All", fem.prev.all), c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")))

male.prev.country <- svyby(~hivres, ~country+period, maledes, svyciprop, vartype=c("ci", "var", "se"))
male.prev.region <- svyby(~hivres, ~region+period, maledes, svyciprop, vartype=c("ci", "var", "se"))
male.prev.all <- svyby(~hivres, ~period, maledes, svyciprop, vartype=c("ci", "var", "se"))
male.prev <- rbind(setNames(male.prev.country, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                  setNames(male.prev.region, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                  setNames(cbind("All", male.prev.all), c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")))


########################################################
####  Figure XX: Prevalence trends in men vs women  ####
########################################################

country.codes <- c("BF", "CM", "CI", "SN", "ET", "KE", "RW", "TZ", "LS", "MW", "ZA", "SZ", "ZW")
figS1 <- data.frame(code=c(country.codes, rep(NA, 4)),
                    fem=subset(fem.prev, period=="per2")$hivres/subset(fem.prev, period=="per1")$hivres - 1,
                     male=subset(male.prev, period=="per2")$hivres/subset(male.prev, period=="per1")$hivres - 1)

## quartz(h=2.7, w=3, pointsize=8)
pdf("currpreg-hiv-trends_figureS1.pdf", h=2.7, w=3, pointsize=8)
par(mar=c(3, 3.5, 0.5, 0.5), cex=1, tcl=-0.25, mgp=c(2, 0.5, 0), cex.axis=0.9, las=1)
plot(100*figS1$fem, 100*figS1$male, type="n", ylim=c(-70, 30), xlim=c(-70, 30),
     xlab="Women: relative prevalence change",
     ylab="",
     xaxt="n", yaxt="n")
abline(0, 1, col="grey")
abline(h=0, v=0, lty=2, col="grey")
text(100*figS1$fem[1:13], 100*figS1$male[1:13], country.codes, cex=0.7, font=2, col="darkred")
points(100*figS1$fem[14:17], 100*figS1$male[14:17], pch=1:4, lwd=1.5, cex=1.2)
axis(1, -7:3*10, c(NA, "-60%", NA, "-40%", NA, "-20%", NA, "0%", NA, "20%", NA))
axis(2, -7:3*10, c(NA, "-60%", NA, "-40%", NA, "-20%", NA, "0%", NA, "20%", NA))
mtext("Men: relative prevalence change", 2, 2.5, las=3)
legend("topleft", c("Western", "Eastern", "Southern", "All"), pch=1:4, lwd=1.5, pt.cex=1.2, lty=0, cex=0.9)
dev.off()
