rm(list=ls())
setwd("./")

####################
####  DHS data  ####
####################

load("currpreg-dhs-datasets.RData")

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

ke07 <- with(subset(ke07, sex == "female" & age %in% 15:49),
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

ke12 <- with(subset(ke12, sex == "female" & age %in% 15:49 & !is.na(abweight)),
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

sa05 <- with(subset(sa05, sex == "female" & age %in% 15:49 & !is.na(hivweight)),
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
                        pophivweight   = hivweight*sa.wpp2012.fempopsize/sum(hivweight)))

sa08 <- with(subset(sa08, sex == "female" & age %in% 15:49 & !is.na(hivweight)),
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
                        pophivweight   = hivweight*sa.wpp2012.fempopsize/sum(hivweight)))

sa12 <- with(subset(sa12, sex == "female" & age %in% 15:49 & !is.na(hivweight)),
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
                        pophivweight   = hivweight*sa.wpp2012.fempopsize/sum(hivweight)))


##########################
####  Swaziland data  ####
##########################

sz07 <- subset(sz07, age %in% 18:49)   # restrict to 18-49 to be comparable with SHIMS

## SZ 2011 from SHIMS survey (18-49 only)
sz11 <- read.csv("~/Documents/Data/Swaziland/SHIMS/Eaton_DURDC_request_12June2014.csv")
sz11 <- with(sz11, data.frame(cluster        = shimsea,
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
                               pophivweight   = sum(sz07$pophivweight)*wtcohort/sum(wtcohort)))


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
pooldat$hivres <- as.numeric(pooldat$hivres)

pooldes <- svydesign(ids=~psu, strata=~stratum+country+survyear, weights=~pophivweight, nest=TRUE, data=pooldat)
femdes <- pooldes
pregdes <- subset(pooldes, currpreg=="yes")


####################
####  Analysis  ####
####################

###  percentage pregnant  ###
frac.preg.country <- svyby(~currpreg, ~country+period, femdes, svyciprop, vartype=c("se", "ci"), na.rm=TRUE)
frac.preg.region <- svyby(~currpreg, ~region+period, femdes, svyciprop, vartype=c("se", "ci"), na.rm=TRUE)
frac.preg.all <- svyby(~currpreg, ~period, femdes, svyciprop, vartype=c("se", "ci"), na.rm=TRUE)
frac.preg <- rbind(setNames(frac.preg.country, c("area", "period", "currpreg", "se", "ci_l", "ci_u")),
                   setNames(frac.preg.region, c("area", "period", "currpreg", "se", "ci_l", "ci_u")),
                   setNames(cbind("All", frac.preg.all), c("area", "period", "currpreg", "se", "ci_l", "ci_u")))

###  HIV prevalence ###
fem.prev.country <- svyby(~hivres, ~country+period, femdes, svyciprop, vartype=c("ci", "var", "se"))
fem.prev.region <- svyby(~hivres, ~region+period, femdes, svyciprop, vartype=c("ci", "var", "se"))
fem.prev.all <- svyby(~hivres, ~period, femdes, svyciprop, vartype=c("ci", "var", "se"))
fem.prev <- rbind(setNames(fem.prev.country, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                  setNames(fem.prev.region, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                  setNames(cbind("All", fem.prev.all), c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")))

preg.prev.country <- svyby(~hivres, ~country+period, pregdes, svyciprop, vartype=c("ci", "var", "se"))
preg.prev.region <- svyby(~hivres, ~region+period, pregdes, svyciprop, vartype=c("ci", "var", "se"))
preg.prev.all <- svyby(~hivres, ~period, pregdes, svyciprop, vartype=c("ci", "var", "se"))
preg.prev <- rbind(setNames(preg.prev.country, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                   setNames(preg.prev.region, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                   setNames(cbind("All", preg.prev.all), c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")))

fem.prev.change <- subset(fem.prev, period=="per2")$hivres - subset(fem.prev, period=="per1")$hivres
preg.prev.change <- subset(preg.prev, period=="per2")$hivres - subset(preg.prev, period=="per1")$hivres

fem.prev.change.var <- subset(fem.prev, period=="per2")$var + subset(fem.prev, period=="per1")$var
preg.prev.change.var <- subset(preg.prev, period=="per2")$var + subset(preg.prev, period=="per1")$var

excess.decline <- fem.prev.change - preg.prev.change
excess.decline.var <- fem.prev.change.var + preg.prev.change.var -
  subset(frac.preg, period=="per1")$currpreg*subset(preg.prev, period=="per1")$var -
  subset(frac.preg, period=="per2")$currpreg*subset(preg.prev, period=="per2")$var


###  Age standardized prevalence  ###

## age-specific prevalence
fem.ageprev.country <- svyby(~hivres, ~country+agegroup+period, femdes, svyciprop, vartype=c("ci", "se", "var"))
fem.ageprev.region <- svyby(~hivres, ~region+agegroup+period, femdes, svyciprop, vartype=c("ci", "se", "var"))
fem.ageprev.all <- svyby(~hivres, ~agegroup+period, femdes, svyciprop, vartype=c("ci", "se", "var"))
fem.ageprev <- rbind(setNames(fem.ageprev.country, c("area", "agegroup", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                     setNames(fem.ageprev.region, c("area", "agegroup", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                     setNames(cbind("All", fem.ageprev.all), c("area", "agegroup", "period", "hivres", "se", "ci_l", "ci_u", "var")))

preg.ageprev.country <- svyby(~hivres, ~country+agegroup+period, pregdes, svyciprop, vartype=c("ci", "se", "var"))
preg.ageprev.region <- svyby(~hivres, ~region+agegroup+period, pregdes, svyciprop, vartype=c("ci", "se", "var"))
preg.ageprev.all <- svyby(~hivres, ~agegroup+period, pregdes, svyciprop, vartype=c("ci", "se", "var"))
preg.ageprev <- rbind(setNames(preg.ageprev.country, c("area", "agegroup", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                     setNames(preg.ageprev.region, c("area", "agegroup", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                     setNames(cbind("All", preg.ageprev.all), c("area", "agegroup", "period", "hivres", "se", "ci_l", "ci_u", "var")))

## age distribution
fem.agedist.country <- svyby(~agegroup, ~country+period, femdes, svymean, vartype=c("ci", "se", "var"))
fem.agedist.region <- svyby(~agegroup, ~region+period, femdes, svymean, vartype=c("ci", "se", "var"), na.rm=TRUE)
fem.agedist.all <- svyby(~agegroup, ~period, femdes, svymean, vartype=c("ci", "se", "var"), na.rm=TRUE)
fem.agedist.region.pool <- svyby(~agegroup, ~region, femdes, svymean, vartype=c("ci", "se", "var"), na.rm=TRUE)
fem.agedist.all.pool <- svyby(~agegroup, ~factor(1), femdes, svymean, vartype=c("ci", "se", "var"), na.rm=TRUE)

preg.agedist.country <- svyby(~agegroup, ~country+period, pregdes, svymean, vartype=c("ci", "se", "var"))
preg.agedist.region <- svyby(~agegroup, ~region+period, pregdes, svymean, vartype=c("ci", "se", "var"), na.rm=TRUE)
preg.agedist.all <- svyby(~agegroup, ~period, pregdes, svymean, vartype=c("ci", "se", "var"), na.rm=TRUE)
preg.agedist.region.pool <- svyby(~agegroup, ~region, pregdes, svymean, vartype=c("ci", "se", "var"), na.rm=TRUE)
preg.agedist.all.pool <- svyby(~agegroup, ~factor(1), pregdes, svymean, vartype=c("ci", "se", "var"), na.rm=TRUE)

nAG <- length(levels(pooldat$agegroup))

fem.agedist.country <- data.frame(area=rep(fem.agedist.country$country, nAG),
                                   agegroup = rep(levels(pooldat$agegroup), each=nrow(fem.agedist.country)),
                                   period = rep(fem.agedist.country$period, nAG),
                                   prop = unlist(fem.agedist.country[,2 + 1:nAG]),
                                   prop.se = unlist(fem.agedist.country[,2 + 1*nAG + 1:nAG]),
                                   prop.ci_l = unlist(fem.agedist.country[,2 + 2*nAG + 1:nAG]),
                                   prop.ci_u = unlist(fem.agedist.country[,2 + 3*nAG + 1:nAG]),
                                   prop.var = unlist(fem.agedist.country[,2 + 4*nAG + 1:nAG]))
fem.agedist.region <- data.frame(area=rep(fem.agedist.region$region, nAG),
                                   agegroup = rep(levels(pooldat$agegroup), each=nrow(fem.agedist.region)),
                                   period = rep(fem.agedist.region$period, nAG),
                                   prop = unlist(fem.agedist.region[,2 + 1:nAG]),
                                   prop.se = unlist(fem.agedist.region[,2 + 1*nAG + 1:nAG]),
                                   prop.ci_l = unlist(fem.agedist.region[,2 + 2*nAG + 1:nAG]),
                                   prop.ci_u = unlist(fem.agedist.region[,2 + 3*nAG + 1:nAG]),
                                   prop.var = unlist(fem.agedist.region[,2 + 4*nAG + 1:nAG]))
fem.agedist.all <- data.frame(area="All",
                               agegroup = rep(levels(pooldat$agegroup), each=nrow(fem.agedist.all)),
                               period = rep(fem.agedist.all$period, nAG),
                               prop = unlist(fem.agedist.all[,1 + 1:nAG]),
                               prop.se = unlist(fem.agedist.all[,1 + 1*nAG + 1:nAG]),
                               prop.ci_l = unlist(fem.agedist.all[,1 + 2*nAG + 1:nAG]),
                               prop.ci_u = unlist(fem.agedist.all[,1 + 3*nAG + 1:nAG]),
                               prop.var = unlist(fem.agedist.all[,1 + 4*nAG + 1:nAG]))
fem.agedist.region.pool <- data.frame(area=rep(fem.agedist.region.pool$region, nAG),
                                      agegroup = rep(levels(pooldat$agegroup), each=nrow(fem.agedist.region.pool)),
                                      period = "both",
                                      prop = unlist(fem.agedist.region.pool[,1 + 1:nAG]),
                                      prop.se = unlist(fem.agedist.region.pool[,1 + 1*nAG + 1:nAG]),
                                      prop.ci_l = unlist(fem.agedist.region.pool[,1 + 2*nAG + 1:nAG]),
                                      prop.ci_u = unlist(fem.agedist.region.pool[,1 + 3*nAG + 1:nAG]),
                                      prop.var = unlist(fem.agedist.region.pool[,1 + 4*nAG + 1:nAG]))
fem.agedist.all.pool <- data.frame(area="All",
                                   agegroup = rep(levels(pooldat$agegroup), each=nrow(fem.agedist.all.pool)),
                                   period = "both",
                                   prop = unlist(fem.agedist.all.pool[,1 + 1:nAG]),
                                   prop.se = unlist(fem.agedist.all.pool[,1 + 1*nAG + 1:nAG]),
                                   prop.ci_l = unlist(fem.agedist.all.pool[,1 + 2*nAG + 1:nAG]),
                                   prop.ci_u = unlist(fem.agedist.all.pool[,1 + 3*nAG + 1:nAG]),
                                   prop.var = unlist(fem.agedist.all.pool[,1 + 4*nAG + 1:nAG]))
fem.agedist <- rbind(fem.agedist.country, fem.agedist.region, fem.agedist.all, fem.agedist.region.pool, fem.agedist.all.pool)

preg.agedist.country <- data.frame(area=rep(preg.agedist.country$country, nAG),
                                   agegroup = rep(levels(pooldat$agegroup), each=nrow(preg.agedist.country)),
                                   period = rep(preg.agedist.country$period, nAG),
                                   prop = unlist(preg.agedist.country[,2 + 1:nAG]),
                                   prop.se = unlist(preg.agedist.country[,2 + 1*nAG + 1:nAG]),
                                   prop.ci_l = unlist(preg.agedist.country[,2 + 2*nAG + 1:nAG]),
                                   prop.ci_u = unlist(preg.agedist.country[,2 + 3*nAG + 1:nAG]),
                                   prop.var = unlist(preg.agedist.country[,2 + 4*nAG + 1:nAG]))
preg.agedist.region <- data.frame(area=rep(preg.agedist.region$region, nAG),
                                   agegroup = rep(levels(pooldat$agegroup), each=nrow(preg.agedist.region)),
                                   period = rep(preg.agedist.region$period, nAG),
                                   prop = unlist(preg.agedist.region[,2 + 1:nAG]),
                                   prop.se = unlist(preg.agedist.region[,2 + 1*nAG + 1:nAG]),
                                   prop.ci_l = unlist(preg.agedist.region[,2 + 2*nAG + 1:nAG]),
                                   prop.ci_u = unlist(preg.agedist.region[,2 + 3*nAG + 1:nAG]),
                                   prop.var = unlist(preg.agedist.region[,2 + 4*nAG + 1:nAG]))
preg.agedist.all <- data.frame(area="All",
                               agegroup = rep(levels(pooldat$agegroup), each=nrow(preg.agedist.all)),
                               period = rep(preg.agedist.all$period, nAG),
                               prop = unlist(preg.agedist.all[,1 + 1:nAG]),
                               prop.se = unlist(preg.agedist.all[,1 + 1*nAG + 1:nAG]),
                               prop.ci_l = unlist(preg.agedist.all[,1 + 2*nAG + 1:nAG]),
                               prop.ci_u = unlist(preg.agedist.all[,1 + 3*nAG + 1:nAG]),
                               prop.var = unlist(preg.agedist.all[,1 + 4*nAG + 1:nAG]))
preg.agedist.region.pool <- data.frame(area=rep(preg.agedist.region.pool$region, nAG),
                                      agegroup = rep(levels(pooldat$agegroup), each=nrow(preg.agedist.region.pool)),
                                      period = "both",
                                      prop = unlist(preg.agedist.region.pool[,1 + 1:nAG]),
                                      prop.se = unlist(preg.agedist.region.pool[,1 + 1*nAG + 1:nAG]),
                                      prop.ci_l = unlist(preg.agedist.region.pool[,1 + 2*nAG + 1:nAG]),
                                      prop.ci_u = unlist(preg.agedist.region.pool[,1 + 3*nAG + 1:nAG]),
                                      prop.var = unlist(preg.agedist.region.pool[,1 + 4*nAG + 1:nAG]))
preg.agedist.all.pool <- data.frame(area="All",
                                   agegroup = rep(levels(pooldat$agegroup), each=nrow(preg.agedist.all.pool)),
                                   period = "both",
                                   prop = unlist(preg.agedist.all.pool[,1 + 1:nAG]),
                                   prop.se = unlist(preg.agedist.all.pool[,1 + 1*nAG + 1:nAG]),
                                   prop.ci_l = unlist(preg.agedist.all.pool[,1 + 2*nAG + 1:nAG]),
                                   prop.ci_u = unlist(preg.agedist.all.pool[,1 + 3*nAG + 1:nAG]),
                                   prop.var = unlist(preg.agedist.all.pool[,1 + 4*nAG + 1:nAG]))
preg.agedist <- rbind(preg.agedist.country, preg.agedist.region, preg.agedist.all, preg.agedist.region.pool, preg.agedist.all.pool)


## age-adjusted prevalence
preg.byage <- merge(preg.agedist, preg.ageprev, all.x=TRUE)
fem.byage <- merge(fem.agedist, fem.ageprev, all.x=TRUE)
prev.byage <- merge(fem.byage, preg.byage, c("area", "agegroup", "period"), suffixes=c(".fem", ".preg"))

ageadj.preg.prev <- tapply(prev.byage$hivres.preg*prev.byage$prop.fem, prev.byage[,c("area", "period")], sum, na.rm=TRUE)
ageadj.preg.prev.var <- tapply(prev.byage$var.preg*prev.byage$prop.fem^2, prev.byage[,c("area", "period")], sum, na.rm=TRUE)

ageadj.preg.prev.change <- ageadj.preg.prev[,"per2"] - ageadj.preg.prev[,"per1"]
ageadj.preg.prev.change.var <- ageadj.preg.prev.var[,"per2"] + ageadj.preg.prev.var[,"per1"]


### linear models  ###
mod.western.rr <- svyglm(hivres~country+currpreg*period, subset(femdes, region=="Western"), family=binomial(log))
mod.eastern.rr <- svyglm(hivres~country+currpreg*period, subset(femdes, region=="Eastern"), family=binomial(log))
mod.southern.rr <- svyglm(hivres~country+currpreg*period, subset(femdes, region=="Southern"), family=binomial(log))
mod.all.rr <- svyglm(hivres~country+currpreg*period, femdes, family=binomial(log))

mod.western.15to24.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 15:24 & region=="Western"), family=binomial(log))
mod.eastern.15to24.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 15:24 & region=="Eastern"), family=binomial(log))
mod.southern.15to24.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 15:24 & region=="Southern"), family=binomial(log))
mod.all.15to24.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 15:24), family=binomial(log))

mod.western.25to34.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 25:34 & region=="Western"), family=binomial(log))
mod.eastern.25to34.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 25:34 & region=="Eastern"), family=binomial(log))
mod.southern.25to34.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 25:34 & region=="Southern"), family=binomial(log))
mod.all.25to34.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 25:34), family=binomial(log))

mod.western.35to49.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 35:49 & region=="Western"), family=binomial(log))
mod.eastern.35to49.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 35:49 & region=="Eastern"), family=binomial(log))
mod.southern.35to49.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 35:49 & region=="Southern"), family=binomial(log))
mod.all.35to49.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 35:49), family=binomial(log))


###################
####  Table 1  ####
###################

## population size, women 15-49
round(with(subset(pooldat, sex=="female"), tapply(pophivweight, list(country, period), sum, na.rm=TRUE))/1e6, 1)
round(with(subset(pooldat, sex=="female"), tapply(pophivweight, list(region, period), sum, na.rm=TRUE))/1e6, 1)
round(with(subset(pooldat, sex=="female"), tapply(pophivweight, period, sum, na.rm=TRUE))/1e6, 1)

## number surveyed, women 15-49
rbind(with(subset(pooldat, sex=="female"), tapply(!is.na(hivres), list(country, period), sum)),
      with(subset(pooldat, sex=="female"), tapply(!is.na(hivres), list(region, period), sum)),
      with(subset(pooldat, sex=="female"), tapply(!is.na(hivres), period, sum)))

## number pregnant, women 15-49
rbind(with(subset(pooldat, sex=="female" & !is.na(hivres)), tapply(currpreg=="yes", list(country, period), sum, na.rm=TRUE)),
with(subset(pooldat, sex=="female" & !is.na(hivres)), tapply(currpreg=="yes", list(region, period), sum, na.rm=TRUE)),
with(subset(pooldat, sex=="female" & !is.na(hivres)), tapply(currpreg=="yes", period, sum, na.rm=TRUE)))

## percentage pregnant

round(100*rbind(subset(frac.preg.country, period=="per1")[,3:4],
      subset(frac.preg.region, period=="per1")[,3:4],
      subset(frac.preg.all, period=="per1")[,2:3]), 1)
round(100*rbind(subset(frac.preg.country, period=="per2")[,3:4],
      subset(frac.preg.region, period=="per2")[,3:4],
      subset(frac.preg.all, period=="per2")[,2:3]), 1)


###################
####  Table 2  ####
###################

print.est <- function(mat) sprintf("%.2f (%.2f, %.2f)", mat[,1], mat[,2], mat[,3])

cbind(tail(print.est(cbind(exp(mod.western.rr$coef), exp(confint.default(mod.western.rr)))), 3),
      tail(print.est(cbind(exp(mod.eastern.rr$coef), exp(confint.default(mod.eastern.rr)))), 3),
      tail(print.est(cbind(exp(mod.southern.rr$coef), exp(confint.default(mod.southern.rr)))), 3),
      tail(print.est(cbind(exp(mod.all.rr$coef), exp(confint.default(mod.all.rr)))), 3))


####################
####  Table S1  ####
####################

sprintf("%4.1f (%.1f) %4.1f (%.1f) %4.1f (%.1f) %4.1f (%.1f) %4.1f (%.1f) %4.1f (%.1f) %4.1f (%.1f) %4.1f (%.1f) %4.1f (%.1f)",
        100*subset(fem.prev, period=="per1")$hivres,
        100*subset(fem.prev, period=="per1")$se,
        100*subset(preg.prev, period=="per1")$hivres,
        100*subset(preg.prev, period=="per1")$se,
        100*ageadj.preg.prev[,"per1"],
        100*sqrt(ageadj.preg.prev.var[,"per1"]),
        100*subset(fem.prev, period=="per2")$hivres,
        100*subset(fem.prev, period=="per2")$se,
        100*subset(preg.prev, period=="per2")$hivres,
        100*subset(preg.prev, period=="per2")$se,
        100*ageadj.preg.prev[,"per2"],
        100*sqrt(ageadj.preg.prev.var[,"per2"]),
        100*fem.prev.change,
        100*sqrt(fem.prev.change.var),
        100*preg.prev.change,
        100*sqrt(preg.prev.change.var),
        100*ageadj.preg.prev.change,
        100*sqrt(ageadj.preg.prev.change.var))



####################
####  Figure 1  ####
####################

library(adegenet)
library(RColorBrewer)

vfnCIplot <- function(mat){c(mat[,1], rev(mat[,2]))}

lty.preg <- 1
lty.fem <- 1

pch.preg <- 17
pch.fem <- 20

col.preg <- "forestgreen"
col.fem <- "royalblue"

lty.per1 <- 1
lty.per2 <- 1

pch.per1 <- 17
pch.per2 <- 20

col.per1 <- brewer.pal(3, "Dark2")[1]
col.per2 <- brewer.pal(3, "Dark2")[2]

## quartz(w=6.8, h=3.2, pointsize=8)
pdf("currpreg-hiv-trends_figure1.pdf", w=6.8, h=3.2, pointsize=8)
##
par(oma=c(0, 0, 1, 0.3))
layout(cbind(0, rbind(1:2, 3:4, 5), 0, rbind(c(6, 0, 7), c(8, 0, 9), 10)), c(0.3, 1, 1, 0.3, 1, 0.1, 1), c(1, 1, 0.35))
par(mar=c(0.5, 0.5, 0.5, 0.5), cex=1, tcl=-0.25, mgp=c(2, 0.5, 0), cex.axis=0.9)
####                  ####
###  Age distribution  ###
####                  ####
plot(subset(preg.agedist, area=="Western" & period=="both")$prop,
     type="l", ylim=c(0, 0.35), xaxt="n", yaxt="n",
     col=col.preg, lwd=2, lty=lty.preg)
lines(subset(fem.agedist, area=="Western" & period=="both")$prop, col=col.fem, lwd=2, lty=lty.fem)
polygon(c(1:7, 7:1), fnCIplot(subset(preg.agedist, area=="Western" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.preg, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.agedist, area=="Western" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.fem, 0.3), border=NA)
points(subset(preg.agedist, area=="Western" & period=="both")$prop, col=col.preg, pch=pch.preg)
points(subset(fem.agedist, area=="Western" & period=="both")$prop, col=col.fem, pch=pch.fem)
mtext("Western", 3, -1.1, font=2, adj=0.95)
axis(1, 1:7, FALSE)
axis(2, 0:3/10, paste(0:3*10, "%", sep=""), las=1)
mtext("Proportion of population", 2, 2.2)
mtext("A", 3, 0.2, adj=-0.35, font=2, cex=1.5)
##
plot(subset(preg.agedist, area=="Eastern" & period=="both")$prop,
     type="l", ylim=c(0, 0.35), xaxt="n", yaxt="n",
     col=col.preg, lwd=2, lty=lty.preg)
lines(subset(fem.agedist, area=="Eastern" & period=="both")$prop, col=col.fem, lwd=2, lty=lty.fem)
polygon(c(1:7, 7:1), fnCIplot(subset(preg.agedist, area=="Eastern" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.preg, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.agedist, area=="Eastern" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.fem, 0.3), border=NA)
points(subset(preg.agedist, area=="Eastern" & period=="both")$prop, col=col.preg, pch=pch.preg)
points(subset(fem.agedist, area=="Eastern" & period=="both")$prop, col=col.fem, pch=pch.fem)
mtext("Eastern", 3, -1.1, font=2, adj=0.95)
axis(1, 1:7, FALSE)
axis(2, 0:3/10, FALSE, las=1)
##
plot(subset(preg.agedist, area=="Southern" & period=="both")$prop,
     type="l", ylim=c(0, 0.35), xaxt="n", yaxt="n",
     col=col.preg, lwd=2, lty=lty.preg)
lines(subset(fem.agedist, area=="Southern" & period=="both")$prop, col=col.fem, lwd=2, lty=lty.fem)
polygon(c(1:7, 7:1), fnCIplot(subset(preg.agedist, area=="Southern" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.preg, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.agedist, area=="Southern" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.fem, 0.3), border=NA)
points(subset(preg.agedist, area=="Southern" & period=="both")$prop, col=col.preg, pch=pch.preg)
points(subset(fem.agedist, area=="Southern" & period=="both")$prop, col=col.fem, pch=pch.fem)
mtext("Southern", 3, -1.1, font=2, adj=0.95)
axis(1, 1:7, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-45","45-49"))
axis(2, 0:3/10, paste(0:3*10, "%", sep=""), las=1)
mtext("Proportion of population", 2, 2.2)
mtext("Age group", 1, 1.5, adj=-0.5)
##
plot(subset(preg.agedist, area=="All" & period=="both")$prop,
     type="l", ylim=c(0, 0.35), xaxt="n", yaxt="n",
     col=col.preg, lwd=2, lty=lty.preg)
lines(subset(fem.agedist, area=="All" & period=="both")$prop, col=col.fem, lwd=2, lty=lty.fem)
polygon(c(1:7, 7:1), fnCIplot(subset(preg.agedist, area=="All" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.preg, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.agedist, area=="All" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.fem, 0.3), border=NA)
points(subset(preg.agedist, area=="All" & period=="both")$prop, col=col.preg, pch=pch.preg)
points(subset(fem.agedist, area=="All" & period=="both")$prop, col=col.fem, pch=pch.fem)
mtext("All", 3, -1.1, font=2, adj=0.95)
axis(1, 1:7, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-45","45-49"))
axis(2, 0:3/10, FALSE, las=1)
##
plot(0, 0, type="n", bty="n", axes=FALSE)
legend("bottom", c("All women", "Curr. pregnant"), col=c(col.fem, col.preg), lwd=2, lty=c(lty.fem, lty.preg), pch=c(pch.fem, pch.preg), xpd=NA, horiz=TRUE, cex=1, inset=-0.1)
####                          ####
###  Age-prevalence all women  ###
####                          ####
plot(subset(fem.ageprev, area=="Western" & period=="per1")$hivres,
     type="l", ylim=c(0, 0.1), xaxt="n", yaxt="n",
     col=col.per1, lwd=2, lty=lty.per1)
lines(subset(fem.ageprev, area=="Western" & period=="per2")$hivres, col=col.per2, lwd=2, lty=lty.per2)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Western" & period=="per1")[,c("ci_l", "ci_u")]), col=transp(col.per1, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Western" & period=="per2")[,c("ci_l", "ci_u")]), col=transp(col.per2, 0.3), border=NA)
points(subset(fem.ageprev, area=="Western" & period=="per1")$hivres, col=col.per1, pch=pch.per1)
points(subset(fem.ageprev, area=="Western" & period=="per2")$hivres, col=col.per2, pch=pch.per2)
mtext("Western", 3, -1.1, font=2, adj=0.05)
axis(1, 1:7, FALSE)
axis(2, 0:5/50, paste(0:5*2, "%", sep=""), las=1)
mtext("HIV prevalence", 2, 2.2)
mtext("B", 3, 0.2, adj=-0.35, font=2, cex=1.5)
##
plot(subset(fem.ageprev, area=="Eastern" & period=="per1")$hivres,
     type="l", ylim=c(0, 0.1), xaxt="n", yaxt="n",
     col=col.per1, lwd=2, lty=lty.per1)
lines(subset(fem.ageprev, area=="Eastern" & period=="per2")$hivres, col=col.per2, lwd=2, lty=lty.per2)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Eastern" & period=="per1")[,c("ci_l", "ci_u")]), col=transp(col.per1, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Eastern" & period=="per2")[,c("ci_l", "ci_u")]), col=transp(col.per2, 0.3), border=NA)
points(subset(fem.ageprev, area=="Eastern" & period=="per1")$hivres, col=col.per1, pch=pch.per1)
points(subset(fem.ageprev, area=="Eastern" & period=="per2")$hivres, col=col.per2, pch=pch.per2)
mtext("Eastern", 3, -1.1, font=2, adj=0.05)
axis(1, 1:7, FALSE)
axis(2, 0:5/50, paste(0:5*2, "%", sep=""), las=1)
##
plot(subset(fem.ageprev, area=="Southern" & period=="per1")$hivres,
     type="l", ylim=c(0, 0.4), xaxt="n", yaxt="n",
     col=col.per1, lwd=2, lty=lty.per1)
lines(subset(fem.ageprev, area=="Southern" & period=="per2")$hivres, col=col.per2, lwd=2, lty=lty.per2)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Southern" & period=="per1")[,c("ci_l", "ci_u")]), col=transp(col.per1, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Southern" & period=="per2")[,c("ci_l", "ci_u")]), col=transp(col.per2, 0.3), border=NA)
points(subset(fem.ageprev, area=="Southern" & period=="per1")$hivres, col=col.per1, pch=pch.per1)
points(subset(fem.ageprev, area=="Southern" & period=="per2")$hivres, col=col.per2, pch=pch.per2)
mtext("Southern", 3, -1.1, font=2, adj=0.05)
axis(1, 1:7, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-45","45-49"))
axis(2, 0:4/10, paste(0:4*10, "%", sep=""), las=1)
mtext("HIV prevalence", 2, 2.2)
mtext("Age group", 1, 1.5, adj=-0.5)
##
plot(subset(fem.ageprev, area=="All" & period=="per1")$hivres,
     type="l", ylim=c(0, 0.20), xaxt="n", yaxt="n",
     col=col.per1, lwd=2, lty=lty.per1)
lines(subset(fem.ageprev, area=="All" & period=="per2")$hivres, col=col.per2, lwd=2, lty=lty.per2)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="All" & period=="per1")[,c("ci_l", "ci_u")]), col=transp(col.per1, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="All" & period=="per2")[,c("ci_l", "ci_u")]), col=transp(col.per2, 0.3), border=NA)
points(subset(fem.ageprev, area=="All" & period=="per1")$hivres, col=col.per1, pch=pch.per1)
points(subset(fem.ageprev, area=="All" & period=="per2")$hivres, col=col.per2, pch=pch.per2)
mtext("All", 3, -1.1, font=2, adj=0.05)
axis(1, 1:7, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-45","45-49"))
axis(2, 0:4/20, paste(0:4*5, "%", sep=""), las=1)
##
plot(0, 0, type="n", bty="n", axes=FALSE)
legend("bottom", c("Period 1", "Period 2"), col=c(col.per1, col.per2), lwd=2, lty=c(lty.per1, lty.per2), pch=c(pch.per1, pch.per2), xpd=NA, horiz=TRUE, cex=1, inset=-0.1)
dev.off()


####################
####  Figure 2  ####
####################

country.codes <- c("BF", "CM", "CI", "SN", "ET", "KE", "RW", "TZ", "LS", "MW", "ZA", "SZ", "ZW")

fem.prev.country.survyear <- svyby(~hivres, by=~country+survyear, femdes, svyciprop, vartype=c("se", "ci"), na.rm=TRUE)
preg.prev.country.survyear <- svyby(~hivres, by=~country+survyear, pregdes, svyciprop, vartype=c("se", "ci"), na.rm=TRUE)

fig2a.dat <- merge(setNames(fem.prev.country.survyear, c("country","survyear", "hivres.fem", "se.fem", "ci_l.fem", "ci_u.fem")),
                  setNames(preg.prev.country.survyear, c("country","survyear", "hivres.preg", "se.preg", "ci_l.preg", "ci_u.preg")))

fig2a.dat$year <- as.numeric(as.character(fig2a.dat$survyear))
fig2a.dat$year[fig2a.dat$survyear=="2003-04"] <- 2003.5
fig2a.dat$year[fig2a.dat$survyear=="2005-06"] <- 2005.5
fig2a.dat$year[fig2a.dat$survyear=="2006-07"] <- 2006.5
fig2a.dat$year[fig2a.dat$survyear=="2007-08"] <- 2007.5
fig2a.dat$year[fig2a.dat$survyear=="2008-09"] <- 2008.5
fig2a.dat$year[fig2a.dat$survyear=="2010-11"] <- 2010.5
fig2a.dat$year[fig2a.dat$survyear=="2011-12"] <- 2011.5

fig2a.dat <- rbind(NA, fig2a.dat)
fig2a.dat[1,1:2] <- c("South Africa", "2002")
fig2a.dat[1,-(1:2)] <- c(0.177, NA, 0.153, 0.203, 0.275, NA, 0.147, 0.454, 2002)

fig2a.list <- split(fig2a.dat, fig2a.dat$country)


## panels B & C

fig2bc <- data.frame(code=c(country.codes, rep(NA, 4)),
                    fem=subset(fem.prev, period=="per2")$hivres/subset(fem.prev, period=="per1")$hivres - 1,
                    preg=subset(preg.prev, period=="per2")$hivres/subset(preg.prev, period=="per1")$hivres - 1,
                    ageadj.preg=(ageadj.preg.prev[,"per2"]/ageadj.preg.prev[,"per1"] - 1))


## quartz(w=6.8, h=5.6, pointsize=8)
pdf("currpreg-hiv-trends_figure2.pdf", w=6.8, h=5.6, pointsize=8)
par(oma=c(1.0, 1.5, 1, 0))
layout(cbind(rbind(1:3, 1:3, 4:6, 4:6, 7:9, 7:9, 10:12, 10:12, c(13, 14, 14), c(13, 14, 14)), 0, rep(15:16, each=5)), w=c(1, 1, 1, 0.2, 2.2))
par(mar=c(0.5, 1, 0.5, 0.5), cex=1, tcl=-0.25, mgp=c(2, 0.5, 0), cex.axis=0.9, las=1)
for(country in levels(pooldat$country)){
  dat <- fig2a.list[[country]]
  if(country %in% c("Botswana")){
    plot(NA, type="n", xlim=c(2002, 2012), ylim=c(0, 40), xaxt="n", ylab="")
  } else { 
    matplot(dat$year, 100*dat[,c("hivres.fem", "hivres.preg")], type="l", lwd=2, col=c(col.fem, col.preg), lty=c(lty.fem, lty.preg),
            xlim=c(2002, 2012), ylim=c(0, 100*1.2*max(c(dat$ci_u.preg, dat$ci_u.fem))),
            ylab="", xaxt="n", xlab="")
    matpoints(dat$year, 100*dat[,c("hivres.fem", "hivres.preg")], pch=c(pch.fem, pch.preg), lwd=1, col=c(col.fem, col.preg))
    polygon(c(dat$year, rev(dat$year)), 100*c(dat$ci_l.fem, rev(dat$ci_u.fem)), col=transp(col.fem, 0.2), border=NA)
    polygon(c(dat$year, rev(dat$year)), 100*c(dat$ci_l.preg, rev(dat$ci_u.preg)), col=transp(col.preg, 0.2), border=NA)
  }
  axis(1, labels=FALSE)
  mtext(country, 3, -1.1, adj=0.95, font=2)
  if(country %in% c("South Africa", "Swaziland", "Zimbabwe"))
    axis(1, lwd.ticks=NA)
}
plot(0, 0, type="n", bty="n", axes=FALSE)
legend("center", c("All women 15-49 y", "Currently pregnant women"), lwd=2, col=c(col.fem, col.preg), lty=c(lty.fem, lty.preg), pch=c(pch.fem, pch.preg), xpd=NA, inset=0.3)
mtext("HIV prevalence (%)", 2, 0.5, outer=TRUE, font=1, las=3, cex=1.2)
mtext("A", 3, -0.3, outer=TRUE, font=2, cex=1.5, adj=-0.03)
####  Panel B  ####
par(mar=c(3, 3, 2, 0.5))
plot(100*fig2bc$fem, 100*fig2bc$preg, type="n", ylim=c(-70, 30), xlim=c(-70, 30),
     xlab="All women: relative prev. change",
     ylab="",
     xaxt="n", yaxt="n")
abline(0, 1, col="grey")
abline(h=0, v=0, lty=2, col="grey")
text(100*fig2bc$fem[1:13], 100*fig2bc$preg[1:13], country.codes, cex=0.7, font=2, col="darkred")
points(100*fig2bc$fem[14:17], 100*fig2bc$preg[14:17], pch=1:4, lwd=1.5, cex=1.2)
axis(1, -7:3*10, c(NA, "-60%", NA, "-40%", NA, "-20%", NA, "0%", NA, "20%", NA))
axis(2, -7:3*10, c(NA, "-60%", NA, "-40%", NA, "-20%", NA, "0%", NA, "20%", NA))
mtext("Pregnant women: relative prev. change", 2, 2.5, las=3)
legend("topleft", c("Western", "Eastern", "Southern", "All"), pch=1:4, lwd=1.5, pt.cex=1.2, lty=0, cex=0.9)
mtext("B", 3, 0.7, font=2, cex=1.5, adj=-0.25)
####  Panel C  ####
plot(100*fig2bc$fem[1:13], 100*fig2bc$ageadj.preg[1:13], type="n", ylim=c(-70, 30), xlim=c(-70, 30),
     xlab="All women: relative prev. change",
     ylab="",
     xaxt="n", yaxt="n")
abline(0, 1, col="grey")
abline(h=0, v=0, lty=2, col="grey")
## text(100*fig2bc$fem[1:13], 100*fig2bc$preg[1:13], fig2bc$code[1:13], cex=0.6, font=1, col=transp("darkred", 0.3))
## points(100*fig2bc$fem[14:17], 100*fig2bc$preg[14:17], pch=1:4, lwd=1, cex=1.2, col=transp(1, 0.3))
text(100*fig2bc$fem[1:13], 100*fig2bc$ageadj.preg[1:13], fig2bc$code[1:13], cex=0.7, font=2, col="darkred")
points(100*fig2bc$fem[14:17], 100*fig2bc$ageadj.preg[14:17], pch=1:4, lwd=1.5, cex=1.2, col=1)
axis(1, -7:3*10, c(NA, "-60%", NA, "-40%", NA, "-20%", NA, "0%", NA, "20%", NA))
axis(2, -7:3*10, c(NA, "-60%", NA, "-40%", NA, "-20%", NA, "0%", NA, "20%", NA))
mtext("Preg. women: age-adjusted rel. prev. change", 2, 2.5, las=3)
legend("topleft", c("Western", "Eastern", "Southern", "All"), pch=1:4, lwd=1.5, pt.cex=1.2, lty=0, cex=0.9)
mtext("C", 3, 1.0, font=2, cex=1.5, adj=-0.25)
dev.off()


####################
####  Figure 3  ####
####################

fem.age15to24.prev <- rbind(svyby(~hivres, ~region+period, subset(pooldes, sex=="female" & age %in% 15:24), svyciprop, na.rm=TRUE, vartype="ci"),
                            cbind(region="All", svyby(~hivres, ~period, subset(pooldes, sex=="female" & age %in% 15:24), svyciprop, na.rm=TRUE, vartype="ci")))

preg.age15to24.prev <- rbind(svyby(~hivres, ~region+period, subset(pooldes, sex=="female" & currpreg=="yes" & age %in% 15:24), svyciprop, na.rm=TRUE, vartype="ci"),
                            cbind(region="All", svyby(~hivres, ~period, subset(pooldes, sex=="female" & currpreg=="yes" & age %in% 15:24), svyciprop, na.rm=TRUE, vartype="ci")))

fem.age25to34.prev <- rbind(svyby(~hivres, ~region+period, subset(pooldes, sex=="female" & age %in% 25:34), svyciprop, na.rm=TRUE, vartype="ci"),
                            cbind(region="All", svyby(~hivres, ~period, subset(pooldes, sex=="female" & age %in% 25:34), svyciprop, na.rm=TRUE, vartype="ci")))
preg.age25to34.prev <- rbind(svyby(~hivres, ~region+period, subset(pooldes, sex=="female" & currpreg=="yes" & age %in% 25:34), svyciprop, na.rm=TRUE, vartype="ci"),
                            cbind(region="All", svyby(~hivres, ~period, subset(pooldes, sex=="female" & currpreg=="yes" & age %in% 25:34), svyciprop, na.rm=TRUE, vartype="ci")))

fem.age35to49.prev <- rbind(svyby(~hivres, ~region+period, subset(pooldes, sex=="female" & age %in% 35:49), svyciprop, na.rm=TRUE, vartype="ci"),
                            cbind(region="All", svyby(~hivres, ~period, subset(pooldes, sex=="female" & age %in% 35:49), svyciprop, na.rm=TRUE, vartype="ci")))
preg.age35to49.prev <- rbind(svyby(~hivres, ~region+period, subset(pooldes, sex=="female" & currpreg=="yes" & age %in% 35:49), svyciprop, na.rm=TRUE, vartype="ci"),
                            cbind(region="All", svyby(~hivres, ~period, subset(pooldes, sex=="female" & currpreg=="yes" & age %in% 35:49), svyciprop, na.rm=TRUE, vartype="ci")))

age15to24.prev <- merge(fem.age15to24.prev, preg.age15to24.prev, by=c("region", "period"), suffixes=c(".fem", ".preg"))
age25to34.prev <- merge(fem.age25to34.prev, preg.age25to34.prev, by=c("region", "period"), suffixes=c(".fem", ".preg"))
age35to49.prev <- merge(fem.age35to49.prev, preg.age35to49.prev, by=c("region", "period"), suffixes=c(".fem", ".preg"))

fnPlotAgeTrend <- function(dat, ylim.val){
  matplot(100*dat[,c("hivres.fem", "hivres.preg")],
          type="l", col=c(col.fem, col.preg), lwd=1.5, lty=c(lty.fem, lty.preg), ylim=ylim.val, xlim=c(0.7, 2.3), xaxt="n")
  matpoints(100*dat[,c("hivres.fem", "hivres.preg")],
            col=c(col.fem, col.preg), lwd=1, pch=c(pch.fem, pch.preg))
  polygon(c(1:2, 2:1), 100*fnCIplot(dat[,c("ci_l.preg", "ci_u.preg")]), col=transp(col.preg, 0.2), border=NA)
  polygon(c(1:2, 2:1), 100*fnCIplot(dat[,c("ci_l.fem", "ci_u.fem")]), col=transp(col.fem, 0.2), border=NA)
  axis(1, 1:2, FALSE)
  return(NULL)
}

## quartz(w=3.23, h=4.6, pointsize=8)
pdf("currpreg-hiv-trends_figure3.pdf", w=3.23, h=4.6, pointsize=8)
par(oma=c(3.5, 1.5, 1.0, 0), mfrow=c(4, 3), mar=c(0.5, 1.2, 1.5, 0.5), cex=1, tcl=-0.25, mgp=c(2, 0.5, 0), cex.axis=0.9, las=1)
###  Western  ###
fnPlotAgeTrend(subset(age15to24.prev, region=="Western"), c(0, 6))
mtext("Western", 3, 0.2, at=0.3, adj=0, font=2)
mtext("Age 15 to 24 y", 3, 1.2, font=2)
fnPlotAgeTrend(subset(age25to34.prev, region=="Western"), c(0, 10))
mtext("Age 25 to 34 y", 3, 1.2, font=2)
fnPlotAgeTrend(subset(age35to49.prev, region=="Western"), c(0, 10))
mtext("Age 35 to 49 y", 3, 1.2, font=2)
###  Eastern  ###
fnPlotAgeTrend(subset(age15to24.prev, region=="Eastern"), c(0, 6))
mtext("Eastern", 3, 0.2, at=0.3, adj=0, font=2)
fnPlotAgeTrend(subset(age25to34.prev, region=="Eastern"), c(0, 10))
fnPlotAgeTrend(subset(age35to49.prev, region=="Eastern"), c(0, 10))
###  Southern  ###
fnPlotAgeTrend(subset(age15to24.prev, region=="Southern"), c(0, 20))
mtext("Southern", 3, 0.2, at=0.3, adj=0, font=2)
fnPlotAgeTrend(subset(age25to34.prev, region=="Southern"), c(0, 40))
fnPlotAgeTrend(subset(age35to49.prev, region=="Southern"), c(0, 40))
###  All  ###
fnPlotAgeTrend(subset(age15to24.prev, region=="All"), c(0, 10))
axis(1, 1:2, c("Per. 1", "Per. 2"), lwd.ticks=NA)
mtext("All", 3, 0.2, at=0.3, adj=0, font=2)
fnPlotAgeTrend(subset(age25to34.prev, region=="All"), c(0, 15))
axis(1, 1:2, c("Per. 1", "Per. 2"), lwd.ticks=NA)
legend("bottom", c("All women 15-49 y", "Currently pregnant"), col=c(col.fem, col.preg), lwd=2, xpd=NA, horiz=TRUE, inset=-0.7, lty=c(lty.fem, lty.preg), pch=c(pch.fem, pch.preg))
fnPlotAgeTrend(subset(age35to49.prev, region=="All"), c(0, 15))
axis(1, 1:2, c("Per. 1", "Per. 2"), lwd.ticks=NA)
mtext("HIV prevalence (%)", 2, 0.4, las=3, cex=1.1, outer=TRUE)
dev.off()


#############################
####  Analysis for text  ####
#############################

### Results, section 1 ###

unclass(svyciprop(~I(age %in% 15:34), pregdes))
unclass(svyciprop(~I(age %in% 15:34), femdes))

svyby(~I(age %in% 15:34), ~region, pregdes, svyciprop, vartype="ci")
svyby(~I(age %in% 15:34), ~region, femdes, svyciprop, vartype="ci")

svyby(~I(age %in% 15:34), ~period+region, pregdes, svyciprop, vartype="ci")
svyby(~I(age %in% 15:34), ~period+region, femdes, svyciprop, vartype="ci")

summary(svyglm(I(age %in% 15:34) ~ period, pregdes, family=binomial))
summary(svyglm(I(age %in% 15:34) ~ region*period, pregdes, family=binomial))


### Prevalence ###

femdes <- update(femdes, agegr3 = cut(age, c(15, 25, 35, 50), right=FALSE))
pregdes <- update(pregdes, agegr3 = cut(age, c(15, 25, 35, 50), right=FALSE))


svyby(~hivres, ~period+agegr3, femdes, svyciprop, vartype="ci")
svyby(~hivres, ~period+region+agegr3, femdes, svyciprop, vartype="ci")
      
unclass(svyciprop(~hivres, femdes))

unclass(svyciprop(~I(age %in% 15:29), pregdes))
unclass(svyciprop(~I(age %in% 15:29), femdes))


### Results, section 2 ###

round(100*preg.prev.all[,c(2,4,5)], 1)
round(100*fem.prev.all[,c(2,4,5)], 1)

round(100*preg.prev.region[,c(3,5,6)], 1)
round(100*fem.prev.region[,c(3,5,6)], 1)



summary(svyglm(hivres~country+period*currpreg, subset(pooldes, sex=="female"), family=binomial(log)))


### Results, section 3 ###

fem.age15to24.prev
preg.age15to24.prev

summary(svyglm(hivres~region+currpreg*period, subset(femdes, age %in% 35:49), family=binomial(log)))

summary(svyglm(hivres~country+currpreg*period, subset(femdes, age %in% 35:49), family=binomial(log)))
summary(svyglm(hivres~country+currpreg*period, subset(femdes, age %in% 35:49), family=binomial(log)))


mod.all.25to34.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 25:34), family=binomial(log))
mod.all.35to49.rr <- svyglm(hivres~country+currpreg*I(period=="per1"), subset(femdes, age %in% 35:49), family=binomial(log))
