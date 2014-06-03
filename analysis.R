rm(list=ls())
setwd("~/Documents/Research/ANC : Population trends/Paper/draft 2/")

####################
####  DHS data  ####
####################

load("currpreg-dhs-datasets.RData")

## assign period
bf03$period <- "per1"
cm04$period <- "per1"
ci05$period <- "per1"
et05$period <- "per1"
ke03$period <- "per1"
ls04$period <- "per1"
mw04$period <- "per1"
rw05$period <- "per1"
sn05$period <- "per1"
tz08$period <- "per1"
zw06$period <- "per1"

bf10$period <- "per2"
cm11$period <- "per2"
ci11$period <- "per2"
et11$period <- "per2"
ke09$period <- "per2"
ls09$period <- "per2"
mw10$period <- "per2"
rw10$period <- "per2"
sn11$period <- "per2"
tz12$period <- "per2"
zw11$period <- "per2"

tz04$period <- NA


#############################
####  South Africa data  ####
#############################

library(foreign)

sa05 <- read.dta("../../South Africa data/ANC vs population prevalence2005.dta")
sa08 <- read.dta("../../South Africa data/ANC vs population prevalence2008.dta")
sa12 <- read.dta("../../South Africa data/ANC vs population prevalence2012.dta")

sa05$country <- "South Africa"
sa08$country <- "South Africa"
sa12$country <- "South Africa"

sa05$region <- "Southern"
sa08$region <- "Southern"
sa12$region <- "Southern"

sa05$survyear <- "2005"
sa08$survyear <- "2008"
sa12$survyear <- "2012"

sa05$period <- NA
sa08$period <- "per1"
sa12$period <- "per2"

sa05$stratum <- factor(sa05$stratum)
sa08$stratum <- factor(sa08$stratum)
sa12$stratum <- factor(sa12$stratum)

sa05$cluster <- sa05$psu
sa08$cluster <- sa08$psu
sa12$cluster <- sa12$psu

sa05$sex <- factor(sa05$sex, c("Male", "Female"), c("male", "female"))
sa08$sex <- factor(sa08$sex, c("male", "female"), c("male", "female"))
sa12$sex <- factor(sa12$sex, c("Male", "Female"), c("male", "female"))

sa05$age <- floor(sa05$age)
sa08$age <- floor(sa08$age)
sa12$age <- floor(sa12$age)

sa05$agegroup <- cut(sa05$age, 3:10*5, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), incl=TRUE)
sa08$agegroup <- cut(sa08$age, 3:10*5, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), incl=TRUE)
sa12$agegroup <- cut(sa12$age, 3:10*5, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), incl=TRUE)

sa05$currpreg <- factor(sa05$currpreg, c("No or unsure", "Yes"), c("no or unsure", "yes"))
sa08$currpreg <- factor(sa08$currpreg, c("No or unsure", "Yes"), c("no or unsure", "yes"))
sa12$currpreg <- factor(sa12$currpreg, c("No or unsure", "Yes"), c("no or unsure", "yes"))

sa05$hivres <- sa05$hivres== "HIV+"
sa08$hivres <- sa08$hivres== "HIV+"
sa12$hivres <- sa12$hivres== "HIV+"

sa05$pophivweight <- NA
sa08$pophivweight <- NA
sa12$pophivweight <- NA

sa05 <- subset(sa05, sex=="female" & age %in% 15:49 & !is.na(hivres), names(bf03))
sa08 <- subset(sa08, sex=="female" & age %in% 15:49 & !is.na(hivres), names(bf03))
sa12 <- subset(sa12, sex=="female" & age %in% 15:49 & !is.na(hivres), names(bf03))

## UN WPP 2012 -- female 15-49 population size, 2012: 14095144
sa05$pophivweight <- sa05$hivweight*14095144/sum(sa05$hivweight)
sa08$pophivweight <- sa08$hivweight*14095144/sum(sa08$hivweight)
sa12$pophivweight <- sa12$hivweight*14095144/sum(sa12$hivweight)


##########################
####  Swaziland data  ####
##########################

sz07 <- subset(sz07, age %in% 18:49)   # restrict to 18-49 to be comparable with SHIMS
sz07$age <- NA                         # omit from age-specific analyses
sz07$agegroup <- NA

## from Bicego et al. PLOS ONE 2013
## N = 9533
## DEFF = 1.2
## fraction currently pregnant = 0.07
## HIV prevalence among currently pregnant = 0.379
## HIV prevalence among not currently pregnant = 0.388

sz11 <- setNames(data.frame(matrix(NA, 9533/1.2, length(sz07))), names(sz07))
sz11$country <- "Swaziland"
sz11$region <- "Southern"
sz11$survyear <- "2011"
sz11$period <- "per2"
sz11$psu <- 1:nrow(sz11)
sz11$stratum <- "s1"
sz11$pophivweight <- sum(sz07$pophivweight)/nrow(sz11)
sz11$sex <- "female"
sz11$age <- NA
sz11$agegroup <- NA
sz11$hivres <- rep(c(1, 0, 1, 0), times=round(c(0.07*0.379, 0.07*(1-0.379), 0.93*0.388, 0.93*(1-0.388))*9533/1.2) - c(0, 0, 0, 1))
sz11$currpreg <- rep(c("yes", "no or unsure"), round(c(0.07, 0.93)*9533/1.2))


##############################
####  Create pooled data  ####
##############################

library(survey)

pooldat <- rbind(bf03, bf10,
                 cm04, cm11,
                 ci05, ci11,
                 et05, et11,
                 ke03, ke09,
                 ls04, ls09,
                 mw04, mw10,
                 rw05, rw10,
                 sn05, sn11,
                 tz08, tz12, tz04,
                 zw06, zw11,                  
                 sa08, sa12, sa05,
                 sz07, sz11)

## pooldat$country <- factor(pooldat$country, c("Burkina Faso", "Cameroon", "Cote d'Ivoire", "Senegal",
##                                                "Ethiopia", "Kenya", "Rwanda", "Tanzania",
##                                                "Botswana", "Lesotho", "Malawi", "South Africa", "Swaziland", "Zimbabwe"))
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


###  Age adjusted prevalence  ###

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

### save analysis
save(frac.preg.country, frac.preg.region, frac.preg.all, frac.preg,
     fem.prev, preg.prev,
     fem.prev.change, preg.prev.change, fem.prev.change.var, preg.prev.change.var,
     excess.decline, excess.decline.var,
     fem.ageprev.country, preg.ageprev.country, fem.ageprev.region, preg.ageprev.region, fem.ageprev.all, preg.ageprev.all, fem.ageprev, preg.ageprev,
     fem.agedist.country, preg.agedist.country, fem.agedist.region, preg.agedist.region, fem.agedist.all, preg.agedist.all, fem.agedist, preg.agedist,
     preg.byage, fem.byage, prev.byage, ageadj.preg.prev, ageadj.preg.prev.var, ageadj.preg.prev.change, ageadj.preg.prev.change.var,
     file="workspace_2014-05-31.RData")

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


mod.western.age.or <- svyglm(hivres~country+agegroup*period+currpreg*period, subset(femdes, region=="Western"), family=binomial)
mod.western.age.rr <- svyglm(hivres~country+agegroup*period+currpreg*period, subset(femdes, region=="Western"), family=binomial(log), start=coef(mod.western.age.or))

mod.eastern.age.or <- svyglm(hivres~country+agegroup*period+currpreg*period, subset(femdes, region=="Eastern"), family=binomial)
mod.eastern.age.rr <- svyglm(hivres~country+agegroup*period+currpreg*period, subset(femdes, region=="Eastern"), family=binomial(log), start=coef(mod.eastern.age.or))

mod.southern.age.or <- svyglm(hivres~country+agegroup*period+currpreg*period, subset(femdes, region=="Southern"), family=binomial)
mod.southern.age.rr <- svyglm(hivres~country+agegroup*period+currpreg*period, subset(femdes, region=="Southern"), family=binomial(log), start=coef(mod.southern.age.or))

mod.all.age.or <- svyglm(hivres~country+agegroup*period+currpreg*period, femdes, family=binomial)
mod.all.age.rr <- svyglm(hivres~country+agegroup*period+currpreg*period, femdes, family=binomial(log), start=coef(mod.all.age.or))

mod.all.age.or <- svyglm(hivres~country+(I(age-30)+I((age-30)^2) + I((age-30)^3))*period+currpreg*period, femdes, family=binomial)
mod.all.age.rr <- svyglm(hivres~country+(I(age-30)+I((age-30)^2) + I((age-30)^3))*period+currpreg*period, femdes, family=binomial(log), start=coef(mod.all.age.or))



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
        100*ageadj.preg.prev[!rownames(ageadj.preg.prev) %in% c("Botswana"),"per1"],
        100*sqrt(ageadj.preg.prev.var[!rownames(ageadj.preg.prev) %in% c("Botswana"),"per1"]),
        100*subset(fem.prev, period=="per2")$hivres,
        100*subset(fem.prev, period=="per2")$se,
        100*subset(preg.prev, period=="per2")$hivres,
        100*subset(preg.prev, period=="per2")$se,
        100*ageadj.preg.prev[!rownames(ageadj.preg.prev) %in% c("Botswana"),"per2"],
        100*sqrt(ageadj.preg.prev.var[!rownames(ageadj.preg.prev) %in% c("Botswana"),"per2"]),
        100*fem.prev.change,
        100*sqrt(fem.prev.change.var),
        100*preg.prev.change,
        100*sqrt(preg.prev.change.var),
        100*ageadj.preg.prev.change[!rownames(ageadj.preg.prev) %in% c("Botswana")],
        100*sqrt(ageadj.preg.prev.change.var[!rownames(ageadj.preg.prev) %in% c("Botswana")]))



####################
####  Figure 1  ####
####################

library(adegenet)
library(RColorBrewer)

fnCIplot <- function(mat){c(mat[,1], rev(mat[,2]))}

col.preg <- "forestgreen"
col.fem <- "royalblue"

col.per1 <- brewer.pal(3, "Dark2")[1]
col.per2 <- brewer.pal(3, "Dark2")[2]

## col.preg <- brewer.pal(5, "Dark2")[4]
## col.fem <- brewer.pal(5, "Dark2")[5]

quartz(w=6.8, h=3.2, pointsize=8)

## pdf("Paper/draft 1/age-specific-prev.pdf", w=6, h=5, pointsize=8)
par(oma=c(0, 0, 1, 0.3))
layout(cbind(0, rbind(1:2, 3:4, 5), 0, rbind(c(6, 0, 7), c(8, 0, 9), 10)), c(0.3, 1, 1, 0.3, 1, 0.1, 1), c(1, 1, 0.35))
par(mar=c(0.5, 0.5, 0.5, 0.5), cex=1, tcl=-0.25, mgp=c(2, 0.5, 0), cex.axis=0.9)
####                  ####
###  Age distribution  ###
####                  ####
plot(subset(preg.agedist, area=="Western" & period=="both")$prop,
     type="l", ylim=c(0, 0.35), xaxt="n", yaxt="n",
     col=col.preg, lwd=2)
lines(subset(fem.agedist, area=="Western" & period=="both")$prop, col=col.fem, lwd=2)
polygon(c(1:7, 7:1), fnCIplot(subset(preg.agedist, area=="Western" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.preg, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.agedist, area=="Western" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.fem, 0.3), border=NA)
mtext("Western", 3, -1.1, font=2, adj=0.95)
axis(1, 1:7, FALSE)
axis(2, 0:3/10, paste(0:3*10, "%", sep=""), las=1)
mtext("Proportion of population", 2, 2.2)
mtext("A", 3, 0.2, adj=-0.35, font=2, cex=1.5)
##
plot(subset(preg.agedist, area=="Eastern" & period=="both")$prop,
     type="l", ylim=c(0, 0.35), xaxt="n", yaxt="n",
     col=col.preg, lwd=2)
lines(subset(fem.agedist, area=="Eastern" & period=="both")$prop, col=col.fem, lwd=2)
polygon(c(1:7, 7:1), fnCIplot(subset(preg.agedist, area=="Eastern" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.preg, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.agedist, area=="Eastern" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.fem, 0.3), border=NA)
mtext("Eastern", 3, -1.1, font=2, adj=0.95)
axis(1, 1:7, FALSE)
axis(2, 0:3/10, FALSE, las=1)
##
plot(subset(preg.agedist, area=="Southern" & period=="both")$prop,
     type="l", ylim=c(0, 0.35), xaxt="n", yaxt="n",
     col=col.preg, lwd=2,)
lines(subset(fem.agedist, area=="Southern" & period=="both")$prop, col=col.fem, lwd=2)
polygon(c(1:7, 7:1), fnCIplot(subset(preg.agedist, area=="Southern" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.preg, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.agedist, area=="Southern" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.fem, 0.3), border=NA)
mtext("Southern", 3, -1.1, font=2, adj=0.95)
axis(1, 1:7, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-45","45-49"))
axis(2, 0:3/10, paste(0:3*10, "%", sep=""), las=1)
mtext("Proportion of population", 2, 2.2)
mtext("Age group", 1, 1.5, adj=-0.5)
##
plot(subset(preg.agedist, area=="All" & period=="both")$prop,
     type="l", ylim=c(0, 0.35), xaxt="n", yaxt="n",
     col=col.preg, lwd=2)
lines(subset(fem.agedist, area=="All" & period=="both")$prop, col=col.fem, lwd=2)
polygon(c(1:7, 7:1), fnCIplot(subset(preg.agedist, area=="All" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.preg, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.agedist, area=="All" & period=="both")[,c("prop.ci_l", "prop.ci_u")]), col=transp(col.fem, 0.3), border=NA)
mtext("All", 3, -1.1, font=2, adj=0.95)
axis(1, 1:7, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-45","45-49"))
axis(2, 0:3/10, FALSE, las=1)
##
plot(0, 0, type="n", bty="n", axes=FALSE)
legend("bottom", c("All women", "Curr. pregnant"), col=c(col.fem, col.preg), lwd=2, xpd=NA, horiz=TRUE, cex=1, inset=-0.1)
####                          ####
###  Age-prevalence all women  ###
####                          ####
plot(subset(fem.ageprev, area=="Western" & period=="per1")$hivres,
     type="l", ylim=c(0, 0.1), xaxt="n", yaxt="n",
     col=col.per1, lwd=2)
lines(subset(fem.ageprev, area=="Western" & period=="per2")$hivres, col=col.per2, lwd=2)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Western" & period=="per1")[,c("ci_l", "ci_u")]), col=transp(col.per1, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Western" & period=="per2")[,c("ci_l", "ci_u")]), col=transp(col.per2, 0.3), border=NA)
mtext("Western", 3, -1.1, font=2, adj=0.05)
axis(1, 1:7, FALSE)
axis(2, 0:5/50, paste(0:5*2, "%", sep=""), las=1)
mtext("HIV prevalence", 2, 2.2)
mtext("B", 3, 0.2, adj=-0.35, font=2, cex=1.5)
##
plot(subset(fem.ageprev, area=="Eastern" & period=="per1")$hivres,
     type="l", ylim=c(0, 0.1), xaxt="n", yaxt="n",
     col=col.per1, lwd=2)
lines(subset(fem.ageprev, area=="Eastern" & period=="per2")$hivres, col=col.per2, lwd=2)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Eastern" & period=="per1")[,c("ci_l", "ci_u")]), col=transp(col.per1, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Eastern" & period=="per2")[,c("ci_l", "ci_u")]), col=transp(col.per2, 0.3), border=NA)
mtext("Eastern", 3, -1.1, font=2, adj=0.05)
axis(1, 1:7, FALSE)
axis(2, 0:5/50, paste(0:5*2, "%", sep=""), las=1)
##
plot(subset(fem.ageprev, area=="Southern" & period=="per1")$hivres,
     type="l", ylim=c(0, 0.4), xaxt="n", yaxt="n",
     col=col.per1, lwd=2)
lines(subset(fem.ageprev, area=="Southern" & period=="per2")$hivres, col=col.per2, lwd=2)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Southern" & period=="per1")[,c("ci_l", "ci_u")]), col=transp(col.per1, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="Southern" & period=="per2")[,c("ci_l", "ci_u")]), col=transp(col.per2, 0.3), border=NA)
mtext("Southern", 3, -1.1, font=2, adj=0.05)
axis(1, 1:7, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-45","45-49"))
axis(2, 0:4/10, paste(0:4*10, "%", sep=""), las=1)
mtext("HIV prevalence", 2, 2.2)
mtext("Age group", 1, 1.5, adj=-0.5)
##
plot(subset(fem.ageprev, area=="All" & period=="per1")$hivres,
     type="l", ylim=c(0, 0.20), xaxt="n", yaxt="n",
     col=col.per1, lwd=2)
lines(subset(fem.ageprev, area=="All" & period=="per2")$hivres, col=col.per2, lwd=2)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="All" & period=="per1")[,c("ci_l", "ci_u")]), col=transp(col.per1, 0.3), border=NA)
polygon(c(1:7, 7:1), fnCIplot(subset(fem.ageprev, area=="All" & period=="per2")[,c("ci_l", "ci_u")]), col=transp(col.per2, 0.3), border=NA)
mtext("All", 3, -1.1, font=2, adj=0.05)
axis(1, 1:7, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-45","45-49"))
axis(2, 0:4/20, paste(0:4*5, "%", sep=""), las=1)
##
plot(0, 0, type="n", bty="n", axes=FALSE)
legend("bottom", c("Period 1", "Period 2"), col=c(col.per1, col.per2), lwd=2, xpd=NA, horiz=TRUE, cex=1, inset=-0.1)

dev.off()






####################
####  Figure 2  ####
####################

library(adegenet)
country.codes <- c("BF", "CM", "CI", "SN", "ET", "KE", "RW", "TZ", "LS", "MW", "SA", "SZ", "ZW")

fem.prev.country.survyear <- svyby(~hivres, by=~country+survyear, femdes, svyciprop, vartype=c("se", "ci"), na.rm=TRUE)
preg.prev.country.survyear <- svyby(~hivres, by=~country+survyear, pregdes, svyciprop, vartype=c("se", "ci"), na.rm=TRUE)

fig2.dat <- merge(setNames(fem.prev.country.survyear, c("country","survyear", "hivres.fem", "se.fem", "ci_l.fem", "ci_u.fem")),
                  setNames(preg.prev.country.survyear, c("country","survyear", "hivres.preg", "se.preg", "ci_l.preg", "ci_u.preg")))

fig2.dat$year <- as.numeric(as.character(fig2.dat$survyear))
fig2.dat$year[fig2.dat$survyear=="2003-04"] <- 2003.5
fig2.dat$year[fig2.dat$survyear=="2005-06"] <- 2005.5
fig2.dat$year[fig2.dat$survyear=="2006-07"] <- 2006.5
fig2.dat$year[fig2.dat$survyear=="2007-08"] <- 2007.5
fig2.dat$year[fig2.dat$survyear=="2008-09"] <- 2008.5
fig2.dat$year[fig2.dat$survyear=="2010-11"] <- 2010.5
fig2.dat$year[fig2.dat$survyear=="2011-12"] <- 2011.5

fig2.dat <- rbind(NA, fig2.dat)
fig2.dat[1,1:2] <- c("South Africa", "2002")
fig2.dat[1,-(1:2)] <- c(0.177, NA, 0.153, 0.203, 0.275, NA, 0.147, 0.454, 2002)

fig2.list <- split(fig2.dat, fig2.dat$country)


## panel B

fig2b <- data.frame(code=c(country.codes, rep(NA, 4)),
                    fem=subset(fem.prev, period=="per2")$hivres/subset(fem.prev, period=="per1")$hivres - 1,
                    preg=subset(preg.prev, period=="per2")$hivres/subset(preg.prev, period=="per1")$hivres - 1)

## panel C

fem.prev.region.exclsz <- svyby(~hivres, ~region+period, subset(femdes, country != "Swaziland"), svyciprop, vartype=c("ci", "var", "se"))
fem.prev.all.exclsz <- svyby(~hivres, ~period, subset(femdes, country != "Swaziland"), svyciprop, vartype=c("ci", "var", "se"))
fem.prev.exclsz <- rbind(setNames(fem.prev.region.exclsz, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                         setNames(cbind("All", fem.prev.all.exclsz), c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")))

preg.prev.region.exclsz <- svyby(~hivres, ~region+period, subset(pregdes, country != "Swaziland"), svyciprop, vartype=c("ci", "var", "se"))
preg.prev.all.exclsz <- svyby(~hivres, ~period, subset(pregdes, country != "Swaziland"), svyciprop, vartype=c("ci", "var", "se"))
preg.prev.exclsz <- rbind(setNames(preg.prev.region.exclsz, c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")),
                         setNames(cbind("All", preg.prev.all.exclsz), c("area", "period", "hivres", "se", "ci_l", "ci_u", "var")))

fem.prev.change.exclsz <- subset(fem.prev.exclsz, period=="per2")$hivres/subset(fem.prev.exclsz, period=="per1")$hivres - 1
preg.prev.change.exclsz <- subset(preg.prev.exclsz, period=="per2")$hivres/subset(preg.prev.exclsz, period=="per1")$hivres - 1

## fem.prev.change.var.exclsz <- subset(fem.prev.exclsz, period=="per2")$var + subset(fem.prev.exclsz, period=="per1")$var
## preg.prev.change.var.exclsz <- subset(preg.prev.exclsz, period=="per2")$var + subset(preg.prev.exclsz, period=="per1")$var

fig2c <- data.frame(area=names(ageadj.preg.prev.change[!names(ageadj.preg.prev.change) %in% c("Swaziland", "Botswana")]),
                    code=c(country.codes[-12], rep(NA, 4)),
                    fem=c(fig2b$fem[c(1:11,13)], fem.prev.change.exclsz),
                    preg=c(fig2b$preg[c(1:11,13)], preg.prev.change.exclsz),
                    ageadj.preg=(ageadj.preg.prev[,"per2"]/ageadj.preg.prev[,"per1"] - 1)[-12])

quartz(w=6.8, h=5.5, pointsize=8)

par(oma=c(1.0, 1.5, 1, 0))
layout(cbind(rbind(1:3, 1:3, 4:6, 4:6, 7:9, 7:9, 10:12, 10:12, c(13, 14, 14), c(13, 14, 14)), 0, rep(15:16, each=5)), w=c(1, 1, 1, 0.2, 2.2))
par(mar=c(0.5, 1, 0.5, 0.5), cex=1, tcl=-0.25, mgp=c(2, 0.5, 0), cex.axis=0.9, las=1)
## layout.show(16)
##
for(country in levels(pooldat$country)){
  dat <- fig2.list[[country]]
  if(country %in% c("Botswana")){
    plot(NA, type="n", xlim=c(2002, 2012), ylim=c(0, 40), xaxt="n", ylab="")
  } else { 
    matplot(dat$year, 100*dat[,c("hivres.fem", "hivres.preg")], type="l", lwd=2, col=c(col.fem, col.preg), lty=1,
            xlim=c(2002, 2012), ylim=c(0, 100*1.2*max(c(dat$ci_u.preg, dat$ci_u.fem))),
            ylab="", xaxt="n", xlab="")
    polygon(c(dat$year, rev(dat$year)), 100*c(dat$ci_l.fem, rev(dat$ci_u.fem)), col=transp(col.fem, 0.2), border=NA)
    polygon(c(dat$year, rev(dat$year)), 100*c(dat$ci_l.preg, rev(dat$ci_u.preg)), col=transp(col.preg, 0.2), border=NA)
  }
  axis(1, labels=FALSE)
  mtext(country, 3, -1.1, adj=0.95, font=2)
  if(country %in% c("South Africa", "Swaziland", "Zimbabwe"))
    axis(1, lwd.ticks=NA)
}
plot(0, 0, type="n", bty="n", axes=FALSE)
legend("center", c("All women 15-49 y", "Currently pregnant women"), lwd=2, col=c(col.fem, col.preg), lty=1, xpd=NA, inset=0.3)
mtext("HIV prevalence (%)", 2, 0.5, outer=TRUE, font=1, las=3, cex=1.2)
mtext("A", 3, -0.3, outer=TRUE, font=2, cex=1.5, adj=-0.03)
####  Panel B  ####
par(mar=c(3, 3, 2, 0.5))
plot(100*fig2b$fem, 100*fig2b$preg, type="n", ylim=c(-70, 30), xlim=c(-70, 30),
     xlab="All women: relative prev. change",
     ylab="",
     xaxt="n", yaxt="n")
abline(0, 1, col="grey")
abline(h=0, v=0, lty=2, col="grey")
text(100*fig2b$fem[1:13], 100*fig2b$preg[1:13], country.codes, cex=0.7, font=2, col="darkred")
points(100*fig2b$fem[14:17], 100*fig2b$preg[14:17], pch=1:4, lwd=1.5, cex=1.2)
axis(1, -7:3*10, c("-70%", NA, "-50%", NA, "-30%", NA, "-10%", NA, "10%", NA, "30%"))
axis(2, -7:3*10, c("-70%", NA, "-50%", NA, "-30%", NA, "-10%", NA, "10%", NA, "30%"))
mtext("Pregnant women: relative prev. change", 2, 2.5, las=3)
legend("topleft", c("Western", "Eastern", "Southern", "All"), pch=1:4, lwd=1.5, pt.cex=1.2, lty=0, cex=0.95)
mtext("B", 3, 0.7, font=2, cex=1.5, adj=-0.25)
####  Panel C  ####
plot(100*fig2c$fem[1:12], 100*fig2c$ageadj.preg[1:12], type="n", ylim=c(-70, 30), xlim=c(-70, 30),
     xlab="All women: relative prev. change",
     ylab="",
     xaxt="n", yaxt="n")
abline(0, 1, col="grey")
abline(h=0, v=0, lty=2, col="grey")
text(100*fig2c$fem[1:12], 100*fig2c$preg[1:12], fig2c$code[1:12], cex=0.6, font=1, col=transp("darkred", 0.3))
points(100*fig2c$fem[13:16], 100*fig2c$preg[13:16], pch=1:4, lwd=1, cex=1.2, col=transp(1, 0.3))
text(100*fig2c$fem[1:12], 100*fig2c$ageadj.preg[1:12], fig2c$code[1:12], cex=0.7, font=2, col="darkred")
points(100*fig2c$fem[13:16], 100*fig2c$ageadj.preg[13:16], pch=1:4, lwd=1.5, cex=1.2, col=1)
axis(1, -7:3*10, c("-70%", NA, "-50%", NA, "-30%", NA, "-10%", NA, "10%", NA, "30%"))
axis(2, -7:3*10, c("-70%", NA, "-50%", NA, "-30%", NA, "-10%", NA, "10%", NA, "30%"))
mtext("Preg. women: age-adjusted rel. prev. change", 2, 2.5, las=3)
legend("topleft", c("Western", "Eastern", "Southern", "All"), pch=1:4, lwd=1.5, pt.cex=1.2, lty=0, cex=0.95)
mtext("C", 3, 1.0, font=2, cex=1.5, adj=-0.25)



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
          type="l", col=c(col.fem, col.preg), lwd=1.5, lty=1, ylim=ylim.val, xlim=c(0.7, 2.3), xaxt="n")
  matpoints(100*dat[,c("hivres.fem", "hivres.preg")],
            col=c(col.fem, col.preg), lwd=1, pch=20)
  polygon(c(1:2, 2:1), 100*fnCIplot(dat[,c("ci_l.preg", "ci_u.preg")]), col=transp(col.preg, 0.2), border=NA)
  polygon(c(1:2, 2:1), 100*fnCIplot(dat[,c("ci_l.fem", "ci_u.fem")]), col=transp(col.fem, 0.2), border=NA)
  axis(1, 1:2, FALSE)
  return(NULL)
}

quartz(w=3.23, h=4.4, pointsize=8)

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
legend("bottom", c("All women 15-49 y", "Currently pregnant"), col=c(col.fem, col.preg), lwd=2, xpd=NA, horiz=TRUE, inset=-0.75)
fnPlotAgeTrend(subset(age35to49.prev, region=="All"), c(0, 15))
axis(1, 1:2, c("Per. 1", "Per. 2"), lwd.ticks=NA)
mtext("HIV prevalence (%)", 2, 0.4, las=3, cex=1.1, outer=TRUE)

dev.off()


####################
####  Table S2  ####
####################


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

summary(svyglm(hivres~country+period*currpreg, subset(pooldes, sex=="female"), family=binomial(log)))


### Results, section 3 ###

fem.age15to24.prev
preg.age15to24.prev

summary(svyglm(hivres~region+currpreg*period, subset(femdes, age %in% 35:49), family=binomial(log)))

summary(svyglm(hivres~country+currpreg*period, subset(femdes, age %in% 35:49), family=binomial(log)))
summary(svyglm(hivres~country+currpreg*period, subset(femdes, age %in% 35:49), family=binomial(log)))
