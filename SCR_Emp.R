## Analyze empirical data 

# SCR Models -----------------------------------------------------------

## Using empirical data
trap16 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/Dilane_TDF_2016.csv")
trap16<-trap16[order(trap16$TrapSite),]
scr16_raw <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/Trapping_Data_2016_2.csv")  #no missing data

trap17 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/DiLane_TDF_2017.csv")
trap17<-trap17[order(trap17$TrapSite),]
scr17_raw <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/trappingdata_Fall2017.csv")       
telem17 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/DL_TrappingTelemetry2017.csv")
telem17 <- telem17[,c(1:2,5,14:15,17)]
telem17$X <- round(telem17$X)
telem17$Y <- round(telem17$Y)
telem17$Session <- "B"


trap18 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/DiLane_TDF_Fall2018.csv")
trap18 <- trap18[order(trap18$TrapSite),]
scr18_raw <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/DiLane_TrapData_Fall2018_2.csv") 

telem18 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/DL_TrappingTelemetry2018.csv")

telem18 <- telem18[,c(1:2,8,14:15,6)]
telem18$X <- round(telem18$X)
telem18$Y <- round(telem18$Y)
telem18$Session <- "C"

## There are a few birds w/ no location b/c of mortality
dim(telem18)
telem18_2<-telem18[telem18$Y>1,]
dim(telem18_2)
summary(telem18_2)

# Only keep columns we need
scr16 <- scr16_raw[,c("ID", "occasion", "trap", "Age")]
scr17 <- scr17_raw[,c("ID", "occasion", "trap", "AGE")]
scr18 <- scr18_raw[,c("ID", "occasion", "trap", "AGE")]

colnames(scr16)<-colnames(scr17)<-colnames(scr18)<-c("ID", "Occasion", "Detector", "Age")

# Add Session column
scr16$Session<-"A"
scr17$Session<-"B"
scr18$Session<-"C"

# Re-order columns so session is first
scr16 <- scr16[,c(5,1:4)]
scr17 <- scr17[,c(5,1:4)]
scr18 <- scr18[,c(5,1:4)]

# Add negative before occasion for birds that died during capture
scr16[scr16$ID=="16-000211",]
#scr16[476, "Occasion"]<- -15
scr16[573, "Occasion"]<- -15

scr17[scr17$ID=="17-000297",]
#scr17[304, "Occasion"]<- -22
scr17[304, "Occasion"]<- -22

library(secr)

# Create telemetry data
telemCH17 <- read.telemetry(data = telem17)
telemCH18 <- read.telemetry(data = telem18_2)

# Extract columns needed for detectors
det16 <- trap16[, c("TrapSite", "X", "Y")]
det17 <- trap17[, c("TrapSite", "X", "Y")]
det18 <- trap18[, c("TrapSite", "X", "Y")]

colnames(det16) <- colnames(det17) <- colnames(det18) <- c("Detector", "x", "y")
rownames(det16) <- rownames(det17) <- rownames(det18) <- NULL

# Create a list of the session-specific detectors
det16_2<-read.traps(data=det16, detector="multi", trapID="Detector")
usage(det16_2)<-as.matrix(trap16[,5:30])
usage(det16_2)
det17_2<-read.traps(data=det17, detector="multi", trapID="Detector")
usage(det17_2)<-as.matrix(trap17[,5:30])
det18_2<-read.traps(data=det18, detector="multi", trapID="Detector")
usage(det18_2)<-as.matrix(trap18[,5:30])

all(scr16$trap %in% det16_2$Detector)
all(scr17$trap %in% det17_2$Detector)
all(scr18$trap %in% det18_2$Detector)

# Create capture history
CH16 <- make.capthist(captures = scr16, traps = det16_2,  noccasions = 26, covnames = "Age")
CH17 <- make.capthist(captures = scr17, traps = det17_2,  noccasions = 26, covnames = "Age")
CH18 <- make.capthist(captures = scr18, traps = det18_2,  noccasions = 26, covnames = "Age")

## Create multisession capture history
dets <- list(det16_2, det17_2, det18_2)
scr <- rbind(scr16, scr17, scr18)

msCH <- make.capthist(captures = scr, traps = dets, noccasions = 26, covnames = "Age")
str(msCH)
summary(msCH)


# Combine trapping and telemetry data
CH17T <- addTelemetry(detectionCH= CH17, telemetryCH=telemCH17, type = c("concurrent"), collapsetelemetry = TRUE, verify = TRUE)

par(mfrow = c(1,2), mar = c(.2,.2,.3,.2))
plot(traps(CH17T), border = 150, bty = 'o') # base plot
plot(CH17T, title = 'Trapping', tracks = TRUE, add = TRUE)
plot(traps(CH17T), border = 150, bty = 'o') # base plot
plot(CH17T, title = 'Telemetry', type = 'telemetry', tracks = TRUE, add = TRUE)

CH18T <- addTelemetry(detectionCH= CH18, telemetryCH=telemCH18, type = c("concurrent"), collapsetelemetry = TRUE, verify = TRUE)
summary(CH18T)

par(mfrow = c(1,2), mar = c(.2,.2,.3,.2))
plot(traps(CH18T), border = 150, bty = 'o') # base plot
plot(CH18T, title = 'Trapping', tracks = TRUE, add = TRUE)
plot(traps(CH18T), border = 150, bty = 'o') # base plot
plot(CH18T, title = 'Telemetry', type = 'telemetry', tracks = TRUE, add = TRUE)

# Multi-session w/ 2017-2018 and telemetry
msCHT <- MS.capthist(list(CH17T, CH18T))
summary(msCHT)

## I think these multi-session models violate the closure assumption leading to overly precise CIs
## Don't use these for the paper.
## Multi-session models
fitDiLane.MS2 <- secr.fit(msCH, model = list(D~session, lambda0~bk, sigma~1), 
                         buffer = 800, 
                         detectfn = "HHN", 
                         CL=FALSE, method = "Nelder-Mead", 
                         biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                         start = list(D = .1, lambda0 = .1, sigma = 300), ncores = 3, control=list(maxit=10000), trace=F)

save(fitDiLane.MS2, file="fitDiLane.trapping.MS2.gzip")
load("fitDiLane.trapping.MS2.gzip")
Dests<-derived(fitDiLane.MS2, se.esa=FALSE)
coef(fitDiLane.MS2)
beta<-coef(fitDiLane.MS2)[4,1]
sebeta<-coef(fitDiLane.MS2)[4,2]
exp(beta) * sqrt(exp(sebeta^2)-1)


fitDiLane.MS4 <- secr.fit(msCHT, model = list(D~session, lambda0~bk, sigma~1), 
                          buffer = 800, 
                          detectfn = "HHN", 
                          CL=FALSE, method = "Nelder-Mead", 
                          biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                          start = list(D = .1, lambda0 = .1, sigma = 300), ncores = 3, control=list(maxit=10000), trace=F)
save(fitDiLane.MS4, file="fitDiLane.trap.tel.MS4.gzip")
DestsT<-derived(fitDiLane.MS4, se.esa=FALSE)



# Sigma and Density vary with session
# Seems to give me some estimation issues
#predict(fitDiLane.MS5)
fitDiLane.MS5 <- secr.fit(msCH, model = list(D~session, lambda0~bk, sigma~session), 
                        buffer = 800, 
                          detectfn = "HHN", 
                          CL=FALSE, method = "Nelder-Mead", 
                          biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                          start = list(D = .1, lambda0 = .1, sigma = 300), ncores = 3, control=list(maxit=10000), trace=F)

save(fitDiLane.MS5, file="fitDiLane.trapping.MS5.gzip")
Dest.MS5<-derived(fitDiLane.MS5, se.esa=FALSE) 


fitDiLane.MS6 <- secr.fit(msCHT, model = list(D~session, lambda0~bk, sigma~session), 
                          buffer = 800, 
                          detectfn = "HHN", 
                          CL=FALSE, method = "Nelder-Mead", 
                          biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                          start = list(D = .1, lambda0 = .1, sigma = 300), ncores = 3, control=list(maxit=10000), trace=F)
save(fitDiLane.MS6, file="fitDiLane.trap.tel.MS6.gzip")

DestsT_2<-derived(fitDiLane.MS6, se.esa=FALSE)

## Fit SINGLE YEAR models in secr
fitDiLane16 <- secr.fit(CH16, model = list(D~1, lambda0~bk, sigma~1), 
                          buffer = 4*102, 
                          detectfn = "HHN", 
                          CL=FALSE, method = "Nelder-Mead", 
                          biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                          start = list(D = .1, lambda0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

esa.plot(fitDiLane16)
abline(v=4*102, lty=2, col="red")
save(fitDiLane16, file="fitDiLane16_8-31-2019.gzip")

fitDiLane17 <- secr.fit(CH17, model = list(D~1, lambda0~bk, sigma~1), 
                        buffer = 4*170, 
                        detectfn = "HHN", 
                        CL=FALSE, method = "Nelder-Mead", 
                        biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                        start = list(D = .1, lambda0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

esa.plot(fitDiLane17)
abline(v=4*170, lty=2, col="red")
save(fitDiLane17, file="fitDiLane17_8-31-2019.gzip")

fitDiLane18 <- secr.fit(CH18, model = list(D~1, lambda0~bk, sigma~1), 
                        buffer =  4*109, 
                        detectfn = "HHN", 
                        CL=FALSE, method = "Nelder-Mead", 
                        biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                        start = list(D = .1, lambda0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

esa.plot(fitDiLane18)
abline(v=4*109, lty=2, col="red")
save(fitDiLane18, file="fitDiLane18_8-31-2019.gzip")

fitDiLane17T <- secr.fit(CH17T, model = list(D~1, lambda0~bk, sigma~1), 
                         buffer = 4*170, 
                         detectfn = "HHN", 
                         CL=FALSE, method = "Nelder-Mead",
                         biasLimit = NA, #mask = percUpland_mask_scale18, 
                         start = list(D = .1, lambda0 = .1, sigma = 300), 
                         details = list(telemetryscale = 1e50),
                         ncores = 1, control=list(maxit=10000), trace=F)


esa.plot(fitDiLane17T)
abline(v=4*170, lty=2, col="red")
coef(fitDiLane17T)
save(fitDiLane17T, file="fitDiLane17_9-10-2019.gzip")

fitDiLane18T <- secr.fit(CH18T, model = list(D~1, lambda0~bk, sigma~1), 
                         buffer = 4*102, 
                         #buffer=800,
                         detectfn = "HHN", 
                         CL=FALSE, method = "Nelder-Mead",
                         biasLimit = NA, #mask = trap_mask18, 
                         start = list(D = .1, lambda0 = .1, sigma = 400), 
                         details = list(telemetryscale = 1e30),
                         ncores = 1, control=list(maxit=1500), trace=T)

coef(fitDiLane18T)
esa.plot(fitDiLane18T)
abline(v=4*102, lty=2, col="red")
save(fitDiLane18T, file="fitDiLane18_9-10-2019.gzip")

## Quick check
round(exp(coef(fitDiLane16)), 5)
round(exp(coef(fitDiLane17)), 5)
round(exp(coef(fitDiLane17T)), 5)
round(exp(coef(fitDiLane18)), 5)
round(exp(coef(fitDiLane18T)), 5)

derived(fitDiLane16, se.esa=FALSE)
derived(fitDiLane17, se.esa=FALSE)
derived(fitDiLane18, se.esa=FALSE)
derived(fitDiLane17T, se.esa=FALSE)
derived(fitDiLane18T, se.esa=FALSE)
# Nathan's way
#combinedCH <- MS.capthist(CH17, telemCH)
#fitDiLane17T2 <- secr.fit(combinedCH, 
#                         model = list(D~DLHab_PercUpland, lambda0~bk, sigma~1), 
#                         buffer = 800, 
#                         detectfn = 14, 
#                         method = "Nelder-Mead",
#                         biasLimit = NA, mask = percUpland_mask_scale17, 
#                         start = list(D = .1, lambda0 = .1, sigma = 300), ncores = 1, control=list(maxit=1500), trace=F)

# Test the buffer size ----------------------------------------------------
## Same results with a larger buffer?
fitDiLane17.1 <- secr.fit(CH17, model = list(D~1, g0~1, sigma~1), 
                          buffer = 4*170, 
                          detectfn = "HN", 
                          CL=FALSE, method = "Nelder-Mead", 
                          # biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                          start = list(D = .1, g0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)


fitDiLane17.2 <- secr.fit(CH17, model = list(D~1, g0~1, sigma~1), 
                          buffer = 1000, 
                          detectfn = "HN", 
                          CL=FALSE, method = "Nelder-Mead", 
                          #biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                          start = list(D = .1, g0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

fitDiLane17.3 <- secr.fit(CH17, model = list(D~1, g0~1, sigma~1), 
                          buffer = 1500, 
                          detectfn = "HN", 
                          CL=FALSE, method = "Nelder-Mead", 
                          #biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                          start = list(D = .1, g0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

# Looks good for ALL buffer sizes
esa.plot(fitDiLane17.1)
esa.plot(fitDiLane17.2)
esa.plot(fitDiLane17.3)
abline(v=4*170, lty=2, col="red")

coef(fitDiLane17.1)
coef(fitDiLane17.2)
coef(fitDiLane17.3)

derived(fitDiLane17.1, se.esa=FALSE)
derived(fitDiLane17.2, se.esa=FALSE)
derived(fitDiLane17.3, se.esa=FALSE)


# Now test w/ the HHN
fitDiLane17.HHN.1 <- secr.fit(CH17, model = list(D~1, lambda0~1, sigma~1), 
                              buffer = 4*170, 
                              detectfn = "HHN", 
                              CL=FALSE, method = "Nelder-Mead", 
                              biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                              start = list(D = .1, lambda0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

# Looking good
esa.plot(fitDiLane17.HHN.1)
abline(v=4*170, lty=2, col="red")

# Now test w/ bk
fitDiLane17.HN.bk1 <- secr.fit(CH17, model = list(D~1, g0~bk, sigma~1), 
                               buffer = 4*170, 
                               detectfn = "HN", 
                               CL=FALSE, method = "Nelder-Mead", 
                               biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                               start = list(D = .1, g0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

esa.plot(fitDiLane17.HN.bk1)
abline(v=4*170, lty=2, col="red")
abline(v=800, lty=2, col="red")

fitDiLane17.HN.bk2 <- secr.fit(CH17, model = list(D~1, g0~bk, sigma~1), 
                               buffer = 1000, 
                               detectfn = "HN", 
                               CL=FALSE, method = "Nelder-Mead", 
                               biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                               start = list(D = .1, g0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

# okay same plot
esa.plot(fitDiLane17.HN.bk2)

fitDiLane17.HHN.bk1 <- secr.fit(CH17, model = list(D~1, g0~bk, sigma~1), 
                                buffer = 4*170, 
                                detectfn = "HHN", 
                                CL=FALSE, method = "Nelder-Mead", 
                                biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                                start = list(D = .1, g0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

esa.plot(fitDiLane17.HHN.bk1) # v similar


fitDiLane17.HHN.bk2 <- secr.fit(CH17, model = list(D~1, g0~bk, sigma~1), 
                                buffer = 1000, 
                                detectfn = "HHN", 
                                CL=FALSE, method = "Nelder-Mead", 
                                biasLimit = NA, #mask = percUpland_mask_scale17_250, 
                                start = list(D = .1, g0 = .1, sigma = 300), ncores = 1, control=list(maxit=10000), trace=F)

## Now look at density
# No difference w/ buffer size or detection curve type, lets look at bk w/ diff buffer
coef(fitDiLane17.HN.bk1)
coef(fitDiLane17.HN.bk2)
coef(fitDiLane17.HHN.bk1)
coef(fitDiLane17.HHN.bk2)



# Results -----------------------------------------------------------------

## Figure of study area

library('GISTools')

png("../comparisons/Dilane.png", width=7, height=7, res=600, units="in")
par(mfrow=c(1,1))
op <- par(mar = c(5,5,4,2) + 0.1)

plot(hab_raster_scale, asp=1)
points(trap17[,2:3], pch="*", cex=1.5, col="blue")
points(locs, pch="+", cex=2, col="black")
north.arrow(xb=396000, yb=3653000, len=300, lab='N')
scalebar(d=5000, xy=c(396000, 3644000), type="bar", divs=4, below="")
text(x=401300, y=3644120, labels="m")

dev.off()
system("open ../comparisons/Dilane.png")


# Parameter estimates for SCR
# loads all SCR and SCR + Telemetry, single year models
# loads distance sampling models, single years and multi-year

load("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/nobo-scr/comparisons/emp/firstime.RData")

summary(csDiLane)

## Single year estimates
round(coef(fitDiLane16), 5)
round(coef(fitDiLane17), 5)
round(coef(fitDiLane18), 5)
round(coef(fitDiLane17T), 5)
round(coef(fitDiLane18T), 5)

## All years estimates
#round(coef(fitDiLane.MS2), 5)
#round(coef(fitDiLane.MS4), 5)


ilogit <- function(x) exp(x)/(1+exp(x))

# Expected response
#ilogit(-3.67) # 2016
#ilogit(-5.65) # 2017
#ilogit(-4.63) # 2018
#ilogit(-6.16) # 2017 T
#ilogit(-4.75) # 2018 T

# 95% CI
#ilogit(-3.88); ilogit(-3.46) # 2016
#ilogit(-6.12); ilogit(-5.18) # 2017
#ilogit(-5.08); ilogit(-4.17) # 2018
#ilogit(-6.28); ilogit(-6.05) # 2017T
#ilogit(-5.13); ilogit(-4.38) # 2018T


ilogit(-4.43321); ilogit(-4.43595); ilogit(-4.43048)


# Pr(recapture | home range centered on trap)
ilogit(-3.67 + 1.69) # 2016
ilogit(-5.65 + 3.64) # 2017
ilogit(-4.63 + 2.65) # 2018

exp(-3.67 + 1.69) # 2016
exp(-5.65 + 3.64) # 2017
exp(-4.63 + 2.65) # 2018

# 95% CI
ilogit(-3.88 + 1.46); ilogit(-3.46 + 1.93)  # 2016
ilogit(-6.12 + 3.14); ilogit(-5.18 + 4.15)  # 2017
ilogit(-5.08 + 2.17); ilogit(-4.17 + 3.13)  # 2018

ilogit(-4.43321 + 2.43860); ilogit(-4.43595 + 2.34157); ilogit(-4.43048 + 2.53564)

# Pr(Being captured at least 1x during the trapping season)
## Multi-session
Pr.nocap<-1-ilogit(-4.43321) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

# 95% CI
Pr.nocap<-1-ilogit(-4.43595) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1xl<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

# 95% CI
Pr.nocap<-1-ilogit(-4.43048) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1xu<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

# 2016
Pr.nocap<-1-ilogit(-3.67) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

# 95% CI
Pr.nocap<-1-ilogit(-3.88) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

Pr.nocap<-1-ilogit(-3.46) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)
# 0.34

# 2017
Pr.nocap<-1-ilogit(-5.65) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

# 95% CI
Pr.nocap<-1-ilogit(-6.12) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

# 95% CI
Pr.nocap<-1-ilogit(-5.18) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

# 2018
Pr.nocap<-1-ilogit(-4.63) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap

# 95% CI
Pr.nocap<-1-ilogit(-5.08) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

# 95% CI
Pr.nocap<-1-ilogit(-4.17) # Pr(not being captured | activity center located on a trap)
Pr.nocap.seas<-rep(Pr.nocap, 13) # Pr(not being captured on each sampling occassion | activity center located on a trap)
Pr.cap1x<-1-prod(Pr.nocap.seas)  # Pr(being captured at least 1x during the season | activity center located on trap)

## Sigma
round(exp(coef(fitDiLane16)), 0)
round(exp(coef(fitDiLane17)), 0)
round(exp(coef(fitDiLane18)), 0)
round(exp(coef(fitDiLane17T)), 0)
round(exp(coef(fitDiLane18T)), 0)

round(exp(coef(fitDiLane.MS2)), 3)
round(exp(coef(fitDiLane.MS4)), 3)


# Calculate CV (SD/D) 
# For a parameter estimate, the CV is just the SE / D
# The SE is a measure of SD. SE is the SD of the (assumed to be normally distributed) sampling distribution. 

# Density varies across years

summary(csDiLane.D)
0.07501 / 0.4581 # 0.16
0.09715 / 0.6529 # 0.15
0.05191 / 0.2625 # 0.20


Dests # trapping
DestsT # trapping + telemetry

# 2016 SCR
#0.0189617 / 0.4092381

# 2017 SCR, SCR + Tel
#0.01957796 / 0.3141865
#0.02517775 / 0.3613268

# 2018 SCR, SCR + TEL
#0.01805944 / 0.2898234
#0.02466496 / 0.3541534

## Single season
library(secr)

Dest16 <- derived(fitDiLane16, se.esa=FALSE)
Dest17 <- derived(fitDiLane17, se.esa=FALSE)
Dest18 <- derived(fitDiLane18, se.esa=FALSE)
Dest17T <- derived(fitDiLane17T, se.esa=FALSE)
Dest18T <- derived(fitDiLane18T, se.esa=FALSE)

# 2016
0.02836969 / 0.4175283

# 2017
0.05460207 / 0.3392356
0.03161898 / 0.3843598

#2018
0.04753764 / 0.3135254
0.04722141 / 0.3217637

d_tab<-matrix(c(0.35, 0.45, 0.60,
                0.37, 0.42, 0.48,
                0,    0,    0   ,
                
                0.50, 0.65, 0.84,
                0.25, 0.34, 0.46,
                0.33, 0.38, 0.45,
                
                0.21, 0.30, 0.41,
                0.23, 0.31, 0.42,
                0.24, 0.32, 0.43
                ), 9, 3, byrow=TRUE)

library(plotrix)

png("../comparisons/D_emp_comparisons.png", width=7, height=7, res=600, units="in")
par(mfrow=c(1,1))
op <- par(mar = c(5,5,4,2) + 0.1)
plotCI(x=c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8), y=d_tab[1:9, 2], ui=d_tab[1:9, 3], li=d_tab[1:9, 1], 
       pch=c(15,16,17,15,16,17,15,16,17), col="black",
       xlim=c(1, 1.8), ylim=c(0.18, 0.86),
       xaxt='n', xlab="Year", ylab="Density (birds/ha)", 
       cex.lab=1.5, cex=1.3)
abline(v=1.25, lty=2)
abline(v=1.55, lty=2)
axis(1, at=c(1.1, 1.4, 1.7), labels=c("2016", "2017", "2018"), cex=1.5)
legend(x=1.55, y=0.82, legend=c("Distance Sampling", "SCR", "SCR + Telemetry"), pch=c(15,16,17), bty="n", cex=0.9)
dev.off()
system("open ../comparisons/D_emp_comparisons.png")

## With availability and time to detection (removal sampling)
ha<-0.4047
d_tab<-matrix(c(0.46, 0.57, 0.76,
                0.1/ha, 0.18/ha, 0.28/ha,
                0.36, 0.42, 0.48,
                0,    0,    0   ,
                
                0.65, 0.80, 0.99,
                0.13/ha, 0.21/ha, 0.32/ha,
                
                0.25, 0.34, 0.46,
                0.34, 0.38, 0.44,
                
                0.14, 0.22, 0.49,
                0.06/ha, 0.15/ha, 0.26/ha,
                
                0.23, 0.31, 0.42,
                0.24, 0.32, 0.43
), 12, 3, byrow=TRUE)


png("../comparisons/D_emp_comparisons_john.png", width=7, height=7, res=600, units="in")
par(mfrow=c(1,1))
op <- par(mar = c(5,5,4,2) + 0.1)
plotCI(x=c(1,1.1,1.2,1.3, 1.4,1.5,1.6,1.7, 1.8,1.9,2,2.1), y=d_tab[1:12, 2], ui=d_tab[1:12, 3], li=d_tab[1:12, 1], 
       pch=c(15,15,16,17,15,15,16,17,15,15,16,17), col=c("black", "red", "black","black","black", "red", "black","black","black", "red", "black","black"),
       xlim=c(1, 2.1), ylim=c(0.1, 1),
       xaxt='n', xlab="Year", ylab="Density (birds/ha)", 
       cex.lab=1.5, cex=1.3)
abline(v=1.33, lty=2)
abline(v=1.75, lty=2)
axis(1, at=c(1.2, 1.55, 1.95), labels=c("2016", "2017", "2018"), cex=1.5)
dev.off()
system("open ../comparisons/D_emp_comparisons_john.png")
# Table of parameter estimates

# Distance sampling
csDiLane_mat<-as.matrix(csDiLane)
csDiLane_q<-t(apply(csDiLane_mat, 2, function(x) quantile(x, probs=c(0.025, 0.50, 0.975))))
write.csv(csDiLane_q, "dist_samp_parests.csv")               


# Model fit
# Deviance from empirical data
#secr.test(object=fitDiLane17, nsim=99, fit=TRUE, ncores=3)

#sims <- sim.secr(fitDiLane17, nsim = 3, ncores = 1)
#deviance(fitDiLane17)
#devs <- c(deviance(fitDiLane17),sims$deviance)
#quantile(devs, probs=c(0.95))
#rank(devs)[1] / length(devs)
