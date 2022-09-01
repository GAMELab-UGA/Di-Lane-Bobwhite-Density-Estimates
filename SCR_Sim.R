## Comparison of methods to estimate bobwhite density
library('secr')

# Simulate data from three different sampling protocols -------------------
## Simulate the density and location of coveys
set.seed(2013)

# Preliminaries
nsites <- 100  # Number of sites
nyears <- 1    # Number of primary periods (e.g., year)

# Expected covey density on the log scale
logD <- log(3.5)
ED <- exp(logD)

## Using the actual trap locations from DiLane
trap18 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/DiLane_TDF_Fall2018.csv")
det18 <- trap18[, c("TrapSite", "X", "Y")]
colnames(det18) <- c("Detector", "x", "y")

library(secr)
tr<-read.traps(data=det18, detector="single", trapID="Detector")

D = (exp(logD)*12*nsites) / ((point.area*nsites)/10000)

# Simulate a homogeneous point process model
pop <- sim.popn(core=tr, D, buffer=0) 

te <- make.telemetry(xy=c(399402, 3648588)) # polygon search area for telemetry data

# 1. Simulate the capture history for SCR data
trCH <- sim.capthist(tr, popn = pop, detectfn = "HHN",
                     detectpar = list(lambda0 = 0.03, sigma = 150),
                     noccasions=25,
                     renumber = FALSE, savepopn = TRUE)

## Select 85 individuals at random from population
popDt <- subset(pop, sample.int(nrow(pop), 85)) 

## 2. Simulate telemetry
teCHD <- sim.capthist(te, popn = popDt, renumber = FALSE, detectfn = "HHN",
                      detectpar = list(sigma = 150), noccasions = 5, exactN = 1)

CHD <- addTelemetry(trCH, teCHD, type = 'concurrent')
session(CHD) <- 'concurrent'

mask <- make.mask(traps(trCH), buffer=2300, type="trapbuffer", nx=32)
plot(mask)
plot(traps(CHD), gridlines = FALSE, bty = 'o', add=TRUE)
plot(CHD, type = 'telemetry', tracks = TRUE, add=TRUE, varycol=FALSE)


# Analyze each data set ---------------------------------------------------

#1. Only trapping data
fit1 <- secr.fit(trCH, method = "Nelder-Mead", detectfn="HHN", biasLimit = NA, mask=mask, start=list(D = .1, lambda0 = .1, sigma = 300), ncores=1)

#2. Trapping and telemetry data
fit2 <- secr.fit(CHD, method = "Nelder-Mead", detectfn="HHN", biasLimit = NA, mask=mask, start=list(D = .1, lambda0 = .1, sigma = 300), ncores=1)

# Expected density
coef(fit1)
coef(fit2)

# Realized density
derived(fit1) 
derived(fit2)