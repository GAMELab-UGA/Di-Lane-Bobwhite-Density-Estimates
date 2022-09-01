## Analyze empirical data 

# Distance sampling data ------------------------------------------------
# Removed observations greater than 520 m
fallcounts16.1 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/FallCounts_DiLane_2016.csv")
fallcounts17.1 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/FallCounts_DiLane_2017.csv")
fallcounts18.1 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/FallCounts_DiLane_2018.csv")

allcounts.1 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/FallCounts_DiLane_2016-2018.csv")

allcounts.2 <- read.csv("C:/Users/peh12971/Dropbox/PostDoc_TallTimbers/wilhite-thesis/data/FallCounts_DiLane_20190219.csv")

# For now just remove the third sample from that one site in 2017
fallcounts17.1<-fallcounts17.1[fallcounts17.1$Visit<3,]

# Conventional distance sampling only uses 1 occasion
fallcounts16<-fallcounts16.1[fallcounts16.1$Visit==1,]
fallcounts17<-fallcounts17.1[fallcounts17.1$Visit==1,]
fallcounts18<-fallcounts18.1[fallcounts18.1$Visit==1,]

fallcounts16<-fallcounts16.1[fallcounts16.1$Visit==2,]
fallcounts17<-fallcounts17.1[fallcounts17.1$Visit==2,]
fallcounts18<-fallcounts18.1[fallcounts18.1$Visit==1,]


## Flush counts?
flush16<-allcounts.2[allcounts.2$Year==2016,]
flush17<-allcounts.2[allcounts.2$Year==2017,]
flush18<-allcounts.2[allcounts.2$Year==2018,]
mean(flush16$BirdsFlushed, na.rm=TRUE) # 9.25
mean(flush17$BirdsFlushed, na.rm=TRUE) # 10.3
mean(flush18$BirdsFlushed, na.rm=TRUE) # 12.25

length(which(!is.na(flush16$BirdsFlushed))) #8
length(which(!is.na(flush17$BirdsFlushed))) #13
length(which(!is.na(flush18$BirdsFlushed))) #4

mean(c(flush16$BirdsFlushed, flush17$BirdsFlushed, flush18$BirdsFlushed), na.rm=TRUE) #10



# Assign distances to distance bins
summary(fallcounts16$Distance)

hist(fallcounts16$Distance)
hist(fallcounts17$Distance)
hist(fallcounts18$Distance)


for(i in 1:nrow(fallcounts16)){
  ifelse(fallcounts16[i,"Distance"]<225,
         fallcounts16[i,"Bin"]<-1,
         ifelse(fallcounts16[i,"Distance"]>=225 && fallcounts16[i,"Distance"]<350,
                fallcounts16[i,"Bin"]<-2,
                ifelse(fallcounts16[i,"Distance"]>=350 && fallcounts16[i,"Distance"]<450,
                fallcounts16[i,"Bin"]<-3,
                fallcounts16[i,"Bin"]<-4)))}

for(i in 1:nrow(fallcounts17)){
  ifelse(fallcounts17[i,"Distance"]<225,
         fallcounts17[i,"Bin"]<-1,
         ifelse(fallcounts17[i,"Distance"]>=225 && fallcounts17[i,"Distance"]<350,
                fallcounts17[i,"Bin"]<-2,
                ifelse(fallcounts17[i,"Distance"]>=350 && fallcounts17[i,"Distance"]<450,
                       fallcounts17[i,"Bin"]<-3,
                       fallcounts17[i,"Bin"]<-4)))}

for(i in 1:nrow(fallcounts18)){
  ifelse(fallcounts18[i,"Distance"]<225,
         fallcounts18[i,"Bin"]<-1,
         ifelse(fallcounts18[i,"Distance"]>=225 && fallcounts18[i,"Distance"]<350,
                fallcounts18[i,"Bin"]<-2,
                ifelse(fallcounts18[i,"Distance"]>=350 && fallcounts18[i,"Distance"]<450,
                       fallcounts18[i,"Bin"]<-3,
                       fallcounts18[i,"Bin"]<-4)))}


## Correct or better way of binning data
# 1. Look at Buckland paper
# 2. Should have increasingly large bins (by area)
# 3. Histogram of counts by bin should start low, increase and then decline
# Not, be really high at the beginning (smaller bins, closer to observer), and then slope down in the way a half normal detection function slopes down.

## Check out the observations that you binned
# Number of observations per distance bin
# Shape of histogram (half-normal?)
#hist(fallcounts16$Distance)

par(mfrow=c(2,3))
hist(fallcounts16$Bin, breaks=c(0,1,2,3,4))
hist(fallcounts17$Bin, breaks=c(0,1,2,3,4))
hist(fallcounts18$Bin, breaks=c(0,1,2,3,4))

## Why might 2017 have an upward bias
head(fallcounts17.1)

# Date?
unique(fallcounts16$CountDate)
unique(fallcounts17$CountDate) # Longest date range?
unique(fallcounts18$CountDate)

# Raw data
raw16<-allcounts.1[allcounts.1$Year==2016,]
raw17<-allcounts.1[allcounts.1$Year==2017,]
raw18<-allcounts.1[allcounts.1$Year==2018,]

# Time of day?
unique(raw16$StartTime) # Start time = 6am-7:10am
unique(raw17$StartTime) # Start time = 6am-7:10am
unique(raw18$StartTime) # Start time = 6am-7:10am

# Conditions?
hist(raw16$NoiseLevel)
hist(raw17$NoiseLevel) # Noisier
hist(raw18$NoiseLevel)

table(raw16$NoiseLevel); 16/sum(table(raw16$NoiseLevel))
table(raw17$NoiseLevel); 72/sum(table(raw17$NoiseLevel))
table(raw18$NoiseLevel); 7/sum(table(raw18$NoiseLevel))


# Observer?
unique(raw16$ObserverName)
unique(raw17$ObserverName)
unique(raw18$ObserverName)

table(raw16$ObserverName)
table(raw17$ObserverName) # Certainly a difference in observers
table(raw18$ObserverName)

# Reformat as a 3d array (site x distance bin x visit)
#ydet16 <- table(fallcounts16$Point,
#                fallcounts16$Bin,
#                fallcounts16$Visit)

ydet16 <- table(fallcounts16$Point,
                fallcounts16$Bin)

#ydet17 <- table(fallcounts17$Point,
#                fallcounts17$Bin,
#                fallcounts17$Visit)

ydet17 <- table(fallcounts17$Point,
                fallcounts17$Bin)

#ydet18 <- table(fallcounts18$Point,
#                fallcounts18$Bin,
#                fallcounts18$Visit)

ydet18 <- table(fallcounts18$Point,
                fallcounts18$Bin)

#ydet17[12,1:3,2]<-NA
#ydet18[c(1:3,15,26),,]<-NA

## For a multi-year analysis
library(abind)
ydet<-abind(ydet16, ydet17, ydet18, along=3)

# For conditional binomial, total number of birds hear
#y16<-apply(ydet16, c(1,3), sum)
#y17<-apply(ydet17, c(1,3), sum)
#y18<-apply(ydet18, c(1,3), sum)

y16<-apply(ydet16, 1, sum)
y17<-apply(ydet17, 1, sum)
y18<-apply(ydet18, 1, sum)

y<-apply(ydet, c(1,3), sum)

# Information needed for point count distance sampling model
db <- c(0, 225, 350, 450, 520)                   # Distance Bins
nBins <-length(db)-1                                # 5 distance classes
xg <- NA                                            # midpoints of distance bins
for(i in 1:(length(db)-1)){xg[i] <- (db[i] + db[i+1])/2 }
delta <- NA                                         # Size of each distance bin
for(i in 1:(length(db)-1)){delta[i] <- db[i+1] - db[i] }
dmax <- db[length(db)]                                      # Max distance
point.area <- pi*dmax^2                             # area of point count survey in ha                   
pix <- (2*xg*delta)/(dmax*dmax)                     # for area calculations; Scaled radial density function

point.area.DiLane <- pi*db[length(db)]^2

# Conditional binomial
jdat_emp16 <- list(nsites=nrow(ydet16), nB=ncol(ydet16), db=db, y=y16, ydet=ydet16, point.area=point.area.DiLane, pix=pix)

jdat_emp17 <- list(nsites=nrow(ydet17), nB=ncol(ydet17), db=db, y=y17, ydet=ydet17, point.area=point.area.DiLane, pix=pix)

jdat_emp18 <- list(nsites=nrow(ydet18), nB=ncol(ydet18), db=db, y=y18, ydet=ydet18, point.area=point.area.DiLane, pix=pix)

jdat_emp <- list(nsites=nrow(ydet), nB=ncol(ydet), nyears=dim(ydet)[3], db=db, y=y, ydet=ydet, point.area=point.area.DiLane, pix=pix)


# Initial values ----------------------------------------------------------

#Min0_16 <- apply(jdat_emp16$y, 2, function(x) x+1)
#Min_16 <- apply(Min0_16, 1, max)

#Min0_17 <- apply(jdat_emp17$y, 2, function(x, na.rm=TRUE) x+1)
#Min_17 <- apply(Min0_17, 1, max, na.rm=TRUE)

#Min0_18 <- apply(jdat_emp18$y, 2, function(x) x+1)
#Min_18 <- apply(Min0_18, 1, max)

Min_16<-jdat_emp16$y+1
Min_17<-jdat_emp17$y+1
Min_18<-jdat_emp18$y+1

Min0<-apply(jdat_emp$y, 2, function(x) x+1)

inits16 <- function(){list(N=Min_16, logED=runif(1, 1, 5), beta0=runif(1,5,7))}
inits17 <- function(){list(N=Min_17, logED=runif(1, 1, 5), beta0=runif(1,5,7))}
inits18 <- function(){list(N=Min_18, logED=runif(1, 1, 5), beta0=runif(1,5,7))}

inits<- function(){list(N=Min0, logED=runif(1, 1, 5), beta0=runif(1,5,7))}

# Conditional binomial
pars<- c(names(inits16())[-1],'sigma','mean_pCirc', 'D', 'D.r')


# Each year separate ------------------------------------------------------
library(rjags)

jmDiLane16 <-jags.model(file="dist_samp_emp.JAG", jdat_emp16, inits16, n.chains=3, n.adapt=2000)
csDiLane16 <- coda.samples(jmDiLane16, pars, thin=1, n.iter=50000)
windows();plot(window(csDiLane16, start=40000), ask=TRUE)
summary(csDiLane16)

jmDiLane17 <-jags.model(file="dist_samp_emp.JAG", jdat_emp17, inits17, n.chains=3, n.adapt=2000)
csDiLane17 <- coda.samples(jmDiLane17, pars, thin=1, n.iter=10000)
windows();plot(csDiLane17)
summary(csDiLane17)

jmDiLane18 <-jags.model(file="dist_samp_emp.JAG", jdat_emp18, inits18, n.chains=3, n.adapt=2000)
csDiLane18 <- coda.samples(jmDiLane18, pars, thin=1, n.iter=10000)
windows();plot(csDiLane18)
summary(csDiLane18)

jmDiLane <-jags.model(file="dist_samp_emp.JAG", jdat_emp, inits, n.chains=3, n.adapt=2000)
csDiLane <- coda.samples(jmDiLane, pars, thin=1, n.iter=10000)
windows();plot(csDiLane)
summary(csDiLane)
save(csDiLane, file="csDiLane_8-31-2019.gzip")

# Allow sigma to vary by year
inits<- function(){list(N=Min0, logED=runif(1, 1, 5), beta0=runif(3,5,7))}

jmDiLane.s <-jags.model(file="dist_samp_emp_sigma2.JAG", jdat_emp, inits, n.chains=3, n.adapt=5000)
csDiLane.s <- coda.samples(jmDiLane.s, pars, thin=1, n.iter=10000)
plot(csDiLane.s, ask=TRUE)
summary(csDiLane.s)
save(csDiLane.s, file="csDiLane.s_8-31-2019.gzip")

# Allow sigma and density to vary by year
inits<- function(){list(N=Min0, logED=runif(3, 1, 5), beta0=runif(3,5,7))}

jmDiLane.sD <-jags.model(file="dist_samp_emp_sigma2.JAG", jdat_emp, inits, n.chains=3, n.adapt=5000)
csDiLane.sD <- coda.samples(jmDiLane.sD, pars, thin=1, n.iter=20000)
plot(window(csDiLane.sD[,c("D.r[1]", "D.r[2]", "D.r[3]")], start=10000))
plot(window(csDiLane.sD[,c("sigma[1]", "sigma[2]", "sigma[3]")], start=10000))

summary(csDiLane.sD)
save(csDiLane.sD, file="csDiLane.sD_8-31-2019.gzip")
load("csDiLane.sD_8-31-2019.gzip")
windows(); plot(csDiLane.sD, ask=TRUE)


# Allow just density to vary by year
inits<- function(){list(N=Min0, logED=runif(3, 1, 5), beta0=runif(1,5,7))}

jmDiLane.D <-jags.model(file="dist_samp_emp_D.JAG", jdat_emp, inits, n.chains=3, n.adapt=5000)
csDiLane.D <- coda.samples(jmDiLane.D, pars, thin=1, n.iter=20000)
windows();plot(csDiLane.D[,c("D.r[1]", "D.r[2]", "D.r[3]")])
summary(csDiLane.D)
save(csDiLane.D, file="csDiLane.D_8-31-2019.gzip")
load(file="csDiLane.D_8-31-2019.gzip")

## Skip over sites 1-3 and 15 in 2018
inits<- function(){list(N=Min0, logED=runif(3, 1, 5), beta0=runif(1,5,7))}
jmDiLane.D <-jags.model(file="dist_samp_emp_D18.JAG", jdat_emp, inits, n.chains=3, n.adapt=5000)
csDiLane.D <- coda.samples(jmDiLane.D, pars, thin=1, n.iter=20000)
windows();plot(csDiLane.D[,c("D.r[1]", "D.r[2]", "D.r[3]")])
summary(csDiLane.D)
save(csDiLane.D, file="csDiLane.D18_9-9-2019.gzip")


# Trace plots and gelman-rubin diagnostic
gelman.diag(csDiLane.D)

library(lattice)
library(gridExtra)

# All years
png("../comparisons/empirical-convergenceplots.png", width=11.25, height=8.75, res=600, units="in")

p1<-xyplot(csDiLane.D[,c("D.r[1]", "D.r[2]", "D.r[3]", "sigma", "mean_pCirc")], as.table=TRUE, layout=c(2,3), strip=strip.custom(var.name=" ", factor.levels=c("D.2016", "D.2017", "D.2018", "sigma", "Overall detection"), strip.levels=rep(TRUE, 5)))

p2<-densityplot(csDiLane.D[,c("D.r[1]", "D.r[2]", "D.r[3]", "sigma", "mean_pCirc")], plot.points=FALSE, as.table=TRUE, layout=c(2, 3), strip=strip.custom(var.name=" ", factor.levels=c("D.2016", "D.2017", "D.2018", "sigma", "Overall detection"), strip.levels=rep(TRUE, 5)))

grid.arrange(p1,p2, ncol=2)

dev.off()
system("open ../comparisons/empirical-convergenceplots.png")

# Other formulations of the state model -----------------------------------


# Fit HDS w/ availability
initsA <- function(){list(M=Min, alpha0=runif(1, 1, 5), beta0=runif(1,5,7), phi0=runif(1,-7,7))}
parsA <- c(names(initsA())[-1],'mean_pCirc', 'mean_pAvail','M.tot')

library(rjags)
jmDiLaneA <-jags.model(file="dist_samp_avail.JAG", jdat_emp, inits, n.chains=3, n.adapt=2000)
csDiLaneA <- coda.samples(jmDiLaneA, parsA, thin=1, n.iter=10000)
csDiLaneA2 <- coda.samples(jmDiLaneA, parsA, thin=1, n.iter=90000)
plot(csDiLaneA2, ask=TRUE)
save(csDiLaneA2, file="csDiLane_Avail.gzip")
gelman.diag(csDiLaneA2)

## Zero-inflated model
Nst <- apply(jdat_emp$y, 1, max) + 1
Nst[is.na(Nst)]<-1

initszip <- function(){list(M=Nst, alpha0=runif(1, 4, 5), beta0=runif(1,5,7), omega=runif(1, 0, 1))}

pars<- c(names(initszip())[-1],'mean_pCirc', 'M.tot')
jmDiLane_zip <-jags.model(file="dist_samp_zip.JAG", jdat_emp, initszip, n.chains=3, n.adapt=2000)
csDiLane_zip <- coda.samples(jmDiLane_zip, pars, thin=1, n.iter=10000)

plot(csDiLane_zip)
summary(csDiLane_zip)

# All years, raster covariate ---------------------------------------------
library(rjags)
load.module('dic')

inits<- function(){list(M=Min, ydet=ydetinit, y=yinit,alpha0=runif(1, 1, 5), beta0=runif(1,5,7))}
parsfull<- c('alpha0', 'alpha1', 'deviance', 'mu', 'y','beta0','mean_pCirc', 'D.tot')

# 250m
jmDiLane_cov <-jags.model(file="dist_samp_ms_cov.JAG", jdat_emp1, inits, n.chains=3, n.adapt=2000)
csDiLane_cov <- coda.samples(jmDiLane_cov, parsfull, thin=1, n.iter=10000)
save(csDiLane_cov, file="csDiLane_ms_cov.gzip")

# 500m
jmDiLane_cov500 <-jags.model(file="dist_samp_ms_cov.JAG", jdat_emp2, inits, n.chains=3, n.adapt=2000)
csDiLane_cov500 <- coda.samples(jmDiLane_cov500, parsfull, thin=1, n.iter=10000)

# 1000m
jmDiLane_cov1000 <-jags.model(file="dist_samp_ms_cov.JAG", jdat_emp3, inits, n.chains=3, n.adapt=2000)
csDiLane_cov1000 <- coda.samples(jmDiLane_cov1000, parsfull, thin=1, n.iter=10000)

# 1500m
jmDiLane_cov1500 <-jags.model(file="dist_samp_ms_cov.JAG", jdat_emp4, inits, n.chains=3, n.adapt=2000)
csDiLane_cov1500 <- coda.samples(jmDiLane_cov1500, parsfull, thin=1, n.iter=10000)

# 2000m
jmDiLane_cov2000 <-jags.model(file="dist_samp_ms_cov.JAG", jdat_emp5, inits, n.chains=3, n.adapt=2000)
csDiLane_cov2000 <- coda.samples(jmDiLane_cov2000, parsfull, thin=1, n.iter=10000)


# Look at deviance
summary(csDiLane_cov[,"deviance"]) # 902 - 963
summary(csDiLane_cov500[,"deviance"])
summary(csDiLane_cov1000[,"deviance"])
summary(csDiLane_cov1500[,"deviance"])
summary(csDiLane_cov2000[,"deviance"])

# waic
mcmc_250<-as.matrix(csDiLane_cov) #niter x npar
mcmc_500<-as.matrix(csDiLane_cov500) 
mcmc_1000<-as.matrix(csDiLane_cov1000) 
mcmc_1500<-as.matrix(csDiLane_cov1500) 
mcmc_2000<-as.matrix(csDiLane_cov2000) 

mu250 <- mcmc_250[,grep('mu[', colnames(mcmc_250), fixed=TRUE)]
y250 <- mcmc_250[,grep('y[', colnames(mcmc_250), fixed=TRUE)]

mu500 <- mcmc_500[,grep('mu[', colnames(mcmc_500), fixed=TRUE)]
y500 <- mcmc_500[,grep('y[', colnames(mcmc_500), fixed=TRUE)]

mu1000 <- mcmc_1000[,grep('mu[', colnames(mcmc_1000), fixed=TRUE)]
y1000 <- mcmc_1000[,grep('y[', colnames(mcmc_1000), fixed=TRUE)]

mu1500 <- mcmc_1500[,grep('mu[', colnames(mcmc_1500), fixed=TRUE)]
y1500 <- mcmc_1500[,grep('y[', colnames(mcmc_1500), fixed=TRUE)]

mu2000 <- mcmc_2000[,grep('mu[', colnames(mcmc_2000), fixed=TRUE)]
y2000 <- mcmc_2000[,grep('y[', colnames(mcmc_2000), fixed=TRUE)]


# Log likelihood for each observation, each iteration (nIter)
loglik250<-matrix(NA, nrow(mu250), ncol(mu250))
loglik500<-matrix(NA, nrow(mu500), ncol(mu500))
loglik1000<-matrix(NA, nrow(mu1000), ncol(mu1000))
loglik1500<-matrix(NA, nrow(mu1500), ncol(mu1500))
loglik2000<-matrix(NA, nrow(mu2000), ncol(mu2000))


for(k in 1:30000){
  for(i in 1:ncol(mu250)){
    loglik250[k,i] <- dbinom(y250[k,i], 1, mu250[k,i], log=TRUE)
    loglik500[k,i] <- dbinom(y500[k,i], 1, mu500[k,i], log=TRUE)
    loglik1000[k,i] <- dbinom(y1000[k,i], 1, mu1000[k,i], log=TRUE)
    loglik1500[k,i] <- dbinom(y1500[k,i], 1, mu1500[k,i], log=TRUE)
    loglik2000[k,i] <- dbinom(y2000[k,i], 1, mu2000[k,i], log=TRUE)
  }
}

mean250<- apply(loglik250, 1, mean, na.rm=T)
mean250[mean250=="-Inf"]<-0
mlppd250 <- -2 * sum(mean250, na.rm = T)
pD250 <- sum(apply(loglik250, 1, var), na.rm = T)
waic250 <- mlppd250 + 2 * pD250

mean500<- apply(loglik500, 1, mean, na.rm=T)
mean500[mean500=="-Inf"]<-0
mlppd500 <- -2 * sum(mean500, na.rm = T)
pD500 <- sum(apply(loglik500, 1, var), na.rm = T)
waic500 <- mlppd500 + 2 * pD500

mean1000<- apply(loglik1000, 1, mean, na.rm=T)
mean1000[mean1000=="-Inf"]<-0
mlppd1000 <- -2 * sum(mean1000, na.rm = T)
pD1000 <- sum(apply(loglik1000, 1, var), na.rm = T)
waic1000 <- mlppd1000 + 2 * pD1000

mean1500<- apply(loglik1500, 1, mean, na.rm=T)
mean1500[mean1500=="-Inf"]<-0
mlppd1500 <- -2 * sum(mean1500, na.rm = T)
pD1500 <- sum(apply(loglik1500, 1, var), na.rm = T)
waic1500 <- mlppd1500 + 2 * pD1500

mean2000<- apply(loglik2000, 1, mean, na.rm=T)
mean2000[mean2000=="-Inf"]<-0
mlppd2000 <- -2 * sum(mean2000, na.rm = T)
pD2000 <- sum(apply(loglik2000, 1, var), na.rm = T)
waic2000 <- mlppd2000 + 2 * pD2000

# All years together
library(rjags)
jmDiLane <-jags.model(file="dist_samp_ms.JAG", jdat_emp, inits, n.chains=3, n.adapt=2000)
csDiLane <- coda.samples(jmDiLane, parsfull, thin=1, n.iter=10000)
save(csDiLane, file="csDiLane_ms.gzip")

load(file="csDiLane_ms.gzip")
summary(csDiLane)
plot(csDiLane, ask=TRUE)
