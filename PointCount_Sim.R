# Set up to simulate and analyze many data sets
library('rjags')
set.seed(2013)

# Parameters that don't change each simulation

# Distance sampling
#nsites 
#nvis 
#nyears
#nB
#dB
#point.area
#esa

# SCR
# tr
# te


# Preliminaries
nsites <- 100  # Number of sites
nyears <- 1    # Number of primary periods (e.g., year)

# Expected covey density on the log scale
logD <- log(2.4)
ED <- exp(logD)

# Intercept of perceptibility for distance sampling model. 
beta0 <- log(250)

# Information needed for point count distance sampling model
db <- c(0, 30, 80, 140, 230, 450)                   # Distance Bins
nBins <-length(db)-1                                # 5 distance classes
xg <- NA                                            # midpoints of distance bins
for(i in 1:(length(db)-1)){xg[i] <- (db[i] + db[i+1])/2 }
delta <- NA                                         # Size of each distance bin
for(i in 1:(length(db)-1)){delta[i] <- db[i+1] - db[i] }
dmax <- db[length(db)]                                      # Max distance
point.area <- pi*dmax^2                             # area of point count survey in ha              
pix <- (2*xg*delta)/(dmax*dmax)                     # for area calculations; Scaled radial density function

# Total area sampled
esa<-(point.area*nsites)/10000

# Function to simulate data
fun.CoveyCallCountSim <- function(logD=logD, beta0=beta0, nsites=nsites, point.area=point.area, coveysize=10, pix=pix){
  
  N <- y <- pMarg <- rep(NA, nsites)
  pi.uc <- pi.c <- p <- rep(NA, nBins)
  ydet <- matrix(NA, nsites, nBins)
  sigma <- NA
  
  # I. Ecological process (abundance)
  # Part 1: Abundance model
  ED <- exp(logD)        # Expected covey density (ED) 
  EQ <- ED*coveysize*nsites / ((point.area*nsites)/10000) # Expected quail density w/i study area assuming a covey size of 12
  N <- rpois(nsites, ED) # Realized covey abundance at each site
  NQ  <- N*coveysize / ((point.area*nsites)/10000) # Realized quail abundance w/i study area
  
  # II. Observation process
  # II.a Perceptibility
  sigma <- exp(beta0)
  
  for(b in 1:nBins){
    # analytical integration of detection function; for a half normal detection function
    # or, how much area is under the curve; for each distance bin. 
    # or, the expected proportion of sightings at each distance from the point count center
    # need the numerator 2x b/c db is the start and end of a bin, not the bin itself
    # in point count data, area of each bin increases as you move away from the center. 
    
    # point.area = total area of point count, pix = proportion of total area in each donut
    p[b] <- (sigma^2*(1-exp(-db[b+1]^2/(2*sigma^2)))- 
               sigma^2*(1-exp(-db[b]^2/(2*sigma^2))))*2*3.1416/
      (point.area*pix[b]) 
    pi.uc[b]<- p[b]*pix[b]
  }
  
  # Overall percepbility at each point (constant)
  pCirc <- sum(pi.uc[1:nBins]) # how much area is under the curve?
  
  # Normalize to sum to 1
  for(b in 1:nBins){
    pi.c[b] <- pi.uc[b]/pCirc
  } # k
  
  
  for(i in 1:nsites){
    # Observation process
    y[i] <- rbinom(1, N[i], pCirc)  # Part 3: Number of detected individuals
    ydet[i,1:nBins] <- rmultinom(1, y[i], pi.c[1:nBins]) # Part 4: Distance
  }
  
  return(list(y=y, ydet=ydet, N=N, NQ=NQ, ED=ED, EQ=EQ, pCirc=pCirc))
}