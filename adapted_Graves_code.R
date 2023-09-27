#Ran in R 2.15.1
library (gdistance) #used to calculate least cost distances 
########################################### 
# Likelihood function for unique origins 
###########################################

#nInd seems to be the number of individuals
nInd <- 5 # this may or may not be right to specify

#r1 should be built beforehand: raster of landcover values
#natal should be built beforehand: df of locations from which the dispersal movements began
#tCoords should be built beforehand: df of locations at which dispersal movements ended
#not sure how to build allCoords


nll <- function(pars) {
  probs2<- rep(NA,length(nInd))
  a0 <- pars[1] #intercept of lsigma 
  a1 <- pars[2] #coefficient for sex 
  lsigma <- a0 + a1*sex 
  sigma <- exp(lsigma) #scale parameter for dispersal distance
  theta <- 1/(sqrt(2*pi)*sigma^2)
  a2 <- pars[3] #resistance coefficient 
  costcell <- exp(a2*var2) #calculate cost for each pixel
  r1 <- setValues(r1,costcell) #assign costs to each cell in raster 
  tr1 <- transition(r1,transitionFunction=function(x) 1/mean(x), directions=8) #calculate the resistance distances among neighbors
  tr1CorrC <- geoCorrection(tr1,type="c",multpl=FALSE,scl=FALSE) #corrects the diagonal distances when 8 neighbors
  for (i in 1:nInd) { #loop through all individuals to calculate the likelihoods of that dispersal 
    costs1 <- costDistance(tr1CorrC, natal[i,], tCoords[i,]) #calc cost of dispersal between origin and settlement location
    costsAll <- costDistance(tr1CorrC, natal[i,], allCoords) #calc cost of dispersal between origin and all locations
    probs1 <- theta[t] * exp(-(costs1^2/(2*sigma[t]^2))) #calc the probability of moving to end location 
                            probsAll <- theta[t] * exp(-(costsAll^2/(2*sigma[t]^2))) #calculate the probability of moving to each location in S, which is the discrete representation of the landscape.
                              probs2[i] <- probs1/sum(probsAll) #find the likelihood for the individual of moving to end location
  } -sum(log(probs2)) #final likelihood used to compare values
}
