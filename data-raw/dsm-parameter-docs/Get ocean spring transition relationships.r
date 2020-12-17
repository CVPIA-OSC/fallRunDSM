###############################################################################
##      Chinook supplemantary analysis for Chinook model:                    ##
##      Estimate probabiliy of ocean transition by month                     ##
##      Primary Author: James T. Peterson June 1, 2018                       ##
##      U.S. Geological Survey, Oregon CooperativeCooperative                ##
##      Fish and Wildlife Research Unit, Oregon State University             ##
##      Corvallis, Oregon 97331-3803, jt.peterson@oregonstate.edu            ##
##                                                                           ##
##     Although this software program has been used by                       ##
##     the U.S. Geological Survey (USGS), no warranty, expressed             ##
##     or implied, is made by the USGS or the U.S. Government as to          ##
##     the accuracy and functioning of the program and related program       ##
##     material nor shall the fact of distribution constitute any            ##
##     such warranty, and no responsibility is assumed by the USGS           ##
##     in connection therewith.                                              ##
###############################################################################

## gamma method of moments to estimate gamma parms
gamma.MOM<- function(meanz,sdz){
  alpha<-(meanz/sdz)^2
  beta<- (sdz^2)/meanz
  return(c(alpha,beta))
}

## spring ocean transition day of the year 1980-2010
transit.day<-c(60,106,92,17,29,76,75,15,5,78,43,80,74,38,56,61,58,62,9,29,57,27,11,61,60)
# notice bimodial distribution with break at day 40
hist(transit.day, breaks = seq(0,120,10), freq=F, xlab = "Julian day of year",  main="Observed Transition Julian days, 1981-2010")
## probability of first mode
first<-sum(transit.day < 40)/length(transit.day)
## probability of second mode
second<-1-first
## parameters for gamma distribution first mode
gam1<-gamma.MOM(mean(transit.day[transit.day< 40]),sd(transit.day[transit.day< 40]))
## parameters for gamma distribution second mode
gam2<-gamma.MOM(mean(transit.day[transit.day> 40]),sd(transit.day[transit.day> 40]))

v<-(runif(1000) < first)*1
T.day<-v*round(rgamma(1000,shape = gam1[1], scale= gam1[2])) + (1-v)*round(rgamma(1000,shape = gam2[1], scale= gam2[2]))

hist(T.day, freq=F, xlab = "Julian day of year",  main="Simulated Transition Julian days")


