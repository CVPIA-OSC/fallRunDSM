###############################################################################
##    FALL CHINOOK SALMON SCIENCE INTEGRATION TEAM MODEL December 2019       ##
##      Primary Authors:                                                     ##
##                                                                           ##
##      James T. Peterson                                                    ##
##      U.S. Geological Survey, Oregon Cooperative Fish and Wildlife         ##
##      Research Unit, Oregon State University                               ##
##      Corvallis, Oregon 97331-3803, jt.peterson@oregonstate.edu            ##
##                                                                           ##
##      Adam Duarte                                                          ##
##      Oregon Cooperative Fish and Wildlife Research Unit,                  ##
##      Oregon State University,                                             ##
##      Corvallis, Oregon 97331-3803, adam.duarte@oregonstate.edu            ##
##                                                                           ##
##     Although this software program has been used by                       ##
##     the U.S. Geological Survey (USGS), no warranty, expressed             ##
##     or implied, is made by the USGS or the U.S. Government as to          ##
##     the accuracy and functioning of the program and related program       ##
##     material nor shall the fact of distribution constitute any            ##
##     such warranty, and no responsibility is assumed by the USGS           ##
##     in connection therewith.                                              ##
##                                                                           ##
##      IP-117068                                                            ##
##                                                                           ##
###############################################################################


#########################################
#### GENERATE OCEAN TRANSITION MONTH
Trans.Mo<- function(){
v<-(runif(1) < 0.36)*1
T.day<-v*round(rgamma(1,shape = 3.212851, scale= 6.225000)) + (1-v)*round(rgamma(1,shape = 19.349501, scale= 3.549833))
### calculate transition month
T.mo<-round(T.day/30)
T.mo
return(T.mo)
}

#Trans.Mo()
