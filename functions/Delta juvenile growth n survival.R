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


#Delta growth and survival function, there should be one for each watershed
Delt.rearfunc<-function(delt_juv,trans_mat_river,D_juv_surv,stoch){
  
  spaces<-dim(delt_juv)[1]*dim(delt_juv)[2]
  
  #survival
  if(max(delt_juv)<=1000000000 & stoch==1){
    junk<-matrix(rbinom(spaces,prob=as.vector(t(D_juv_surv)),size=round(as.vector(t(delt_juv)))),ncol=4,byrow=TRUE)
  } else{junk<-round(delt_juv*D_juv_surv)}

  delt_rear<-(junk*stoch)+(delt_juv*D_juv_surv)*(1-stoch)
  
  #Growth
  if(stoch==1){
    delt_rear<-round(delt_rear %*% trans_mat_river)
  } else{delt_rear<-delt_rear %*% trans_mat_river}
      
  return(delt_rear)
}

# countz<- matrix(c(1000,100,10,1,1000,100,10,1,1000,100,10,1),ncol = 4, byrow = T)
# T.mtx<-Hab.growth(daily.rates=c(0.5,1.06))
# Delt.rearfunc(delt_juv=countz,trans_mat_river=T.mtx[,,1],D_juv_surv= test)