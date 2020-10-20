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


################################################################
################################################################
##     Juvenile Chinook habitat-specific growth     ############
################################################################
##  daily.rates = mean daily growth rate mm/day in main channel 
##  and floodplain (MC,FP)  assumes CV daily growth rate 0.3
## Number of weeks inundated wks.fld
## Default is 2 week FP inundation then fish in channel
##  with correction May 3 2016

Hab.growth<- function(daily.rates,wks.fld=rep(2,31)){
  inch<-ifelse(wks.fld>0,(4-wks.fld)/4,1)
  inwk<-1-inch
  
  T.mtx<-array(rep(0,32),dim=c(4,4,2))
  T.mtx[4,4,]<-1
  
  ## gamma MOM to estimate gamma parms
  gamma.MOM<- function(meanz,sdz){
    alpha<-(meanz/sdz)^2
    beta<- (sdz^2)/meanz
    return(c(alpha,beta)) }
  
  rats<-daily.rates*30
  
  grow<-matrix(gamma.MOM(rats,(rats*0.3)),ncol=2)
  
  cutz<-c(35,43,73,109)
  
  for(i in 2:4){
    gam.grow.parm<-matrix(gamma.MOM((rats+mean(c(cutz[i],cutz[i-1]))),rats*0.3),ncol = 2)
    
    T.mtx[(i-1),1,1] <-pgamma(cutz[2],gam.grow.parm[1,1], scale = gam.grow.parm[1,2])
    T.mtx[(i-1),2,1] <-pgamma(cutz[3],gam.grow.parm[1,1], scale = gam.grow.parm[1,2]) - T.mtx[(i-1),1,1]
    T.mtx[(i-1),3,1] <-pgamma(cutz[4],gam.grow.parm[1,1], scale = gam.grow.parm[1,2]) - sum(T.mtx[(i-1),1:2,1])
    T.mtx[(i-1),4,1] <-1- pgamma(cutz[4],gam.grow.parm[1,1], scale = gam.grow.parm[1,2])
    
    
    T.mtx[(i-1),1,2] <-pgamma(cutz[2],gam.grow.parm[2,1], scale = gam.grow.parm[2,2])
    T.mtx[(i-1),2,2] <-pgamma(cutz[3],gam.grow.parm[2,1], scale = gam.grow.parm[2,2]) - T.mtx[(i-1),1,2]
    T.mtx[(i-1),3,2] <-pgamma(cutz[4],gam.grow.parm[2,1], scale = gam.grow.parm[2,2]) - sum(T.mtx[(i-1),1:2,2])
    T.mtx[(i-1),4,2] <-1- pgamma(cutz[4],gam.grow.parm[2,1], scale = gam.grow.parm[2,2])
  }
  
  T.mtx.fp<-array(rep(0,4*4*31),dim=c(4,4,31))
  T.mtx.fp[4,4,]<-1
  
  for(jj in 1:31){
    T.mtx.fp[,,jj]<- (T.mtx[,,1])*(inch[jj]) + (T.mtx[,,2])*inwk[jj]
    
    # Eliminate nosense transitions and normalize just in case - floodplain
    T.mtx.fp[2,1,jj]<-T.mtx.fp[3,1,jj]<-T.mtx.fp[3,2,jj]<-0
    T.mtx.fp[,,jj]<-T.mtx.fp[,,jj]/rowSums(T.mtx.fp[,,jj])
  }
  
  # Eliminate nosense transitions and normalize just in case - inchannel
  T.mtx[2,1,1]<-T.mtx[3,1,1]<-T.mtx[3,2,1]<-0
  T.mtx[,,1]<-T.mtx[,,1]/rowSums(T.mtx[,,1])
  
  outz<-list("T.mtx.ic"=T.mtx[,,1],"T.mtx.fp"=T.mtx.fp)

  return(outz)
}


# # ### baseline rates
# rates<-c(0.5,1.06)
# T.mtx<-Hab.growth(daily.rates=rates)
# 
# no<-c(1000,0,0,0)
# 
# no  %*% T.mtx[,,1]
# 
# no  %*% T.mtx[,,2]
