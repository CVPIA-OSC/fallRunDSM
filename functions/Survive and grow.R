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

#tester<-fill.Lmain.func(juvs,flood_hab,juv_hab,territory_size)

#river_surv<-flood_surv<-matrix(c(rep(.1,4),rep(.3,4),rep(.5,4)),ncol = 4, byrow = T)

#T.mtx<-Hab.growth(daily.rates=c(0.5,1.06))

#Growth and survival function, there should be one for each watershed
rearfunc<-function(river_rear,flood_rear,trans_mat_river,trans_mat_flood,flood_surv,river_surv,stoch){
  
  spaces<-dim(river_rear)[1]*dim(river_rear)[2]
  
  if(max(river_rear)<=1000000000 & stoch==1){
    junk<-matrix(rbinom(spaces,prob=as.vector(t(river_surv)),size=round(as.vector(t(river_rear)))),ncol=4,byrow=TRUE)
  } else{junk<-round(river_rear*river_surv)}
  
  if(max(flood_rear)<=1000000000 & stoch==1){
    junk2<-matrix(rbinom(spaces,prob=as.vector(t(flood_surv)),size=round(as.vector(t(flood_rear)))),ncol=4,byrow=TRUE)
  } else{junk2<-round(flood_rear*flood_surv)}
  
  
  #survival
  river_rear<-(junk*stoch)+(river_rear*river_surv)*(1-stoch)
  flood_rear<-(junk2*stoch)+(flood_rear*flood_surv)*(1-stoch)
  
  #Growth
  river_rear<-river_rear %*% trans_mat_river
  flood_rear.outz<-c()
  if(length(dim(trans_mat_flood))>2){
    for(ii in 1:dim(trans_mat_flood)[3]){
      this<-flood_rear[ii,] %*% trans_mat_flood[,,ii]
      flood_rear.outz<-rbind(flood_rear.outz,this)
    }
  } else{flood_rear.outz<- flood_rear  %*% trans_mat_flood}
  
  if(stoch==1){
    flood_rear.outz<-round(flood_rear.outz)
    river_rear<-round(river_rear)
  }
  
  list(riv.rear=river_rear,flood.rear=flood_rear.outz)
  
}
# 
# trans_mat_river<-T.mtx[,,1]
# trans_mat_flood<-T.mtx[,,2]
# 
# river_rear<-tester$Lmain.river
# flood_rear<-tester$Lmain.flood
# 
# 
# rearfunc(river_rear,flood_rear,trans_mat_river,trans_mat_flood,flood_surv,river_surv)




# 
# y<-matrix(c(0.1,0.7,0.2,0,0.4,0.6,0,0,1),ncol=3,byrow=TRUE)
# x<-matrix(c(100,50,90,25,75,85,1,2,36,1000,10,90),ncol=3)
# x%*%y
# # [,1]  [,2]   [,3]
# # [1,] 10.0 100.0  101.0
# # [2,]  5.0  69.0 1061.0
# # [3,]  9.0  63.4   28.6
# # [4,]  2.5  18.3   96.2
# 
# tada<-array(NA,dim=c(4,3,1000))
# for(time in 1:1000){
#   for(what in 1:dim(x)[1]){
#     tada[what,,time]<-as.vector(rmultinom(1,x[what,1],y[1,]))+as.vector(rmultinom(1,x[what,2],y[2,]))+as.vector(rmultinom(1,x[what,3],y[3,]))
#     
#   }}
# 
# apply(tada,c(1,2),mean)
# # [,1]    [,2]     [,3]
# # [1,] 10.128 100.075  100.797
# # [2,]  4.920  69.064 1061.016
# # [3,]  9.057  63.414   28.529
# # [4,]  2.499  18.393   96.108

# 
# 
# tada<-array(NA,dim=c(15,4,1000))
# for(time in 1:1000){
#   
# 
# for(what in 1:dim(river_rear)[1]){
#   tada[what,,time]<-as.vector(rmultinom(1,river_rear[what,1],trans_mat_river[1,]))+as.vector(rmultinom(1,river_rear[what,2],trans_mat_river[2,]))+as.vector(rmultinom(1,river_rear[what,3],trans_mat_river[3,]))+as.vector(rmultinom(1,river_rear[what,4],trans_mat_river[4,]))
#   
# }
# }




