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

#
#juvs<-matrix(c(20000,200,20,2,
#               50000,500,50,5,
#               100000,1000,100,10), ncol = 4, byrow = T)

#flood_hab <- c(100,100,100)
#juv_hab <-c(500,500,500)


#Assign fish to habitats function, there should be one for each watershed
fill.AMR.func<-function(juvs,flood_hab,juv_hab,territory_size,stoch){
  
  xxx <- 1

  #Holding vectors
  migrants<-matrix(rep(0,4),ncol=4,nrow= xxx)
  flood_rear<-matrix(rep(0,4),ncol=4,nrow=xxx)
  river_rear<-matrix(rep(0,4),ncol=4,nrow=xxx)
  
  #Assigning individuals to flood habitat largest first
  flood_avail<-flood_hab
  if(stoch==0){
    for(r in 1:xxx){
      for(i in 2:1){
        flood_rear[r,i]<-min((flood_avail[r]/territory_size[i]),juvs[i])
        flood_avail[r]<-max(flood_avail[r]-flood_rear[r,i]*territory_size[i],0)
      }}
    
    #Who's left for in-river rearing
    juvs<-juvs-flood_rear
    #In-river rearing
    rear_avail<-juv_hab
    for(r in 1:xxx){
      for(i in 2:1){
        river_rear[r,i]<-min((rear_avail[r]/territory_size[i]),juvs[i])
        rear_avail[r]<-max(rear_avail[r]-river_rear[r,i]*territory_size[i])
      }}
  } else{
    for(r in 1:xxx){
      for(i in 2:1){
        flood_rear[r,i]<-min(floor(flood_avail[r]/territory_size[i]),juvs[i])
        flood_avail[r]<-max(flood_avail[r]-flood_rear[r,i]*territory_size[i])
      }}
    
    #Who's left for in-river rearing
    juvs<-juvs-flood_rear
    #In-river rearing
    rear_avail<-juv_hab
    for(r in 1:xxx){
      for(i in 2:1){
        river_rear[r,i]<-min(floor(rear_avail[r]/territory_size[i]),juvs[i])
        rear_avail[r]<-max(rear_avail[r]-river_rear[r,i]*territory_size[i])
      }}
  }
 
  
  #The rest leave
  migrants<-juvs-river_rear
  
  flood_rear<-pmax(flood_rear,0)
  river_rear<-pmax(river_rear,0)
  migrants<-pmax(migrants,0)
  
  list(river=river_rear,flood =flood_rear,migr=migrants)
}

#fill.trib.func(juvs,flood_hab,juv_hab,territory_size)
