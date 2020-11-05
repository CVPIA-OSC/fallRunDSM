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


#juvs<-matrix(c(20000,200,20,2,
#               50000,500,50,5,
#               100000,1000,100,10), ncol = 4, byrow = T)
#
#flood_hab <- c(100,100,100)
#juv_hab <-c(500,500,500)


#Assign fish to habitats function, there should be one for each watershed
fill.Umain.func<-function(juvs,flood_hab,juv_hab,territory_size,stoch){
  
  all.sheds<-orig.tot<- colSums(juvs)
  
  props<- t(t(juvs)/ all.sheds)
  props[is.nan(props)]<-0 #cant divide by zero
  
  #Holding vectors
  migrants<-matrix(rep(0,4),ncol=4,nrow=1)
  flood_rear<-matrix(rep(0,4),ncol=4,nrow=1)
  river_rear<-matrix(rep(0,4),ncol=4,nrow=1)
  
  #Assigning individuals to flood habitat largest first
  flood_avail<-flood_hab
  
  if(stoch==0){
    for(i in 3:1){
      flood_rear[i]<-min((flood_avail/territory_size[i]),all.sheds[i])
      flood_avail<-max(flood_avail-flood_rear[i]*territory_size[i],0)
    }
    
    #Who's left for in-river rearing
    all.sheds<-all.sheds-flood_rear
    #In-river rearing
    rear_avail<-juv_hab
    for(i in 3:1){
      river_rear[i]<-min((rear_avail/territory_size[i]),all.sheds[i])
      rear_avail<-max(rear_avail-river_rear[i]*territory_size[i],0)
    }
    
    #The rest leave
    migrants<-pmax(all.sheds-river_rear,0)
    
    prop.flood<-flood_rear/orig.tot
    prop.river<- river_rear/orig.tot
    prop.migrant<- migrants/orig.tot
    
    prop.flood[is.nan(prop.flood)]<-0 #still cant divide by zero
    prop.river[is.nan(prop.river)]<-0
    prop.migrant[is.nan(prop.migrant)]<-0
    
    # prop.f<- t(t(props)*as.vector(prop.flood))
    # prop.r<- t(t(props)*as.vector(prop.river))
    # prop.m<- t(t(props)*as.vector(prop.migrant))
    
    flood_rear<-(t(t(juvs)*as.vector(prop.flood)))
    river_rear<-(t(t(juvs)*as.vector(prop.river)))
    migrants<-(t(t(juvs)*as.vector(prop.migrant)))
  } else{
    for(i in 3:1){
      flood_rear[i]<-min(floor(flood_avail/territory_size[i]),all.sheds[i])
      flood_avail<-max(flood_avail-flood_rear[i]*territory_size[i],0)
    }
    
    #Who's left for in-river rearing
    all.sheds<-all.sheds-flood_rear
    #In-river rearing
    rear_avail<-juv_hab
    for(i in 3:1){
      river_rear[i]<-min(floor(rear_avail/territory_size[i]),all.sheds[i])
      rear_avail<-max(rear_avail-river_rear[i]*territory_size[i],0)
    }
    
    #The rest leave
    migrants<-pmax(all.sheds-river_rear,0)
    
    prop.flood<-flood_rear/orig.tot
    prop.river<- river_rear/orig.tot
    prop.migrant<- migrants/orig.tot
    
    prop.flood[is.nan(prop.flood)]<-0
    prop.river[is.nan(prop.river)]<-0
    prop.migrant[is.nan(prop.migrant)]<-0
    
    # prop.f<- t(t(props)*as.vector(prop.flood))
    # prop.r<- t(t(props)*as.vector(prop.river))
    # prop.m<- t(t(props)*as.vector(prop.migrant))
    
    flood_rear<-round(t(t(juvs)*as.vector(prop.flood)))
    river_rear<-round(t(t(juvs)*as.vector(prop.river)))
    migrants<-round(t(t(juvs)*as.vector(prop.migrant)))
  }
  
  # flood_rear[is.nan(flood_rear)]<-0
  # river_rear[is.nan(river_rear)]<-0
  # migrants[is.nan(migrants)]<-0

  flood_rear<-pmax(flood_rear,0)
  river_rear<-pmax(river_rear,0)
  migrants<-pmax(migrants,0)
  
    
  list(Umain.river=river_rear,Umain.flood =flood_rear,migr=migrants)
}

#fill.Umain.func(juvs,flood_hab,juv_hab,territory_size)
