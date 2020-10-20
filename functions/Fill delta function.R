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

#fill.delt.func(juvs=N.delt.fsh,est_hab=DLThab[1],territory_size)

#Assign fish to habitats function, there should be one for each watershed
fill.delt.func<-function(juvs,est_hab,territory_size,stoch){

    all.sheds<-orig.tot<- colSums(juvs)
    
    props<- t(t(juvs)/ all.sheds)
    props[is.nan(props)]<-0 #cant divide by zero
    
    #Holding vectors
    migrants<-matrix(rep(0,4),ncol=4,nrow=1)
    delta_rear<-matrix(rep(0,4),ncol=4,nrow=1)
    
    #Who's left for in-delta rearing
    #In-delta rearing
    rear_avail<-est_hab
    
    if(stoch==0){
      for(i in 3:1){
        delta_rear[i]<-min((rear_avail/territory_size[i]),all.sheds[i])
        rear_avail<-max(rear_avail-delta_rear[i]*territory_size[i],0)
      }
      
      #The rest leave
      migrants<-pmax(all.sheds-delta_rear,0)
      
      prop.delta<-delta_rear/orig.tot 
      prop.migrant<-migrants/orig.tot
      
      prop.delta[is.nan(prop.delta)]<-0 #still cant divide by zero
      prop.migrant[is.nan(prop.migrant)]<-0
      
      # prop.r<- t(t(props)*as.vector(prop.delta))
      # prop.m<- t(t(props)*as.vector(prop.migrant))
      
      delta_rear<-(t(t(juvs)*as.vector(prop.delta)))
      migrants<-(t(t(juvs)*as.vector(prop.migrant))) 
      
      # delta_rear[is.nan(delta_rear)]<-0
      # migrants[is.nan(migrants)]<-0
    } else{
      
      for(i in 3:1){
        delta_rear[i]<-min(floor(rear_avail/territory_size[i]),all.sheds[i])
        rear_avail<-max(rear_avail-delta_rear[i]*territory_size[i],0)
      }
      
      #The rest leave
      migrants<-pmax(all.sheds-delta_rear,0)
      
      prop.delta<-delta_rear/orig.tot
      prop.migrant<-migrants/orig.tot
      
      prop.delta[is.nan(prop.delta)]<-0
      prop.migrant[is.nan(prop.migrant)]<-0
      
      # prop.r<- t(t(props)*as.vector(prop.delta))
      # prop.m<- t(t(props)*as.vector(prop.migrant))
      
      delta_rear<-round(t(t(juvs)*as.vector(prop.delta)))
      migrants<-round(t(t(juvs)*as.vector(prop.migrant))) 
      
      # delta_rear[is.nan(delta_rear)]<-0
      # migrants[is.nan(migrants)]<-0
    }
    
    delta_rear<- pmax(delta_rear,0)
    migrants<- pmax(migrants,0)
    
  list(Delt.rear=delta_rear,out2ocean=migrants)
}

#fill.delt.func(juvs,est_hab,territory_size)
