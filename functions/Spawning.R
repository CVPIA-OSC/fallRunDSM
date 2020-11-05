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




# spawnfun<-function(escapement,s_adult_inriver,sex_ratio,spawn_hab,redd_size,prob_scour,fecund,s_egg_to_fry,stoch, vary, pctil){
#   spawners<-((escapement*s_adult_inriver*sex_ratio)>(spawn_hab)/redd_size)*(spawn_hab)/redd_size +
#     ((escapement*s_adult_inriver*sex_ratio)<=(spawn_hab)/redd_size)*(escapement*s_adult_inriver*sex_ratio)
#   newfry<-spawners*(1-prob_scour)*fecund*s_egg_to_fry
#   zzs<-matrix(rep(0,length(escapement)*3),ncol = 3)
#   newfry<- cbind(newfry,zzs)
#   return(newfry)
# }


#Reproductive Success Function
spawnfun<-function(escapement,s_adult_inriver,sex_ratio,spawn_hab,redd_size,prob_scour,fecund,s_egg_to_fry,stoch, vary, pctil){

  if(sum(vary == "reprod.sexr")) sex_ratio<- sex_ratio*pctil[vary == "reprod.sexr"]
  if(sum(vary == "reprod.redd")) redd_size<- redd_size*pctil[vary == "reprod.redd"]
  if(sum(vary == "reprod.fecund")) fecund<- fecund*pctil[vary == "reprod.fecund"]


  spawnCapacity<-spawn_hab/redd_size
  meanSpawners<-escapement*s_adult_inriver*sex_ratio
  
  if(max(escapement)<=1000000000 & stoch==1){
    randSpawners<-rbinom(31,round(escapement),(s_adult_inriver*sex_ratio))
  } else{randSpawners<-round(meanSpawners)}
  
  # randSpawners2<-pmax(round(rnorm(31,meanSpawners,(sqrt(meanSpawners)/2))),0)

  spawners<-((randSpawners>spawnCapacity)*round(spawnCapacity) +
                ((randSpawners<=spawnCapacity)*randSpawners))*stoch+
                (((meanSpawners>spawnCapacity)*spawnCapacity +
                (meanSpawners<=spawnCapacity)*meanSpawners))*(1-stoch)

  meanfry<-as.numeric(spawners*(1-prob_scour)*fecund*s_egg_to_fry)
  # randFry<-rbinom(31,rpois(31,rbinom(31,round(spawners),(1-prob_scour))*fecund),s_egg_to_fry)
  if(max(meanfry)<=1000000000 & stoch==1){
    randFry2<-pmax(round(rnorm(31,meanfry,(sqrt(meanfry)/2))),0)
  } else{randFry2<-round(meanfry)}
  
  

  newfry<-randFry2*stoch+meanfry*(1-stoch)
  
  zzs<-matrix(rep(0,length(escapement)*3),ncol=3)
  newfry<-cbind(newfry,zzs)
  return(newfry)
}

