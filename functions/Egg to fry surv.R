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

## proportion natural-origin spawners: prop.nat
## Scouring flow: scour = 1 if scour flow occured otherwise 1
egg2fry<-function(prop.nat,scour,tmp.eff, vary, pctil){
    
  if(sum(vary == "egg.viab")){
    viab<-log((tmp.eff+ 0.000001)/((1-tmp.eff)+0.0000001))
    viab<- viab*pctil[vary == "egg.viab"]
    tmp.eff<-inv.logit(viab)
  }

  B0<- 0.041; if(sum(vary == "egg.int")) B0<- 0.041*pctil[vary == "egg.int"]
  nat.ad<- 0.533; if(sum(vary == "egg.nat.ad")) nat.ad<- 0.533*pctil[vary == "egg.nat.ad"]
  scur<- -0.655; if(sum(vary == "egg.scour")) scur<- -0.655*pctil[vary == "egg.scour"]
  
  inv.logit(B0 + prop.nat*nat.ad + scur*scour)*tmp.eff
}

#egg2fry(prop.nat= c(1,.75,.25),scour= c(0,0,.2))
        