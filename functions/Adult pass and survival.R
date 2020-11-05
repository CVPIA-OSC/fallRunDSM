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



#############################################################################
###             Adult en route survival function            #################
#############################################################################
###   Water temperature: aveT23 = 1 if ave daily temps > 22oC otherwise 0
###   BPovrT = 1 when Tisdale Bypass and or Yolo Bypass are overtopped 
###   only pertains to Tributaries above bypasses
###   harvest = harvest rate for each watershed
Adult.S<-function(aveT23,BPovrT, harvest, vary, pctil){
  BA<- 3;  if(sum(vary == "adult.s.int")) BA<-BA*pctil[vary == "adult.s.int"]
  temp<- -0.26; if(sum(vary == "adult.s.tmp")) temp<- -0.26*pctil[vary == "adult.s.tmp"]
  ovrtop<- - 0.019; if(sum(vary == "adult.s.ovrtop")) ovrtop<- - 0.019*pctil[vary == "adult.s.ovrtop"]
  harv<-harvest;if(sum(vary == "adult.s.harv")) harv<-harv*pctil[vary == "adult.s.harv"]
  
  S<-inv.logit(BA + temp*aveT23 + ovrtop*BPovrT) - harv
  ifelse(S > 0,S,0)
}



#Adult.S(aveT23 = c(0,1,0,0),BPovrT=c(0,0,1,0), harvest=c(0,0,.45,0))

#############################################################################
###             Adult prespwan survival function            #################
#############################################################################
###   DegDay = average degree days
Adult.PSS<-function(DegDay, vary, pctil){
  BP<- 3; if(sum(vary == "adult.ps.int")) BP<- BP*pctil[vary == "adult.ps.int"]
  DD<- -0.000669526; if(sum(vary == "adult.ps.dd")) DD<- DD*pctil[vary == "adult.ps.dd"]
  inv.logit(BP + DD*DegDay) 
}

#Adult.PSS(DegDay=seq(100,5000,1000))

##########################################################################
###             Adult straying function                  #################
##########################################################################
###   wild = 1 if  wild fish returning, otherwise 0
###   propDLTtrans = proportion hatchery fish trucked transported to delta
###   propBAYtans = proportion hatchery fish trucked transported to bay
###   pctQnatl = proportion flows at tributary junction coming from natal watershed
###   SCDLT = 1 if fish returning to S Central delta otherwise 0 
###   CrxChn number days cross channel closed

Ad.Stray<-function(wild,propDLTtrans=0, propBAYtans=0,pctQnatl,SCDLT,CrxChn, vary, pctil){
  BT<- 3;if(sum(vary == "stray.int")) BT<- BT*pctil[vary == "stray.int"]
  wld<- -5.5; if(sum(vary == "stray.wild")) wld<- wld*pctil[vary == "stray.wild"]
  natl.Q<- -1.99; if(sum(vary == "stray.natl.q")) natl.Q<- natl.Q*pctil[vary == "stray.natl.q"]
  cc.open<- -0.174; if(sum(vary == "stray.cc.open")) cc.open<- cc.open*pctil[vary == "stray.cc.open"]
  trk.bay<- 2.09; if(sum(vary == "stray.trk.bay")) trk.bay<- trk.bay*pctil[vary == "stray.trk.bay"]
  trk.dlta<- 2.89; if(sum(vary == "stray.trk.dlta")) trk.dlta<- trk.dlta*pctil[vary == "stray.trk.dlta"]
  
  inv.logit(BT +wld*wild + natl.Q*pctQnatl + cc.open*SCDLT*CrxChn +
              trk.bay*propBAYtans*(1-wild) + trk.dlta*propDLTtrans*(1-wild))
  
}

#Ad.Stray(wild= c(1,1,1,0,0),pctQnatl=c(1,0.5,0.25,0.5, 0.5),SCDLT=c(0,0,1,0,0),
#         CrxChn=rep(0,5),propDLTtrans=rep(1,5), propBAYtans=rep(1,5))

