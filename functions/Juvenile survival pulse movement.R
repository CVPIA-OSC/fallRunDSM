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


#################################################################################
###          Juvenile survival functions, in channel habitat monthly   ##########
#################################################################################
# 10-day maximum temperature > 25oC during rearing: maxT25 1 if yes otherwise 0
# Average daily temperature > 20oC during rearing: aveT20 1 if yes otherwise 0
# High predator prevalence: high.pred = 1 if high, otherwise zero
# Number of contact points: no.con.pts
# Proportion of streamflow diverted: prop.div
# Total volume cms of streamflow diverted: tot.div
# Stranded: strand = 1 if stranding event occurs, otherwise 0

Juv.IC.S <- function(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand,int,vary,pctil){
  
  B0<- int
  b.contact<-0.0358
  b.pro.div<-0.05
  b.tot.div<-0.215
  
  if(sum(vary == "juv.rearS.int")) B0<- B0*pctil[vary == "juv.rearS.int"]
  av.tmp<- -0.717; if(sum(vary == "juv.rearS.av.tmp")) av.tmp<- av.tmp*pctil[vary == "juv.rearS.av.tmp"]
  contact<- -0.189*b.contact; if(sum(vary == "juv.rearS.contact")) contact <- contact*pctil[vary == "juv.rearS.contact"]
  pred <- -0.122; if(sum(vary == "juv.rearS.pred")) pred <- pred*pctil[vary == "juv.rearS.pred"]
  pdiv <- -3.51*b.pro.div; if(sum(vary == "juv.rearS.pdiv")) pdiv <- pdiv*pctil[vary == "juv.rearS.pdiv"]
  tdiv <- -0.0021*b.tot.div; if(sum(vary == "juv.rearS.tdiv")) tdiv <- tdiv*pctil[vary == "juv.rearS.tdiv"]
  strnd <- -1.939; if(sum(vary == "juv.rearS.strnd")) strnd <- strnd*pctil[vary == "juv.rearS.strnd"]
  hi.tmp <- 0.0001; if(sum(vary == "juv.rearS.hi.tmp")) hi.tmp <- hi.tmp*pctil[vary == "juv.rearS.hi.tmp"]
  medium<- 1.48; large<-2.223
  
  if(sum(vary == "juv.rearS.size")){ medium <- medium*pctil[vary == "juv.rearS.size"]
  large <- large*pctil[vary == "juv.rearS.size"]}
  
  s<-inv.logit(B0 + av.tmp*aveT20 + pred*high.pred + contact*no.con.pts*high.pred +
                 pdiv*prop.div + tdiv*tot.div + strnd*strand)*(1-maxT25) + maxT25*hi.tmp
  
  m<- inv.logit(B0 + medium + av.tmp*aveT20 + pred*high.pred + contact*no.con.pts*high.pred +
                  pdiv*prop.div + tdiv*tot.div + strnd*strand)*(1-maxT25) + maxT25*hi.tmp
  
  l<- inv.logit(B0 + large + av.tmp*aveT20 + pred*high.pred + contact*no.con.pts*high.pred +
                  pdiv*prop.div + tdiv*tot.div + strnd*strand)*(1-maxT25) + maxT25*hi.tmp
  
  t(rbind(s,m,l,1))
}



###########################################################################
##     Juvenile survival functions floodplains monthly                 ####
###########################################################################
## All inputs same as in channel except 
## Number of weeks inundated wks.fld
## Default is 2 week FP inundation then fish in channel

Juv.FP.S <- function(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand, wks.fld=rep(2,31),int,vary,pctil){
  
  inch<-ifelse(wks.fld>0,(4-wks.fld)/4,1)
  inwk<-1-inch
  
  B0<- int#-0.75
  b.contact<-0.0358
  b.pro.div<-0.05
  b.tot.div<-0.215
  
  if(sum(vary == "juv.rearS.int")) B0<- B0*pctil[vary == "juv.rearS.int"]
  av.tmp<- -0.717; if(sum(vary == "juv.rearS.av.tmp")) av.tmp<- av.tmp*pctil[vary == "juv.rearS.av.tmp"]
  contact<- -0.189*b.contact; if(sum(vary == "juv.rearS.contact")) contact <- contact*pctil[vary == "juv.rearS.contact"]
  pred <- -0.122; if(sum(vary == "juv.rearS.pred")) pred <- pred*pctil[vary == "juv.rearS.pred"]
  pdiv <- -3.51*b.pro.div; if(sum(vary == "juv.rearS.pdiv")) pdiv <- pdiv*pctil[vary == "juv.rearS.pdiv"]
  tdiv <- -0.0021*b.tot.div; if(sum(vary == "juv.rearS.tdiv")) tdiv <- tdiv*pctil[vary == "juv.rearS.tdiv"]
  strnd <- -1.939; if(sum(vary == "juv.rearS.strnd")) strnd <- strnd*pctil[vary == "juv.rearS.strnd"]
  hi.tmp <- 0.0001; if(sum(vary == "juv.rearS.hi.tmp")) hi.tmp <- hi.tmp*pctil[vary == "juv.rearS.hi.tmp"]
  medium<- 1.48; large<-2.223
  
  if(sum(vary == "juv.rearS.size")){ medium <- medium*pctil[vary == "juv.rearS.size"]
  large <- large*pctil[vary == "juv.rearS.size"]}
  
  fldhab <- 0.47; if(sum(vary == "juv.rearS.fldhab")) fldhab <- fldhab*pctil[vary == "juv.rearS.fldhab"]
  
  s1<-(inv.logit(B0 + av.tmp*aveT20 + pred*high.pred + contact*no.con.pts*high.pred +
                   pdiv*prop.div + tdiv*tot.div + strnd*strand)*(1-maxT25) + maxT25*hi.tmp)^inch
  
  m1<- (inv.logit(B0 + medium + av.tmp*aveT20 + pred*high.pred + contact*no.con.pts*high.pred +
                    pdiv*prop.div + tdiv*tot.div + strnd*strand)*(1-maxT25) + maxT25*hi.tmp)^inch
  
  l1<- (inv.logit(B0 + large + av.tmp*aveT20 + pred*high.pred + contact*no.con.pts*high.pred +
                    pdiv*prop.div + tdiv*tot.div + strnd*strand)*(1-maxT25) + maxT25*hi.tmp)^inch
  
  s2<-(inv.logit(B0 + fldhab + av.tmp*aveT20 + pred*high.pred)*(1-maxT25) + maxT25*hi.tmp)^inwk
  
  m2<- (inv.logit(B0 + fldhab + medium + av.tmp*aveT20 + pred*high.pred)*(1-maxT25) + maxT25*hi.tmp)^inwk
  
  l2<- (inv.logit(B0 + fldhab + large + av.tmp*aveT20 + pred*high.pred)*(1-maxT25) + maxT25*hi.tmp)^inwk
  
  t(rbind(s1*s2,m1*m2,l1*l2,1))
}



################################################################################
###      Juvenile survival functions delta rearing,                  ###########
###      monthly same as in channel without stranding                ###########
################################################################################

Juv.DLT.S <- function(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div,int,vary,pctil){
  
  B0<-int#9.496763 # 4.9
  b.contact<-0.0358
  b.tot.div<-0.5
  
  if(sum(vary == "juv.dltaS.int")) B0<- B0*pctil[vary == "juv.dltaS.int"]
  av.tmp<- -0.717; if(sum(vary == "juv.dltaS.av.tmp")) av.tmp<- av.tmp*pctil[vary == "juv.dltaS.av.tmp"]
  contact<- -0.189*b.contact; if(sum(vary == "juv.dltaS.contact")) contact <- contact*pctil[vary == "juv.dltaS.contact"]
  pred <- -0.122; if(sum(vary == "juv.dltaS.pred")) pred <- pred*pctil[vary == "juv.dltaS.pred"]
  pdiv <- -3.51; if(sum(vary == "juv.dltaS.pdiv")) pdiv <- pdiv*pctil[vary == "juv.dltaS.pdiv"]
  tdiv <- -0.0021*b.tot.div; if(sum(vary == "juv.dltaS.tdiv")) tdiv <- tdiv*pctil[vary == "juv.dltaS.tdiv"]
  hi.tmp <- 0.0001; if(sum(vary == "juv.dltaS.hi.tmp")) hi.tmp <- hi.tmp*pctil[vary == "juv.dltaS.hi.tmp"]
  medium<- 1.48; large<-2.223
  
  if(sum(vary == "juv.dltaS.size")){ medium <- medium*pctil[vary == "juv.dltaS.size"]
  large <- large*pctil[vary == "juv.dltaS.size"]}
  
  s<-inv.logit(B0 + av.tmp*aveT20 + pred*high.pred + contact*no.con.pts*high.pred +
                 pdiv*prop.div + tdiv*tot.div)*(1-maxT25) + maxT25*hi.tmp
  
  m<- inv.logit(B0 + medium + av.tmp*aveT20 + pred*high.pred + contact*no.con.pts*high.pred +
                  pdiv*prop.div + tdiv*tot.div)*(1-maxT25) + maxT25*hi.tmp
  
  l<- inv.logit(B0 + large + av.tmp*aveT20 + pred*high.pred + contact*no.con.pts*high.pred +
                  pdiv*prop.div + tdiv*tot.div)*(1-maxT25) + maxT25*hi.tmp
  
  t(rbind(s,m,l,1))
}

# Juv.DLT.S(maxT25=c(0,1,0,0), aveT20=c(0,0,1,0), high.pred=c(0,0,0,1), 
#          no.con.pts=rep(10,4), prop.div=rep(0,4), tot.div=rep(0,4))

###########################################################################
##     Juvenile survival functions floodplains Bypasses                ####
###########################################################################
## All inputs same as floodplains 

Juv.BYP.S <- function(maxT25, aveT20, high.pred,int, vary, pctil){
  B0<- int; if(sum(vary == "juv.bypS.int")) B0<- B0*pctil[vary == "juv.bypS.int"]
  av.tmp<- -0.717; if(sum(vary == "juv.bypS.av.tmp")) av.tmp<- av.tmp*pctil[vary == "juv.bypS.av.tmp"]
  pred <- -0.122; if(sum(vary == "juv.bypS.pred")) pred <- pred*pctil[vary == "juv.bypS.pred"]
  hi.tmp <- 0.0001; if(sum(vary == "juv.bypS.hi.tmp")) hi.tmp <- hi.tmp*pctil[vary == "juv.bypS.hi.tmp"]
  medium<- 1.48; large<-2.223
  if(sum(vary == "juv.bypS.size")){ medium <- medium*pctil[vary == "juv.bypS.size"]
  large <- large*pctil[vary == "juv.bypS.size"]}
  
  fldhab <- 0.47; if(sum(vary == "juv.bypS.fldhab")) fldhab <- fldhab*pctil[vary == "juv.bypS.fldhab"]
  
  s<- inv.logit(B0 + fldhab + av.tmp*aveT20 + pred*high.pred)*(1-maxT25) + maxT25*hi.tmp
  
  m<- inv.logit(B0 + fldhab + medium + av.tmp*aveT20 + pred*high.pred)*(1-maxT25) + maxT25*hi.tmp
  
  l<- inv.logit(B0 + fldhab + large + av.tmp*aveT20 + pred*high.pred)*(1-maxT25) + maxT25*hi.tmp
  
  t(rbind(s,m,l,1))
}

# Juv.BYP.S(maxT25=c(0,1,0,0,0), aveT20=c(0,0,1,0,0), high.pred=c(0,0,0,1,0))

#################################################################################
###          Juvenile pulse flow movement, in channel habitat monthly   ##########
#################################################################################
# 10-day maximum temperature > 25oC during rearing: maxT25 1 if yes otherwise 0
# prop.pulse relative change in discharge from base value (Q_pulse - Q_base)/Q_base

Juv.PLS.M <- function(prop.pulse, vary, pctil){
  B0<- -7.70744; if(sum(vary == "juv.mvmt.int")) B0<- B0*pctil[vary == "juv.mvmt.int"]
  ppulse<- 0.26579; if(sum(vary == "juv.mvmt.ppulse")) ppulse<- ppulse*pctil[vary == "juv.mvmt.ppulse"]
  
  medium<- 1.66845; large<-0.5706; vlarge<- -4.305
  if(sum(vary == "juv.mvmt.size")){ medium <- medium*pctil[vary == "juv.mvmt.size"]
  large <- large*pctil[vary == "juv.mvmt.size"]; vlarge <- vlarge*pctil[vary == "juv.mvmt.size"]}
  
  med_puls<- -0.25477; lar_puls<- -0.44778; vlar_puls<- 0.329
  if(sum(vary == "juv.mvmt.size_pul")){ med_puls <- med_puls*pctil[vary == "juv.mvmt.size_pul"]
  lar_puls <- lar_puls*pctil[vary == "juv.mvmt.size_pul"]; vlar_puls <- vlar_puls*pctil[vary == "juv.mvmt.size_pul"]}
  
  s<-inv.logit(B0 + ppulse*prop.pulse)
  
  m<-inv.logit(B0 + ppulse*prop.pulse + medium + med_puls*prop.pulse)
  
  l<-inv.logit(B0 + ppulse*prop.pulse + large + lar_puls*prop.pulse)
  
  vl<-inv.logit(B0 + ppulse*prop.pulse + vlarge + vlar_puls*prop.pulse)
  

  t(rbind(s,m,l,vl))
}


Juv.SJ.mig.S<- function(int,vary,pctil){

  B0<- int; if(sum(vary == "juv.sjmigS.int")) B0<- B0*pctil[vary == "juv.sjmigS.int"]
  medium<- 1.48; large<-2.223
  if(sum(vary == "juv.sjmigS.size")){ medium <- medium*pctil[vary == "juv.sjmigS.size"]
  large <- large*pctil[vary == "juv.sjmigS.size"]}
  
  s<- inv.logit(B0)
  m<- inv.logit(B0 + medium)
  l<- inv.logit(B0 + large)
  vl<- inv.logit(B0 + large)
  
  t(rbind(s,m,l,vl))
}


Juv.OUTM.S <- function(Q.cms, aveT,tot.div,prop.div,int1,int2,vary,pctil){
  
  b.pro.div<-0.05
  b.tot.div<-0.215
  
  B0<- int1; if(sum(vary == "juv.sacmigS.int1")) B0<- B0*pctil[vary == "juv.sacmigS.int1"]
  B1<- int2; if(sum(vary == "juv.sacmigS.int2")) B1<- B1*pctil[vary == "juv.sacmigS.int2"]
  av.tmp<- 0.554; if(sum(vary == "juv.sacmigS.av.tmp")) av.tmp<- av.tmp*pctil[vary == "juv.sacmigS.av.tmp"]
  pdiv <- -3.51*b.pro.div; if(sum(vary == "juv.sacmigS.pdiv")) pdiv <- pdiv*pctil[vary == "juv.sacmigS.pdiv"]
  tdiv <- -0.0021*b.tot.div; if(sum(vary == "juv.sacmigS.tdiv")) tdiv <- tdiv*pctil[vary == "juv.sacmigS.tdiv"]
  flow <- 0.0092; if(sum(vary == "juv.sacmigS.flow")) flow <- flow*pctil[vary == "juv.sacmigS.flow"]
  medium<- 1.48; large<-2.223
  if(sum(vary == "juv.sacmigS.size")){ medium <- medium*pctil[vary == "juv.sacmigS.size"]
  large <- large*pctil[vary == "juv.sacmigS.size"]}
  
  modwt <- 0.5; if(sum(vary == "juv.sacmigS.modwt")) modwt <- modwt*pctil[vary == "juv.sacmigS.modwt"]
  
  s<- inv.logit(B0 + flow*Q.cms + pdiv*prop.div + tdiv*tot.div)*modwt + inv.logit(B1 + av.tmp*aveT + pdiv*prop.div + tdiv*tot.div)*(1-modwt)
  
  m<- inv.logit(B0+ medium + flow*Q.cms + pdiv*prop.div + tdiv*tot.div)*modwt + inv.logit(B1+ medium + av.tmp*aveT + pdiv*prop.div + tdiv*tot.div)*(1-modwt)
  
  l<- vl<- inv.logit(B0+ large + flow*Q.cms + pdiv*prop.div + tdiv*tot.div)*modwt + inv.logit(B1+ large + av.tmp*aveT + pdiv*prop.div + tdiv*tot.div)*(1-modwt)
  
  t(rbind(s,m,l,vl))
}

#medQ[medQ$year==2011,]

#Juv.OUTM.S(Q.cms=1010, aveT=5,tot.div=0,prop.div=0)^0.5

# -Delta_flow: NDOI flow (CMS) collected from dayflow estimates, the OUT variable, daily estimate
# -Delta_divers: Percent delta exports (%) collected from dayflow estimates, the DIVER variable, daily estimate
# -Delta_temp: temperature (C) collected from the Rio Vista gauge, summarized as daily median
# -Delta_turb: an index consisting of the product of suspended sediment and flow at the Freeport gauge, daily estimates
# -Delta_pred: predator prevalence (CPUE) from the IEP Mast report, yearly cumulative CPUE. This is the one variable that is summarized per year, rather than per day like all the others. I have included it at the request of the SIT team, but I don't believe it's entirely appropriate in this analysis.

JuvD.OUTM.S <- function(Q.cms, pctdiv, aveT,int1,int2,int3,vary,pctil){
  
  B0<- int1; if(sum(vary == "juv.deltmigS.int1")) B0<- B0*pctil[vary == "juv.deltmigS.int1"]
  B1<- int2; if(sum(vary == "juv.deltmigS.int2")) B1<- B1*pctil[vary == "juv.deltmigS.int2"]
  B2<- int3; if(sum(vary == "juv.deltmigS.int3")) B2<- B2*pctil[vary == "juv.deltmigS.int3"]
  av.tmp<- 0.386; if(sum(vary == "juv.deltmigS.av.tmp")) av.tmp<- av.tmp*pctil[vary == "juv.deltmigS.av.tmp"]
  pdiv <- -0.033; if(sum(vary == "juv.deltmigS.pdiv")) pdiv <- pdiv*pctil[vary == "juv.deltmigS.pdiv"]
  flow <- 0.0013; if(sum(vary == "juv.deltmigS.flow")) flow <- flow*pctil[vary == "juv.deltmigS.flow"]
  medium<- 1.48; large<-2.223
  if(sum(vary == "juv.deltmigS.size")){ medium <- medium*pctil[vary == "juv.deltmigS.size"]
  large <- large*pctil[vary == "juv.deltmigS.size"]}
  
  modwt<-rep(0.333,3)
  if(sum(vary == "juv.deltmigS.modwt")){ 
    if(pctil[vary == "juv.deltmigS.modwt"] > 1) modwt<-c(0.5,0,0.5)
    if(pctil[vary == "juv.deltmigS.modwt"] > 1.2) modwt<-c(0.5,0.5,0)
    if(pctil[vary == "juv.deltmigS.modwt"] > 1.4) modwt<-c(1,0,0)
    if(pctil[vary == "juv.deltmigS.modwt"] == 1) modwt<-rep(1/3,3)
    if(pctil[vary == "juv.deltmigS.modwt"] < 1) modwt<-c(0,1,0)
    if(pctil[vary == "juv.deltmigS.modwt"] < 0.8) modwt<-c(0,0.5,0.5)
    if(pctil[vary == "juv.deltmigS.modwt"] < 0.6) modwt<-c(0,0,1)
  }
  
  s<-inv.logit(B0 + flow*Q.cms)*modwt[1] + inv.logit(B1 + av.tmp*aveT)*modwt[2] + 
    inv.logit(B2 + pdiv*pctdiv)*modwt[3]
  
  m<-inv.logit(B0+ medium + flow*Q.cms)*modwt[1] + inv.logit(B1+ medium + av.tmp*aveT)*modwt[2] + 
    inv.logit(B2+ medium + pdiv*pctdiv)*modwt[3] 
  
  vl<-l<-inv.logit(B0+ large + flow*Q.cms)*modwt[1] + inv.logit(B1+ large + av.tmp*aveT)*modwt[2] + 
    inv.logit(B2+ large + pdiv*pctdiv)*modwt[3] 
  
  t(rbind(s,m,l,vl))
}


