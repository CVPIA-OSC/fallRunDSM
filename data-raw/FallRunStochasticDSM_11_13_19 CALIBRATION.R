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
##                                                                           ##
##                                                                           ##
###############################################################################


###################################################
# BEGIN POPULATION FUNCTION
##################################################
run.scenarios<-function(){
  stochastic=0
  scenario=0
  vary="Nothing"
  pctil=1
  
  grow.ic<-0.5; if(sum(vary == "juv.grow.ic")) grow.ic<-grow.ic*pctil[vary == "juv.grow.ic"]
  grow.fp<-1.06; if(sum(vary == "juv.grow.fp")) grow.fp<-grow.fp*pctil[vary == "juv.grow.fp"]
  territory_size<-c(0.05423379,0.14539419,0.48471474,0.00000000); if(sum(vary == "juv.terr.size")) territory_size<-territory_size*pctil[vary == "juv.terr.size"]
    
  spwn.hab.deg=states$spwnDecay; if(sum(vary == "spawn.hab.decay")) states$spwnDecay<-pmin(states$spwnDecay*pctil[vary == "spawn.hab.decay"],1)
  rear.hab.deg=states$rearDecay; if(sum(vary == "rear.hab.decay")) states$rearDecay<-pmin(states$rearDecay*pctil[vary == "rear.hab.decay"],1)
  
  
  vect<-c(3.5000000 , 1.5000000, -2.5000000, -2.9000000, -1.1092908,
          -3.5000000,  3.5000000, -3.5000000,  2.5000000, -1.2000000,
           1.9999999, -0.2000000, -0.1081707, -3.4999959, -0.4000000, 
          -3.5000000,  1.4000000, -3.5000000,  2.5000000,  0.3000000,
          -3.5000000,  0.3000000, -3.5000000,  1.2000000, -0.5108849, 
          -3.3233638, -3.2304288, -3.4148335, -3.5000000, -3.5000000,
          -1.3083410, -1.9841364,  2.5000007, -3.5000000, -3.0000000, -0.9000000)
  
  vect2<-c(2.0000000, 0.5059781, 1.6702959, 0.8441507, 1.6434544, 2.0000000, 0.5000000,
          1.0815585, 1.9624035, 0.6232790, 1.0783194, 1.9318056, 1.2704583, 0.9537940,
          0.9066874, 2.0000000, 1.0847540, 1.4589099, 2.0000000, 0.5769185, 1.0589013,
          0.5709694, 2.0000000, 0.6716419, 0.5237730, 1.8253104, 1.0990632, 2.0000000,
          1.4615010, 1.1809537, 0.9577044, 0.9697722, 1.1437721, 1.7819260)
  
  surv.adj<-rep(1,31)
  surv.adj[c(2,4,5,8,9,11,14,13,15,18)]<-0.025
  surv.adj[c(8)]<-0.50
  surv.adj[c(9)]<-0.25
  
  IChab.spawn[1,,]<-IChab.spawn[1,,]*vect2[1] # Upper Sac
  IChab.spawn[6,,]<-IChab.spawn[6,,]*vect2[2] #Butte
  IChab.spawn[7,,]<-IChab.spawn[7,,]*vect2[3] #Clear
  IChab.spawn[10,,]<-IChab.spawn[10,,]*vect2[4] # Deer
  IChab.spawn[12,,]<-IChab.spawn[12,,]*vect2[5] # Mill
  IChab.spawn[19,,]<-IChab.spawn[19,,]*vect2[6] # Feather
  IChab.spawn[20,,]<-IChab.spawn[20,,]*vect2[7]# Yuba
  IChab.spawn[23,,]<-IChab.spawn[23,,]*vect2[8] # American
  IChab.spawn[26,,]<-IChab.spawn[26,,]*vect2[9] # Cosumness
  IChab.spawn[27,,]<-IChab.spawn[27,,]*vect2[10] # Mokelumne
  IChab.spawn[28,,]<-IChab.spawn[28,,]*vect2[11] # Merced
  IChab.spawn[29,,]<-IChab.spawn[29,,]*vect2[12] # Stanislaus
  IChab.spawn[30,,]<-IChab.spawn[30,,]*vect2[13] # Tuolumne
  
  IChab.fry[1,,]<-IChab.fry[1,,]*vect2[14] # Upper Sac
  IChab.fry[6,,]<-IChab.fry[6,,]*vect2[15] # Butte
  IChab.fry[7,,]<-IChab.fry[7,,]*vect2[16] # Clear
  IChab.fry[10,,]<-IChab.fry[10,,]*vect2[17] # Deer
  IChab.fry[12,,]<-IChab.fry[12,,]*vect2[18] # Mill
  IChab.fry[16,,]<-IChab.fry[16,,]*vect2[19] # Upper-mid Sac (corridor for above)
  # Sutter (corridor for above) is changed below
  IChab.fry[19,,]<-IChab.fry[19,,]*vect2[20] # Feather 
  IChab.fry[20,,]<-IChab.fry[20,,]*vect2[21] # Yuba
  IChab.fry[21,,]<-IChab.fry[21,,]*vect2[22] # Lower-mid Sac (corridor for above)
  # Yolo (corridor for above) is changed below
  IChab.fry[23,,]<-IChab.fry[23,,]*vect2[23] # American
  IChab.fry[24,,]<-IChab.fry[24,,]*vect2[24] # Lower Sac (corridor for above)
  IChab.fry[26,,]<-IChab.fry[26,,]*vect2[25] # Cosumness 
  IChab.fry[27,,]<-IChab.fry[27,,]*vect2[26] # Mokelumne 
  IChab.fry[28,,]<-IChab.fry[28,,]*vect2[27] # Merced
  IChab.fry[29,,]<-IChab.fry[29,,]*vect2[28] # Stanislaus 
  IChab.fry[30,,]<-IChab.fry[30,,]*vect2[29] # Tuolumne
  IChab.fry[31,,]<-IChab.fry[31,,]*vect2[30] # SJ (corridor for Merced, Stan, and Tuolumne)
  
  IChab.juv[1,,]<-IChab.juv[1,,]*vect2[14] # Upper Sac
  IChab.juv[6,,]<-IChab.juv[6,,]*vect2[15] # Butte
  IChab.juv[7,,]<-IChab.juv[7,,]*vect2[16] # Clear
  IChab.juv[10,,]<-IChab.juv[10,,]*vect2[17] # Deer
  IChab.juv[12,,]<-IChab.juv[12,,]*vect2[18] # Mill
  IChab.juv[16,,]<-IChab.juv[16,,]*vect2[19] # Upper-mid Sac (corridor for above)
  # Sutter (corridor for above) is changed below
  IChab.juv[19,,]<-IChab.juv[19,,]*vect2[20] # Feather 
  IChab.juv[20,,]<-IChab.juv[20,,]*vect2[21] # Yuba
  IChab.juv[21,,]<-IChab.juv[21,,]*vect2[22] # Lower-mid Sac (corridor for above)
  # Yolo (corridor for above) is changed below
  IChab.juv[23,,]<-IChab.juv[23,,]*vect2[23] # American
  IChab.juv[24,,]<-IChab.juv[24,,]*vect2[24] # Lower Sac (corridor for above)
  IChab.juv[26,,]<-IChab.juv[26,,]*vect2[25] # Cosumness 
  IChab.juv[27,,]<-IChab.juv[27,,]*vect2[26] # Mokelumne 
  IChab.juv[28,,]<-IChab.juv[28,,]*vect2[27] # Merced
  IChab.juv[29,,]<-IChab.juv[29,,]*vect2[28] # Stanislaus 
  IChab.juv[30,,]<-IChab.juv[30,,]*vect2[29] # Tuolumne
  IChab.juv[31,,]<-IChab.juv[31,,]*vect2[30] # SJ (corridor for Merced, Stan, and Tuolumne)
  IChab.sutter<-IChab.sutter*vect2[31]
  IChab.yolo<-IChab.yolo*vect2[32]
  DLThab[,,1]<-DLThab[,,1]*vect2[33]
  DLThab[,,2]<-DLThab[,,2]*vect2[34]
  
  spwners<-adultEscapeesKnown<-adultEscapeesSimulated<-adultTotalEscapees<-matrix(rep(0,31*30),ncol=30)
  selectedOptimalDecisions<-optimalDecisions<-matrix(rep(NA,31*20),ncol=20)
  juvenileBiomass<-matrix(rep(NA,31*25),ncol=25)
  survivalIncrease<-rep(0,31)
  proportionNatural<-matrix(rep(NA,31*25),ncol=25)

  
  #################################################
  # annual dynamics begins here 
  #################################################
  for(yr2 in 1:19){ 
    #  yr2 = 3
    yr<-yr2#ifelse(yr2>5,yr2-5,yr2) #use first 5 years to seed the model then run a full 20 years of decisions
    
    # Placeholders
    hatcheryFishByMonth<-spwnersBymonth<-nat.adultByMonth<-init.adultByMonth<-matrix(0,nrow=31,ncol=12)
    up.sac.fsh<-matrix(rep(0,15*4),ncol=4) # first 15 upper sac tribs
    sutter.fsh<-matrix(rep(0,17*4),ncol=4) # plus up-mid sac and sutter - rows 16 and 17
    yolo.fsh<-matrix(rep(0,20*4),ncol=4) # add 3 more rows for Bear, Feather, Yuba
    low.mid.sac.fsh<-matrix(rep(0,20*4),ncol=4) # lower Sac tribs
    low.sac.fsh<-matrix(rep(0,23*4),ncol=4) # add the American River row to lower Sac
    sj.fsh<-matrix(0,ncol=4,nrow=3) # SJ tribs
    juv.leav.shed<-juv.at.chips<-N.delt.fsh<-S.delt.fsh<-matrix(rep(0,31*4),ncol=4) 
    adults.in.ocean<-rep(0,31)
    
    #natural straying allocation
    cc.aloc<-c(rep(1,15),0,0,2,2,2,0,0,3,0,rep(0,7))/24
    oth.aloc<-c(rep(1,15),0,0,1,1,1,0,0,1,0,rep(1,6),0)/25
    
    # average ocean transition month
    T.mo<-(2*(1-stochastic))+(Trans.Mo()*stochastic)
    
    #use triangle distribution to distribute returning spawners (including hatchery fish) across 3 months (October-December)
    returnProps<-c(0.2222222,0.5555556,0.2222222)
    
    #select which month each fish returns
    for(sitex in 1:31){
      if(known.Adults.seed[sitex,yr+2]<=1000000000 & stochastic==1){
        randSpawnersBymonth<-as.vector(rmultinom(1,known.Adults.seed[sitex,yr+2],returnProps))
      } else{randSpawnersBymonth<-round(known.Adults.seed[sitex,yr+2]*returnProps)}
      spwnersBymonth[sitex,10:12]<-(randSpawnersBymonth*stochastic)+((known.Adults.seed[sitex,yr+2]*returnProps)*(1-stochastic))
      randSpawnersBymonth<-NULL
    }
    
    #allocate hatchery fish returning from ocean to watersheds
    totHatchReturn<-round(runif(1,83097.01,532203.1))
      if(totHatchReturn<=1000000000 & stochastic==1){
        randHatchReturns<-as.vector(rmultinom(1,totHatchReturn,inps$hatch.alloc))
      } else{randHatchReturns<-round(inps$hatch.alloc*totHatchReturn)}
    hatch.adult<-(randHatchReturns*stochastic)+(inps$hatch.alloc*mean(c(532203.05,83097.01,143209.3,188001.3))*(1-stochastic)) # mean number hatchery returns based on CWT reports
    totHatchReturn<-randHatchReturns<-NULL
    
    # use grandTab estimates for first 5 years to get the number of in-ocean adults, 
    for(mnth in 10:12){
      # mnth=10  
      # No strays, adult en route mortality, or hatchery additions since I am seeding the model 
      # with a known number of adult fish that made it to the spawning grounds
      nat.adultByMonth[,mnth]<-spwnersBymonth[,mnth]
      
      # remove natural adults for hatchery operations
      nat.adultByMonth[,mnth]<-rbin2Vectors(nat.adultByMonth[,mnth],(1-inps$prop.nat.remov),stochastic)
      
      # total adult fish on spawning grounds
      init.adultByMonth[,mnth]<-nat.adultByMonth[,mnth]
    }
    nat.adult<-apply(nat.adultByMonth,1,sum)
    init.adult<-apply(init.adultByMonth,1,sum)
    adultTotalEscapees[,yr2]<-init.adult
    adultEscapeesKnown[,yr2]<-nat.adult #This is what I will use to produce juveniles for the calibration
    prop.nat<-1-prop.hatch
    
    if(yr2>5){
      #select which month each fish returns
      for(sitex in 1:31){
        if(spwners[sitex,yr2]<=1000000000 & stochastic==1){
          randSpawnersBymonth<-as.vector(rmultinom(1,round(spwners[sitex,yr2]),returnProps))
        } else{randSpawnersBymonth<-round(spwners[sitex,yr2]*returnProps)}
        spwnersBymonth[sitex,10:12]<-(randSpawnersBymonth*stochastic)+((spwners[sitex,yr2]*returnProps)*(1-stochastic))
        randSpawnersBymonth<-NULL
      }
      
      for(sitex in 1:31){
        if(hatch.adult[sitex]<=1000000000 & stochastic==1){
          randHatchBymonth<-as.vector(rmultinom(1,round(hatch.adult[sitex]),returnProps))
        } else{randHatchBymonth<-round(hatch.adult[sitex]*returnProps)}
        hatcheryFishByMonth[sitex,10:12]<-(randHatchBymonth*stochastic)+((hatch.adult[sitex]*returnProps)*(1-stochastic))
        randHatchBymonth<-NULL
      }
      
      for(mnth in 10:12){
        # mnth=12  
        # number of wild fish that stray 
        prop.nat.stray<-Ad.Stray(wild=1,pctQnatl=retQ[,(1+yr)],SCDLT=inps$SCDELT,CrxChn=dlt.gates$days_closed[mnth],vary=vary,pctil=pctil)
        stray<-rbin2Vectors(spwnersBymonth[,mnth],prop.nat.stray,stochastic)
        
        if(max(stray)<=1000000000 & stochastic==1){
          randstray2<-as.vector(rmultinom(1,round(sum(stray*inps$SCDELT)),cc.aloc))
          randstray3<-as.vector(rmultinom(1,round(sum(stray*(1-inps$SCDELT))),oth.aloc))
        } else{
          randstray2<-round(sum(stray*inps$SCDELT)*cc.aloc)
          randstray3<-round(sum(stray*(1-inps$SCDELT))*oth.aloc)
        }
        nat.adultByMonth[,mnth]<-((spwnersBymonth[,mnth]-stray+randstray2+randstray3)*stochastic)+ 
          ((spwnersBymonth[,mnth]-stray+sum(stray*inps$SCDELT)*cc.aloc+sum(stray*(1-inps$SCDELT))*oth.aloc)*(1-stochastic)) 
        randstray2<-randstray3<-NULL
        
        # are tisdale or yolo bypasses overtopped?
        BPovrT<-gate.top[mnth,yr,1]*inps$TISD+gate.top[mnth,yr,2]*inps$YOLO
        BPovrT<-ifelse(BPovrT>0,1,0)
        
        # adult en route survival
        adult_en_route<-Adult.S(aveT23=ptemp20mc[,mnth+2],BPovrT,harvest=inps$A.HARV,vary=vary,pctil=pctil)
        
        # estimate adult fish that made it to spawning grounds and remove natural adults for hatchery operations
        nat.adultByMonth[,mnth]<-rbin2Vectors(rbin2Vectors(nat.adultByMonth[,mnth],adult_en_route,stochastic),(1-inps$prop.nat.remov),stochastic)
        hatcheryFishByMonth[,mnth]<-rbin2Vectors(hatcheryFishByMonth[,mnth],adult_en_route,stochastic)

        # total adult fish on spawning grounds
        init.adultByMonth[,mnth]<-nat.adultByMonth[,mnth]+hatcheryFishByMonth[,mnth]
      }
      nat.adult<-apply(nat.adultByMonth,1,sum)
      init.adult<-apply(init.adultByMonth,1,sum)
      adultTotalEscapees[,yr2]<-init.adult
      adultEscapeesSimulated[,yr2]<-nat.adult 
      prop.nat<-nat.adult/(init.adult+0.0001)
    }
    
    proportionNatural[,yr2]<-prop.nat
    # egg to fry survival
    eg2fr<-egg2fry(prop.nat=prop.nat,scour=inps$P.scour.nst,tmp.eff=egg.tmp.eff$mean_temp_effect,vary=vary,pctil=pctil)

    # used the minimum amount of habitat from October to December
    spawn_hab<-apply(IChab.spawn[,10:12,yr],1,min)
    
    # prespawn survival using the average number of degree days per site
    avg.DegDay<-(init.adultByMonth[,10]*(DegDay[,10,yr]+DegDay[,11,yr]+DegDay[,12,yr])+ #if you returned in October you get 3 months of degree days
                   init.adultByMonth[,11]*(DegDay[,11,yr]+DegDay[,12,yr])+              #if you returned in November you get 2 months of degree days
                   init.adultByMonth[,12]*(DegDay[,12,yr]))/(init.adult+0.0001)         #if you returned in December you get 1 month of degree days
    pre.spawn.S<-Adult.PSS(DegDay=avg.DegDay,vary=vary,pctil=pctil)
    
    # simulate spawning to get a number of newly hatched fry
    fry<-spawnfun(escapement=init.adult,
                  s_adult_inriver=pre.spawn.S,
                  sex_ratio=0.5,
                  spawn_hab=spawn_hab,
                  redd_size=9.29,
                  prob_scour=inps$P.scour.nst,
                  s_egg_to_fry=eg2fr,
                  fecund=5522,
                  stoch=stochastic,vary=vary,pctil=pctil)
    juvs<-fry
    
    floodPActivation<-matrix(0,ncol=12,nrow=31)
    if(stochastic==1){
      for(floodTrib in 1:31){
        if(floodPNew[floodTrib]>0){ # did you add FP habitat?
          if(rbinom(1,1,0.67)==1){ # is it activated this year?
            fld.mnth<-round(runif(1,0.51,3.49)) # what month is it activated this year?
            floodPActivation[floodTrib,c(fld.mnth,fld.mnth+1)]<-floodPNew[floodTrib]
          }
        }
      }
    }else{
      for(floodTrib in 1:31){
        if(floodPNew[floodTrib]>0){
          floodPActivation[floodTrib,c(2,3)]<-floodPNew[floodTrib]*0.67
        }
      }
    }
    
    #simulate monthly juvenile dynamics (January - August)
    for(mnth in 1:8){
      #mnth = 3
      # estimate growth
      growthMatrices<-Hab.growth(daily.rates=c(grow.ic,grow.fp),wks.fld=fp.weeks[,mnth,yr+1])
      trans_mat_river<-growthMatrices$T.mtx.ic
      trans_mat_flood<-growthMatrices$T.mtx.fp
      
      # set habitat amounts
      FP.hab<-floodP[,mnth,yr+1]+floodPActivation[,mnth]
      
      FP.sutter<-floodp.sutter[mnth,yr+1]
      FP.yolo<-floodp.yolo[mnth,yr+1]
      if(mnth<4) RI.hab<-IChab.fry[,mnth,yr+1] else RI.hab<-IChab.juv[,mnth,yr+1]
      
      RI.hab.sutter<-IChab.sutter[mnth,yr+1]
      RI.hab.yolo<-IChab.yolo[mnth,yr+1]
      
      NDLThab<-DLThab[mnth,yr+1,1]
      SDLThab<-DLThab[mnth,yr+1,2]
      
      # set proportion fish stranding
      if(mnth<4) ws.strand<-inps$P.strand.early else ws.strand<-inps$P.strand.late
      ws.strand<-(rbinom(31,1,ws.strand)*stochastic)+(ws.strand*(1-stochastic))
      
      # estimate probability leaving as function of pulse flow
      pulse.flozz<-prop.pulse[,mnth+1]/100              
      p.pulse.leave<-Juv.PLS.M(pulse.flozz,vary=vary,pctil=pctil)
      
      # monthly temperature
      aveT20<-(rbinom(31,1,inv.logit(-14.32252+0.72102*juv.tmp[,mnth,yr+1]))*stochastic)+(inv.logit(-14.32252+0.72102*juv.tmp[,mnth,yr+1])*(1-stochastic))
      maxT25<-(rbinom(31,1,inv.logit(-23.1766+1.4566*juv.tmp[,mnth,yr+1]))*stochastic)+(inv.logit(-23.1766+1.4566*juv.tmp[,mnth,yr+1])*(1-stochastic))
      aveT20D<-(rbinom(2,1,inv.logit(-18.30017+0.96991*juv.tmp.dlt[mnth,yr+1,]))*stochastic)+(inv.logit(-18.30017+0.96991*juv.tmp.dlt[mnth,yr+1,])*(1-stochastic))
      maxT25D<-(rbinom(2,1,inv.logit(-157.537+6.998*juv.tmp.dlt[mnth,yr+1,]))*stochastic)+(inv.logit(-157.537+6.998*juv.tmp.dlt[mnth,yr+1,])*(1-stochastic))
      
      # proportion and total water diverted 
      prop.div<-p.diver[,mnth,yr+1]
      tot.div<-t.diver[,mnth,yr+1]
      dlt.prop.div<-dlt.divers[mnth,yr+1,]
      dlt.tot.div<-dlt.divers.tot[mnth,yr+1,]
      
      # predator information
      high.pred<-(rbinom(31,1,inps$High.pred)*stochastic)+(inps$High.pred*(1-stochastic))
      C.points<-inps$contact
      D.points<-Dlt.inp$contct.pts
      Dlt.High.pred<-Dlt.inp$High.pred
      
      # estimate juvenile rearing survival
      #clear creek and upper-mid sac tribs that are missing data
      river_surv<-Juv.IC.S(maxT25=maxT25,aveT20=aveT20,high.pred=high.pred, 
                           no.con.pts=C.points,prop.div=prop.div,tot.div=tot.div,strand=ws.strand,vect[1],vary=vary,pctil=pctil)
      flood_surv<-Juv.FP.S(maxT25=maxT25,aveT20=aveT20,high.pred=high.pred,no.con.pts=C.points,
                           prop.div=prop.div,tot.div=tot.div,strand=ws.strand,wks.fld=fp.weeks[,mnth,yr+1],vect[1],vary=vary,pctil=pctil)
      #upper sac on its own
      river_surv[1,]<-Juv.IC.S(maxT25=maxT25[1],aveT20=aveT20[1],high.pred=high.pred[1], 
                               no.con.pts=C.points[1],prop.div=prop.div[1],tot.div=tot.div[1],strand=ws.strand[1],vect[2],vary=vary,pctil=pctil)
      flood_surv[1,]<-Juv.FP.S(maxT25=maxT25[1],aveT20=aveT20[1],high.pred=high.pred[1],no.con.pts=C.points[1],
                               prop.div=prop.div[1],tot.div=tot.div[1],strand=ws.strand[1],wks.fld=fp.weeks[1,mnth,yr+1],vect[2],vary=vary,pctil=pctil)
      #Butte Creek on its own
      river_surv[6,]<-Juv.IC.S(maxT25=maxT25[6],aveT20=aveT20[6],high.pred=high.pred[6], 
                               no.con.pts=C.points[6],prop.div=prop.div[6],tot.div=tot.div[6],strand=ws.strand[6],vect[3],vary=vary,pctil=pctil)
      flood_surv[6,]<-Juv.FP.S(maxT25=maxT25[6],aveT20=aveT20[6],high.pred=high.pred[6],no.con.pts=C.points[6],
                               prop.div=prop.div[6],tot.div=tot.div[6],strand=ws.strand[6],wks.fld=fp.weeks[6,mnth,yr+1],vect[3],vary=vary,pctil=pctil)
      #Deer Creek on its own
      river_surv[10,]<-Juv.IC.S(maxT25=maxT25[10],aveT20=aveT20[10],high.pred=high.pred[10], 
                                no.con.pts=C.points[10],prop.div=prop.div[10],tot.div=tot.div[10],strand=ws.strand[10],vect[4],vary=vary,pctil=pctil)
      flood_surv[10,]<-Juv.FP.S(maxT25=maxT25[10],aveT20=aveT20[10],high.pred=high.pred[10],no.con.pts=C.points[10],
                                prop.div=prop.div[10],tot.div=tot.div[10],strand=ws.strand[10],wks.fld=fp.weeks[10,mnth,yr+1],vect[4],vary=vary,pctil=pctil)
      #Mill Creek on its own
      river_surv[12,]<-Juv.IC.S(maxT25=maxT25[12],aveT20=aveT20[12],high.pred=high.pred[12], 
                                no.con.pts=C.points[12],prop.div=prop.div[12],tot.div=tot.div[12],strand=ws.strand[12],vect[5],vary=vary,pctil=pctil)
      flood_surv[12,]<-Juv.FP.S(maxT25=maxT25[12],aveT20=aveT20[12],high.pred=high.pred[12],no.con.pts=C.points[12],
                                prop.div=prop.div[12],tot.div=tot.div[12],strand=ws.strand[12],wks.fld=fp.weeks[12,mnth,yr+1],vect[5],vary=vary,pctil=pctil)
      
      #sections of the Sac River
      for(ii in c(16,21,24)){ 
        river_surv[ii,]<-Juv.IC.S(maxT25=maxT25[ii],aveT20=aveT20[ii],high.pred=high.pred[ii], 
                                  no.con.pts=C.points[ii],prop.div=prop.div[ii],tot.div=tot.div[ii],strand=ws.strand[ii],vect[6],vary=vary,pctil=pctil)
        flood_surv[ii,]<-Juv.FP.S(maxT25=maxT25[ii],aveT20=aveT20[ii],high.pred=high.pred[ii],no.con.pts=C.points[ii],
                                  prop.div=prop.div[ii],tot.div=tot.div[ii],strand=ws.strand[ii],wks.fld=fp.weeks[ii,mnth,yr+1],vect[6],vary=vary,pctil=pctil)
        
      }
      #middle sac tribs
      for(ii in c(18,19)){ 
        river_surv[ii,]<-Juv.IC.S(maxT25=maxT25[ii],aveT20=aveT20[ii],high.pred=high.pred[ii], 
                                  no.con.pts=C.points[ii],prop.div=prop.div[ii],tot.div=tot.div[ii],strand=ws.strand[ii],vect[7],vary=vary,pctil=pctil)
        flood_surv[ii,]<-Juv.FP.S(maxT25=maxT25[ii],aveT20=aveT20[ii],high.pred=high.pred[ii],no.con.pts=C.points[ii],
                                  prop.div=prop.div[ii],tot.div=tot.div[ii],strand=ws.strand[ii],wks.fld=fp.weeks[ii,mnth,yr+1],vect[7],vary=vary,pctil=pctil)
        
      }
      river_surv[20,]<-Juv.IC.S(maxT25=maxT25[20],aveT20=aveT20[20],high.pred=high.pred[20], 
                                no.con.pts=C.points[20],prop.div=prop.div[20],tot.div=tot.div[20],strand=ws.strand[20],vect[8],vary=vary,pctil=pctil)
      flood_surv[20,]<-Juv.FP.S(maxT25=maxT25[20],aveT20=aveT20[20],high.pred=high.pred[20],no.con.pts=C.points[20],
                                prop.div=prop.div[20],tot.div=tot.div[20],strand=ws.strand[20],wks.fld=fp.weeks[20,mnth,yr+1],vect[8],vary=vary,pctil=pctil)
      
      #American
      river_surv[23,]<-Juv.IC.S(maxT25=maxT25[23],aveT20=aveT20[23],high.pred=high.pred[23], 
                                no.con.pts=C.points[23],prop.div=prop.div[23],tot.div=tot.div[23],strand=ws.strand[23],vect[9],vary=vary,pctil=pctil)
      flood_surv[23,]<-Juv.FP.S(maxT25=maxT25[23],aveT20=aveT20[23],high.pred=high.pred[23],no.con.pts=C.points[23],
                                prop.div=prop.div[23],tot.div=tot.div[23],strand=ws.strand[23],wks.fld=fp.weeks[23,mnth,yr+1],vect[9],vary=vary,pctil=pctil)
      
      #delta tribs
      for(ii in 25:26){ 
        river_surv[ii,]<-Juv.IC.S(maxT25=maxT25[ii],aveT20=aveT20[ii],high.pred=high.pred[ii], 
                                  no.con.pts=C.points[ii],prop.div=prop.div[ii],tot.div=tot.div[ii],strand=ws.strand[ii],vect[10],vary=vary,pctil=pctil)
        flood_surv[ii,]<-Juv.FP.S(maxT25=maxT25[ii],aveT20=aveT20[ii],high.pred=high.pred[ii],no.con.pts=C.points[ii],
                                  prop.div=prop.div[ii],tot.div=tot.div[ii],strand=ws.strand[ii],wks.fld=fp.weeks[ii,mnth,yr+1],vect[10],vary=vary,pctil=pctil)
        
      }
      river_surv[27,]<-Juv.IC.S(maxT25=maxT25[27],aveT20=aveT20[27],high.pred=high.pred[27], 
                                no.con.pts=C.points[27],prop.div=prop.div[27],tot.div=tot.div[27],strand=ws.strand[27],vect[11],vary=vary,pctil=pctil)
      flood_surv[27,]<-Juv.FP.S(maxT25=maxT25[27],aveT20=aveT20[27],high.pred=high.pred[27],no.con.pts=C.points[27],
                                prop.div=prop.div[27],tot.div=tot.div[27],strand=ws.strand[27],wks.fld=fp.weeks[27,mnth,yr+1],vect[11],vary=vary,pctil=pctil)
      
      #SJ tribs
      river_surv[28,]<-Juv.IC.S(maxT25=maxT25[28],aveT20=aveT20[28],high.pred=high.pred[28], 
                                no.con.pts=C.points[28],prop.div=prop.div[28],tot.div=tot.div[28],strand=ws.strand[28],vect[12],vary=vary,pctil=pctil)
      flood_surv[28,]<-Juv.FP.S(maxT25=maxT25[28],aveT20=aveT20[28],high.pred=high.pred[28],no.con.pts=C.points[28],
                                prop.div=prop.div[28],tot.div=tot.div[28],strand=ws.strand[28],wks.fld=fp.weeks[28,mnth,yr+1],vect[12],vary=vary,pctil=pctil)
      river_surv[29,]<-Juv.IC.S(maxT25=maxT25[29],aveT20=aveT20[29],high.pred=high.pred[29], 
                                no.con.pts=C.points[29],prop.div=prop.div[29],tot.div=tot.div[29],strand=ws.strand[29],vect[13],vary=vary,pctil=pctil)
      flood_surv[29,]<-Juv.FP.S(maxT25=maxT25[29],aveT20=aveT20[29],high.pred=high.pred[29],no.con.pts=C.points[29],
                                prop.div=prop.div[29],tot.div=tot.div[29],strand=ws.strand[29],wks.fld=fp.weeks[29,mnth,yr+1],vect[13],vary=vary,pctil=pctil)
      river_surv[30,]<-Juv.IC.S(maxT25=maxT25[30],aveT20=aveT20[30],high.pred=high.pred[30], 
                                no.con.pts=C.points[30],prop.div=prop.div[30],tot.div=tot.div[30],strand=ws.strand[30],vect[14],vary=vary,pctil=pctil)
      flood_surv[30,]<-Juv.FP.S(maxT25=maxT25[30],aveT20=aveT20[30],high.pred=high.pred[30],no.con.pts=C.points[30],
                                prop.div=prop.div[30],tot.div=tot.div[30],strand=ws.strand[30],wks.fld=fp.weeks[30,mnth,yr+1],vect[14],vary=vary,pctil=pctil)
      
      #SJ main
      river_surv[31,]<-Juv.IC.S(maxT25=maxT25[31],aveT20=aveT20[31],high.pred=high.pred[31], 
                                no.con.pts=C.points[31],prop.div=prop.div[31],tot.div=tot.div[31],strand=ws.strand[31],vect[15],vary=vary,pctil=pctil)
      flood_surv[31,]<-Juv.FP.S(maxT25=maxT25[31],aveT20=aveT20[31],high.pred=high.pred[31],no.con.pts=C.points[31],
                                prop.div=prop.div[31],tot.div=tot.div[31],strand=ws.strand[31],wks.fld=fp.weeks[31,mnth,yr+1],vect[15],vary=vary,pctil=pctil)
      
      river_surv<-river_surv*(matrix(c(rep(surv.adj,3),rep(1,length(surv.adj))),byrow = FALSE,ncol=4))
      flood_surv<-flood_surv*(matrix(c(rep(surv.adj,3),rep(1,length(surv.adj))),byrow = FALSE,ncol=4))
      
      river_surv<-river_surv+cbind(survivalIncrease,survivalIncrease,survivalIncrease,survivalIncrease)
      flood_surv<-flood_surv+cbind(survivalIncrease,survivalIncrease,survivalIncrease,survivalIncrease)
      river_surv<-ifelse(river_surv>1,1,river_surv)
      flood_surv<-ifelse(flood_surv>1,1,flood_surv)
      
      BP_surv<-Juv.BYP.S(maxT25=maxT25[22],aveT20=aveT20[22],high.pred=0,vect[16],vary=vary,pctil=pctil)
      Sut.S<-Yolo.S<-Juv.BYP.S(maxT25=maxT25[22],aveT20=aveT20[22],high.pred=0,vect[16],vary=vary,pctil=pctil)^0.5
      
      
      D_juv_surv<-Juv.DLT.S(maxT25=maxT25D,aveT20=aveT20D,high.pred=Dlt.High.pred, 
                            no.con.pts=D.points,prop.div=dlt.prop.div,tot.div=dlt.tot.div,vect[17],vary=vary,pctil=pctil)
      
      # estimate juvenile migratory survival
      newDsurv<-DeltaS(DCC_open=dlt.gates$days_closed[mnth],hor_barr=0,Q_free[mnth,yr+1],Q_vern[mnth,yr+1],
                       Q_stck[mnth,yr+1],Temp_vern[mnth,yr+1],Temp_pp[mnth,yr+1],CVP_exp[mnth,yr+1],SWP_exp[mnth,yr+1],Trap_trans,vary=vary,pctil=pctil)
      SacFlo<-upSacQ[mnth,yr+1]
      SJ.S<-Juv.SJ.mig.S(vect[18],vary=vary,pctil=pctil)
      UM.Sac.S<-Juv.OUTM.S(Q.cms=SacFlo,prop.div=prop.div[16],tot.div=tot.div[16],aveT=juv.tmp[16,mnth,yr+1],vect[19],vect[20],vary=vary,pctil=pctil)^0.5
      LM.Sac.S<-Juv.OUTM.S(Q.cms=SacFlo,prop.div=prop.div[21],tot.div=tot.div[21],aveT=juv.tmp[21,mnth,yr+1],vect[19],vect[20],vary=vary,pctil=pctil)^0.5
      LL.Sac.S<-Juv.OUTM.S(Q.cms=SacFlo,prop.div=prop.div[24],tot.div=tot.div[24],aveT=juv.tmp[24,mnth,yr+1],vect[19],vect[20],vary=vary,pctil=pctil)^0.5
      
      Sac.Delt.S<-JuvD.OUTM.S(Q.cms=Dlt.inf[mnth,yr+1,],pctdiv=dlt.prop.div*100,aveT=juv.tmp.dlt[mnth,yr+1,],vect[21],vect[22],vect[23],vary=vary,pctil=pctil)
      Bay.S<-mean(c(0.43,0.46,0.26,0.25,0.39)) # Chipps island to bay 
      
      #simulate fish moving through the system
      # UPPER SACRAMENTO WATERSHEDS
      if(mnth<8){ 
        USc.sheds<-fill.trib.func(juvs=juvs[1:15,],flood_hab=FP.hab[1:15],juv_hab=RI.hab[1:15],territory_size,
                                  stoch=stochastic)
        left<-rbin2Matrices(USc.sheds$river,p.pulse.leave[1:15,],stochastic)
        USc.sheds$river<-(USc.sheds$river-left)*stochastic+(USc.sheds$river*(1-p.pulse.leave[1:15,]))*(1-stochastic)
        USc.sheds$migr<-USc.sheds$migr+left; left<-NULL
      } else USc.sheds<-list(migr=juvs[1:15,])
      
      # keep track of fish leaving each watershed for SIT metric
      juv.leav.shed[1:15,]<-juv.leav.shed[1:15,]+USc.sheds$migr
      
      # migratory
      surv.mig<-USc.sheds$migr
      
      # MIDDLE SACRAMENTO AND SUTTER BP
      # add migrants to existing mid sac mainstem fish
      up.sac.fsh<-up.sac.fsh+surv.mig
      
      # detour fish to sutter and add sutter detoured fish to existing sutter fish
      detoured.fish<-rbinMatObject(up.sac.fsh,prop.Q.bypasses[mnth,yr+1,1],stochastic)
      sutter.fsh<-sutter.fsh+rbind(detoured.fish,matrix(0,ncol=4,nrow=2))  
      # remove detoured fish
      up.sac.fsh<-(up.sac.fsh-detoured.fish)*stochastic+(up.sac.fsh*(1-prop.Q.bypasses[mnth,yr+1,1]))*(1-stochastic); detoured.fish<-NULL
      
      # fill up sutter habitats no effect of pulsed flows here
      if(mnth<8){
        sutter.fsh<-fill.Umain.func(juvs=sutter.fsh,flood_hab=FP.sutter,juv_hab=RI.hab.sutter,territory_size,
                                    stoch=stochastic)
      } else sutter.fsh<-list(migr=sutter.fsh)
      
      # fill up upper mainstem, but in river fish can leave due to pulses
      if(mnth<8) {
        up.sac.fsh<-fill.Umain.func(juvs=up.sac.fsh,flood_hab=FP.hab[16],juv_hab=RI.hab[16],territory_size,
                                    stoch=stochastic)
        # pulsed flows
        left<-rbinMatVector(up.sac.fsh$Umain.river,p.pulse.leave[16,],stochastic)
        up.sac.fsh$Umain.river<-(up.sac.fsh$Umain.river-left)*stochastic+t((1-p.pulse.leave[16,])*t(up.sac.fsh$Umain.river))*(1-stochastic)
        up.sac.fsh$migr<-up.sac.fsh$migr+left; left<-NULL; 
      } else up.sac.fsh<-list(migr=up.sac.fsh)
      
      # fish leaving Sutter and Mid-Sacramento (survival migratory placeholder)
      surv.mig<-(rbind(rbin2MatSpec(up.sac.fsh$migr,UM.Sac.S,stochastic),matrix(0,ncol=4,nrow=2))+rbin2MatSpec(sutter.fsh$migr,Sut.S,stochastic))*stochastic+
                (rbind(up.sac.fsh$migr%z%UM.Sac.S,matrix(0,ncol=4,nrow=2))+sutter.fsh$migr%z%Sut.S)*(1-stochastic)

      # LOWER-MID SACRAMENTO WATERSHEDS
      # fill upstream watersheds first and send fish out if size 4 or not enough habitat
      if(mnth<8){ 
        LSc.sheds<-fill.trib.func(juvs=juvs[18:20,],flood_hab=FP.hab[18:20],juv_hab=RI.hab[18:20],territory_size,
                                  stoch=stochastic)
        left<-rbin2Matrices(LSc.sheds$river,p.pulse.leave[18:20,],stochastic)
        LSc.sheds$river<-(LSc.sheds$river-left)*stochastic+(LSc.sheds$river*(1-p.pulse.leave[18:20,]))*(1-stochastic)
        LSc.sheds$migr<-LSc.sheds$migr+left; left<-NULL; 
      } else LSc.sheds<-list(migr=juvs[18:20,])
      juv.leav.shed[18:20,]<-juv.leav.shed[18:20,]+LSc.sheds$migr
      
      # add lower mid sac trab migrants to migrants from upper sac tribs
      surv.mig<-rbind(surv.mig,LSc.sheds$migr)
      
      # migrating fish added to existing lower mid sacamento fish 
      low.mid.sac.fsh<-low.mid.sac.fsh+surv.mig
      
      # detour fish to Yolo and add Yolo detoured fish to existing Yolo fish 
      detoured.fish<-rbinMatObject(low.mid.sac.fsh,prop.Q.bypasses[mnth,yr+1,5],stochastic)
      yolo.fsh<-yolo.fsh+detoured.fish
      
      # remove detoured fish
      low.mid.sac.fsh<-(low.mid.sac.fsh-detoured.fish)*(stochastic)+(low.mid.sac.fsh*(1-prop.Q.bypasses[mnth,yr+1,5]))*(1-stochastic); detoured.fish<-NULL
      
      # fill up yolo habitats no effect of pulsed flows here
      if(mnth<8){
        yolo.fsh<-fill.Umain.func(juvs=yolo.fsh,flood_hab=FP.yolo,juv_hab=RI.hab.yolo,territory_size,
                                  stoch=stochastic)
      } else yolo.fsh<-list(migr=yolo.fsh)
      
      # assign LMSF fish to open habitat, in none leave or pulse flow in river leave. 
      if(mnth<8){ 
        low.mid.sac.fsh<-fill.Lmain.func(juvs=low.mid.sac.fsh,flood_hab=FP.hab[21],juv_hab=RI.hab[21],territory_size,
                                         stoch=stochastic) 
        left<-rbinMatVector(low.mid.sac.fsh$Lmain.river,p.pulse.leave[21,],stochastic)
        low.mid.sac.fsh$Lmain.river<-(low.mid.sac.fsh$Lmain.river-left)*stochastic+t((1-p.pulse.leave[21,])*t(low.mid.sac.fsh$Lmain.river))*(1-stochastic)
        low.mid.sac.fsh$migr<-low.mid.sac.fsh$migr+left; left<-NULL
      } else low.mid.sac.fsh<-list(migr=low.mid.sac.fsh)
      
      # Note these fish passed through and must pay survival cost through - survival migratory placeholder
      surv.mig<-rbind((rbin2MatSpec(low.mid.sac.fsh$migr,LM.Sac.S,stochastic)*stochastic+(low.mid.sac.fsh$migr%z%LM.Sac.S)*(1-stochastic)),matrix(0,ncol=4,nrow=3))

      # LOWER SACRAMENTO MAINSTEM - American River fish can use too
      # fill american R watershed first and send fish out if size 4 or not enough habitat
      if(mnth<8){ 
        AMR.sheds<-fill.AMR.func(juvs=juvs[23,],flood_hab=FP.hab[23],juv_hab=RI.hab[23],territory_size,
                                 stoch=stochastic)
        left<-rbinMatVector(AMR.sheds$river,p.pulse.leave[23,],stochastic)
        AMR.sheds$river<-(AMR.sheds$river-left)*stochastic+t((1-p.pulse.leave[23,])*t(AMR.sheds$river))*(1-stochastic)
        AMR.sheds$migr<-AMR.sheds$migr+left; left<-NULL; junk<-NULL
      } else AMR.sheds<-list(migr=juvs[23,])
      juv.leav.shed[23,]<-juv.leav.shed[23,]+AMR.sheds$migr
      
      # add lower mid sac trib migrants to migrants from upper sac tribs
      surv.mig[23,]<-surv.mig[23,]+AMR.sheds$migr
      
      # migrating fish added to existing lower sacamento fish 
      low.sac.fsh<-low.sac.fsh+surv.mig
      
      # assign LSF fish to open habitat, in none leave or pulse flow in river leave. 
      if(mnth<8){ 
        low.sac.fsh<-fill.Lmain.func(juvs=low.sac.fsh,flood_hab=FP.hab[24],juv_hab=RI.hab[24],territory_size,
                                     stoch=stochastic) 
        left<-rbinMatVector(low.sac.fsh$Lmain.river,p.pulse.leave[24,],stochastic)
        low.sac.fsh$Lmain.river<-(low.sac.fsh$Lmain.river-left)*stochastic+t((1-p.pulse.leave[24,])*t(low.sac.fsh$Lmain.river))*(1-stochastic)
        low.sac.fsh$migr<-low.sac.fsh$migr + left; left<-NULL
      } else low.sac.fsh<-list(migr=low.sac.fsh)
      
      # DELTA WATERSHEDS
      # fill upstream watersheds first and send fish out if size 4 or not enough habitat
      if(mnth<8){
        DL.sheds<-fill.trib.func(juvs=juvs[25:27,],flood_hab=FP.hab[25:27],juv_hab=RI.hab[25:27],territory_size,
                                 stoch=stochastic) 
        left<-rbin2Matrices(DL.sheds$river,p.pulse.leave[25:27,],stochastic)
        DL.sheds$river<-(DL.sheds$river-left)*stochastic+(DL.sheds$river*(1-p.pulse.leave[25:27,]))*(1-stochastic)
        DL.sheds$migr<-DL.sheds$migr+left; left<-NULL; junk<-NULL
      } else DL.sheds<-list(migr=juvs[25:27,])
      juv.leav.shed[25:27,]<-juv.leav.shed[25:27,]+DL.sheds$migr
      
      # SAN JOAQUIN WATERSHEDS
      if(mnth<8){
        SJ.sheds<-fill.trib.func(juvs=juvs[28:30,],flood_hab=FP.hab[28:30],juv_hab=RI.hab[28:30],territory_size,
                                 stoch=stochastic) 
        left<-rbin2Matrices(SJ.sheds$river,p.pulse.leave[28:30,],stochastic)
        SJ.sheds$river<-(SJ.sheds$river-left)*stochastic+(SJ.sheds$river*(1-p.pulse.leave[28:30,]))*(1-stochastic)
        SJ.sheds$migr<-SJ.sheds$migr+left; left<-NULL
      } else SJ.sheds<-list(migr=juvs[28:30,])
      juv.leav.shed[28:30,]<-juv.leav.shed[28:30,]+SJ.sheds$migr
      
      # add migrants to existing SJ mainstem fish
      sj.fsh<-sj.fsh+SJ.sheds$migr 
      
      # assign SJ fish to open habitat, in none leave or pulse flow in river leave. 
      if(mnth<8){
        sj.fsh<-fill.Lmain.func(juvs=sj.fsh,flood_hab=FP.hab[31],juv_hab=RI.hab[31],territory_size,
                                stoch=stochastic) 
        left<-rbinMatVector(sj.fsh$Lmain.river,p.pulse.leave[31,],stochastic)
        sj.fsh$Lmain.river<-(sj.fsh$Lmain.river-left)*stochastic+t((1-p.pulse.leave[31,])*t(sj.fsh$Lmain.river))*(1-stochastic)
        sj.fsh$migr<-sj.fsh$migr+left; left<-NULL; junk<-NULL
      } else sj.fsh<-list(migr=sj.fsh)
      
      # SEND FISH TO THE DELTA - note zeros for Low Sac rearing and SJ mainstem
      # route fish from the lower Sac to the south delta based on the flows at Freeport and Delta Cross Channel Gates
      survLowSacFish<-(rbin2MatSpec(low.sac.fsh$migr,LL.Sac.S,stochastic)*stochastic)+((low.sac.fsh$migr%z%LL.Sac.S)*(1-stochastic))
      
      prop.dlt.entrain<-sum(route.2.sDlt(freeportQ[mnth,yr+1]*35.3147,dlt.gates$days_closed[mnth],vary=vary,pctil=pctil)[3:4]) #*35.3147 converts cms to cfs for function
      
      # north delta first
      sac.not.entrained<-rbinMatObject(survLowSacFish,(1-prop.dlt.entrain),stochastic)
      N.delt.fsh[1:23,]<-N.delt.fsh[1:23,]+sac.not.entrained+rbind((((rbin2MatSpec(yolo.fsh$migr,Yolo.S,stochastic)*stochastic)+(yolo.fsh$migr%z%Yolo.S))*(1-stochastic)),matrix(0,ncol=4,nrow=3))
      
      if(mnth<8){
        N.delt.fsh<-fill.delt.func(juvs=N.delt.fsh,est_hab=NDLThab,territory_size,
                                   stoch=stochastic)
      } else N.delt.fsh<-list(out2ocean=N.delt.fsh)
      
      # south delta next
      S.delt.fsh[25:27,]<-S.delt.fsh[25:27,]+DL.sheds$migr
      S.delt.fsh[28:30,]<-S.delt.fsh[28:30,]+(rbin2MatSpec(sj.fsh$migr,SJ.S,stochastic)*stochastic)+((sj.fsh$migr%z%SJ.S)*(1-stochastic))
      S.delt.fsh<-S.delt.fsh+rbind((((survLowSacFish-sac.not.entrained)*stochastic)+((survLowSacFish*prop.dlt.entrain)*(1-stochastic))),matrix(0,ncol=4,nrow=8))
      sac.not.entrained<-NULL

      if(mnth<8){
        S.delt.fsh<-fill.delt.func(juvs=S.delt.fsh,est_hab=SDLThab,territory_size,
                                   stoch=stochastic)
      } else S.delt.fsh<-list(out2ocean=S.delt.fsh)

      # estimate fish at Golden Gate Bridge and Chipps Island
      ## new routing and survival for delta and SJ tribs
      holdSdelta<-array(0,dim=dim(S.delt.fsh$out2ocean))
      holdSdelta[1:24,]<-rbinMatVector(S.delt.fsh$out2ocean[1:24,],newDsurv[1,],stochastic)
      holdSdelta[26:27,]<-rbinMatVector(S.delt.fsh$out2ocean[26:27,],newDsurv[2,],stochastic)
      holdSdelta[25,]<-rbin2Vectors(S.delt.fsh$out2ocean[25,],newDsurv[3,],stochastic)
      holdSdelta[28:31,]<-rbinMatVector(S.delt.fsh$out2ocean[28:31,],newDsurv[4,],stochastic)
      migrants.out<-rbinMatVector(N.delt.fsh$out2ocean,Sac.Delt.S[1,],stochastic)
      migrants.at.GG<-rbinMatObject(migrants.out,Bay.S,stochastic)+rbinMatObject(holdSdelta,Bay.S,stochastic)
      juv.at.chips<-juv.at.chips+migrants.out+holdSdelta
      migrants.out<-holdSdelta<-NULL
      
      # JUVENILE GROWTH AND SURVIVAL    
      if(mnth<8){
        # upper sac tribs
        USc.sheds<-rearfunc(river_rear=USc.sheds$river,
                            flood_rear=USc.sheds$flood,
                            trans_mat_river=trans_mat_river,
                            trans_mat_flood=trans_mat_flood[,,1:15],
                            flood_surv=flood_surv[1:15,],
                            river_surv=river_surv[1:15,],
                            stoch=stochastic)
        juvs<-USc.sheds$riv.rear+USc.sheds$flood.rear
        
        # lower sac tribs
        LSc.sheds<-rearfunc(river_rear=LSc.sheds$river,
                            flood_rear=LSc.sheds$flood,
                            trans_mat_river=trans_mat_river,
                            trans_mat_flood=trans_mat_flood[,,18:20],
                            flood_surv=flood_surv[18:20,],
                            river_surv=river_surv[18:20,],
                            stoch=stochastic)
        juvs<-rbind(juvs,c(0,0,0,0),c(0,0,0,0),(LSc.sheds$riv.rear+LSc.sheds$flood.rear))
        
        # American River
        AMR.sheds<-rearfunc(river_rear=AMR.sheds$river,
                            flood_rear=AMR.sheds$flood,
                            trans_mat_river=trans_mat_river,
                            trans_mat_flood=trans_mat_flood[,,23],
                            flood_surv=flood_surv[23,],
                            river_surv=river_surv[23,],
                            stoch=stochastic)
        juvs<-rbind(juvs,c(0,0,0,0),c(0,0,0,0),(AMR.sheds$riv.rear+AMR.sheds$flood.rear))
        
        # delta tribs
        DL.sheds<-rearfunc(river_rear=DL.sheds$river,
                           flood_rear=DL.sheds$flood,
                           trans_mat_river=trans_mat_river,
                           trans_mat_flood=trans_mat_flood[,,25:27],
                           flood_surv=flood_surv[25:27,],
                           river_surv=river_surv[25:27,],
                           stoch=stochastic)
        juvs<-rbind(juvs,c(0,0,0,0),(DL.sheds$riv.rear+DL.sheds$flood.rear))
        
        # SJ tribs
        SJ.sheds<-rearfunc(river_rear=SJ.sheds$river,
                           flood_rear=SJ.sheds$flood,
                           trans_mat_river=trans_mat_river,
                           trans_mat_flood=trans_mat_flood[,,28:30],
                           flood_surv=flood_surv[28:30,],
                           river_surv=river_surv[28:30,],
                           stoch=stochastic)
        juvs<-rbind(juvs,(SJ.sheds$riv.rear+SJ.sheds$flood.rear),c(0,0,0,0))
        
        # upper sac rearing
        up.sac.fsh<-rearfunc(river_rear=up.sac.fsh$Umain.river,
                             flood_rear=up.sac.fsh$Umain.flood,
                             trans_mat_river=trans_mat_river,
                             trans_mat_flood=trans_mat_flood[,,16],
                             flood_surv=flood_surv[16,],
                             river_surv=river_surv[16,],
                             stoch=stochastic)
        up.sac.fsh<-up.sac.fsh$riv.rear+up.sac.fsh$flood.rear
        
        # lower mid sac rearing
        low.mid.sac.fsh<-rearfunc(river_rear=low.mid.sac.fsh$Lmain.river,
                                  flood_rear=low.mid.sac.fsh$Lmain.flood,
                                  trans_mat_river=trans_mat_river,
                                  trans_mat_flood=trans_mat_flood[,,21],
                                  flood_surv=flood_surv[21,],
                                  river_surv=river_surv[21,],
                                  stoch=stochastic)
        low.mid.sac.fsh<-low.mid.sac.fsh$riv.rear+low.mid.sac.fsh$flood.rear
        
        # lower sac rearing
        low.sac.fsh<-rearfunc(river_rear=low.sac.fsh$Lmain.river,
                              flood_rear=low.sac.fsh$Lmain.flood,
                              trans_mat_river=trans_mat_river,
                              trans_mat_flood=trans_mat_flood[,,24],
                              flood_surv=flood_surv[24,],
                              river_surv=river_surv[24,],
                              stoch=stochastic)
        low.sac.fsh<-low.sac.fsh$riv.rear+low.sac.fsh$flood.rear
        
        # SJ rearing
        sj.fsh<-rearfunc(river_rear=sj.fsh$Lmain.river,
                         flood_rear=sj.fsh$Lmain.flood,
                         trans_mat_river=trans_mat_river,
                         trans_mat_flood=trans_mat_flood[,,31],
                         flood_surv=flood_surv[31,],
                         river_surv=river_surv[31,],
                         stoch=stochastic)
        sj.fsh<-sj.fsh$riv.rear+sj.fsh$flood.rear
        
        # sutter BP rearing
        sutter.fsh<-rearfunc(river_rear=sutter.fsh$Umain.river,
                             flood_rear=sutter.fsh$Umain.flood,
                             trans_mat_river=trans_mat_river,
                             trans_mat_flood=trans_mat_flood[,,17],
                             flood_surv=BP_surv[1,],
                             river_surv=BP_surv[1,],
                             stoch=stochastic)
        sutter.fsh<-sutter.fsh$riv.rear+sutter.fsh$flood.rear
        
        # yolo BP rearing
        yolo.fsh<-rearfunc(river_rear=yolo.fsh$Umain.river,
                           flood_rear=yolo.fsh$Umain.flood,
                           trans_mat_river=trans_mat_river,
                           trans_mat_flood=trans_mat_flood[,,22],
                           flood_surv=BP_surv[1,],
                           river_surv=BP_surv[1,],
                           stoch=stochastic)
        yolo.fsh<-yolo.fsh$riv.rear+yolo.fsh$flood.rear
        
        # north delta rearing
        N.delt.fsh<-Delt.rearfunc(delt_juv=N.delt.fsh$Delt.rear,
                                  trans_mat_river=trans_mat_river,
                                  D_juv_surv=D_juv_surv[1,],
                                  stoch=stochastic)
        
        # south-central delta rearing
        S.delt.fsh<-Delt.rearfunc(delt_juv=S.delt.fsh$Delt.rear,
                                  trans_mat_river=trans_mat_river,
                                  D_juv_surv=D_juv_surv[2,],
                                  stoch=stochastic)
      }
      # OCEAN ENTRY SURVIVAL
      adults.in.ocean<-adults.in.ocean+Ocean.ent.surv2(migrants=migrants.at.GG,mnth,T.mo,mo.ocean.surv,
                                                       vect[24],vect[25],vect[26],vect[27],vect[28],vect[29],vect[30],
                                                       vect[31],vect[32],vect[33],vect[34],vect[35],vect[36],stoch=stochastic,vary=vary,pctil=pctil)
      
    }
    # allocate fish among returns
    #select which year each fish returns
    for(sitex in 1:31){
      if(adults.in.ocean[sitex]<=1000000000 & stochastic==1){
        randReturnSpawners<-as.vector(rmultinom(1,round(adults.in.ocean[sitex]),c(0.25,0.5,0.25)))
      } else{randReturnSpawners<-round(adults.in.ocean[sitex]*c(0.25,0.5,0.25))}
      spwners[sitex,(yr2+2):(yr2+4)]<-spwners[sitex,(yr2+2):(yr2+4)]+(randReturnSpawners*stochastic)+((adults.in.ocean[sitex]*c(0.25,0.5,0.25))*(1-stochastic))
      randReturnSpawners<-NULL
    }
    
    # bio<-c(0.5,1.8,9.1,31.4)
    # juvenileBiomass[,yr2]<-apply(t(t(juv.at.chips)*bio),1,sum) #keep track of juvenile biomass at Chipps
    # 
    #degrade habitat if no creation/maintenance action was taken this year in each trib
    # if(yr2>5 & yr2<25){
    #   for(trib in 1:31){
    #     if(states$regulated[trib]==1){
    #       if(states$grp[trib]<7){
    #         if(is.na(selectedOptimalDecisions[trib,yr])==TRUE){
    #           for(yrs in (yr+1):20){
    #             IChab.spawn[trib,,yrs]<-IChab.spawn[trib,,yrs]*(runif(1,(states$spwnDecay[trib]-(1-states$spwnDecay[trib])),1)*stochastic+(states$spwnDecay[trib]*(1-stochastic))) #degrade future spawning habitat
    #             IChab.fry[trib,,yrs+1]<-IChab.fry[trib,,yrs+1]*(runif(1,(states$rearDecay[trib]-(1-states$rearDecay[trib])),1)*stochastic+(states$rearDecay[trib]*(1-stochastic))) #degrade future rearing habitat
    #             IChab.juv[trib,,yrs+1]<-IChab.juv[trib,,yrs+1]*(runif(1,(states$rearDecay[trib]-(1-states$rearDecay[trib])),1)*stochastic+(states$rearDecay[trib]*(1-stochastic)))  #degrade future rearing habitat
    #           }
    #         }
    #         else if (selectedOptimalDecisions[trib,yr] != 2){
    #           for(yrs in (yr+1):20){
    #             IChab.spawn[trib,,yrs]<-IChab.spawn[trib,,yrs]*(runif(1,(states$spwnDecay[trib]-(1-states$spwnDecay[trib])),1)*stochastic+(states$spwnDecay[trib]*(1-stochastic))) #degrade future spawning habitat
    #           }
    #         }
    #         else if(selectedOptimalDecisions[trib,yr] != 3){
    #           for(yrs in (yr+1):20){
    #             IChab.fry[trib,,yrs+1]<-IChab.fry[trib,,yrs+1]*(runif(1,(states$rearDecay[trib]-(1-states$rearDecay[trib])),1)*stochastic+(states$rearDecay[trib]*(1-stochastic))) #degrade future rearing habitat
    #             IChab.juv[trib,,yrs+1]<-IChab.juv[trib,,yrs+1]*(runif(1,(states$rearDecay[trib]-(1-states$rearDecay[trib])),1)*stochastic+(states$rearDecay[trib]*(1-stochastic))) #degrade future rearing habitat
    #           }
    #         }
    #       }
    #       
    #       if(states$grp[trib]==7){
    #         if(is.na(selectedOptimalDecisions[trib,yr])==TRUE | selectedOptimalDecisions[trib,yr] == 4){
    #           for(yrs in (yr+1):20){
    #             IChab.fry[trib,,yrs+1]<-IChab.fry[trib,,yrs+1]*(runif(1,(states$rearDecay[trib]-(1-states$rearDecay[trib])),1)*stochastic+(states$rearDecay[trib]*(1-stochastic))) #degrade future rearing habitat
    #             IChab.juv[trib,,yrs+1]<-IChab.juv[trib,,yrs+1]*(runif(1,(states$rearDecay[trib]-(1-states$rearDecay[trib])),1)*stochastic+(states$rearDecay[trib]*(1-stochastic))) #degrade future rearing habitat
    #           }
    #         }
    #       }
    #     }
    #     
    #     
    #   }
    # }
  }
  # 
  # 
  # 
  # #SIT Metrics
  # 
  # all.spawners<-data.frame(known.Adults$watershed,round(adultTotalEscapees[,6:25]))
  # names(all.spawners)<-c("watershed","year6","year7","year8","year9","year10","year11","year12","year13","year14",
  #                        "year15","year16","year17","year18",'year19',"year20",
  #                        "year21","year22","year23",'year24',"year25")
  # 
  # proportionNatural<-data.frame(known.Adults$watershed,round(proportionNatural[,6:25],3))
  # names(proportionNatural)<-c("watershed","year6","year7","year8","year9","year10","year11","year12","year13","year14",
  #                             "year15","year16","year17","year18",'year19',"year20",
  #                             "year21","year22","year23",'year24',"year25")
  # 
  # nat.spawners<-data.frame(known.Adults$watershed,round(adultEscapees[,6:25]))
  # names(nat.spawners)<-c("watershed","year6","year7","year8","year9","year10","year11","year12","year13","year14",
  #                        "year15","year16","year17","year18",'year19',"year20",
  #                        "year21","year22","year23",'year24',"year25")
  # 
  # juvenileBiomass.Chipps<-data.frame(known.Adults$watershed,round(juvenileBiomass[,6:25]))
  # names(juvenileBiomass.Chipps)<-c("watershed","year6","year7","year8","year9","year10","year11","year12","year13","year14",
  #                                  "year15","year16","year17","year18",'year19',"year20",
  #                                  "year21","year22","year23",'year24',"year25")
  # 
  # lambda<-matrix(NA,ncol=20,nrow=31)
  # for(mm in 1:20){
  #   lambda[,mm]<-adultTotalEscapees[,4+mm]/(adultTotalEscapees[,5+mm]+1)
  # }
  # lambda2<-ifelse(lambda>=1,1,0)
  # hatch<-ifelse(proportionNatural[,2:21]>=0.9,1,0)
  # abundance<-ifelse(all.spawners[,2:21]>=833,1,0)
  # viable<-ifelse(lambda2+hatch+abundance==3,1,0)
  # viable<-data.frame(states,viable)
  # viabilityMetric1<-matrix(NA,nrow=4,ncol=20)
  # for(nn in 1:4){
  #   this<-viable[which(viable$DiversityGroup==nn),(ncol(viable)-19):ncol(viable)]
  #   viabilityMetric1[nn,]<-apply(this,2,sum)
  # }
  # # viabilityMetric<-data.frame(1:4,viabilityMetric1)
  # # names(viabilityMetric)<-c("diversityGroup","year6","year7","year8","year9","year10","year11","year12","year13","year14",
  # #                                  "year15","year16","year17","year18",'year19',"year20",
  # #                                  "year21","year22","year23",'year24',"year25")
  # 
  # # Actions<-data.frame(known.Adults$watershed,selectedOptimalDecisions,states$DiversityGroup)
  # # names(Actions)<-c("watershed","year6","year7","year8","year9","year10","year11","year12","year13","year14",
  # #                   "year15","year16","year17","year18",'year19',"year20",
  # #                   "year21","year22","year23",'year24',"year25","DiversityGroup")
  # # 
  # # Lambda<-data.frame(known.Adults$watershed,lambda,states$DiversityGroup)
  # # names(Lambda)<-c("watershed","year6","year7","year8","year9","year10","year11","year12","year13","year14",
  # #                   "year15","year16","year17","year18",'year19',"year20",
  # #                   "year21","year22","year23",'year24',"year25","DiversityGroup")
  # 
  # 
  # tada<-list(allspawners=adultTotalEscapees[,6:25],
  #            nat.spawners=adultEscapees[,6:25],
  #            juvenileBiomass.Chipps=round(juvenileBiomass[,6:25],2),
  #            viabilityMetric=viabilityMetric1,
  #            Actions=selectedOptimalDecisions)
  # return(tada)
  
  par(mfrow=c(1,1))
  
  # minimize difference in observed vs. predicted spawners
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-1 #upper sac
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor1<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Upper Sac. River, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-6 #butte
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada3<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor3<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Butte Creek, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-7 #clear
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada4<-sum((knwn.trib.adults-est.trib.adults)^2) # CV based on sum of squared errors
  cor4<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Clear Creek, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-10 #deer
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada5<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor5<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Deer Creek, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-12 #mill
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada6<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor6<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Mill Creek, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-19 #feather
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada7<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor7<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Feather River, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-20 #yuba
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada8<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor8<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Yuba River, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-23 #American
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada9<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor9<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("American River, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-26 #cosumnes
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada10<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor10<-cor(est.trib.adults,knwn.trib.adults)
  
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Cosumnes River, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-27 #mokeulumne
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada11<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor11<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Mokelumne River, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  
  
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-28 #merced
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada12<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor12<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Merced River, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-29 #stanislaus
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada13<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor13<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Stanislaus River, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-30 #tuolumne
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove data points that were filled in
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch[trib])
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  # each tributary
  tada14<-sum((knwn.trib.adults-est.trib.adults)^2)#/mean(knwn.trib.adults) # CV based on sum of squared errors
  cor14<-cor(est.trib.adults,knwn.trib.adults)
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("Tuolumne River, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  
  
  est.trib.adults<-knwn.trib.adults<-knwn.trib.adults.NAs<-c()
  for(kk in 6:19){
    # kk=6
    trib<-c(1,6,7,10,12,19,20,
            23,26:30) # all tribs
    knwn.t.ad<-adultEscapeesKnown[trib,kk]
    knwn.t.ad<-knwn.t.ad*(1-prop.hatch[trib])
    knwn.t.ad.NAs<-known.Adults2[trib,kk+2]
    est.t.ad<-adultEscapeesSimulated[trib,kk]
    
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    knwn.trib.adults.NAs<-c(knwn.trib.adults.NAs,knwn.t.ad.NAs)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
    
  }
  want<-which(is.na(knwn.trib.adults.NAs)==FALSE) #remove NAs
  est.trib.adults<-est.trib.adults[want]
  knwn.trib.adults<-knwn.trib.adults[want]
  lwr.bound<-100
  upr.bound<-max(knwn.trib.adults)+100
  want<-which(knwn.trib.adults>lwr.bound & knwn.trib.adults<upr.bound)#get subset values to calibrate to
  knwn.trib.adults<-knwn.trib.adults[want]
  est.trib.adults<-est.trib.adults[want]
  
  if(plott.me=="YES"){
    x<-y<-seq(0,upr.bound,100)
    this<-round(cor(est.trib.adults,knwn.trib.adults),3)
    print(plot(y~x,type="l",xlab="Known adults",ylab="Predicted adults",main=paste("All tribs, cor = ",this),xlim=c(0,upr.bound),ylim=c(0,max(c(upr.bound,max(est.trib.adults))))))
    print(points(est.trib.adults~knwn.trib.adults,las= 1))
  }
  
  total.cor<-cor(est.trib.adults,knwn.trib.adults)
  cor.total<-ifelse(total.cor<0,500000000,0)
  cor.all<-c(cor1,cor3,cor4,cor5,cor6,cor7,cor8,cor9,cor10,cor11,cor12,cor13,cor14)
  cor.all<-sum(ifelse(cor.all<0,rep(50000000,13),rep(0,13)))
  
  
  total.sse<-tada+tada3+tada4+tada5+tada6+tada7+tada8+tada9+tada10+tada11+tada12+tada13+tada14+cor.all+cor.total
  
  # #Compare spawner abundances
  # tada<-data.frame(known.Adults$watershed,round(adultEscapeesSimulated[,6:20]))
  # names(tada)<-c("watershed","year6","year7","year8","year9","year10","year11","year12","year13","year14",
  #                "year15","year16","year17","year18",'year19',"year20")
  
  
  return(total.sse)
  
  

}



setwd("C:/Users/duartead/Box Sync/CVPIA/Phase II/FY2017_2018/State depend decisions/Fall Run Stochastic DSM")
library(cvpiaCalibration)
# library(cvpiaData)
library(data.table)
calib_data<-readRDS("delta-calibration-1980_2017.rds")

# READ IN FUNCTIONS AND DATA
fun.place<-paste(getwd(),"/functions",sep="")
functs<-dir(path=fun.place,full.names=FALSE,pattern=".R",recursive=FALSE)
for(ia in 1:length(functs)) source(paste(fun.place,functs[ia],sep="/"))

# all_inputs<-cvpiaData::load_baseline_data('fall')
all_inputs<-cvpiaCalibration::fall_inputs #for calibration (1998-2017)
list2env(all_inputs,.GlobalEnv)

ptemp20mc<-p.tempMC20

known.Adults.seed<-read.csv("filledknownAdults_1998_2016.csv") #Fall run GrandTab data
known.Adults.seed<-known.Adults.seed[order(known.Adults.seed$order),]

known.Adults2<-read.csv("knownAdults_1998_2016.csv") #Fall run GrandTab data
known.Adults2<-known.Adults2[order(known.Adults2$order),]

prop.hatch<-read.csv("HatcheryProp_FallRun.csv") #proportion hatchery for each stream
prop.hatch<-prop.hatch$prop.Hatch

maxHabitat<-read.csv("theoretical_max_area.csv")
table(maxHabitat$species)
maxHabitat<-maxHabitat[which(maxHabitat$species=="fr"),]
maxHabitat<-maxHabitat[-1*(c(which(maxHabitat$watershed=="North Delta"),which(maxHabitat$watershed=="South Delta"))),]
maxSpwnHab<-maxHabitat[which(maxHabitat$lifestage=="spawning"),c(1,4)]
maxSpwnHab<-merge(known.Adults2[,1:2],maxSpwnHab,all=TRUE)
maxSpwnHab<-maxSpwnHab[order(maxSpwnHab$order),]
maxRearHab<-maxHabitat[which(maxHabitat$lifestage=="rearing"),c(1,4)]
maxRearHab<-merge(known.Adults2[,1:2],maxRearHab,all=TRUE)
maxRearHab<-maxRearHab[order(maxRearHab$order),]

#calculate total habitat for sutter and yolo
IChab.sutter<-IChab.bypass[1,,]+IChab.bypass[2,,]+IChab.bypass[3,,]+IChab.bypass[4,,]
IChab.yolo<-IChab.bypass[5,,]+IChab.bypass[6,,]
floodp.sutter<-floodp.bypass[1,,]+floodp.bypass[2,,]+floodp.bypass[3,,]+floodp.bypass[4,,]
floodp.yolo<-floodp.bypass[5,,]+floodp.bypass[6,,]

Q_free<-calib_data$Q_free[,19:38]   
Q_vern<-calib_data$Q_vern[,19:38]   
Q_stck<-calib_data$Q_stck[,19:38]      
Temp_vern<-calib_data$Temp_vern[,19:38]    
# Temp_vern<-fix.tmp(Temp_vern)
Temp_pp<-calib_data$Temp_pp[,19:38]   
# Temp_pp<-fix.tmp(Temp_pp)   
CVP_exp<-calib_data$CVP_exp[,19:38]      
SWP_exp<-calib_data$SWP_exp[,19:38]    
Trap_trans <-0

aveT201<-aveT20
aveT20D1<-aveT20D

#fill in NAs with 0s
p.diver[is.na(p.diver)]<-0 
t.diver[is.na(t.diver)]<-0 
DegDay[is.na(DegDay)]<-0 
retQ[is.na(retQ)]<-0 
p.tempMC20[is.na(p.tempMC20)]<-0 
prop.pulse[is.na(prop.pulse)]<-0 
IChab.spawn[is.na(IChab.spawn)]<-0 
IChab.fry[is.na(IChab.fry)]<-0 
IChab.juv[is.na(IChab.juv)]<-0 
floodP[is.na(floodP)]<-0

Q_free[is.na(Q_free)]<-0
Q_vern[is.na(Q_vern)]<-0
Q_stck[is.na(Q_stck)]<-0
Temp_vern[is.na(Temp_vern)]<-0  
Temp_pp[is.na(Temp_pp)]<-0
CVP_exp[is.na(CVP_exp)]<-0
SWP_exp[is.na(SWP_exp)]<-0

Goals<-read.csv("Doubling goals.csv")

regulated<-c(1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,1,1,1,0,1,1,1,0,1,1,1,1,1)

states<-data.frame(known.Adults2$watershed,known.Adults2$order,regulated)
colnames(states)<-c("watershed","order","regulated")
groups<-read.csv("Grouping.csv")
states<-merge(states,groups)
states<-states[order(states$order),]
states$grp<-ifelse(states$grp==9,7,groups$grp) #all mainstem sections are now group 7
states$DiversityGroup[c(18,19,20,23,26,27)]<-5 #split diversity group
states$DiversityGroup<-ifelse(states$grp==7,6,states$DiversityGroup) #all mainstem sections are now diversity group 6

states$spwnDecay<-ifelse(states$grp==1,0.973647181,ifelse(states$grp==2,0.994949237,ifelse(states$grp==3,0.994949237,
                                                                                           ifelse(states$grp==4,0.979148362,ifelse(states$grp==5,0.962256359,0.989793782)))))
states$rearDecay<-ifelse(states$grp==1,0.987175243,ifelse(states$grp==2,0.997487405,ifelse(states$grp==3,0.997487405,
                                                                                           ifelse(states$grp==4,0.989793782,ifelse(states$grp==5,0.981853233,0.994949237)))))

hatcheryStream<-data.frame(known.Adults2$watershed,c(rep(0,2),1,rep(0,15),1,rep(0,3),1,rep(0,3),1,1,rep(0,3)))
colnames(hatcheryStream)<-c("watershed","hatchery")

floodPNew<-rep(0,31)

oldEst<-c(3.50000000,  0.48940401, -1.53858125, -1.85978054, -0.84925303,
          -3.50000000,  3.50000000, -3.50000000,  3.50000000, -2.21525050,
          1.04407821,  0.79798627, -0.11105801, -3.01589854,  0.57965531,
          -3.50000000,  2.43083427, -3.33003650,  3.49997015, -0.65391464,
          -3.50000000,  1.30929259, -3.50000000,  0.18697141, -1.27938990,
          -3.49996592, -3.50000000, -3.49999567, -3.50000000, -3.50000000,
          -1.85967633, -2.79302904,  3.45854461, -3.49966246, -2.04369227,
          0.08985323)

# newoldEst<-c(3.5000000 , 1.5000000, -2.5000000, -2.9000000, -1.1092908,
#           -3.5000000,  3.5000000, -3.5000000,  2.5000000, -1.2000000,
#          1.9999999, -0.2000000, -0.1081707, -3.4999959, -0.4000000, 
#          -3.5000000,  1.4000000, -3.5000000,  2.5000000,  0.3000000,
#          -3.5000000,  0.3000000, -3.5000000,  1.2000000, -0.5108849, 
#          -3.3233638, -3.2304288, -3.4148335, -3.5000000, -3.5000000,
#         -1.3083410, -1.9841364,  2.5000007, -3.5000000, -3.0000000, -0.9000000)

plott.me<-"YES"
baser<-run.scenarios()

#use this to calibrate intercepts for survival
minz<-round(pmax(oldEst-1,-3.5),1)#rep(-3.5,36)
maxz<-round(pmin(oldEst+1,3.5),1)#rep(3.5,36)


#use this for scaling habitat
minz<-rep(0.5,34)
maxz<-rep(2,34)
oldEst<-rep(1,34)



require(rgenoud)

min.max<-cbind(minz,maxz) ## lower and upper bound of intercept,slope
plott.me<-"NO"
ga<- genoud(
  fn = run.scenarios,        ### name of function
  nvars = 34,                 ### number of parameters to estimate
  max = FALSE,               ### find the minimum
  pop.size = 1000,           ### population size (number of searchers)
  starting.values=oldEst,    ### initial values of intercept,slope
  Domains = min.max,         ### matrix of boundary values created above
  boundary.enforcement = 2)  ### specifies stay within upper and lower bound
plott.me<-"YES"
baser<-run.scenarios(vect2=ga$par)
ga$par
#intercepts again



