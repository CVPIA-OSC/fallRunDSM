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

inv.logit<-function(eta){1/(1+exp(-eta))}

###############################################################################
## Delta survival and routing function for Chinook Salmon                    ##
###############################################################################

# Inputs to routing and survival function
# DCC_open: Delta crosschannel gate open (1) or closed (0)
# hor_barr: Head of old river physical barrier in place (1) or not (0)
# Q_free: average daily discharge at Freeport cms
# Q_vern: average daily discharge at Vernalis cms
# Q_stck: average daily discharge at Stockton cms
# Temp_vern: average daily temperature at Vernalis C
# Temp_pp: average daily temperature SJR at Prisoners Point C
# CVP_exp: average daily exports CVP cms
# SWP_exp: average daily exports SWP cms
# Trap_trans: proportion of smolts trapped at Vernalis and transported 
# to Chips island
# output is proportion of fish from the Sacramento at Feeeport (NorFish)
# Moke and Cosumnes (CosMokFish), Calaveras (CalFish) and San Joaquin
# tributaries (from Vernalis) arriving alive at Chipps Island in four size groups
#  35-42mm,42-72mm,72-110mm, >110mm Note that the models were fit to 
# data that were >80 mm. Therefore, this does not predict outside
# of the data so sizes <= 80mm are assumed to me 80mm long as
# requested by Russ Perry

DeltaS<-function(DCC_open,hor_barr,Q_free,Q_vern,
                 Q_stck,Temp_vern,Temp_pp,CVP_exp,SWP_exp,Trap_trans, vary, pctil){
  
  DCC_open<-min(DCC_open,30) #treat each month as 30 days
  DCC_open<-30-DCC_open
  DCC_open<-DCC_open/30 #calculate the proportion DCC is open that month
  # cutoffs for number of CVP pumps operating
  cutz<-c(37.5,60,95.6,NA,500)
  no.pump<- 1
  if(CVP_exp>60) no.pump<- 2
  if(CVP_exp>95.6) no.pump<- 3
  if(CVP_exp>499) no.pump <-5
  
  #### First estimate North Delta parameters
  
  #Entrained into sutter/steamboat
  psi_steam<- 0.36241455*inv.logit(2.014670488 + 2.458233791*((Q_free-610.1)/814.2))
  
  # remain in Sacramento
  psi_sac1<- 1- psi_steam
  
  # entrained DCC
  psi_dcc<-inv.logit((-1.515076654 - 1.282849232*((Q_free-610.1)/814.2) + 0.030214424*DCC_open))*DCC_open + (1-DCC_open)*inv.logit(-10)
  
  # entrained georgiana slough
  psi_geo<-(1-psi_dcc)*(0.2669 + (1-0.2669)*inv.logit(-3.111 - 0.9443*DCC_open - 3.1743*((Q_free-610.1)/814.2)))
  
  # remain in Sacramento
  psi_sac2<- 1- psi_dcc - psi_geo
  
  #size cutoffs 42,72,110, use min from study as smallest
  FL<-c(81,81,81,140)
  #Survival Sac Freeport to Sutter/Steamboat junction
  S_1<-inv.logit(3.243+ 0.3225*DCC_open + 1.1049*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Sac Sutter/Steamboat junction to Georgiana
  S_2<-inv.logit(3.243 + 0.0673*DCC_open + 1.1049*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Sutter/Steamboat Slough
  S_3<-inv.logit(1.2095 + 0.1508*DCC_open + 2.2758*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Sac Georgiana Junction to Rio Vista
  S_4<-inv.logit(2.533 - 0.7343*DCC_open + 2.5756*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Georgiana Slough
  S_5<-inv.logit(1.1175 - 0.0769*DCC_open + 2.1591*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival DCC to Moke
  S_6<-inv.logit(0.03667 - 0.2541*DCC_open + 1.1510*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Sac  Rio Vista to Chipps Island
  S_7<-inv.logit(1.0934 - 0.4816*DCC_open + 0.0379*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  # Survival interior Delta
  S_8<-inv.logit(-0.46002 -0.12312*DCC_open + 0.03898*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #### Next estimate South Delta parameters
  # Probability of remaining in SJR at HOR
  psi_sjr1<-inv.logit(-0.75908 + 1.72020*hor_barr + 0.00361*Q_vern + 0.02718*hor_barr*Q_vern)
  
  # Probability of entering old river
  psi_OR <-1-psi_sjr1
  
  #Probability of remaining in SJR at Turner Cut
  psi_sjr2<-inv.logit(5.83131 - 0.037708993*Q_stck)
  
  # probability of entering Turner cut
  psi_TC<-1-psi_sjr2
  
  #Probability of entrainment at CVP (Karp et al 2017) logit link
  psi_CVP<-inv.logit(-3.9435+ 2.9025*no.pump -0.3771*no.pump^2)
  
  #Probability of entrainment at SWP
  psi_SWP<-(1-psi_CVP)*inv.logit(-1.48969+ 0.016459209*SWP_exp)
  
  # Probability of remaining old river north
  psi_ORN<- 1- psi_CVP - psi_SWP
  
  #Survival Tributaries to HOR logit link
  S_prea<-inv.logit(5.77500 + 0.00706*Q_vern - 0.32810*Temp_vern+ 0.152*(FL-155.1)/21.6)
  
  #Survival HOR to Turner Cut logit link
  S_a<-inv.logit(-2.90330+ 0.01059*Q_vern+ 0.152*(FL-155.1)/21.6)
  
  #Survival SJR Turner Cut to Chipps
  S_bc<-inv.logit(13.41840 - 0.90070*Temp_pp+ 0.152*(FL-155.1)/21.6)
  
  #Survival down OR HOR to CVP
  S_d<-inv.logit(2.16030 -0.20500*Temp_vern+ 0.152*(FL-155.1)/21.6)
  
  #Survival ORN to Chipps Island (SJRGA)
  S_efc<-0.01
  
  #Survival through CVP (Karp et al 2017) logit link
  S_CVP<-inv.logit(-3.0771 + 1.8561*no.pump - 0.2284*no.pump^2)
  
  #Survival through SWP (Gingras 1997)
  S_SWP<-0.1325
  
  
  if(sum(vary=="psi_steam.X")) psi_steam <- psi_steam*pctil[vary=="psi_steam.X"]
  if(sum(vary=="psi_dcc.X")) psi_dcc <- psi_dcc*pctil[vary=="psi_dcc.X"]
  if(sum(vary=="psi_geo.X")) psi_geo <- psi_geo*pctil[vary=="psi_geo.X"]
  if(sum(vary=="S_1.X")) S_1 <- S_1*pctil[vary=="S_1.X"]
  if(sum(vary=="S_2.X")) S_2 <- S_2*pctil[vary=="S_2.X"]
  if(sum(vary=="S_3.X")) S_3 <- S_3*pctil[vary=="S_3.X"]
  if(sum(vary=="S_4.X")) S_4 <- S_4*pctil[vary=="S_4.X"]
  if(sum(vary=="S_5.X")) S_5 <- S_5*pctil[vary=="S_5.X"]
  if(sum(vary=="S_6.X")) S_6 <- S_6*pctil[vary=="S_6.X"]
  if(sum(vary=="S_7.X")) S_7 <- S_7*pctil[vary=="S_7.X"]
  if(sum(vary=="S_8.X")) S_8 <- S_8*pctil[vary=="S_8.X"]
  if(sum(vary=="psi_sjr1.X")) psi_sjr1 <- psi_sjr1*pctil[vary=="psi_sjr1.X"]
  if(sum(vary=="psi_sjr2.X")) psi_sjr2 <- psi_sjr2*pctil[vary=="psi_sjr2.X"]
  if(sum(vary=="psi_SWP.X")) psi_SWP <- psi_SWP*pctil[vary=="psi_SWP.X"]
  if(sum(vary=="psi_CVP.X")) psi_CVP <- psi_CVP*pctil[vary=="psi_CVP.X"]
  if(sum(vary=="S_a.X")) S_a <- S_a*pctil[vary=="S_a.X"]
  if(sum(vary=="S_bc.X")) S_bc <- S_bc*pctil[vary=="S_bc.X"]
  if(sum(vary=="S_d.X")) S_d <- S_d*pctil[vary=="S_d.X"]
  if(sum(vary=="S_efc.X")) S_efc <- S_efc*pctil[vary=="S_efc.X"]
  if(sum(vary=="S_CVP.X")) S_CVP <- S_CVP*pctil[vary=="S_CVP.X"]
  if(sum(vary=="S_SWP.X")) S_SWP <- S_SWP*pctil[vary=="S_SWP.X"]
  
  # North origin fish movement and survival
  NorFish<-S_1*psi_steam*S_3*S_7 + S_1*psi_sac1*S_2*psi_sac2*S_4*S_7 + 
    S_1*psi_sac1*S_2*psi_dcc*S_6*S_8 + S_1*psi_sac1*S_2*psi_geo*S_5*S_8
  
  # Cosumnes and Mokelume fish
  CosMokFish<-S_6*(S_bc^1/2)
  
  #Calavaras River
  CalFish<-S_bc
  
  #South origin fish
  SouFish<-(1-Trap_trans)*S_prea*psi_sjr1*S_a*psi_sjr2*S_bc + (1-Trap_trans)*S_prea*psi_sjr1*S_a*psi_TC*S_efc + 
    (1-Trap_trans)*S_prea*psi_OR*S_d*psi_ORN*S_efc + (1-Trap_trans)*S_prea*psi_OR*S_d*psi_CVP*S_CVP +
    (1-Trap_trans)*S_prea*psi_OR*S_d*psi_SWP*S_SWP + Trap_trans
  
  rbind(NorFish,CosMokFish,CalFish,SouFish)
  

  
  

}


# DCC_open<-1
# Q_free<-700
# CVP_exp<-99
# SWP_exp<- 100
# hor_barr<-0
# Q_vern<- 200
# Q_stck<- 10
# Temp_vern<-15
# Temp_pp<-20
# DeltaS(DCC_open,hor_barr,Q_free,Q_vern, Q_stck,Temp_vern,Temp_pp,CVP_exp,SWP_exp,
#        Trap_trans=.5)
