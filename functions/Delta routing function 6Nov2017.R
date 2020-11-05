#*******************************************************************************
#  Function: get.routing
#
#  Purpose: To predict the proportion of fish entering Sutter/Steamboat Sloughs,
#           the Delta Cross Channel, and Georgiana Slough as a function of
#           1) discharge of the Sacramento River at Freeport, and
#           2) DCC gate position
#
#  Updated: 6 Nov 2017 by R. Perry
#
#  Arguments:
#  Qfpt -         Daily discharge (ft^3/s) of the Sacramento River at Freeport.
#  DCCgate.pos -  Position of the Delta Cross Channel gate. 1 = open, 0 = closed.
#
#  Output:
#  Matrix with following columns:
#  psi.SS - proportion of fish entering Sutter and Steamboat Sloughs
#  psi.SAC1 - proportion of fish remaining the Sacramento at Sutter/Steamboat
#  psi.DCC - proportion of fish entering the Delta Cross Channel
#  psi.GEO - proportion of fish entering Georgiana Slough
#  psi.SAC2 - proportion of fish remaining in the Sacramento River
#
#  Notes: Based on the Bayesian multistate mark-recapture model that has been
#        accepted with minor revision by CJFAS (Perry et al.)
#
#  Also, note that routing probabilities at DCC and Geo are conditional on fish
#  remaining in the Sacramento River at the junction of Sutter/Steamboat such that:
#  psi.DCC + psi.GEO + psi.SAC2 = 1
#  psi.SS + psi.SAC1 = 1
#
#  To get the unconditional probabilities of fish taking all four routes:
#  SS = psi.SS
#  Sac = psi.SAC1 * psi.SAC2
#  DCC = psi.SAC1 * psi.DCC
#  Geo = psi.SAC1 * psi.GEO
#*******************************************************************************


route.2.sDlt<- function(Qfpt, DCCgate.pos, vary, pctil){
  DCCgate.pos<-min(DCCgate.pos,30) #treat each month as 30 days
  open<-30-DCCgate.pos
  gate<-c(rep(0,DCCgate.pos),rep(1,open))
  SS1<-Sac1<-DCC1<-Geo1<-c()
  for(ii in 1:30){
    # Mean and SD for standardizing discharge
    mean.Qfpt = 21546.19
    sd.Qfpt = 14375.9
    
    # standardize discharge
    sQfpt <- (Qfpt - mean.Qfpt) / (2*sd.Qfpt)
    
    #-- Parameter definitions - All parameters are posterior medians.
    # b0 = intercept
    # b1 = slope for Freeport discharge
    # b2 = DCC effect on routing
    # U = upper asymptote for Sutter and Steamboat Slough
    # L = lower asymptote for Georgiana Slough
    
    #-- Parameters for Sutter and Steamboat Sloughs
    b0.SS <-  1.8922350
    b1.SS <-  2.1703750
    U <- 0.3512465
    
    #-- Parameters for Delta Cross Channel
    b0.DCC <- -1.4896200
    b1.DCC <- -1.2488650
    
    #-- Parameters for Georgiana Slough
    b0.GEO <- -2.9481450
    b1.GEO <- -2.9118350
    b2.GEO <- -0.5548430
    L <- 0.2729845
    
    #----- First Junction, Sacramento and Sutter/Steamboat  ---------
    # Probability of entering Sutter/Steamboat
    lpsi.SS <- b0.SS + b1.SS * sQfpt
    psi.SS <- U / (1 + exp(-lpsi.SS))
    
    if(sum(vary=="psi_steam.X")) psi.SS <- psi.SS*pctil[vary=="psi_steam.X"]
    
    # Probability of remaining in Sacramento at junction with Sutter/Steamboat
    psi.SAC1 <- 1 - psi.SS
    
    #----- Second junction, Sacramento, DCC, and Georgiana Slough  ---------
    # Probability of entering DCC conditional on arriving at the river junction
    # (i.e, conditional on remaining in the Sacramento River at the Sutter/Steamboat)
    lpsi.DCC <- b0.DCC + b1.DCC * sQfpt
    psi.DCC <- (1 / (1 + exp(-lpsi.DCC))) * gate[ii]
    if(sum(vary=="psi_dcc.X")) psi.DCC <- psi.DCC*pctil[vary=="psi_dcc.X"]
    
    # Probability of entering Geo conditional on arriving at junction and
    # not entering DCC
    lpsi.GEO.notDCC <- b0.GEO + b1.GEO * sQfpt + b2.GEO * gate[ii]
    psi.GEO.notDCC <- L + (1 - L)/(1 + exp(-lpsi.GEO.notDCC))
    
    if(sum(vary=="psi_geo.X")) psi.GEO.notDCC <- psi.GEO.notDCC*pctil[vary=="psi_geo.X"]
    
    # Unconditional probability of entering Georgiana Slough, but conditional
    # on arriving at the junction of Sac, DCC, and Geo.
    psi.GEO <- (1 - psi.DCC) * psi.GEO.notDCC
    
    #Unconditional probability of remaining in Sacramento River
    psi.SAC2 <- 1 - psi.DCC - psi.GEO
    SS = psi.SS
    Sac = psi.SAC1 * psi.SAC2
    DCC = psi.SAC1 * psi.DCC
    Geo = psi.SAC1 * psi.GEO
    
    SS1<-c(SS1,SS)
    Sac1<-c(Sac1,Sac)
    DCC1<-c(DCC1,DCC)
    Geo1<-c(Geo1,Geo)
  }
  
  #calculate the average probability of each route for the month
  SS<-mean(SS1)
  Sac<-mean(Sac1)
  DCC<-mean(DCC1)
  Geo<-mean(Geo1)
  routing.probs <- cbind(SS, Sac, DCC, Geo)
  return(routing.probs)
}
# 
# # Example applications
# 
# # DCC open
# flow.closed = seq(6000, 70000, 1000)
# gate = rep(0, length(flow.closed))
# routing.closed <- route.2.sDlt(Qfpt = flow.closed, DCCgate.pos = gate)
# 
# windows(); bringToTop(stay = T)
# matplot(flow.closed, routing.closed, type = "l", lty = 1, lwd = 2, ylim = c(0,1),
#    main = "DCC closed", ylab = "Routing probability", xlim = range(flow.closed),
#    xlab = expression(Sacramento~ ~discharge~ ~at~ ~Freeport~ ~(ft^3/s)))
# legend("topright", legend = colnames(routing.closed), lty = 1, lwd = 2, col = 1:5)
# 
# # DCC closed, Note: DCC cannot operate over 25000 cfs, so always closed above that level
# flow.open = seq(6000, 25000, 1000)
# gate = rep(1, length(flow.open))
# routing.open <- get.routing(Qfpt = flow.open, DCCgate.pos = gate)
# 
# windows(); bringToTop(stay = T)
# matplot(flow.open, routing.open, type = "l", lty = 1, lwd = 2, ylim = c(0,1),
#    main = "DCC open", ylab = "Routing probability", xlim = range(flow.closed),
#    xlab = expression(Sacramento~ ~discharge~ ~at~ ~Freeport~ ~(ft^3/s)))
# legend("topright", legend = colnames(routing.closed), lty = 1, lwd = 2, col = 1:5)
