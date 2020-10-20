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



### inverse logit link function
inv.logit<-function(eta){1/(1+exp(-eta))}



######### special multiplier
'%z%'<-function(x,y){
  for(ttb in 1:4) x[,ttb]<-x[,ttb]*y[ttb]
  return(x)    
}

## special function for getting month of greatest inundation 
## and average floodplain for that month and watershed combination
getz.me<-function(fp.array){
  ffp<-array(0,dim(fp.array)[1:2])
  
  for(tyt in 1:dim(fp.array)[3]) ffp<-ffp+as.matrix(fp.array[,,tyt])
  
  ffp<- ffp/dim(fp.array)[3]
  obj<-NULL
  for(i in 1: nrow(ffp)) obj = rbind(obj,c(which(max(ffp[i,1:8])==ffp[i,1:8]),max(ffp[i,1:8])))
  colnames(obj) = c("Month","Habitat")
  return(obj)
}


calc.change<-function(baslin,test,shed){
  
  work1<-test$shed.metrics[test$shed.metrics[,2] == shed,]
  Wbase<-baslin$shed.metrics[baslin$shed.metrics[,2] == shed,]
  
  W.delt<- (work1-Wbase)[,c(-1,-2)]
  colnames(W.delt)<-paste(colnames(W.delt),"Delta",sep=".")
  
  W.pct<-100*((work1-Wbase)/(Wbase+0.0001))[,c(-1,-2)]
  colnames(W.pct)<-paste(colnames(W.pct),"PctCh",sep=".")
  
  
  work2<-test$vally.metrics
  Vbase<-baslin$vally.metrics
  
  V.delt<- (work2-Vbase)[,c(-1)]
  colnames(V.delt)<-paste(colnames(V.delt),"Delta",sep=".")
  
  V.pct<-100*((work2-Vbase)/(Vbase+0.0001))[,c(-1)]
  colnames(V.pct)<-paste(colnames(V.pct),"PctCh",sep=".")
  
  list(vally=cbind(work2,V.delt,V.pct),watshd=cbind(work1,W.delt,W.pct))
}



calc.main.change<-function(baslin,test){
  
  work1<-test$shed.metrics
  Wbase<-baslin$shed.metrics
  
  W.delt<- (work1-Wbase)[,c(-1,-2)]
  colnames(W.delt)<-paste(colnames(W.delt),"Delta",sep=".")
  
  W.pct<-100*((work1-Wbase)/(Wbase+0.0001))[,c(-1,-2)]
  colnames(W.pct)<-paste(colnames(W.pct),"PctCh",sep=".")
  
  work2<-test$vally.metrics
  Vbase<-baslin$vally.metrics
  
  V.delt<- (work2-Vbase)[,c(-1)]
  colnames(V.delt)<-paste(colnames(V.delt),"Delta",sep=".")
  
  V.pct<-100*((work2-Vbase)/(Vbase+0.0001))[,c(-1)]
  colnames(V.pct)<-paste(colnames(V.pct),"PctCh",sep=".")
  
  list(vally=cbind(work2,V.delt,V.pct),watshd=cbind(work1,W.delt,W.pct))
}


# fix BD temperature inputs for chinook model
fix.tmp<-function(zzq){
  zzq<-as.data.frame(zzq)
  zzq$Year<-as.numeric(format(zzq$date, format = "%Y"))
  zzq$Month<-as.numeric(format(zzq$date, format = "%m"))
  
  zzq<-subset(zzq, Year > 1979 & Year < 2000)
  
  subb<-aggregate(zzq[c("water_temp")], by = zzq[c("Year","Month")], mean, na.rm=T)
  matrix(zzq$water_temp,ncol=20, byrow=T)
}










######
#R functions to use rbinom when moving fish through the system
rbin2Matrices<-function(mat1,mat2,stoch){
  XXXX<-dim(mat1)
  if(max(mat1)<=1000000000 & stoch==1){
    junk<-matrix(rbinom((XXXX[1]*XXXX[2]),prob=as.vector(t(mat2)),size=round(as.vector(t(mat1)))),ncol=4,byrow=TRUE)
  }else{junk<-round(mat2*mat1)}
  out<-junk*stoch+(mat2*mat1)*(1-stoch)
  return(out)
}

# left<-rbin2Matrices(USc.sheds$river,p.pulse.leave[1:15,],stochastic)


rbinMatObject<-function(mat1,prob,stoch){
  XXXX<-dim(mat1)
  if(max(mat1)<=1000000000 & stoch==1){
    junk<-matrix(rbinom((XXXX[1]*XXXX[2]),round(t(mat1)),prob),ncol=4,byrow=TRUE)
  }else{junk<-round(mat1*prob)}
  
  out<-(junk*stoch)+(mat1*prob)*(1-stoch)
  return(out)
}

# detoured.fish<-rbinMatObject(up.sac.fsh,prop.Q.bypasses[mnth,yr+1,1],stochastic)


rbinMatVector<-function(mat1,vec1,stoch){
  XXXX<-dim(mat1)
  if(max(mat1)<=1000000000 & stoch==1){
    junk<-matrix(rbinom((XXXX[1]*XXXX[2]),prob=as.vector(t(matrix(rep(vec1,XXXX[1]),ncol=4,byrow=TRUE))),size=round(as.vector(t(mat1)))),ncol=4,byrow=TRUE)
  }else{junk<-round(t(vec1*t(mat1)))}
  out<-junk*stoch+(t(vec1*t(mat1)))*(1-stoch)
  return(out)
}

# left<-rbinMatVector(up.sac.fsh$Umain.river,p.pulse.leave[16,],stochastic)

rbin2MatSpec<-function(mat1,mat2,stoch){
  XXXX<-dim(mat1)
  if(max(mat1)<=1000000000 & stoch==1){
    out<-matrix(rbinom((XXXX[1]*XXXX[2]),prob=as.vector(t(matrix(rep(mat2,XXXX[1]),ncol=4,byrow=TRUE))),size=round(as.vector(t(mat1)))),ncol=4,byrow=TRUE)
  }else{out<-round(mat1%z%mat2)}

  return(out)
}

# left<-rbin2MatSpec(up.sac.fsh$migr,UM.Sac.S,stochastic)


rbin2Vectors<-function(vect1,vect2,stoch){
  if(max(vect1)<=1000000000 & stoch==1){
    junk<-rbinom(length(vect1),round(vect1),vect2)
  }else{junk<-round(vect1*vect2)}
  out<-junk*stoch+(vect1*vect2)*(1-stoch)
  return(out)
}

# left<-rbin2Vectors(S.delt.fsh$out2ocean[25,],newDsurv[3,],stochastic)














