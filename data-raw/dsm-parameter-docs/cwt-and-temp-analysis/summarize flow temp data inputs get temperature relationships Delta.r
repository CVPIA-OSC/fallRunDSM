###############################################################################
##      Chinook supplemantary analysis for Chinook model:                    ##
##      Create models to estimate temperature metrics using monthly          ##
##      median flows in SF bay-Delta                                         ##
##      Primary Author: James T. Peterson June 1, 2018                       ##
##      U.S. Geological Survey, Oregon CooperativeCooperative                ##
##      Fish and Wildlife Research Unit, Oregon State University             ##
##      Corvallis, Oregon 97331-3803, jt.peterson@oregonstate.edu            ##
##                                                                           ##
##     Although this software program has been used by                       ##
##     the U.S. Geological Survey (USGS), no warranty, expressed             ##
##     or implied, is made by the USGS or the U.S. Government as to          ##
##     the accuracy and functioning of the program and related program       ##
##     material nor shall the fact of distribution constitute any            ##
##     such warranty, and no responsibility is assumed by the USGS           ##
##     in connection therewith.                                              ##
###############################################################################
library(plyr)
library(gdata)
library(date)
library(igraph)
inv.logit<-function(eta) {1/(1+exp(-eta))}
## set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Bay delta temps
one<-read.csv("JERSEY POINT.csv")
two<-read.csv("DUTCH SLOUGH.csv")
one<-rbind(one,two)
two<-read.csv("GEORGIANA SLOUGH.csv")
one<-rbind(one,two)
two<-read.csv("LPotatoSlough.csv")
one<-rbind(one,two)

one$gt20 <-ifelse(one$TempC > 20,1,0)
one$gt25 <-ifelse(one$TempC > 25,1,0)

sss<-aggregate(one[c("TempC","gt20","gt25")], by=one[c("year","month")], mean, na.rm=T)


sss$gt20<-ifelse(sss$gt20==0,0.001,sss$gt20)
sss$gt20<-ifelse(sss$gt20==1,0.999,sss$gt20)

sss$logit<-log(sss$gt20/(1-sss$gt20))

summary(lm(logit~TempC,sss))

pred<-inv.logit(-18.11910 + 0.94687*mo.meanz)

plot(pred~mo.means)
plot(gt20~TempC,sss)


summ<-aggregate(one[c("gt25")], by=one[c("year","month")], sum)
summ$mo.mean<-aggregate(one[c("TempC")], by=one[c("year","month")], mean)[,3]

summ$gt25<-ifelse(summ$gt25 > 1, 1,summ$gt25)

summary(summ)

summary(glm(gt25~mo.mean,data=summ, family='binomial'))

mo.meanz<-c(5:25)
pred<-inv.logit(-157.537+ 6.998*mo.meanz)

plot(gt25~mo.mean,summ)
lines(pred~mo.meanz, col='red')

