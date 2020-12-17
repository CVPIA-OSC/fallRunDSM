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

one$gt20 <-ifelse(one$TempC >= 20,1,0)
one$gt25 <-ifelse(one$TempC >= 25,1,0)
one$gt25Ever <-ifelse(one$TempC >= 25,1,0)

# sss<-aggregate(one[c("TempC","gt20","gt25")], by=one[c("year","month")], mean, na.rm=T)

summ<-aggregate(one[c("gt25Ever")], by=one[c("year","month")], sum)
summ$prop25<-aggregate(one[c("gt25")], by=one[c("year","month")], mean)[,3]
summ$prop20<-aggregate(one[c("gt20")], by=one[c("year","month")], mean)[,3]
summ$mo.mean<-aggregate(one[c("TempC")], by=one[c("year","month")], mean)[,3]

summary(summ)

# Model proportion month with temps > 20 C
# using logit link, adjust for zero of 1's
# adjust to allow logit link use
summ$prop20<-ifelse(summ$prop20 == 0, 0.001,summ$prop20)
summ$prop20<-ifelse(summ$prop20 == 1, 0.999,summ$prop20)

summ$prop25<-ifelse(summ$prop25 == 0, 0.001,summ$prop25)
summ$prop25<-ifelse(summ$prop25 == 1, 0.999,summ$prop25)
# logit link
summ$prop.eta20<-log(summ$prop20/(1-summ$prop20))
summ$prop.eta25<-log(summ$prop25/(1-summ$prop25))

summary(summ)

summary(lm(prop.eta20~mo.mean,data=summ))
summary(lm(prop.eta25~mo.mean,data=summ))

mo.meanz<-c(5:25)
pred20<-plogis(-18.30017 + 0.96991 *mo.meanz)

plot(prop20~mo.mean,summ, xlab= "Monthly median temperature", ylab="Proportion days temp >= 20C" )
lines(pred20~mo.meanz, col="red")

# adjust to allow logit link use
summ$gt25Ever<-ifelse(summ$gt25Ever > 1, 1,summ$gt25Ever)

summary(summ)

summary(glm(gt25Ever~mo.mean,data=summ, family='binomial'))

mo.meanz<-c(5:25)
pred<-plogis(-157.537+6.998*mo.meanz)

plot(gt25Ever~mo.mean,summ)
lines(pred~mo.meanz, col='red')






















