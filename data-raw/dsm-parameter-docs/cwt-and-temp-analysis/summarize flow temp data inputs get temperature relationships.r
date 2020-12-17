###############################################################################
##      Chinook supplemantary analysis for Chinook model:                    ##
##      Create models to estimate temperature metrics using monthly          ##
##      median flows in CV rivers                                            ##
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
# read in data
combo<- read.csv("Full temperature data.csv")

combo2<-na.omit(combo)
summary(combo2)
combo2$gt20<-ifelse(combo2$medC>20,1,0)
combo2$gt25<-ifelse(combo2$maxC>25,1,0)

sum(combo2$gt25)

summ<-aggregate(combo2[c("gt25")], by=combo2[c("year","month")], sum)
summ$prop<-aggregate(combo2[c("gt20")], by=combo2[c("year","month")], mean)[,3]
summ$mo.mean<-aggregate(combo2[c("medC")], by=combo2[c("year","month")], mean)[,3]

# Model proportion month with temps > 20 C
# using logit link, adjust for zero of 1's
# adjust to allow logit link use
summ$prop<-ifelse(summ$prop == 0, 0.001,summ$prop)
summ$prop<-ifelse(summ$prop == 1, 0.999,summ$prop)
# logit link
summ$prop.eta<-log(summ$prop/(1-summ$prop))

summary(summ)

summary(lm(prop.eta~mo.mean,data=summ))

mo.meanz<-c(5:25)
pred<-inv.logit(-14.34722+ 0.71738*mo.meanz)

plot(prop~mo.mean,summ, xlab= "Monthly median temperature", ylab="Proportion days temp > 20C" )
lines(pred~mo.meanz, col="red")

# adjust to allow logit link use
summ$gt25<-ifelse(summ$gt25 > 1, 1,summ$gt25)

summary(summ)

summary(glm(gt25~mo.mean,data=summ, family='binomial'))

mo.meanz<-c(5:25)
pred<-inv.logit(-18.6688+ 1.1480*mo.meanz)

plot(gt25~mo.mean,summ)
lines(pred~mo.meanz, col='red')


