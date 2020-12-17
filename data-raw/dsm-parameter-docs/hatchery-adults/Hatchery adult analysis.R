
######################################################################
### EVALUATION OF NUMBER AND DISTRIBUTION OF HATCHERY ORIGIN FISH
### IN THE CENTRAL VALLEY 2010 - 2012, MOST DATA FROM CWT ANALYSES
### WITH LIMITED DATA IN 2010 FROM CVPIA CHINOOKPROD DATABASE
### VERSION 2 SEPTEMBER 22, 2017
### J.T. PETERSON OREGON COOPERATIVE FISH AND WILDLIFE RESERACH UNIT
######################################################################

dat<-read_csv("data-raw/dsm-parameter-docs/hatchery-adults/hatchery_fish_fate2010_2012.csv")

summary(dat)

###############################################################################
############ Feather and Yuba Rivers do not seperate spring and fall run
############ in totals, here we apply the proportion of spring hatchery return
############ adults that are used to adjust the totals for 2010 - 2012
############ Data used were from GrandTab 2017 version
###############################################################################

prop.spring<-data.frame(prp=c(0.076777295, 0.056932196, 0.081441457), Year=c(2010,2011,2012))

FeaYub<- subset(dat,Run=="Fall/Spr")

FeaYubFa<-FeaYubSp<-merge(FeaYub,prop.spring)

FeaYubFa$Total_escape <- FeaYubFa$Total_escape*(1-FeaYubFa$prp)
FeaYubFa$No_hatch_ret <- FeaYubFa$No_hatch_ret*(1-FeaYubFa$prp)
FeaYubFa$Run <- "Fall"
FeaYubSp$Total_escape <- FeaYubSp$Total_escape*FeaYubSp$prp
FeaYubSp$No_hatch_ret <- FeaYubSp$No_hatch_ret*FeaYubSp$prp
FeaYubSp$Run <- "Spring"
FeaYubFa$prp<-FeaYubSp$prp<-NULL

dat<-subset(dat,Run!="Fall/Spr")
dat<-rbind(dat,FeaYubFa,FeaYubSp)

fall<-subset(dat, Run == "Fall")

yr.tots<-aggregate(fall[c(6)], by = fall[c(5)], sum)

in_wild<-subset(fall,Hatch.flag==0)

wild.tots<-aggregate(in_wild[c(6)], by = in_wild[c(5)], sum)

wild.tots$pct.spwn<-round((wild.tots$No_hatch_ret/yr.tots$No_hatch_ret)*100)

print(paste("Average percent of Fall hatchery origin spawning 2010-2012:",round(mean(wild.tots$pct.spwn))))
print(paste("Average number hatchery Fall origin spawning 2010-2012:",round(mean(wild.tots$No_hatch_ret))))
names(wild.tots)[2]<-"tot.hatch.spwn"
in_wild<-merge(in_wild,wild.tots)
# calculate the fate of hatchery spawners as percent hatchery spawning in rach trib by year
in_wild$pcthatch2trib<- in_wild$No_hatch_ret/in_wild$tot.hatch.spwn

fall.yr.mns<-aggregate(in_wild[c("pcthatch2trib")], by= in_wild[c("Trib","Year")], mean)

fall.yr.mns<-aggregate(fall.yr.mns[c("pcthatch2trib")], by= fall.yr.mns[c("Trib")], mean)

# 5 missing tribs assign lowest proportion in observed dataset
missing<-data.frame(Trib=c("Bear Creek", "Elder Creek", "Stony Creek", "Thomes Creek", "Bear River"),
                    pcthatch2trib=rep(min(fall.yr.mns$pcthatch2trib),5))
#add missing to observed
fall.yr.mns<-rbind(fall.yr.mns,missing)

#renormalize values so they sum to 1
fall.yr.mns$pcthatch2trib<-fall.yr.mns$pcthatch2trib/sum(fall.yr.mns$pcthatch2trib)


# now estimate proportion of naturally produced fish used in a hatchery
# i.e., they did not spawn

FAhatch<-subset(fall, Hatch.flag == 1)

FAhatch$remove<-FAhatch$Natural*FAhatch$Total_escape


pairs<-matrix(c("Coleman National Fish Hatchery ","Battle Creek",
"Feather River Hatchery","Feather River",
"Merced River Fish Facility ","Merced River",
"Nimbus Fish Hatchery ","American River",
"Mokelumne Hatchery ","Mokelumne River"), ncol=2, byrow=T)
tada<-NULL
for(i in 1:5){
  #i=1
  keep<-subset(FAhatch, Trib == pairs[i,1])
  keep<-keep[order(keep$Year),]
  remove<-subset(fall, Trib == pairs[i,2])
  remove<-remove[order(remove$Year),]

  remove$nat<-remove$Natural*remove$Total_escape
  prop.rem<- keep$remove/(keep$remove + remove$nat)
  tada<-rbind(tada,data.frame(Trib=rep(pairs[i,2],3),prop.rem))

}

### mean proportion natural removed from each hatchery trib
aggregate(tada[c("prop.rem")], by= tada[c("Trib")], mean)

##################################################################
########### Spring run
########### not much available in the CWT so some assumptions made
#################################

spring<-subset(dat, Run == "Spring")

pct.hatch.trib<-aggregate(spring[c(2)], by = spring[c(1)], mean)
write.csv(pct.hatch.trib, "spring hatchery spawners.csv", row.names = F)

yr.tots<-aggregate(spring[c(6)], by = spring[c(5)], sum)

in_wild<-subset(spring,Hatch.flag==0)

wild.tots<-aggregate(in_wild[c(6)], by = in_wild[c(5)], sum)

wild.tots$pct.spwn<-round((wild.tots$No_hatch_ret/yr.tots$No_hatch_ret)*100)

print(paste("Average percent of Spring hatchery origin spawning 2010-2012:",round(mean(wild.tots$pct.spwn, na.rm=T))))
print(paste("Average number hatchery Spring origin spawning 2010-2012:",round(mean(wild.tots$No_hatch_ret))))
names(wild.tots)[2]<-"tot.hatch.spwn"
in_wild<-merge(in_wild,wild.tots)
# calculate the fate of hatchery spawners as percent hatchery spawning in rach trib by year
in_wild$pcthatch2trib<- in_wild$No_hatch_ret/in_wild$tot.hatch.spwn

spg.yr.mns<-aggregate(in_wild[c("pcthatch2trib")], by= in_wild[c("Trib","Year")], mean)

spg.yr.mns<-aggregate(spg.yr.mns[c("pcthatch2trib")], by= spg.yr.mns[c("Trib")], mean)

spg.yr.mns[spg.yr.mns$Trib=="Mill Creek",2]<-0

#renormalize values so they sum to 1
spg.yr.mns$pcthatch2trib<-spg.yr.mns$pcthatch2trib/sum(spg.yr.mns$pcthatch2trib)

## fate of hatchery spawners

# now estimate proportion of naturally produced fish used in a hatchery
# i.e., they did not spawn

SPhatch<-subset(spring, Hatch.flag == 1)

SPhatch$remove<-SPhatch$Natural*SPhatch$Total_escape

remove<-subset(spring, Trib == "Feather River")
remove<-remove[order(remove$Year),]
remove$nat<-remove$Natural*remove$Total_escape
prop.rem<- SPhatch$remove/(SPhatch$remove + remove$nat)

## proportion of natural spring run in Feather
## removed by hatchery 22%
mean(prop.rem)

###########################################################
##### for winter there is even less to go on
###########################################################

wint<-subset(dat, Run == "Winter")

yr.tots<-aggregate(wint[c(6)], by = wint[c(5)], sum)

in_wild<-subset(wint,Hatch.flag==0)

wild.tots<-aggregate(in_wild[c(6)], by = in_wild[c(5)], sum)

wild.tots$pct.spwn<-round((wild.tots$No_hatch_ret/yr.tots$No_hatch_ret)*100)

print(paste("Average percent of Winter hatchery origin spawning 2010-2012:",round(mean(wild.tots$pct.spwn))))
print(paste("Average number hatchery Winter origin spawning 2010-2012:",round(mean(wild.tots$No_hatch_ret))))
names(wild.tots)[2]<-"tot.hatch.spwn"
in_wild<-merge(in_wild,wild.tots)
# calculate the fate of hatchery spawners as percent hatchery spawning in each trib by year
in_wild$pcthatch2trib<- in_wild$No_hatch_ret/in_wild$tot.hatch.spwn

wint.yr.mns<-aggregate(in_wild[c("pcthatch2trib")], by= in_wild[c("Trib","Year")], mean)

## should be 100% for now because we are only considering upper sac, I think
wint.yr.mns<-aggregate(wint.yr.mns[c("pcthatch2trib")], by= wint.yr.mns[c("Trib")], mean)

#renormalize values so they sum to 1
#wint.yr.mns$pcthatch2trib<-wint.yr.mns$pcthatch2trib/sum(wint.yr.mns$pcthatch2trib)

## fate of hatchery spawners

# now estimate proportion of naturally produced fish used in a hatchery
# i.e., they did not spawn

WIhatch<-subset(wint, Hatch.flag == 1)

WIhatch$remove<-WIhatch$Natural*WIhatch$Total_escape

## note we are assuming that fish in traps at RBB and Keswik would have spawned in Upper Sac
remove<-subset(wint, Trib == "Upper Sacramento River ")
remove<-remove[order(remove$Year),]
remove$nat<-remove$Natural*remove$Total_escape
prop.rem<- WIhatch$remove/(WIhatch$remove + remove$nat)

## proportion natural winter run in Upper Sac
## removed by hatchery 5%
mean(prop.rem)
