
######################################################################
### EVALUATION OF NUMBER AND DISTRIBUTION OF HATCHERY ORIGIN FISH
### IN THE CENTRAL VALLEY 2010 - 2019, MOST DATA FROM CWT ANALYSES
### WITH LIMITED DATA IN 2010 FROM CVPIA CHINOOKPROD DATABASE
### VERSION 2 APRIL 2022
### J_T_ PETERSON OREGON COOPERATIVE FISH AND WILDLIFE RESERACH UNIT
######################################################################

###############################################################################
### function calculates proportion of hatchery origin spawners to each tributary
### operations
###############################################################################

calc_prop_hatch <- function(file, run){

  #file =d at; run= "Spring"

  df  <-  subset(file, Run == run & Harvest == 0)
  # calculate all hatchery fish returning by year
  yr_tots  <-  aggregate(df["No_hatch_ret"], by = df["Year"], sum)
  ## select fish that spawned in the wild
  in_wild  <-  subset(df, Hatch.flag == 0 & Harvest == 0)

  if (run == "Spring") {
    # BUG. Missing 2015 and 2016. This is a temporary fix.
    wild_tots  <-  aggregate(in_wild["No_hatch_ret"], by = in_wild["Year"], sum)
    wild_tots <- wild_tots %>%
      add_row(Year = 2015, No_hatch_ret = 0) %>%
      add_row(Year = 2016, No_hatch_ret = 0) %>%
      arrange(Year)
  } else {
    wild_tots  <-  aggregate(in_wild["No_hatch_ret"], by = in_wild["Year"], sum)
  }

  wild_tots$pct_spwn  <-  round((wild_tots$No_hatch_ret / yr_tots$No_hatch_ret) * 100)

  print(paste("Average and SD of % of spawners are hatchery origin 2010-2019:",
              paste(round(mean(wild_tots$pct_spwn)), round(sd(wild_tots$pct_spwn)), sep=" +/- ")))
  print(paste("Average and SD of number hatchery origin spawners 2010-2019:",
              paste(round(mean(wild_tots$No_hatch_ret)), round(sd(wild_tots$No_hatch_ret), 1), sep=" +/- ")))


  ## FILL IN MISSSING FOR SMALL WATRERSHEDS USING ANNUAL TOTALS OF NEIGHBORS
  if (run == "Fall") {
    wrk  <-  subset(in_wild, Trib %in% c("Deer Creek", "Paynes Creek", "Cow Creek"))
    wrk2  <-  aggregate(wrk["No_hatch_ret"], by=wrk["Year"], min)
    Miss  <-  expand_grid(Trib=c("Bear Creek", "Elder Creek", "Stony Creek", "Thomes Creek", "Bear River","Antelope Creek",
                               "Big Chico Creek"), Year = wrk2[, 1])
    Miss  <-  merge(Miss,wrk2)
    wrk = subset(in_wild, !(Trib %in% c("Big Chico Creek", "Antelope Creek")))
    in_wild  <-  rbind(wrk[,c("Year","Trib","No_hatch_ret")],Miss)
    wrk  <-  aggregate(in_wild["No_hatch_ret"], by = in_wild["Year"], sum)
    names(wrk)[2] = "tot_hatch_spwn"
    in_wild  <-  merge(in_wild,wrk)
  } else {
    names(wild_tots)[2]  <-  "tot_hatch_spwn"
    in_wild <- merge(in_wild, wild_tots)
  }

  # calculate the fate of hatchery spawners as percent hatchery spawning in each trib by year
  in_wild$pcthatch2trib  <-  in_wild$No_hatch_ret / in_wild$tot_hatch_spwn

  runyr_mns <- aggregate(in_wild[c("pcthatch2trib")], by = in_wild[c("Trib","Year")], mean)

  runyr_mn_sd <- aggregate(runyr_mns[c("pcthatch2trib")], by = runyr_mns[c("Trib")], mean)
  names(runyr_mn_sd)[2] <- "Mean_prop_hatch"
  runyr_mn_sd$SD_prop_hatch <- aggregate(runyr_mns[c("pcthatch2trib")], by = runyr_mns[c("Trib")], sd)[,2]
  #renormalize values so they sum to 1
  runyr_mn_sd$Mean_prop_hatch <- runyr_mn_sd$Mean_prop_hatch / sum(runyr_mn_sd$Mean_prop_hatch)
  return(runyr_mn_sd)
}

###############################################################################
### function calculates proportion of wild fish removed by hatchery
### operations
###############################################################################
Rem_wild_spwn  <-  function(df, run, Tribs) {

  hatch <- subset(df, Run == run & Trib %in% Tribs & Harvest == 0)

  hatch$remove <- hatch$Natural * hatch$Total_escape

  unique(hatch$Trib)

  pairs <- matrix(Tribs, ncol = 2, byrow = T)

  tada <- NULL
  for(i in 1:nrow(pairs)){
    #i=3
    keep <- subset(hatch, Trib == pairs[i,1])
    keep <- keep[order(keep$Year), ]
    remove <- subset(hatch, Trib == pairs[i,2])
    remove <- remove[order(remove$Year), ]

    remove$nat <- remove$Natural * remove$Total_escape
    prop_rem <-  keep$remove / (keep$remove + remove$nat)
    tada <- rbind(tada, data.frame(Trib = rep(pairs[i,2], length(prop_rem)), prop_rem))

  }

  ### mean proportion natural removed from each hatchery trib
  rem_wild <- aggregate(tada[c("prop_rem")], by = tada[c("Trib")], mean)
  rem_wild$SD_prop_rem <- aggregate(tada[c("prop_rem")], by = tada[c("Trib")], sd)[,2]
  return(rem_wild)
}

## locations of run-specific model data
# Fplace <- "C:/Users/peterjam/jpeterson/GitHub/fallRunDSM/data/"
# Splace <- "C:/Users/peterjam/jpeterson/GitHub/springRunDSM/data/"
# Wplace <- "C:/Users/peterjam/jpeterson/GitHub/winterRunDSM/data/"
# LFplace <- "C:/Users/peterjam/Box/CVPIA/GitHub/lateFallRunDSM/data/"

Tribz <- read.csv("data-raw/misc/Tributary relationships.csv")
dat <- read.csv("data-raw/misc/hatchery_fish_fate2010_2019.csv", stringsAsFactors = F)


###############################################################################
############ CWT reports do not separate spring and fall run in Feather and
############ Yuba Rivers, here we apply the proportion of naturally produced
############ return adults to hatcheries by run and use it to adjust the totals
###############################################################################

unique(dat$Trib)
unique(dat$Run)

FeaYubHatch <-  subset(dat, Trib == "Feather River Hatchery" & Hatch.flag == 1)
FeaYubHatch$NatProd <-  FeaYubHatch$Natural * FeaYubHatch$Total_escape
spg <-  subset(FeaYubHatch, Run == "Spring")
names(spg)[11] <- "spring"
fall <-  subset(FeaYubHatch, Run == "Fall")
names(fall)[11] <- "fall"
both <- merge(spg[, c(5,11)], fall[, c(5,11)])
both$prp <- both$spring / (both$spring + both$fall) + 0.001
prop_spring <- both[,c(1,4)]

FeaYub <-  subset(dat, Run=="Fall/Spr" & Harvest==0 & Hatch.flag ==0)
unique(FeaYub$Trib)
# need to combine above and below dam estimates for Yuba
FeaYub$No_naturl <-  FeaYub$Total_escape*FeaYub$Natural

FeaYub$Trib  <-  ifelse(FeaYub$Trib %in% c("Yuba River above", "Yuba River below"),
                      "Yuba River", FeaYub$Trib)

names(dat)
FeaYub <- aggregate(FeaYub[c("No_naturl","No_hatch_ret")], by=FeaYub[c("Trib", "Year")], sum)

FeaYub$Total_escape <- with(FeaYub, No_naturl + No_hatch_ret)
FeaYub$Hatchery <- with(FeaYub, No_hatch_ret / Total_escape)
FeaYub$Natural <- with(FeaYub, No_naturl / Total_escape)
FeaYub$Hatch.flag <- FeaYub$Harvest <- 0; FeaYub$Source <- "CWT"

FeaYubFa <- FeaYubSp <- merge(FeaYub[,-3], prop_spring)

FeaYubFa$Total_escape  <-  FeaYubFa$Total_escape * (1 - FeaYubFa$prp)
FeaYubFa$No_hatch_ret  <-  FeaYubFa$No_hatch_ret * (1 - FeaYubFa$prp)
FeaYubFa$Run  <-  "Fall"
FeaYubSp$Total_escape  <-  FeaYubSp$Total_escape * FeaYubSp$prp
FeaYubSp$No_hatch_ret  <-  FeaYubSp$No_hatch_ret * FeaYubSp$prp
FeaYubSp$Run  <-  "Spring"
FeaYubFa$prp <- FeaYubSp$prp <- NULL

FeaHarv <-  subset(dat, Trib == "Feather River" & Harvest == 1)

FeaHarvFa <- FeaHarvSp <- merge(FeaHarv,prop_spring)
FeaHarvFa$Total_escape  <-  FeaHarvFa$Total_escape*(1-FeaHarvFa$prp)
FeaHarvFa$Run  <-  "Fall"
FeaHarvSp$Total_escape  <-  FeaHarvSp$Total_escape*FeaHarvSp$prp
FeaHarvSp$Run  <-  "Spring"
FeaHarvFa$prp <- FeaHarvSp$prp <- NULL
FeaHarv <- rbind(FeaHarvFa,FeaHarvSp)
FeaHarv$No_hatch_ret <-  FeaHarv$Total_escape*FeaHarv$Hatchery

#names(FeaYubFa) %in% names(dat)
dat <- subset(dat,Run !="Fall/Spr" & Trib !="Upper San Joaquin River" )
dat <- rbind(dat,FeaYubFa,FeaYubSp,FeaHarv)


################################################################################
##############              Fall Run                          ##################
################################################################################

#############################################################
############ calculate mean and sd of hatchery spawner
############ distribution
############################################################
# load(paste(Fplace,"hatchery_allocation_rda", sep=""))
hatchery_allocation <- lateFallRunDSM::params$hatchery_allocation

Fa_Trib_Hat_Allocation <- cbind(Tribz, hatchery_allocation)
names(Fa_Trib_Hat_Allocation)[5] <- "Old_Hatchery_Allocation"

fall_tribs <- calc_prop_hatch(file <- dat, run="Fall")
Fa_Trib_Hat_Allocation <- merge(Fa_Trib_Hat_Allocation,fall_tribs[,1:2], by="Trib", all = T)
Fa_Trib_Hat_Allocation[is.na(Fa_Trib_Hat_Allocation[,6]),6] <- 0
Fa_Trib_Hat_Allocation <- Fa_Trib_Hat_Allocation[order(Fa_Trib_Hat_Allocation$Srt),]

names(Fa_Trib_Hat_Allocation)[6] <- "New_Hatchery_Allocation"

##############################################################################
# now estimate proportion of naturally produced fish used in a hatchery
# i_e_, they did not spawn
##############################################################################

#load(paste(Fplace,"natural_adult_removal_rate_rda", sep=""))

natural_adult_removal_rate <- lateFallRunDSM::params$natural_adult_removal_rate

Fa_Trib_Rem_rate <- cbind(Tribz, natural_adult_removal_rate)
names(Fa_Trib_Rem_rate)[5] <-  "Old_Natural_Removal_Rate"

Fall_Rem <- Rem_wild_spwn(df = dat, run = "Fall", Tribs =c ("Coleman National Fish Hatchery","Battle Creek",
                                                     "Feather River Hatchery","Feather River",
                                                     "Merced River Hatchery","Merced River",
                                                     "Nimbus Fish Hatchery","American River",
                                                     "Mokelumne River Hatchery","Mokelumne River"))
Fa_Trib_Rem_rate$New_Natural_Removal_Rate <- 0

# MR EDIT 5/20: updated Fa_TribH_rate to Fa_Trib_Rem_rate within the ifelse since the former did not exist
Fa_Trib_Rem_rate$New_Natural_Removal_Rate <- ifelse(Fa_Trib_Rem_rate$Trib == "American River", Fall_Rem[1,2],
                                                  Fa_Trib_Rem_rate$New_Natural_Removal_Rate)
Fa_Trib_Rem_rate$New_Natural_Removal_Rate <- ifelse(Fa_Trib_Rem_rate$Trib=="Battle Creek", Fall_Rem[2,2],
                                                  Fa_Trib_Rem_rate$New_Natural_Removal_Rate)
Fa_Trib_Rem_rate$New_Natural_Removal_Rate <- ifelse(Fa_Trib_Rem_rate$Trib=="Feather River", Fall_Rem[3,2], # TODO: Feather is not located in Fall_Rem file
                                                  Fa_Trib_Rem_rate$New_Natural_Removal_Rate)
Fa_Trib_Rem_rate$New_Natural_Removal_Rate <- ifelse(Fa_Trib_Rem_rate$Trib=="Merced River", Fall_Rem[4,2],
                                                  Fa_Trib_Rem_rate$New_Natural_Removal_Rate)
Fa_Trib_Rem_rate$New_Natural_Removal_Rate <- ifelse(Fa_Trib_Rem_rate$Trib=="Mokelumne River", Fall_Rem[5,2], # TODO: should this value be NA?
                                                  Fa_Trib_Rem_rate$New_Natural_Removal_Rate)
New_Old_Fall_Parms <- merge(Fa_Trib_Hat_Allocation, Fa_Trib_Rem_rate)
#################################################################################
##### Now Fall adult harvest rate
#################################################################################

#load(paste(Fplace,"adult_harvest_rate_rda", sep=""))
adult_harvest_rate <- fallRunDSM::adult_harvest_rate

Fa_TribH_rate <- cbind(Tribz,adult_harvest_rate)
names(Fa_TribH_rate)[5] <-  "Old_Harvest_Rate"

TotEsc <- read.csv("data-raw/misc/Total Escapement CV 2000_2020.csv")

harv <- subset(dat,Run=="Fall" & Harvest==1)
cols <- c("American","Feather","SacEsc","Moke","UppSacEsc")
trib <- sort(unique(harv$Trib))
H_rates <- NULL

for (i in 1:length(cols)){
  calcH <- merge(subset(harv, Trib == trib[i])[,c(1,4:5)], TotEsc)
  calcH$H_rate <- round(calcH$Total_escape / (calcH[,cols[i]] + calcH$Total_escape), 3)
  H_rates <- rbind(H_rates,calcH[,c("Year", "Trib","H_rate")])
}

Fall_H_rate <- aggregate(H_rates["H_rate"], by = H_rates["Trib"], mean)
Fall_H_rate$SD <- aggregate(H_rates["H_rate"], by = H_rates["Trib"], sd)[,2]

upSacRat <- (1 - (1 - Fall_H_rate[3, 2]) * (1 - Fall_H_rate[5, 2]))
FeaRat <- (1 - (1 - Fall_H_rate[3, 2]) * (1 - Fall_H_rate[2, 2]))

Fa_TribH_rate$New_Harvest_Rate <- upSacRat
Fa_TribH_rate$New_Harvest_Rate[25 : 31] <- Fall_H_rate[4, 2]

Fa_TribH_rate$New_Harvest_Rate <- ifelse(Fa_TribH_rate$Trib == "American River", Fall_H_rate[1, 2],
                                       Fa_TribH_rate$New_Harvest_Rate)
Fa_TribH_rate$New_Harvest_Rate <- ifelse(Fa_TribH_rate$Trib %in% c("Feather River","Bear River","Yuba River"),
                                       FeaRat, Fa_TribH_rate$New_Harvest_Rate)
Fa_TribH_rate$New_Harvest_Rate <- ifelse(Fa_TribH_rate$Old_Harvest_Rate==0,
                                       0, Fa_TribH_rate$New_Harvest_Rate)

New_Old_Fall_Parms <- merge(New_Old_Fall_Parms, Fa_TribH_rate)

write.csv(New_Old_Fall_Parms, "New Fall run model parameters_vFW.csv")

############################################################################
###########             Spring run                     #####################
############################################################################
#load(paste(Splace,"hatchery_allocation_rda", sep =""))
hatchery_allocation <- springRunDSM::params$hatchery_allocation

Sp_Trib_Hat_Allocation <- cbind(Tribz, hatchery_allocation)
names(Sp_Trib_Hat_Allocation)[5] <- "Old_Hatchery_Allocation"

#TODO: bug. wild_tots (created in f(calc_prop_hatch)) is missing years 2015, 2016
spring_tribs <- calc_prop_hatch(file = dat, run = "Spring")
spring_tribs[, 2] <- round(spring_tribs[,2], 5)
Sp_Trib_Hat_Allocation <- merge(Sp_Trib_Hat_Allocation,spring_tribs[,1:2], all=T)

Sp_Trib_Hat_Allocation[is.na(Sp_Trib_Hat_Allocation[,6]), 6] <- 0
Sp_Trib_Hat_Allocation <- Sp_Trib_Hat_Allocation[order(Sp_Trib_Hat_Allocation$Srt), ]

names(Sp_Trib_Hat_Allocation)[6]  <- "New_Hatchery_Allocation"

#########################################################################
# Estimate proportion of naturally produced fish used in a hatchery
# i_e_, they did not spawn
#########################################################################
#load(paste(Splace,"natural_adult_removal_rate_rda", sep=""))
natural_adult_removal_rate <- springRunDSM::params$natural_adult_removal_rate

Sp_Trib_Rem_rate <- cbind(Tribz,natural_adult_removal_rate)
names(Sp_Trib_Rem_rate)[5] <-  "Old_Natural_Removal_Rate"

spring_remov <- Rem_wild_spwn(df= dat, run = "Spring", Tribs = c("Feather River Hatchery", "Feather River"))

Sp_Trib_Rem_rate$New_Natural_Removal_Rate <- ifelse(Sp_Trib_Rem_rate$Trib == "Feather River", spring_remov[1,2], 0)

New_Old_Spring_Parms <- merge(Sp_Trib_Hat_Allocation,Sp_Trib_Rem_rate)

write.csv(New_Old_Spring_Parms, "New Spring run model parameters_vFW.csv")

############################################################################
###########             Winter run                     #####################
############################################################################
#load(paste(Wplace,"hatchery_allocation_rda", sep=""))
hatchery_allocation <- winterRunDSM::params$hatchery_allocation

Wi_Trib_Hat_Allocation <- cbind(Tribz,hatchery_allocation)
names(Wi_Trib_Hat_Allocation)[5] <- "Old_Hatchery_Allocation"

winter_tribs <- calc_prop_hatch(file=dat, run="Winter")

Wi_Trib_Hat_Allocation$New_Hatchery_Allocation <- 0
Wi_Trib_Hat_Allocation$New_Hatchery_Allocation[1] <- winter_tribs[1,2]

#########################################################################
# Estimate proportion of naturally produced fish used in a hatchery
# i_e_, they did not spawn
#########################################################################
#load(paste(Wplace,"natural_adult_removal_rate_rda", sep=""))
natural_adult_removal_rate <- winterRunDSM::params$natural_adult_removal_rate

Wi_Trib_Rem_rate <- cbind(Tribz,natural_adult_removal_rate)
names(Wi_Trib_Rem_rate)[5] <-  "Old_Natural_Removal_Rate"

winter_rem <- Rem_wild_spwn(df= dat, run="Winter", Tribs=c("Coleman National Fish Hatchery", "Upper Sacramento River"))

Wi_Trib_Rem_rate$New_Natural_Removal_Rate <- 0

Wi_Trib_Rem_rate$New_Natural_Removal_Rate[1] <- winter_rem[1,2]

New_Old_Winter_Parms <- merge(Wi_Trib_Hat_Allocation,Wi_Trib_Rem_rate)

write.csv(New_Old_Winter_Parms, "New Winter run model parameters_vFW.csv")

############################################################################
###                 Late Fall run                      #####################
### Current version (April 2022) is parameterized with most up to date CWT data
############################################################################


