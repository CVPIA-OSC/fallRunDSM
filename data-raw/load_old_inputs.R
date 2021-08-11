all_inputs <- read_rds('data-raw/misc/all_old_inputs.rds')
params <- fallRunDSM::params

all_inputs$floodp.sutter -> params$sutter_floodplain_habitat
all_inputs$floodp.yolo -> params$yolo_floodplain_habitat

all_inputs$territory_size -> params$territory_size
spawn_dimnames <- dimnames(params$spawning_habitat)
all_inputs$IChab.spawn -> params$spawning_habitat[,,1:20]
dimnames(params$spawning_habitat) <- spawn_dimnames

rear_dimnames <- dimnames(params$inchannel_habitat_fry)
all_inputs$IChab.fry -> params$inchannel_habitat_fry[,,1:20]
dimnames(params$inchannel_habitat_fry) <- rear_dimnames
all_inputs$IChab.juv -> params$inchannel_habitat_juvenile[,,1:20]
dimnames(params$inchannel_habitat_juvenile) <- rear_dimnames

flood_dimnames <- dimnames(params$floodplain_habitat)
all_inputs$floodP -> params$floodplain_habitat[,,1:20]
flood_dimnames -> dimnames(params$floodplain_habitat)

bypass_dimnames <- dimnames(params$sutter_habitat)
all_inputs$IChab.sutter -> params$sutter_habitat[,1:20]
dimnames(params$sutter_habitat) <- bypass_dimnames
all_inputs$IChab.yolo -> params$yolo_habitat[,1:20]
dimnames(params$yolo_habitat) <- bypass_dimnames


delta_dimnames <- dimnames(params$delta_habitat)
all_inputs$DLThab -> params$delta_habitat[,1:20,]
dimnames(params$delta_habitat) <- delta_dimnames

all_inputs$cc.aloc -> params$cross_channel_stray_rate
names(params$cross_channel_stray_rate) <- fallRunDSM::watershed_labels
all_inputs$oth.aloc -> params$stray_rate
names(params$stray_rate) <- fallRunDSM::watershed_labels

all_inputs$returnProps -> params$month_return_proportions
names(params$month_return_proportions) <- month.abb[10:12]

all_inputs$prop.hatch -> params$proportion_hatchery
names(params$proportion_hatchery) <- fallRunDSM::watershed_labels

retq_dimnames <- dimnames(params$prop_flow_natal)
as.matrix(all_inputs$retQ[,-1]) -> params$prop_flow_natal[,1:20]
dimnames(params$prop_flow_natal) <- retq_dimnames

overtop_dimnames <- dimnames(params$gates_overtopped)
all_inputs$gate.top -> params$gates_overtopped
dimnames(params$gates_overtopped) <- overtop_dimnames

mc_dimnames <- dimnames(params$migratory_temperature_proportion_over_20)
as.matrix(all_inputs$ptemp20mc[,c(-1,-2)]) -> params$migratory_temperature_proportion_over_20
mc_dimnames -> dimnames(params$migratory_temperature_proportion_over_20)

dimnames_31_12_22 <- dimnames(params$degree_days)
all_inputs$DegDay -> params$degree_days[,,1:20]
dimnames(params$degree_days) <- dimnames_31_12_22

dimnames_31_12_21 <- dimnames(params$avg_temp)
all_inputs$juv.tmp -> params$avg_temp[,,1:20]
dimnames(params$avg_temp) <- dimnames_31_12_21

all_inputs$p.diver -> params$proportion_diverted[,,1:20]
dimnames(params$proportion_diverted) <- dimnames_31_12_21
all_inputs$t.diver -> params$total_diverted[,,1:20]
dimnames(params$total_diverted) <- dimnames_31_12_21

dimnames_31_12 <- dimnames(params$prop_pulse_flows)
as.matrix(all_inputs$prop.pulse[,-1])/100 -> params$prop_pulse_flows
dimnames(params$prop_pulse_flows) <- dimnames_31_12

all_inputs$dlt.divers -> params$delta_proportion_diverted[,1:20,]
dimnames(params$delta_proportion_diverted) <- delta_dimnames
all_inputs$dlt.divers.tot -> params$delta_total_diverted[,1:20,]
dimnames(params$delta_total_diverted) <- delta_dimnames

all_inputs$fp.weeks -> params$weeks_flooded[,,1:20]
dimnames(params$weeks_flooded) <- dimnames_31_12_21

upsq_dn <- dimnames(params$upper_sacramento_flows)
as.matrix(all_inputs$upSacQ) -> params$upper_sacramento_flows[,1:20]
upsq_dn -> dimnames(params$upper_sacramento_flows)

all_inputs$Dlt.inf -> params$delta_inflow[,1:20,]
dimnames(params$delta_inflow) <- delta_dimnames
all_inputs$juv.tmp.dlt -> params$avg_temp_delta[,1:20,]
dimnames(params$avg_temp_delta) <- delta_dimnames

pqb_dn <- dimnames(params$proportion_flow_bypass)
all_inputs$prop.Q.bypasses[,,1] -> params$proportion_flow_bypass[,1:20,1]
all_inputs$prop.Q.bypasses[,,5] -> params$proportion_flow_bypass[,1:20,2]
pqb_dn -> dimnames(params$proportion_flow_bypass)

all_inputs$inps$hatch.alloc -> params$hatchery_allocation
names(params$hatchery_allocation) <- fallRunDSM::watershed_labels
all_inputs$inps$prop.nat.remov -> params$natural_adult_removal_rate
names(params$natural_adult_removal_rate) <- fallRunDSM::watershed_labels
all_inputs$inps$SCDELT -> params$south_delta_routed_watersheds
names(params$south_delta_routed_watersheds) <- fallRunDSM::watershed_labels
all_inputs$inps$TISD -> params$tisdale_bypass_watershed
names(params$tisdale_bypass_watershed) <- fallRunDSM::watershed_labels
all_inputs$inps$YOLO -> params$yolo_bypass_watershed
names(params$yolo_bypass_watershed) <- fallRunDSM::watershed_labels
all_inputs$inps$A.HARV -> params$adult_harvest_rate
names(params$adult_harvest_rate) <- fallRunDSM::watershed_labels
all_inputs$inps$P.scour.nst -> params$prob_nest_scoured
names(params$prob_nest_scoured) <- fallRunDSM::watershed_labels
all_inputs$inps$P.strand.early -> params$prob_strand_early
names(params$prob_strand_early) <- fallRunDSM::watershed_labels
all_inputs$inps$P.strand.late -> params$prob_strand_late
names(params$prob_strand_late) <- fallRunDSM::watershed_labels
all_inputs$inps$High.pred -> params$prop_high_predation
names(params$prop_high_predation) <- fallRunDSM::watershed_labels
all_inputs$inps$contact -> params$contact_points
names(params$contact_points) <- fallRunDSM::watershed_labels

all_inputs$Dlt.inp$contct.pts -> params$delta_contact_points
names(params$delta_contact_points) <- c("North Delta", "South Delta")
all_inputs$Dlt.inp$High.pred -> params$delta_prop_high_predation
names(params$delta_prop_high_predation) <- c("North Delta", "South Delta")

all_inputs$egg.tmp.eff$mean_temp_effect -> params$mean_egg_temp_effect

# synthetic shuffle these
all_inputs$Q_free -> params$freeport_flows
all_inputs$Q_vern -> params$vernalis_flows
all_inputs$Q_stck -> params$stockton_flows
all_inputs$Temp_vern -> params$vernalis_temps
all_inputs$Temp_pp -> params$prisoners_point_temps
all_inputs$CVP_exp -> params$CVP_exports
all_inputs$SWP_exp -> params$SWP_exports

# spawn_decay_rate = DSMscenario::spawn_decay_rate
all_inputs$states$spwnDecay -> params$spawn_decay_rate
# rear_decay_rate = DSMscenario::rear_decay_rate
all_inputs$states$rearDecay -> params$rear_decay_rate

# confirmed they are the same
# all_inputs$dlt.gates$days_closed <- params$cc_gates_prop_days_closed
# all_inputs$dlt.gates$days_closed/days_in_month(1:12)

# TODO this is wierd
gro_dn <- dimnames(params$growth_rates)
structure(c(0.0045914028239995, 0, 0, 0, 0.995346079527392, 0.508197615704544,
            0, 0, 6.2517648608762e-05, 0.491802384294065, 0.750034293109025,
            0, 0, 1.39133149446025e-12, 0.249965706890975, 1), .Dim = c(4L, 4L)) -> params$growth_rates
# growthMatrices<-Hab.growth(daily.rates=c(grow.ic,grow.fp),wks.fld=fp.weeks[,mnth,yr+1])
# trans_mat_river<-growthMatrices$T.mtx.ic
# trans_mat_flood<-growthMatrices$T.mtx.fp
gro_dn -> dimnames(params$growth_rates)

structure(c(0.0045914028239995, 0, 0, 0, 0.995346079527392, 0.508197615704544,
            0, 0, 6.2517648608762e-05, 0.491802384294065, 0.750034293109025,
            0, 0, 1.39133149446025e-12, 0.249965706890975, 1, 0.00353734264565751,
            0, 0, 0, 0.898316948523529, 0.389212293741988, 0, 0, 0.0980860690180065,
            0.603923710042661, 0.57993723717849, 0, 5.96398128066e-05, 0.00686399621535116,
            0.42006276282151, 1, 0.00248328246731551, 0, 0, 0, 0.801287817519667,
            0.270226971754649, 0, 0, 0.196109620387404, 0.71604503581461,
            0.409840181211634, 0, 0.0001192796256132, 0.0137279924307407,
            0.590159818788366, 1, 0.00142922228897352, 0, 0, 0, 0.704258686515805,
            0.151241649742527, 0, 0, 0.294133171756802, 0.828166361609913,
            0.239743125208455, 0, 0.0001789194384198, 0.0205919886475598,
            0.760256874791545, 1, 0.000375162110631526, 0, 0, 0, 0.607229555511942,
            0.0322563277056229, 0, 0, 0.3921567231262, 0.940287687428568,
            0.0696460691689542, 0, 0.0002385592512264, 0.0274559848658086,
            0.930353930831046, 1), .Dim = c(4L, 4L, 5L)) -> params$growth_rates_floodplain

adam_grand_tab <- structure(c(56214.27, 3090.42, 7366.8125, NA, 2056.035, 9149.258767908,
                              12091.56, 54067.065431528, 119.56, 311.5855914725, 468.509155785,
                              1741.09, 1157.205, 27470.52, 2226.66, 4948.7875, 252, 1010.22,
                              5510.3091844135, 6520.04, 32765.41695332, 1183.84, 233.175268265,
                              605.543454315, 1184.425, 1061.44, 35917.56, 3765.675, 11525.66,
                              808.92, 2056.035, 5000.5560011675, 7528.36, 20696.397956436,
                              362.6, 1527.9734518675, 359.622442899, 420.965, 357.38, 34944.84,
                              1699.2, 6548.105, 1600.2, 1189.0425, 7772.749849607, 3467.94,
                              8103.02662535999, 519.4, 254.319625085, 264.6243413505, 567.285,
                              300.67, 10748.43, 1084.125, 3232.0675, 472.92, 721.2225, 2228.581307149,
                              1112.28, 3341.59044208, 75.46, 69.0128312875, 89.8130199825,
                              130.685, 119.84, 15588.09, 243.375, 5968.8675, 162.96, 184.755,
                              604.208738697, 1498.22, 830.114463575999, 14.7, 25.40259534625,
                              72.0355974705, 255.175, 207.58, 3671.01, 270.81, 2509.77, 48.72,
                              86.445, 493.09744725, 1979.38, 1749.051835148, 0, 99.84835165,
                              66.294971451, 175.525, 66.34, 10314.36, 327.45, 5591.78, 139.44,
                              122.04, 4568.6026373765, 6138.7, 4849.928894592, 725.2, 281.9247576,
                              120.5531464095, 320.37, 288.9, 7532.91, 368.16, 3763.8775, 556.08,
                              1043.2725, 4810.220386529, 3812.48, 8461.62022418399, 51.94,
                              392.6389592825, 290.9201121495, 386.155, 477.755, 18081.63, 719.505,
                              5933.1025, 733.32, 754.275, 6474.3694823925, 3274.74, 12655.778504352,
                              1049.58, 803.33872334875, 372.3999653295, 1181.77, 418.905, 25252.92,
                              1947, 10369.5175, 861.84, 1861.9575, 15380.8051518325, 6353.98,
                              19226.692515952, 0, 1038.27602134875, 523.322875197, 839.275,
                              1030.41, 21971.88, 1249.62, 12279.835, 713.16, 2108.58, 6225.1909057155,
                              4960.18, 8741.9572089, 365.54, 484.11766969125, 170.737328709,
                              902.7, 234.33, 18055.8, 72.57, 6848.9975, 514.08, 875.4675, 2092.048102866,
                              2778.86, 5194.654232688, 199.92, 672.65485133625, 230.9213111715,
                              1810.12, 60.455, 2703.33, 554.01, 1928.9775, 212.52, 510.195,
                              3944.2316919475, 1732.82, 4778.936607532, 1223.04, 291.32224952,
                              470.5461521145, 2752.35, 720.645), .Dim = 13:14, .Dimnames = list(
                                c("1", "6", "7", "10", "12", "19", "20", "23", "26", "27",
                                  "28", "29", "30"), NULL))
