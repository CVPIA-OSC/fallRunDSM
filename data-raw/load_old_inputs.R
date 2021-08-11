all_inputs <- read_rds('data-raw/misc/all_old_inputs.rds')
params <- fallRunDSM::params

all_inputs$territory_size -> params$territory_size
spawn_dimnames <- dimnames(params$spawning_habitat)
all_inputs$IChab.spawn -> params$spawning_habitat
dimnames(params$spawning_habitat) <- spawn_dimnames

rear_dimnames <- dimnames(params$inchannel_habitat_fry)
all_inputs$IChab.fry -> params$inchannel_habitat_fry
dimnames(params$inchannel_habitat_fry) <- rear_dimnames
all_inputs$IChab.juv -> params$inchannel_habitat_juvenile
dimnames(params$inchannel_habitat_juvenile) <- rear_dimnames

bypass_dimnames <- dimnames(params$sutter_habitat)
all_inputs$IChab.sutter -> params$sutter_habitat
dimnames(params$sutter_habitat) <- bypass_dimnames
all_inputs$IChab.yolo -> params$yolo_habitat
dimnames(params$yolo_habitat) <- bypass_dimnames

delta_dimnames <- dimnames(params$delta_habitat)
all_inputs$DLThab -> params$delta_habitat
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
all_inputs$retQ -> params$prop_flow_natal
dimnames(params$prop_flow_natal) <- retq_dimnames

overtop_dimnames <- dimnames(params$gates_overtopped)
all_inputs$gate.top -> params$gates_overtopped
dimnames(params$gates_overtopped) <- overtop_dimnames

mc_dimnames <- dimnames(params$migratory_temperature_proportion_over_20)
all_inputs$ptemp20mc -> params$migratory_temperature_proportion_over_20
mc_dimnames -> dimnames(params$migratory_temperature_proportion_over_20)

dimnames_31_12_21 <- dimnames(params$degree_days)
all_inputs$DegDay -> params$degree_days
dimnames(params$degree_days) <- dimnames_31_12_21
all_inputs$juv.tmp -> params$avg_temp
dimnames(params$avg_temp) <- dimnames_31_12_21
all_inputs$p.diver -> params$proportion_diverted
dimnames(params$proportion_diverted) <- dimnames_31_12_21
all_inputs$t.diver -> params$total_diverted
dimnames(params$total_diverted) <- dimnames_31_12_21

dimnames_31_12 <- dimnames(params$prop_pulse_flows)
all_inputs$prop.pulse -> params$prop_pulse_flows
dimnames(params$prop_pulse_flows) <- dimnames_31_12

all_inputs$dlt.divers -> params$delta_proportion_diverted
dimnames(params$delta_proportion_diverted) <- delta_dimnames
all_inputs$dlt.divers.tot -> params$delta_total_diverted
dimnames(params$delta_total_diverted) <- delta_dimnames

all_inputs$fp.weeks -> params$weeks_flooded
dimnames(params$weeks_flooded) <- dimnames_31_12_21

upsq_dn <- dimnames(params$upper_sacramento_flows)
all_inputs$upSacQ -> params$upper_sacramento_flows
upsq_dn -> dimnames(params$upper_sacramento_flows)

all_inputs$Dlt.inf -> params$delta_inflow
dimnames(params$delta_inflow) <- delta_dimnames
all_inputs$juv.tmp.dlt -> params$avg_temp_delta
dimnames(params$avg_temp_delta) <- delta_dimnames

pqb_dn <- dimnames(params$proportion_flow_bypass)
all_inputs$prop.Q.bypasses[,,1] -> params$proportion_flow_bypass[,,1]
all_inputs$prop.Q.bypasses[,,5] -> params$proportion_flow_bypass[,,2]
pqb_dn -> dimnames(params$proportion_flow_bypass)

all_inputs$inps$hatch.alloc -> params$hatchery_allocation
dimnames(params$hatchery_allocation) <- fallRunDSM::watershed_labels
all_inputs$inps$prop.nat.remov -> params$natural_adult_removal_rate
dimnames(params$natural_adult_removal_rate) <- fallRunDSM::watershed_labels
all_inputs$inps$SCDELT -> params$south_delta_routed_watersheds
dimnames(params$south_delta_routed_watersheds) <- fallRunDSM::watershed_labels
all_inputs$inps$TISD -> params$tisdale_bypass_watershed
dimnames(params$tisdale_bypass_watershed) <- fallRunDSM::watershed_labels
all_inputs$inps$YOLO -> params$yolo_bypass_watershed
dimnames(params$yolo_bypass_watershed) <- fallRunDSM::watershed_labels
all_inputs$inps$A.HARV -> params$adult_harvest_rate
dimnames(params$adult_harvest_rate) <- fallRunDSM::watershed_labels
all_inputs$inps$P.scour.nst -> params$prob_nest_scoured
dimnames(params$prob_nest_scoured) <- fallRunDSM::watershed_labels
all_inputs$inps$P.strand.early -> params$prob_strand_early
dimnames(params$prob_strand_early) <- fallRunDSM::watershed_labels
all_inputs$inps$P.strand.late -> params$prob_strand_late
dimnames(params$prob_strand_late) <- fallRunDSM::watershed_labels
all_inputs$inps$High.pred -> params$prop_high_predation
dimnames(params$prop_high_predation) <- fallRunDSM::watershed_labels
all_inputs$inps$contact -> params$contact_points
dimnames(params$contact_points) <- fallRunDSM::watershed_labels

all_inputs$Dlt.inp$contct.pts -> params$delta_contact_points
names(params$delta_contact_points) <- c("North Delta", "South Delta")
all_inputs$Dlt.inp$High.pred -> params$delta_prop_high_predation
names(params$delta_prop_high_predation) <- c("North Delta", "South Delta")

all_inputs$egg.tmp.eff$mean_temp_effect -> params$mean_egg_temp_effect

# synthetic shuffle these
all_inputs$Q_free[,1:21] -> params$freeport_flows
all_inputs$Q_vern[,1:21] -> params$vernalis_flows
all_inputs$Q_stck[,1:21] -> params$stockton_flows
all_inputs$Temp_vern[,1:21] -> params$vernalis_temps
all_inputs$Temp_pp[,1:21] -> params$prisoners_point_temps
all_inputs$CVP_exp[,1:21] -> params$CVP_exports
all_inputs$SWP_exp[,1:21] -> params$SWP_exports

spawn_decay_rate = DSMscenario::spawn_decay_rate
states$spwnDecay <- spawn_decay_rate
rear_decay_rate = DSMscenario::rear_decay_rate
states$rearDecay <- rear_decay_rate

# confirmed they are the same
# all_inputs$dlt.gates$days_closed <- params$cc_gates_prop_days_closed
# all_inputs$dlt.gates$days_closed/days_in_month(1:12)

# TODO this is wierd
# growthMatrices <- params$growth_rates
# growthMatrices<-Hab.growth(daily.rates=c(grow.ic,grow.fp),wks.fld=fp.weeks[,mnth,yr+1])
# trans_mat_river<-growthMatrices$T.mtx.ic
# trans_mat_flood<-growthMatrices$T.mtx.fp
