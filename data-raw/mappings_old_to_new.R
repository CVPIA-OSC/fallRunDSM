territory_size <- params$territory_size

spawn_decay_rate = DSMscenario::spawn_decay_rate
states$spwnDecay <- spawn_decay_rate
rear_decay_rate = DSMscenario::rear_decay_rate
states$rearDecay <- rear_decay_rate

# theirs 31, 12, 22 : ours 31, 12, 22
IChab.spawn <- params$spawning_habitat

# theirs 31, 12, 21 : ours 31, 12, 21
IChab.fry <- params$inchannel_habitat_fry
IChab.juv <- params$inchannel_habitat_juvenile

# theirs 12, 21 : ours 12, 21
IChab.sutter <- params$sutter_habitat
IChab.yolo <- params$yolo_habitat

# theirs 12, 21, 2 : ours 12, 21, 2
DLThab <- params$delta_habitat

cc.aloc <- params$cross_channel_stray_rate
oth.aloc <- params$stray_rate

returnProps <- params$month_return_proportions

inps$hatch.alloc <- params$hatchery_allocation

inps$prop.nat.remov <- params$natural_adult_removal_rate

prop.hatch <- params$proportion_hatchery

retQ <- params$prop_flow_natal

inps$SCDELT <- params$south_delta_routed_watersheds

dlt.gates$days_closed <- params$cc_gates_prop_days_closed # TODO

# theirs 12 20  2, ours 12 20  2
gate.top <- params$gates_overtopped

inps$TISD <- params$tisdale_bypass_watershed
inps$YOLO <- params$yolo_bypass_watershed

# theirs is 31 14, ours 31 12
ptemp20mc <- params$migratory_temperature_proportion_over_20

inps$A.HARV <- params$adult_harvest_rate

inps$P.scour.nst <- params$prob_nest_scoured
egg.tmp.eff$mean_temp_effect <- params$mean_egg_temp_effect

# theirs 31 12 22, ours 31 12 22
DegDay <- params$degree_days

# TODO this is wierd
# growthMatrices <- params$growth_rates
# growthMatrices<-Hab.growth(daily.rates=c(grow.ic,grow.fp),wks.fld=fp.weeks[,mnth,yr+1])
# trans_mat_river<-growthMatrices$T.mtx.ic
# trans_mat_flood<-growthMatrices$T.mtx.fp

inps$P.strand.early <- params$prob_strand_early
inps$P.strand.late <- params$prob_strand_late

# theirs 31 13, ours 31 12
prop.pulse <- params$prop_pulse_flows

juv.tmp <- params$avg_temp

p.diver <- params$proportion_diverted
t.diver <- params$total_diverted

dlt.divers <- params$delta_proportion_diverted
dlt.divers.tot <- params$delta_total_diverted

inps$High.pred <- params$prop_high_predation

inps$contact <- params$contact_points

Dlt.inp$contct.pts <- params$delta_contact_points
Dlt.inp$High.pred <- params$delta_prop_high_predation

fp.weeks <- params$weeks_flooded

Q_free <- params$freeport_flows
Q_vern <- params$vernalis_flows
Q_stck <- params$stockton_flows
Temp_vern <- params$vernalis_temps
Temp_pp <- params$prisoners_point_temps
CVP_exp <- params$CVP_exports
SWP_exp <- params$SWP_exports

upSacQ <- params$upper_sacramento_flows

Dlt.inf <- params$delta_inflow

juv.tmp.dlt <- params$avg_temp_delta

prop.Q.bypasses <- params$proportion_flow_bypass

# run in old code ---
all_inputs <- list(
  growthMatrices = growthMatrices,
  dlt.gates = dlt.gates,
  territory_size = territory_size,
  states = states,
  inps = inps,
  egg.tmp.eff = egg.tmp.eff,
  IChab.spawn = IChab.spawn,
  IChab.fry = IChab.fry,
  IChab.juv = IChab.juv,
  IChab.sutter = IChab.sutter,
  IChab.yolo = IChab.yolo,
  DLThab = DLThab,
  cc.aloc = cc.aloc,
  oth.aloc = oth.aloc,
  returnProps = returnProps,
  Dlt.inp = Dlt.inp,
  prop.hatch = prop.hatch,
  retQ = retQ,
  gate.top = gate.top,
  ptemp20mc = ptemp20mc,
  DegDay = DegDay,
  prop.pulse = prop.pulse,
  juv.tmp = juv.tmp,
  p.diver = p.diver,
  t.diver = t.diver,
  dlt.divers = dlt.divers,
  dlt.divers.tot = dlt.divers.tot,
  fp.weeks = fp.weeks,
  Q_free = Q_free,
  Q_vern = Q_vern,
  Q_stck = Q_stck,
  Temp_vern = Temp_vern,
  Temp_pp = Temp_pp,
  CVP_exp = CVP_exp,
  SWP_exp = SWP_exp,
  upSacQ = upSacQ,
  Dlt.inf = Dlt.inf,
  juv.tmp.dlt = juv.tmp.dlt,
  prop.Q.bypasses = prop.Q.bypasses
)
write_rds(all_inputs, 'all_old_inputs.rds')
