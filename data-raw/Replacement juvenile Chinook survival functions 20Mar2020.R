
## this replaces the function "Juv.OUTM.S"

New.Juv.OUTM.S<-function(Q.cms){

  rep((Q.cms <= 122)*0.03 + (Q.cms > 122 & Q.cms <= 303)*0.189 + (Q.cms > 303)*0.508,4)

}
### try it out
New.Juv.OUTM.S(500)

# this replaces the function "Juv.DLT.S" for the north delta only

New.ND_juv_surv<-function(aveT){
  rep((aveT <= 16.5)*.42 + (aveT > 16.5 & aveT < 19.5)*0.42/(1.55^(aveT-15.5)) + (aveT > 19.5 & aveT < 25)*0.035,4)
}
### try it out
New.ND_juv_surv(15)
