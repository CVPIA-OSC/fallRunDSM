scale_habitat_params <- function(params) {
  vect2 <- c(2.0000000, 0.5059781, 1.6702959, 0.8441507, 1.6434544, 2.0000000, 0.5000000,
             1.0815585, 1.9624035, 0.6232790, 1.0783194, 1.9318056, 1.2704583, 0.9537940,
             0.9066874, 2.0000000, 1.0847540, 1.4589099, 2.0000000, 0.5769185, 1.0589013,
             0.5709694, 2.0000000, 0.6716419, 0.5237730, 1.8253104, 1.0990632, 2.0000000,
             1.4615010, 1.1809537, 0.9577044, 0.9697722, 1.1437721, 1.7819260)

  params$spawning_habitat[1,,]<-params$spawning_habitat[1,,]*vect2[1] # Upper Sac
  params$spawning_habitat[6,,]<-params$spawning_habitat[6,,]*vect2[2] #Butte
  params$spawning_habitat[7,,]<-params$spawning_habitat[7,,]*vect2[3] #Clear
  params$spawning_habitat[10,,]<-params$spawning_habitat[10,,]*vect2[4] # Deer
  params$spawning_habitat[12,,]<-params$spawning_habitat[12,,]*vect2[5] # Mill
  params$spawning_habitat[19,,]<-params$spawning_habitat[19,,]*vect2[6] # Feather
  params$spawning_habitat[20,,]<-params$spawning_habitat[20,,]*vect2[7]# Yuba
  params$spawning_habitat[23,,]<-params$spawning_habitat[23,,]*vect2[8] # American
  params$spawning_habitat[26,,]<-params$spawning_habitat[26,,]*vect2[9] # Cosumness
  params$spawning_habitat[27,,]<-params$spawning_habitat[27,,]*vect2[10] # Mokelumne
  params$spawning_habitat[28,,]<-params$spawning_habitat[28,,]*vect2[11] # Merced
  params$spawning_habitat[29,,]<-params$spawning_habitat[29,,]*vect2[12] # Stanislaus
  params$spawning_habitat[30,,]<-params$spawning_habitat[30,,]*vect2[13] # Tuolumne



  params$inchannel_habitat_fry[1,,]<-params$inchannel_habitat_fry[1,,]*vect2[14] # Upper Sac
  params$inchannel_habitat_fry[6,,]<-params$inchannel_habitat_fry[6,,]*vect2[15] # Butte
  params$inchannel_habitat_fry[7,,]<-params$inchannel_habitat_fry[7,,]*vect2[16] # Clear
  params$inchannel_habitat_fry[10,,]<-params$inchannel_habitat_fry[10,,]*vect2[17] # Deer
  params$inchannel_habitat_fry[12,,]<-params$inchannel_habitat_fry[12,,]*vect2[18] # Mill
  params$inchannel_habitat_fry[16,,]<-params$inchannel_habitat_fry[16,,]*vect2[19] # Upper-mid Sac (corridor for above)
  # Sutter (corridor for above) is changed below
  params$inchannel_habitat_fry[19,,]<-params$inchannel_habitat_fry[19,,]*vect2[20] # Feather
  params$inchannel_habitat_fry[20,,]<-params$inchannel_habitat_fry[20,,]*vect2[21] # Yuba
  params$inchannel_habitat_fry[21,,]<-params$inchannel_habitat_fry[21,,]*vect2[22] # Lower-mid Sac (corridor for above)
  # Yolo (corridor for above) is changed below
  params$inchannel_habitat_fry[23,,]<-params$inchannel_habitat_fry[23,,]*vect2[23] # American
  params$inchannel_habitat_fry[24,,]<-params$inchannel_habitat_fry[24,,]*vect2[24] # Lower Sac (corridor for above)
  params$inchannel_habitat_fry[26,,]<-params$inchannel_habitat_fry[26,,]*vect2[25] # Cosumness
  params$inchannel_habitat_fry[27,,]<-params$inchannel_habitat_fry[27,,]*vect2[26] # Mokelumne
  params$inchannel_habitat_fry[28,,]<-params$inchannel_habitat_fry[28,,]*vect2[27] # Merced
  params$inchannel_habitat_fry[29,,]<-params$inchannel_habitat_fry[29,,]*vect2[28] # Stanislaus
  params$inchannel_habitat_fry[30,,]<-params$inchannel_habitat_fry[30,,]*vect2[29] # Tuolumne
  params$inchannel_habitat_fry[31,,]<-params$inchannel_habitat_fry[31,,]*vect2[30] # SJ (corridor for Merced, Stan, and Tuolumne)



  params$inchannel_habitat_juvenile[1,,]<-params$inchannel_habitat_juvenile[1,,]*vect2[14] # Upper Sac
  params$inchannel_habitat_juvenile[6,,]<-params$inchannel_habitat_juvenile[6,,]*vect2[15] # Butte
  params$inchannel_habitat_juvenile[7,,]<-params$inchannel_habitat_juvenile[7,,]*vect2[16] # Clear
  params$inchannel_habitat_juvenile[10,,]<-params$inchannel_habitat_juvenile[10,,]*vect2[17] # Deer
  params$inchannel_habitat_juvenile[12,,]<-params$inchannel_habitat_juvenile[12,,]*vect2[18] # Mill
  params$inchannel_habitat_juvenile[16,,]<-params$inchannel_habitat_juvenile[16,,]*vect2[19] # Upper-mid Sac (corridor for above)
  # Sutter (corridor for above) is changed below
  params$inchannel_habitat_juvenile[19,,]<-params$inchannel_habitat_juvenile[19,,]*vect2[20] # Feather
  params$inchannel_habitat_juvenile[20,,]<-params$inchannel_habitat_juvenile[20,,]*vect2[21] # Yuba
  params$inchannel_habitat_juvenile[21,,]<-params$inchannel_habitat_juvenile[21,,]*vect2[22] # Lower-mid Sac (corridor for above)
  # Yolo (corridor for above) is changed below
  params$inchannel_habitat_juvenile[23,,]<-params$inchannel_habitat_juvenile[23,,]*vect2[23] # American
  params$inchannel_habitat_juvenile[24,,]<-params$inchannel_habitat_juvenile[24,,]*vect2[24] # Lower Sac (corridor for above)
  params$inchannel_habitat_juvenile[26,,]<-params$inchannel_habitat_juvenile[26,,]*vect2[25] # Cosumness
  params$inchannel_habitat_juvenile[27,,]<-params$inchannel_habitat_juvenile[27,,]*vect2[26] # Mokelumne
  params$inchannel_habitat_juvenile[28,,]<-params$inchannel_habitat_juvenile[28,,]*vect2[27] # Merced
  params$inchannel_habitat_juvenile[29,,]<-params$inchannel_habitat_juvenile[29,,]*vect2[28] # Stanislaus
  params$inchannel_habitat_juvenile[30,,]<-params$inchannel_habitat_juvenile[30,,]*vect2[29] # Tuolumne
  params$inchannel_habitat_juvenile[31,,]<-params$inchannel_habitat_juvenile[31,,]*vect2[30] # SJ (corridor for Merced, Stan, and Tuolumne)

  params$sutter_habitat<-params$sutter_habitat*vect2[31]
  params$yolo_habitat<-params$yolo_habitat*vect2[32]
  params$delta_habitat[,,"North Delta"]<-params$delta_habitat[,,"North Delta"]*vect2[33]
  params$delta_habitat[,,"South Delta"]<-params$delta_habitat[,,"South Delta"]*vect2[34]

  return(params)
}
