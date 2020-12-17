vect2<-c(2.0000000, 0.5059781, 1.6702959, 0.8441507, 1.6434544, 2.0000000, 0.5000000,
         1.0815585, 1.9624035, 0.6232790, 1.0783194, 1.9318056, 1.2704583, 0.9537940,
         0.9066874, 2.0000000, 1.0847540, 1.4589099, 2.0000000, 0.5769185, 1.0589013,
         0.5709694, 2.0000000, 0.6716419, 0.5237730, 1.8253104, 1.0990632, 2.0000000,
         1.4615010, 1.1809537, 0.9577044, 0.9697722, 1.1437721, 1.7819260)
# # Scale some of the inputs
# IChab.spawn[1,,]<-IChab.spawn[1,,]*vect2[1] # Upper Sac
# IChab.spawn[6,,]<-IChab.spawn[6,,]*vect2[2] #Butte
# IChab.spawn[7,,]<-IChab.spawn[7,,]*vect2[3] #Clear
# IChab.spawn[10,,]<-IChab.spawn[10,,]*vect2[4] # Deer
# IChab.spawn[12,,]<-IChab.spawn[12,,]*vect2[5] # Mill
# IChab.spawn[19,,]<-IChab.spawn[19,,]*vect2[6] # Feather
# IChab.spawn[20,,]<-IChab.spawn[20,,]*vect2[7]# Yuba
# IChab.spawn[23,,]<-IChab.spawn[23,,]*vect2[8] # American
# IChab.spawn[26,,]<-IChab.spawn[26,,]*vect2[9] # Cosumness
# IChab.spawn[27,,]<-IChab.spawn[27,,]*vect2[10] # Mokelumne
# IChab.spawn[28,,]<-IChab.spawn[28,,]*vect2[11] # Merced
# IChab.spawn[29,,]<-IChab.spawn[29,,]*vect2[12] # Stanislaus
# IChab.spawn[30,,]<-IChab.spawn[30,,]*vect2[13] # Tuolumne
#
# IChab.fry[1,,]<-IChab.fry[1,,]*vect2[14] # Upper Sac
# IChab.fry[6,,]<-IChab.fry[6,,]*vect2[15] # Butte
# IChab.fry[7,,]<-IChab.fry[7,,]*vect2[16] # Clear
# IChab.fry[10,,]<-IChab.fry[10,,]*vect2[17] # Deer
# IChab.fry[12,,]<-IChab.fry[12,,]*vect2[18] # Mill
# IChab.fry[16,,]<-IChab.fry[16,,]*vect2[19] # Upper-mid Sac (corridor for above)
# # Sutter (corridor for above) is changed below
# IChab.fry[19,,]<-IChab.fry[19,,]*vect2[20] # Feather
# IChab.fry[20,,]<-IChab.fry[20,,]*vect2[21] # Yuba
# IChab.fry[21,,]<-IChab.fry[21,,]*vect2[22] # Lower-mid Sac (corridor for above)
# # Yolo (corridor for above) is changed below
# IChab.fry[23,,]<-IChab.fry[23,,]*vect2[23] # American
# IChab.fry[24,,]<-IChab.fry[24,,]*vect2[24] # Lower Sac (corridor for above)
# IChab.fry[26,,]<-IChab.fry[26,,]*vect2[25] # Cosumness
# IChab.fry[27,,]<-IChab.fry[27,,]*vect2[26] # Mokelumne
# IChab.fry[28,,]<-IChab.fry[28,,]*vect2[27] # Merced
# IChab.fry[29,,]<-IChab.fry[29,,]*vect2[28] # Stanislaus
# IChab.fry[30,,]<-IChab.fry[30,,]*vect2[29] # Tuolumne
# IChab.fry[31,,]<-IChab.fry[31,,]*vect2[30] # SJ (corridor for Merced, Stan, and Tuolumne)
#
# IChab.juv[1,,]<-IChab.juv[1,,]*vect2[14] # Upper Sac
# IChab.juv[6,,]<-IChab.juv[6,,]*vect2[15] # Butte
# IChab.juv[7,,]<-IChab.juv[7,,]*vect2[16] # Clear
# IChab.juv[10,,]<-IChab.juv[10,,]*vect2[17] # Deer
# IChab.juv[12,,]<-IChab.juv[12,,]*vect2[18] # Mill
# IChab.juv[16,,]<-IChab.juv[16,,]*vect2[19] # Upper-mid Sac (corridor for above)
# # Sutter (corridor for above) is changed below
# IChab.juv[19,,]<-IChab.juv[19,,]*vect2[20] # Feather
# IChab.juv[20,,]<-IChab.juv[20,,]*vect2[21] # Yuba
# IChab.juv[21,,]<-IChab.juv[21,,]*vect2[22] # Lower-mid Sac (corridor for above)
# # Yolo (corridor for above) is changed below
# IChab.juv[23,,]<-IChab.juv[23,,]*vect2[23] # American
# IChab.juv[24,,]<-IChab.juv[24,,]*vect2[24] # Lower Sac (corridor for above)
# IChab.juv[26,,]<-IChab.juv[26,,]*vect2[25] # Cosumness
# IChab.juv[27,,]<-IChab.juv[27,,]*vect2[26] # Mokelumne
# IChab.juv[28,,]<-IChab.juv[28,,]*vect2[27] # Merced
# IChab.juv[29,,]<-IChab.juv[29,,]*vect2[28] # Stanislaus
# IChab.juv[30,,]<-IChab.juv[30,,]*vect2[29] # Tuolumne
# IChab.juv[31,,]<-IChab.juv[31,,]*vect2[30] # SJ (corridor for Merced, Stan, and Tuolumne)
# IChab.sutter<-IChab.sutter*vect2[31]
# IChab.yolo<-IChab.yolo*vect2[32]
# DLThab[,,1]<-DLThab[,,1]*vect2[33]
# DLThab[,,2]<-DLThab[,,2]*vect2[34]

# TODO
# ingrate calib scaling if needed
juv_scalers
fry_scalers
spawn_scalers
watershed_attributes # SIT metrics



