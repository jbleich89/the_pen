setwd("C:/Users/jbleich/workspace/the_pen/data/letters")

library(plyr)
library(signal)
library(dtw)

training = buildTrainingAlphabet(list.files())

buildTrainingAlphabet = function(file_list){
  training_list = lapply(file_list, getTrainingData)
  names(training_list) = substr(list.files(), LETTER_LOC ,LETTER_LOC + 2)
  labels = sapply(training_list, function(s) s$label)
  dat_list = lapply(training_list, function(s) s$pc_dat)  
  dat_oriented = unlist(lapply(unique(labels), orientPCDirection, dat_list, labels), recursive = FALSE)
  dat_oriented
}

getTrainingData = function(file){
  raw_dat = read.csv(file, header = F)
  rm_ix = c(1:5, nrow(raw_dat) : (nrow(raw_dat) - 5))
  raw_dat = raw_dat[-rm_ix, COLS_TO_KEEP]
  datf = apply(raw_dat, 2, function(s) fftfilt(rep(1, FILTER_NUM)/FILTER_NUM, s))
  datf = datf[- (1 : FILTER_NUM), ]
  pca_obj = prcomp(datf, scale = T)
  pc_dat = pca_obj$x[,1 : NUM_PCS]
 # pca_obj$rotation[,1:4]
  label = substr(file, LETTER_LOC ,LETTER_LOC)
  list(pc_dat = pc_dat, label = label)
}

orientPCDirection = function(letter, dat_list, labels){
##Reorients all of the PC directions for a particular letter   
  letter_list = dat_list[which(labels == letter)]
  base_pc_mat = letter_list[[1]]
  letter_list_fixed = lapply( letter_list, fixFullPCMatrix, base_pc_mat)
  letter_list_fixed
}

getMinDistancePCOrientation = function(base_col, ref_col){
  ##Returns the min DTW distance between base col and ref col by attempting a sign flip of ref col
  ##This is meant to orient an individual principle component
  d1 = dtw(base_col, ref_col)$distance
  d2 = dtw(base_col, -1 * ref_col)$distance
  if(d1 < d2) return(ref_col)
  return(-1 * ref_col)  
}

fixFullPCMatrix = function(ref_mat, base_mat){
  ##Orients an entire PC matrix so that the DTW distances are minimized with respect to base_mat
  sapply(1 : ncol(base_mat), function(s) getMinDistancePCOrientation(base_mat[,s], ref_mat[,s]))
}

##Scratch Work 

# 
# temp = letter_list
# 
# temp[[2]][,1] = -temp[[2]][,1]
# temp2 = fixFullPCMatrix(base_pc_mat, temp[[2]])
# class(temp2)
# 
# letter_list[[2]] = 
# 
# 
# plot(temp2[,1], type = "l")
# plot(temp[[2]][,1], type = "l")
# 
# head(dat)
# pc = prcomp(dat, scale = T)
# pc$sdev^2/sum(pc$sdev^2)
# 
# pc$rotation
# 
# 
# library(wavethresh)
# wy <- wd(dat[1:256,1])
# thresh <- threshold(wy, type="soft")
# yr <- wr(thresh)
# plot(yr,type = "l")
# plot(dat[1:256,1], type = "l")
# d = daubcqf(6)
# plot(denoise.dwt(dat[1:256,1], d$h.0))
# z <- fftfilt(rep(1, 5)/5, dat[,2])
# plot(dat[,1],type = "l")
# lines(z, col = "red", lwd = 3)
# 
# z <- fftfilt(rep(1, 5)/5, dat2[,2])
# plot(dat2[,2],type = "l")
# lines(z, col = "red", lwd = 3)
# par(mfrow = c(1,2))
# 
# 
# t <- seq(0, 1, len = 100)                     # 1 second sample
# x <- sin(2*pi*t*2.3) + 0.25*rnorm(length(t))  # 2.3 Hz sinusoid+noise
# z <- fftfilt(rep(1, 5)/5, x) # apply 10-point averaging filter
# plot(t, x, type = "l")
# lines(t, z, col = "red")
# 
# datf = apply(dat, 2, function(s) fftfilt(rep(1,5)/5, s))
# pcf = prcomp(datf, scale = T)
# pcf$sdev^2/sum(pcf$sdev^2)
