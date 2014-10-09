setwd("C:/Users/jbleich/workspace/the_pen/data/letters")

library(plyr)
library(signal)
library(dtw)
##GLOBAL CONSTANTS
NUM_PCS = 2
COLS_TO_KEEP = 1:17
FILTER_NUM = 5
ACCEL_COLS  = 1:3
MAG_COLS  = 4:6

training = buildTrainingAlphabet(list.files())


plot(training[[21]][,1], type = "l")
plot(training[[23]][,1], type = "l")
file_list = list.files()
labels[11]

buildTrainingAlphabet = function(file_list){
  training_list = lapply(file_list, getTrainingData) ##get all of the PCA data
  names(training_list) = substr(list.files(), LETTER_LOC ,LETTER_LOC + 2)
  labels = sapply(training_list, function(s) s$label)
  raw_dat_list = lapply(training_list, function(s) s$raw_dat)  
  ##fix the acccelerations
  raw_dat_list_fixed_accel = orientAcceleration(raw_dat_list)
  ##do the pca
  raw_dat_list_fixed_accel_smooth = lapply(raw_dat_list_fixed_accel, smoothData, FILTER_NUM)
  
  pc_dat = lapply(raw_dat_list_fixed_accel_smooth, getPCA)
  #reorient the pca
  dat_oriented = unlist(lapply(unique(labels), orientPCDirection, pc_dat, labels), recursive = FALSE)
  dat_oriented
}

getTrainingData = function(file){
  raw_dat = read.csv(file, header = F)
  rm_ix = c(1:5, nrow(raw_dat) : (nrow(raw_dat) - 5))
  raw_dat = raw_dat[-rm_ix, COLS_TO_KEEP]
  #datf = apply(raw_dat, 2, function(s) fftfilt(rep(1, FILTER_NUM)/FILTER_NUM, s))
  #datf = datf[- (1 : FILTER_NUM), ]
#   pca_obj = prcomp(datf[,1:3], scale = T)
#   pc_dat = pca_obj$x[,1 : NUM_PCS]
#  # pca_obj$rotation[,1:4]
   label = substr(file, LETTER_LOC ,LETTER_LOC)
  list(raw_dat = raw_dat, label = label)
}

smoothData = function(dat, filter_num){
  datf = apply(dat, 2, function(s) fftfilt(rep(1, FILTER_NUM)/FILTER_NUM, s))
  datf = datf[ - c(1 : FILTER_NUM), ]
  datf
}


getPCA = function(dat){
    pca_obj = prcomp(dat[, 1:3], scale = T) ##HARD CODE
    pc_dat = pca_obj$x[,1 : NUM_PCS]
    pc_dat
}


orientPCDirection = function(letter, dat_list, labels){
##Reorients all of the PC directions for a particular letter   
  letter_list = dat_list[which(labels == letter)]
  ref_pc_mat = letter_list[[1]]
  letter_list_fixed = lapply( letter_list, fixFullPCMatrix, ref_pc_mat)
  letter_list_fixed
}

getMinDistancePCOrientation = function(test_col, ref_col){
  ##Returns the min DTW distance between base col and ref col by attempting a sign flip of ref col
  ##This is meant to orient an individual principle component
  d1 = dtw(ref_col, test_col)$distance
  d2 = dtw(ref_col, -1 * test_col)$distance
  if(d1 < d2) return(test_col)
  return(-1 * test_col)  
}

fixFullPCMatrix = function(test_mat, ref_mat){
  ##Orients an entire PC matrix so that the DTW distances are minimized with respect to base_mat
  sapply(1 : ncol(ref_mat), function(s) getMinDistancePCOrientation(test_mat[,s], ref_mat[,s]))
}

orientAcceleration = function(dat_list){
  ##Reorients all of the accelerations to earth frame
  new_accel_list = lapply(dat_list, function(s) fixAcceleration(s[, ACCEL_COLS], s[, MAG_COLS])) ##BAD!
  lapply(1 : length(dat_list), function(s) dat_list[[s]][ ,ACCEL_COLS] = new_accel_list[[s]])
}


fixAcceleration = function(accel_vec, mag_vec){
  Rmat = createRotationMatrix(data.matrix(accel_vec[1,]), data.matrix(mag_vec[1, ])) ##assumes person does not rotate pen once writing letter
  new_accel = t(apply(accel_vec, 1, function(s) Rmat %*% s))
  new_accel
}


normalize = function(vec) (vec/sqrt(sum(vec^2)))

createRotationMatrix = function(accel, mag){
  accel_n = normalize(accel)
  mag_n = normalize(mag)
  E = CrossProduct3D(accel_n, mag_n)
  En = normalize(E)
  N = CrossProduct3D(En, accel_n)
  Nn = normalize(N)
  Up = CrossProduct3D(En, Nn)
  Rmat = rbind(Nn, En, Up)
  Rmat
}

CrossProduct3D <- function(x, y, i=1:3) {
  # Project inputs into 3D, since the cross product only makes sense in 3D.
  To3D <- function(x) head(c(x, rep(0, 3)), 3)
  x <- To3D(x)
  y <- To3D(y)
  
  # Indices should be treated cyclically (i.e., index 4 is "really" index 1, and
  # so on).  Index3D() lets us do that using R's convention of 1-based (rather
  # than 0-based) arrays.
  Index3D <- function(i) (i - 1) %% 3 + 1
  
  # The i'th component of the cross product is:
  # (x[i + 1] * y[i + 2]) - (x[i + 2] * y[i + 1])
  # as long as we treat the indices cyclically.
  return (x[Index3D(i + 1)] * y[Index3D(i + 2)] -
            x[Index3D(i + 2)] * y[Index3D(i + 1)])
}


