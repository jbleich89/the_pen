setwd("C:/Users/jbleich/workspace/the_pen/data/letters")

library(plyr)
library(signal)
library(dtw)
NUM_PCS = 4

training = buildTrainingAlphabet(list.files())
names(training)[11]

a1 = read.csv(list.files()[2], header = F)
e2 = read.csv(list.files()[11], header = F)
e3 = read.csv(list.files()[15], header = F)

plot(e2[,1], type = "l")
plot(e3[,3], type = "l")

# spectrum(a1[,1])
# spectrum(e2[,3])
# spectrum(e3[,1])

# plot(a1[,3])
# 
# a1f = apply(a1[-c(1:5),1:3] ,2, fft)
# a2f = apply(a2[-c(1:5),1:3], 2, fft)
# plot(a1f[,1])
# plot(a2f[,1])
# a1r = Conj(a1f) %*% t(a1f)
# a2r = Conj(a2f) %*% t(a2f)
# dim(a1f)
# a1d = diag(Re(a1r))
# a2d = diag(Re(a2r))
# which.max(a1d)
# dtw(log(a1d[-1]),log(a2d[-1]))$distance
# plot(a1d[-1])
# plot(a2d[-1])
# 
# 
# theta1 = atan2(e2[,4], e2[,5])
# phi1 = atan2(e2[,6], e2[,5])
# 
# 
# theta2 = atan2(e3[,4], e3[,5])
# phi2 = atan2(e3[,6], e3[,5])
# 
# Ry(phi2)
# phi = phi2[1]
# theta = theta2[1]
# Rx(theta)
# 
# head(e2)
# test1 = as.numeric(e2[1, 4:6])
# test2 = as.numeric(e3[1, 4:6])
# theta1 = atan2(test1[3], test1[2])
# phi1 = atan2(test1[1], test1[2])
# theta2 = atan2(test2[3], test2[2])
# phi2 = atan2(test2[1], test2[2])
# 
# m1 = Rx(theta1)%*%Ry(phi1) %*%(test1)
# 
# m2 = Rx(theta2)%*%Ry(phi2) %*%(test2)
# 
# atan2(m1[2], m1[1])
# atan2(m2[2], m2[1])
# 
# accel = e2[,1:3]
# mag = e2[,4:6]




# accel_n = apply(accel, 1, function(s), s/sqrt(sum(s^2)))
# mag_n = apply(mag, 1, function(s), s/sqrt(sum(s^2)))
# ex = (accel_n[,3] * mag_n[,2] - accel_n[,2]*mag_n[,3])
# ey = (accel_n[,1] * mag_n[,2] - accel_n[,2]*mag_n[,1])
# ez = (accel_n[,1] * mag_n[,3] - accel_n[,3]*mag_n[,1])
# e_mat = cbind(ex,ey,ez)
# ue = apply(e_mat, 1, function(s), s/sqrt(sum(s^2)))
# 
# CrossProduct3D()
# 
# Ry = function(phi){
#   mat = matrix(c(cos(phi),0 , sin(phi), 0, 1, 0, -sin(phi), 0, cos(phi)), ncol = 3)
#   mat
# }
# 
# a1 = c(-0.02531, -.15889, -.97099)
# b1 = c(4.117188,12.433563,-35.76912) 
# 
# 
# 
# 
# normalize = function(vec) (vec/sqrt(sum(vec^2)))
# a1n = normalize(a1)
# b1n = normalize(b1)
# E = CrossProduct3D(a1n, b1n)
# En = normalize(E)
# N = CrossProduct3D(En, a1n)
# Nn = normalize(N)
# crossprod(a1n, a1n) * b1[1] - crossprod(a1n, b1) * a1n[1]
# Up = CrossProduct3D(En, Nn)
# 
# Rmat = rbind(Nn, En, Up)
# 
# t(Rmat) %*% (Rmat %*% a1n)

pc1 = prcomp(e2, scale = T)
crossprod(pc1$rotation[,1], pc1$rotation[,3])


e2accel = e2[, 1:3]
e2mag = e2[, 4:6]
e3accel = e3[, 1:3]
e3mag = e3[, 4:6]

Re2 = createRotationMatrix(as.numeric(e2accel[1,]), as.numeric(e2mag[1,]))
Re3 = createRotationMatrix(as.numeric(e3accel[1,]), as.numeric(e3mag[1,]))


e2a_new = t(apply(e2accel, 1, function(s) Re2 %*% s))
e3a_new = t(apply(e3accel, 1, function(s) Re3 %*% s))

plot(e2a_new[,1], type = "l")
plot(e3a_new[,1], type = "l")

plot(e2[,1], type = "l")
plot(e3[,1], type = "l")




a1n[2]*b1n[3] - a1n[3]*b1n[2]
  
Rx = function(theta){
  mat = matrix(c(1, 0 , 0, 0, cos(theta), -sin(theta), 0, sin(theta), cos(phi)), ncol = 3)
  mat
}

cnew = reorientAcceleration(test_accel=c2, c1)

test_accel = c2
ref_accel = c1
list.files()[23]

plot(e2[,1], type = "l")
plot(e3[,1], type = "l")
plot(cnew[,3], type = "l")

pca_c1 = prcomp(c1[,1:3], scale = T)
pca_c2 = prcomp(c2[,1:3], scale = T)

pca_c1$rotation[,1]
pca_c2$rotation[,1]

pca_c1$sdev^2/sum(pca_c1$sdev^2)
pca_c2$sdev/

plot(pca_c1[,1], type = "l")
plot(pca_c2[,1], type = "l")

plot(training[[21]][,1], type = "l")
plot(training[[25]][,1], type = "l")
file_list = list.files()
names(training)[21]

buildTrainingAlphabet = function(file_list){
  training_list = lapply(file_list, getTrainingData) ##get all of the PCA data
  names(training_list) = substr(list.files(), LETTER_LOC ,LETTER_LOC + 2)
  labels = sapply(training_list, function(s) s$label)
  raw_dat_list = lapply(training_list, function(s) s$datf)  
  ##fix the acccelerations
  raw_dat_list_fixed_accel = unlist(lapply(unique(labels), orientAcceleration, raw_dat_list, labels), recursive = FALSE)
  ##do the pca
  pc_dat = lapply(raw_dat_list_fixed_accel, getPCA)
  #reorient the pca
  dat_oriented = unlist(lapply(unique(labels), orientPCDirection, pc_dat, labels), recursive = FALSE)
  dat_oriented
}

getTrainingData = function(file){
  raw_dat = read.csv(file, header = F)
  rm_ix = c(1:5, nrow(raw_dat) : (nrow(raw_dat) - 5))
  raw_dat = raw_dat[-rm_ix, COLS_TO_KEEP]
  datf = apply(raw_dat, 2, function(s) fftfilt(rep(1, FILTER_NUM)/FILTER_NUM, s))
  datf = datf[- (1 : FILTER_NUM), ]
#   pca_obj = prcomp(datf[,1:3], scale = T)
#   pc_dat = pca_obj$x[,1 : NUM_PCS]
#  # pca_obj$rotation[,1:4]
   label = substr(file, LETTER_LOC ,LETTER_LOC)
  list(datf = datf, label = label)
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

orientAcceleration = function(letter, dat_list, labels){
  ##Reorients all of the PC directions for a particular letter   
  letter_list = dat_list[which(labels == letter)]
  ref_accel_mat = letter_list[[1]]
  letter_list_fixed = lapply( letter_list, fixAcceleration, ref_accel_mat)
  letter_list_fixed
}

fixAcceleration = function(test_accel, ref_accel){
  ##switch first and third axis for acceleration if closer match
  d1 =dtw(ref_accel[,1], test_accel[,1])$distance + dtw(ref_accel[,3], test_accel[,3])$distance
  d2 = dtw(ref_accel[,1], test_accel[,3])$distance + dtw(ref_accel[,3], test_accel[,1])$distance
  d3 = dtw(ref_accel[,1], -1  * test_accel[,3])$distance + dtw(ref_accel[,3],  test_accel[,1])$distance
  d4 = dtw(ref_accel[,1], test_accel[,3])$distance + dtw(ref_accel[,3], -1 * test_accel[,1])$distance
  d5 = dtw(ref_accel[,3], -1 * test_accel[,3])$distance + dtw(ref_accel[,3],  -1 * test_accel[,1])$distance
  min_dist = which.min(c(d1, d2, d3, d4, d5))
  if(min_dist == 1){
    return(test_accel)
  }else if(min_dist == 2){
    return(test_accel[, c(3,2,1)])
  }else if(min_dist == 3){
    z = test_accel[,1]
    y = test_accel[,2]
    x = -1 * test_accel[,3]
    return(cbind(x, y, z))
  }else if(min_dist == 4){
    z = -1 * test_accel[,1]
    y = test_accel[,2]
    x = test_accel[,3]
    return(cbind(x, y, z))
  }else{
    z = - 1 * test_accel[,1]
    y = test_accel[,2]
    x = -1 *  test_accel[,3]
    return(cbind(x, y, z))
  }
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
