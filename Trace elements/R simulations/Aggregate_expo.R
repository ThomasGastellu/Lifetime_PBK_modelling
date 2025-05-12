#### SIMULATION OF INTERNAL EXPOSURE FOR MIXTURE OF TRACE ELEMENTS ####

library(mrgsolve)
library(dplyr)
library(readr)
library(ggplot2)
library(triangle)

load("Exposure trajectories/pop_save_4ETM_daily_diet_soil_air_dust_INCA2.RData")

### Selection of population parameters 
# Gender
sexes <- sapply(pop_ref, function(x) x$sexe)
sexes <- as.vector(t(sexes))
sexes[sexes==2]<-0
sexes <- as.integer(sexes[1:10])

# Year of birth
annee_naiss <- sapply(pop_ref, function(x) x$annee_naiss)

# Region of habitation
region <- sapply(pop_ref, function(x) x$reg)

### Tables for saving
Cd_Urinecr<- tibble()
Cd_Urine <- tibble()
Cd_Kidney <- tibble()
Cd_Blood <- tibble()

Pb_Urinecr <- tibble()
Pb_Urine <- tibble()
Pb_Blood <- tibble()
Pb_Kidney <- tibble()

Hg_Hair <- tibble()
Hg_Urinecr <- tibble()
Hg_Urine <- tibble()
Hg_Kidney <- tibble()

As_Urinecr <- tibble()
As_Urine <- tibble()
As_Kidney <- tibble()

### Reading of PBK models
model_Cd <- mread("Model_Cd", file = "PBK models/Model_Cd.cpp")
model_Pb <- mread("Model_Pb", file = "PBK models/Model_Pb.cpp")
model_As <- mread("Model_As", file = "PBK models/Model_As.cpp")
model_THg <- mread("Model_THg", file = "PBK models/Model_THg.cpp")

### Selection of exposure trajectories to different sources of exposure
# Air contaminations are in ng/m3; except for lead (µg/m3)
# Soil and dust contamination are in µg/g of soil/dust;
# Dietary exposure is in µg/kg/day.
traj_diet_cd <- sapply(pop_ref, function(x) x$expo_cd)
traj_soil_cd <- sapply(pop_ref, function(x) x$soil_cd)
traj_dust_cd <- sapply(pop_ref, function(x) x$dust_cd)
traj_air_cd <- sapply(pop_ref, function(x) x$air_cd/1000000) ## Conversion into µg/L 
traj_diet_pb <- sapply(pop_ref, function(x) x$expo_pb)
traj_soil_pb <- sapply(pop_ref, function(x) x$soil_pb)
traj_dust_pb <- sapply(pop_ref, function(x) x$dust_pb)
traj_air_pb <- sapply(pop_ref, function(x) x$air_pb/1000) ## Conversion into µg/L
traj_diet_as3 <- sapply(pop_ref, function(x) x$expo_asIII)
traj_diet_as5 <- sapply(pop_ref, function(x) x$expo_asV)
traj_soil_as <- sapply(pop_ref, function(x) x$soil_as)
traj_air_as <- sapply(pop_ref, function(x) x$air_as/1000000) ## Conversion into µg/L
traj_dust_as <- sapply(pop_ref, function(x) x$dust_as)
traj_diet_mehg <- sapply(pop_ref, function(x) x$expo_mehg)
traj_diet_ihg <- sapply(pop_ref, function(x) x$expo_hgi)
traj_soil_hg <- sapply(pop_ref, function(x) x$soil_hg)
traj_air_hg <- sapply(pop_ref, function(x) x$air_hg/1000000) ## Conversion into µg/L


rm(pop_ref)


for (i in c(1:10)){
  
  ## Determination of the factor of distribution of individuals
  
  # Bodyweight
  Delta_BW_ind = rnorm(1, mean = 1, sd = 0.15)
  
  # Creatinine excretion
  Delta_creat_ind = rnorm(1, mean = 1, sd = 0.15)
  
  ## Variability for male
  Delta_Brain_M_ind = abs(rnorm(1, mean = 1, sd = 0.05))
  Delta_Kidney_M_ind = abs(rnorm(1, mean = 1, sd = 0.25))
  Delta_Liver_M_ind = abs(rnorm(1, mean = 1, sd = 0.24))
  Delta_Pancreas_M_ind = abs(rnorm(1, mean = 1, sd = 0.27))
  Delta_Stomach_M_ind = abs(rnorm(1, mean = 1, sd = 0.31))
  Delta_Small_Intestine_M_ind = abs(rnorm(1, mean = 1, sd = 0.12))
  Delta_Large_Intestine_M_ind = abs(rnorm(1, mean = 1, sd = 0.20))
  Delta_Heart_M_ind = abs(rnorm(1, mean = 1, sd = 0.19))
  Delta_Bone_M_ind = abs(rnorm(1, mean = 1, sd = 0.01))
  Delta_Gonad_M_ind = abs(rnorm(1, mean = 1, sd = 0.05))
  Delta_Lung_M_ind = abs(rlnorm(1, mean = 1, sd = .33))
  Delta_Spleen_M_ind = abs(rlnorm(1, mean = 1, sd = .38))
  Delta_Muscle_M_ind = abs(rnorm(1, mean = 1, sd = .27))
  Delta_Adipose_M_ind = abs(rnorm(1, mean = 1, sd = .42))
  
  ## Variability for female
  Delta_Brain_F_ind = abs(rnorm(1, mean = 1, sd = 0.05))
  Delta_Kidney_F_ind = abs(rnorm(1, mean = 1, sd = 0.25))
  Delta_Liver_F_ind = abs(rnorm(1, mean = 1, sd = 0.25))
  Delta_Pancreas_F_ind = abs(rnorm(1, mean = 1, sd = .29))
  Delta_Stomach_F_ind = abs(rnorm(1, mean = 1, sd = .31))
  Delta_Small_Intestine_F_ind = abs(rnorm(1, mean = 1, sd = .13))
  Delta_Large_Intestine_F_ind = abs(rnorm(1, mean = 1, sd = .14))
  Delta_Heart_F_ind = abs(rnorm(1, mean = 1, sd = .25))
  Delta_Bone_F_ind = abs(rnorm(1, mean = 1, sd = .01))
  Delta_Gonad_F_ind = abs(rnorm(1, mean = 1, sd = .05))
  Delta_Lung_F_ind = abs(rlnorm(1, mean = 1, sd = .33))
  Delta_Spleen_F_ind = abs(rlnorm(1, mean = 1, sd = .38))
  Delta_Muscle_F_ind = abs(rlnorm(1, mean = 1, sd = .27))
  Delta_Adipose_F_ind = abs(rlnorm(1, mean = 1, sd = .42))
  
  ## Quantity of soil and dust ingested (in g/day)
  conso_soil <- matrix(data = NA, ncol = 1, nrow = 29094)
  conso_dust <- matrix(data = NA, ncol = 1, nrow = 29094)
  
  for (t in c(1:29094)){
    age = t/365
    if(age < 1/12){
      conso_soil[t] <- 0
      conso_dust[t] <- sqrt(rnorm(1,32,44)**2)/1000
    }
    if(age >= 1/12 & age < 3/12){
      conso_soil[t] <- 0
      conso_dust[t] <- sqrt(rnorm(1,36,53)**2)/1000
    }
    if(age >= 3/12 & age < 0.5){
      conso_soil[t] <- 0
      conso_dust[t] <- sqrt(rnorm(1,37,47)**2)/1000
    }
    if(age >= 0.5 & age < 1){
      conso_soil[t] <- 0
      conso_dust[t] <- sqrt(rnorm(1,44,70)**2)/1000
    }
    if(age >= 1 & age < 2){
      conso_soil[t] <- sqrt(rnorm(1,10,16)**2)/1000
      conso_dust[t] <- sqrt(rnorm(1,37,54)**2)/1000
    }
    if(age >= 2 & age < 3){
      conso_soil[t] <- sqrt(rnorm(1,27,39)**2)/1000
      conso_dust[t] <- sqrt(rnorm(1,26,45)**2)/1000
    }
    if(age >= 3 & age < 6){
      conso_soil[t] <- sqrt(rnorm(1,31,48)**2)/1000
      conso_dust[t] <- sqrt(rnorm(1,28,40)**2)/1000
    }
    if(age >= 6 & age < 11){
      conso_soil[t] <- sqrt(rnorm(1,31,54)**2)/1000
      conso_dust[t] <- sqrt(rnorm(1,25,38)**2)/1000
    }
    if(age >= 11 & age < 16){
      conso_soil[t] <- sqrt(rnorm(1,23,45)**2)/1000
      conso_dust[t] <- sqrt(rnorm(1,21,41)**2)/1000
    }
    if(age >= 16 & age < 21){
      conso_soil[t] <- sqrt(rnorm(1,12,33)**2)/1000
      conso_dust[t] <- sqrt(rnorm(1,11,27)**2)/1000
    }
    if(age>=21){
      conso_soil[t] <- 0
      conso_dust[t] <- 0
    }	
    
  }
  
  ## Establishment of daily exposure by soil with refined with time
  for(j in c(1 : 29094)){
    x <- as.numeric(annee_naiss[i]) + j/365
    if(x < 2005){
      traj_soil_cd[j,i] <- traj_soil_cd[j,i]*(1 - 0.005*(2005 - x)) * conso_soil[j]
      traj_soil_pb[j,i] <- traj_soil_pb[j,i]*(1 - 0.005*(2005 - x)) * conso_soil[j]
      traj_soil_hg[j,i] <- traj_soil_hg[j,i] * conso_soil[j]
      traj_soil_as[j,i] <- traj_soil_as[j,i] * conso_soil[j]
    }
    else{
      traj_soil_pb[j,i] <- traj_soil_pb[j,i] * conso_soil[j]
      traj_soil_cd[j,i] <- traj_soil_cd[j,i] * conso_soil[j]
      traj_soil_hg[j,i] <- traj_soil_hg[j,i] * conso_soil[j]
      traj_soil_as[j,i] <- traj_soil_as[j,i] * conso_soil[j]
    }
  }
  
  ## Extrapolation of air contamination over time
  Conta_Cd_NO <- function(x){-5.061e-06 * x^2 + 1.307e-02 * x -5.406e+00} 
  Conta_Cd_O <- function(x){-7.452e-05 * x^2 + 2.909e-01 * x -2.834e+02} 
  Conta_Cd_E <- function(x){-2.577e-04 * x^2 + 1.007e+00 * x -9.822e+02} 
  Conta_Cd_IdF <- function(x){-1.039e-04 * x^2 + 4.037e-01 * x -3.915e+02} 
  Conta_Cd_CE <- function(x){-1.469e-04 * x^2 + 5.737e-01 * x -5.592e+02} 
  Conta_Cd_SE <- function(x){-1.872e-04 * x^2 + 7.308e-01 * x -7.1225e+02} 
  Conta_Cd_SO <- function(x){-3.073e-04 * x^2 + 1.198e+00 * x -1.165e+03} 
  Conta_Cd_C <- function(x){-1.034e-04 * x^2 +  4.018e-01 * x -3.894e+02} 
  
  Conta_Pb_NO <- function(x){-7.253e-03 * x^2 + 2.781e+01 * x -2.656e+04} 
  Conta_Pb_O <- function(x){-1.664e-03 * x^2 + 6.420e+00 * x -6.176e+03 } 
  Conta_Pb_E <- function(x){-8.928e-03 * x^2 + 3.461e+01 * x -3.348e+04} 
  Conta_Pb_IdF <- function(x){-5.120e-03 * x^2 + 1.969e+01 * x -1.887e+04} 
  Conta_Pb_CE <- function(x){-4.042e-03 * x^2 + 1.559e+01 * x -1.4995e+04} 
  Conta_Pb_SE <- function(x){-1.960e-03 * x^2 + 7.482e+00 * x -7.113e+03} 
  Conta_Pb_SO <- function(x){-1.254e-03 * x^2 + 4.793e+00 * x -4.563e+03} 
  Conta_Pb_C <- function(x){-1.178e-03 * x^2 + 4.493e+00 * x -4.265e+03}
  
  Conta_As_NO <- function(x){-1.561e-04 * x^2 + 5.994e-01 * x -5.736e+02} 
  Conta_As_O <- function(x){-2.053e-04 * x^2 + 7.922e-01 * x -7.625e+02} 
  Conta_As_E <- function(x){-2.191e-04 * x^2 + 8.476e-01 * x -8.180e+02} 
  Conta_As_IdF <- function(x){-5.246e-04 * x^2 + 2.017e+00 * x -1.932e+03} 
  Conta_As_CE <- function(x){-2.848e-04 * x^2 + 1.094e+00 * x -1.0475e+03} 
  Conta_As_SE <- function(x){-4.525e-04 * x^2 + 1.760e+00 * x -1.708e+03} 
  Conta_As_SO <- function(x){-1.226e-04 * x^2 + 4.682e-01 * x -4.451e+02} 
  Conta_As_C <- function(x){-1.848e-04 * x^2 + 7.157e-01 * x -6.912e+02} 
  
  ## Establishment of daily exposure by dust scaled on air  with refined with time
  for(j in c(1:29120)){
    x <- as.numeric(annee_naiss[i]) + j/365
    if(x < 2020){
      
      if(region[j,i] == 0){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_NO(x)/Conta_Cd_NO(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_NO(x)/Conta_Pb_NO(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_NO(x)/Conta_As_NO(2009))}
      
      if(region[j,i] == 1){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_E(x)/Conta_Cd_E(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_E(x)/Conta_Pb_E(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_E(x)/Conta_As_E(2009))}
      
      if(region[j,i] == 2){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_IdF(x)/Conta_Cd_IdF(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_IdF(x)/Conta_Pb_IdF(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_IdF(x)/Conta_As_IdF(2009))}
      
      if(region[j,i] == 3){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_O(x)/Conta_Cd_O(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_O(x)/Conta_Pb_O(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_O(x)/Conta_As_O(2009))}
      
      if(region[j,i] == 4){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_C(x)/Conta_Cd_C(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_C(x)/Conta_Pb_C(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_C(x)/Conta_As_C(2009))}
      
      if(region[j,i] == 5){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_CE(x)/Conta_Cd_CE(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_CE(x)/Conta_Pb_CE(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_CE(x)/Conta_As_CE(2009))}
      
      if(region[j,i] == 6){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_SO(x)/Conta_Cd_SO(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_SO(x)/Conta_Pb_SO(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_SO(x)/Conta_As_SO(2009))}
      
      if(region[j,i] == 7){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_SE(x)/Conta_Cd_SE(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_SE(x)/Conta_Pb_SE(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_SE(x)/Conta_As_SE(2009))}
    }
    
    else{
      if(region[j,i] == 0){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_NO(2020)/Conta_Cd_NO(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_NO(2020)/Conta_Pb_NO(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_NO(2020)/Conta_As_NO(2009))}
      
      if(region[j,i] == 1){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_E(2020)/Conta_Cd_E(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_E(2020)/Conta_Pb_E(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_E(2020)/Conta_As_E(2009))}
      
      if(region[j,i] == 2){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_IdF(2020)/Conta_Cd_IdF(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_IdF(2020)/Conta_Pb_IdF(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_IdF(2020)/Conta_As_IdF(2009))}
      
      if(region[j,i] == 3){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_O(2020)/Conta_Cd_O(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_O(2020)/Conta_Pb_O(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_O(2020)/Conta_As_O(2009))}
      
      if(region[j,i] == 4){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_C(2020)/Conta_Cd_C(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_C(2020)/Conta_Pb_C(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_C(2020)/Conta_As_C(2009))}
      
      if(region[j,i] == 5){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_CE(2020)/Conta_Cd_CE(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_CE(2020)/Conta_Pb_CE(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_CE(2020)/Conta_As_CE(2009))}
      
      if(region[j,i] == 6){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_SO(2020)/Conta_Cd_SO(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_SO(2020)/Conta_Pb_SO(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_SO(2020)/Conta_As_SO(2009))}
      
      if(region[j,i] == 7){
        traj_dust_cd[j,i] <- traj_dust_cd[j,i] * conso_dust[j] * (Conta_Cd_SE(2020)/Conta_Cd_SE(2009))
        traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_SE(2020)/Conta_Pb_SE(2009))
        traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_SE(2020)/Conta_As_SE(2009))}
    }
  }
  
  
  ## Extraction of cadmium dietary exposure for PBK modelling
  expo_diet_cd <- ev(amt = (traj_diet_cd[,i]), 
                     ii=1,
                     cmt = "DIET", 
                     addl = 0)
  expo_diet_cd <- as_data_set(expo_diet_cd)
  expo_diet_cd$time <- c(0:29093)
  
  ## Extraction of cadmium exposure by soil for PBK modelling
  expo_soil_cd <- ev(amt =(traj_soil_cd[c(1:29094),i]), 
                     ii=1,  
                     cmt = "SOIL", 
                     addl = 0)
  expo_soil_cd <- as_data_set(expo_soil_cd)
  expo_soil_cd$time <- c(0:29093)
  
  ## Extraction of cadmium exposure by air for PBK modelling
  expo_air_cd <- ev(amt = (traj_air_cd[c(1:29094),i]), 
                    ii=1,
                    cmt = "AIR", 
                    addl = 0)
  expo_air_cd <- as_data_set(expo_air_cd)
  expo_air_cd$time <- c(0:29093)
  
  ## Extraction of cadmium exposure by dust for PBK modelling
  expo_dust_cd <- ev(amt = (traj_dust_cd[c(1:29094),i]), 
                     ii=1,
                     cmt = "DUST", 
                     addl = 0)
  expo_dust_cd <- as_data_set(expo_dust_cd)
  expo_dust_cd$time <- c(0:29093)
  
  ## Compilation of cadmium exposures
  expo_cd <- rbind(expo_diet_cd, expo_soil_cd, expo_air_cd, expo_dust_cd)
  expo_cd <- expo_cd[order(expo_cd$time),]
  
  ## Extraction of lead dietary exposure for PBK modelling
  expo_diet_pb <- ev(amt = (traj_diet_pb[,i]), 
                     ii=1,
                     cmt = "DIET", 
                     addl = 0)
  expo_diet_pb <- as_data_set(expo_diet_pb)
  expo_diet_pb$time <- c(0:29093)
  
  ## Extraction of lead exposure by soil for PBK modelling
  expo_soil_pb <- ev(amt =(traj_soil_pb[c(1:29094),i]),
                     ii=1, 
                     cmt = "SOIL", 
                     addl = 0)
  expo_soil_pb <- as_data_set(expo_soil_pb)
  expo_soil_pb$time <- c(0:29093)
  
  ## Extraction of lead exposure by dust for PBK modelling
  expo_dust_pb <- ev(amt =(traj_dust_pb[c(1:29094),i]),
                     ii=1, 
                     cmt = "DUST", 
                     addl = 0)
  expo_dust_pb <- as_data_set(expo_dust_pb)
  expo_dust_pb$time <- c(0:29093)
  
  ## Extraction of lead exposure by air for PBK modelling
  expo_air_pb <- ev(amt =(traj_air_pb[c(1:29094),i]),
                    ii=1, 
                    cmt = "AIR", 
                    addl = 0)
  expo_air_pb <- as_data_set(expo_air_pb)
  expo_air_pb$time <- c(0:29093)
  
  ## Compilation of lead exposures
  expo_pb <- rbind(expo_diet_pb, expo_soil_pb, expo_air_pb, expo_dust_pb)
  expo_pb <- expo_pb[order(expo_pb$time),]
  
  ## Extraction of methylmercury dietary exposure for PBK modelling
  expo_diet_mehg <- ev(amt = (traj_diet_mehg[,i]), 
                       ii=1,
                       cmt = "expomehg", 
                       addl = 0)
  expo_diet_mehg <- as_data_set(expo_diet_mehg)
  expo_diet_mehg$time <- c(0:29093)
  
  ## Extraction of inorganic mercury dietary exposure for PBK modelling
  expo_diet_ihg <- ev(amt = (traj_diet_ihg[,i]), 
                      ii=1,
                      cmt = "expohgi", 
                      addl = 0)
  expo_diet_ihg <- as_data_set(expo_diet_ihg)
  expo_diet_ihg$time <- c(0:29093)
  
  ## Extraction of mercury exposure by soil for PBK modelling
  expo_soil_hg <- ev(amt =(traj_soil_hg[c(1:29094),i]),
                     ii=1, 
                     cmt = "SOIL", 
                     addl = 0)
  expo_soil_hg <- as_data_set(expo_soil_hg)
  expo_soil_hg$time <- c(0:29093)  
  
  ## Extraction of mercury exposure by air for PBK modelling
  expo_air_hg <- ev(amt =(traj_air_hg[c(1:29094),i]),
                    ii=1, 
                    cmt = "AIR", 
                    addl = 0)
  expo_air_hg <- as_data_set(expo_air_hg)
  expo_air_hg$time <- c(0:29093) 
  
  ## Compilation of mercury exposures
  expo_hg <- rbind(expo_diet_mehg, expo_diet_ihg, expo_soil_hg, expo_air_hg)
  expo_hg <- expo_hg[order(expo_hg$time),]
  
  ## Extraction of inorganic arsenic 5+ dietary exposure for PBK modelling
  expo_diet_as3 <- ev(amt = (traj_diet_as3[,i]), 
                      ii=1,
                      cmt = "DIET3", 
                      addl = 0,
                      evid = 1)
  expo_diet_as3 <- as_data_set(expo_diet_as3)
  expo_diet_as3$time <- c(0:29093)
  
  ## Extraction of inorganic arsenic 3+ dietary exposure for PBK modelling  
  expo_diet_as5 <- ev(amt = (traj_diet_as5[,i]), 
                      ii=1,
                      cmt = "DIET5", 
                      addl = 0,
                      evid = 1)
  expo_diet_as5 <- as_data_set(expo_diet_as5)
  expo_diet_as5$time <- c(0:29093)
  
  ## Extraction of inorganic arsenic exposure by soil for PBK modelling
  expo_soil_as <- ev(amt =(traj_soil_as[c(1:29094),i]),
                     ii=1, 
                     cmt = "SOIL", 
                     addl = 0)
  expo_soil_as <- as_data_set(expo_soil_as)
  expo_soil_as$time <- c(0:29093)
  
  ## Extraction of inorganic arsenic exposure by dust for PBK modelling
  expo_dust_as <- ev(amt =(traj_dust_as[c(1:29094),i]),
                     ii=1, 
                     cmt = "DUST", 
                     addl = 0)
  expo_dust_as <- as_data_set(expo_dust_as)
  expo_dust_as$time <- c(0:29093)
  
  ## Extraction of inorganic arsenic exposure by air for PBK modelling
  expo_air_as <- ev(amt =(traj_air_as[c(1:29094),i]),
                    ii=1, 
                    cmt = "AIR", 
                    addl = 0)
  expo_air_as <- as_data_set(expo_air_as)
  expo_air_as$time <- c(0:29093)
  
  ## Compilation of inorganic arsenic exposures
  expo_as <- rbind(expo_diet_as5, expo_diet_as3, expo_soil_as, expo_air_as, expo_dust_as)
  expo_as <- expo_as[order(expo_as$time),]
  
  ## Estimation of the internal exposure of cadmium
  Result_Cadmium <- model_Cd %>% param(Delta_BW         = Delta_BW_ind,
                                       Delta_creat      = Delta_creat_ind, 
                                       k1_cig           = rtriangle(1,0.1,0.2,0.1),
                                       k1_dust          = rtriangle(1,0.4,0.9,0.7),
                                       k2_cig           = rtriangle(1,0.4,0.6,0.4),
                                       k2_dust          = rtriangle(1,0.1,0.3,0.13),
                                       k3               = rtriangle(1,0.01,1,0.05),
                                       k4               = rtriangle(1,0.001,0.1,0.005),
                                       k5_h             = rtriangle(1,0.03,0.1,0.05),
                                       k5_f             = rtriangle(1,0.06,0.20,0.10),
                                       k7               = rtriangle(1,0.2,0.4,0.25), 
                                       k8               = rtriangle(1,0.5,5,1),
                                       k9               = rtriangle(1,0.4,0.8,0.44),
                                       k10              = rtriangle(1,0.00004,0.0002,0.00014),
                                       k11              = rtriangle(1,0.05,0.5,0.27),
                                       k12              = rtriangle(1,0.1,0.4,0.25),
                                       kx               = rtriangle(1,0.01,0.05,0.04),
                                       k13              = rtriangle(1,0,0.0001,0.00003),
                                       k14              = rtriangle(1,0.0001,0.0003,0.00016),
                                       k15              = rtriangle(1,0,0.0001,0.00005),
                                       k16              = rtriangle(1,0.004,0.015,0.012),
                                       k17              = rtriangle(1,0.8,0.98,0.95),
                                       k18              = rtriangle(1,0,0.0001,0.00001),
                                       k19              = rtriangle(1,0.00002,0.0002,0.00014),
                                       k21              = rtriangle(1,0,0.000002,0.0000011),
                                       age_deb          = 0,   ## Beginning of smoking (years)
                                       age_fin          = 0,   ## End of smoking (years)
                                       kcig             = 0.1, ## Coefficient of absorption by smoking
                                       smoke_cd         = 1,   ## Quantity of cadmium in one cigaret (in µg)
                                       nb_cig           = 0,   ## Number of cigarets smoked by day
                                       Delta_Brain_M    = Delta_Brain_M_ind,
                                       Delta_Kidney_M   = Delta_Kidney_M_ind,
                                       Delta_Liver_M    = Delta_Liver_M_ind,
                                       Delta_Pancreas_M = Delta_Pancreas_M_ind,
                                       Delta_Stomach_M  = Delta_Stomach_M_ind,
                                       Delta_Small_Intestine_M = Delta_Small_Intestine_M_ind,
                                       Delta_Large_Intestine_M = Delta_Large_Intestine_M_ind,
                                       Delta_Heart_M    = Delta_Heart_M_ind,
                                       Delta_Bone_M     = Delta_Bone_M_ind,
                                       Delta_Gonad_M    = Delta_Gonad_M_ind,
                                       Delta_Lung_M     = Delta_Lung_M_ind,
                                       Delta_Spleen_M   = Delta_Spleen_M_ind,
                                       Delta_Muscle_M   = Delta_Muscle_M_ind,
                                       Delta_Adipose_M  = Delta_Adipose_M_ind,
                                       Delta_Brain_F    = Delta_Brain_F_ind,
                                       Delta_Kidney_F   = Delta_Kidney_F_ind,
                                       Delta_Liver_F    = Delta_Liver_F_ind,
                                       Delta_Pancreas_F = Delta_Pancreas_F_ind,
                                       Delta_Stomach_F  = Delta_Stomach_F_ind,
                                       Delta_Small_Intestine_F = Delta_Small_Intestine_F_ind,
                                       Delta_Large_Intestine_F = Delta_Large_Intestine_F_ind,
                                       Delta_Heart_F    = Delta_Heart_F_ind,
                                       Delta_Bone_F     = Delta_Bone_F_ind,
                                       Delta_Gonad_F    = Delta_Gonad_F_ind,
                                       Delta_Lung_F     = Delta_Lung_F_ind,
                                       Delta_Spleen_F   = Delta_Spleen_F_ind,
                                       Delta_Muscle_F   = Delta_Muscle_F_ind,
                                       Delta_Adipose_F  = Delta_Adipose_F_ind,
                                       SEXBABY = sexes[i]) %>%
    mrgsim(delta = 1, end = 29094, events = expo_cd)
  
  Result_Cadmium <- as.data.frame(Result_Cadmium)
  Result_Cadmium <- Result_Cadmium[,c("time","ucdcr", "Conc_urine_cd","kidney_burden","BLOOD")] 
  Result_Cadmium <- aggregate(Result_Cadmium, by = list(Result_Cadmium$time), mean)
  
  Cd_Urinecr <- rbind(Cd_Urinecr, Result_Cadmium$ucdcr)
  Cd_Urine <- rbind(Cd_Urine, Result_Cadmium$Conc_urine_cd)
  Cd_Kidney <- rbind(Cd_Kidney, Result_Cadmium$kidney_burden)
  Cd_Blood <- rbind(Cd_Blood, Result_Cadmium$BLOOD)
  
  save(Cd_Urinecr, file = "Results/Cd_Urinecr_aggregated.RData")
  save(Cd_Urine, file = "Results/Cd_Urine_aggregated.RData")
  save(Cd_Kidney, file = "Results/Cd_Kidney_aggregated.RData")
  save(Cd_Blood, file = "Results/Cd_Blood_aggregated.RData")
  
  ## Estimation of internal exposure of lead
  Result_Lead <- model_Pb %>% param(Delta_BW         = Delta_BW_ind,
                                    Delta_creat      = Delta_creat_ind, 
                                    age_deb          = 0, ## Beginning of smoking (years)
                                    age_fin          = 0, ## End of smoking (years)
                                    smoke_Pb         = 1, ## Quantity of lead in one cigaret (in µg)
                                    nb_cig           = 0, ## Number of cigarets smoked by day
                                    Delta_Brain_M    = Delta_Brain_M_ind,
                                    Delta_Kidney_M   = Delta_Kidney_M_ind,
                                    Delta_Liver_M    = Delta_Liver_M_ind,
                                    Delta_Pancreas_M = Delta_Pancreas_M_ind,
                                    Delta_Stomach_M  = Delta_Stomach_M_ind,
                                    Delta_Small_Intestine_M = Delta_Small_Intestine_M_ind,
                                    Delta_Large_Intestine_M = Delta_Large_Intestine_M_ind,
                                    Delta_Heart_M    = Delta_Heart_M_ind,
                                    Delta_Bone_M     = Delta_Bone_M_ind,
                                    Delta_Gonad_M    = Delta_Gonad_M_ind,
                                    Delta_Lung_M     = Delta_Lung_M_ind,
                                    Delta_Spleen_M   = Delta_Spleen_M_ind,
                                    Delta_Muscle_M   = Delta_Muscle_M_ind,
                                    Delta_Adipose_M  = Delta_Adipose_M_ind,
                                    Delta_Brain_F    = Delta_Brain_F_ind,
                                    Delta_Kidney_F   = Delta_Kidney_F_ind,
                                    Delta_Liver_F    = Delta_Liver_F_ind,
                                    Delta_Pancreas_F = Delta_Pancreas_F_ind,
                                    Delta_Stomach_F  = Delta_Stomach_F_ind,
                                    Delta_Small_Intestine_F = Delta_Small_Intestine_F_ind,
                                    Delta_Large_Intestine_F = Delta_Large_Intestine_F_ind,
                                    Delta_Heart_F    = Delta_Heart_F_ind,
                                    Delta_Bone_F     = Delta_Bone_F_ind,
                                    Delta_Gonad_F    = Delta_Gonad_F_ind,
                                    Delta_Lung_F     = Delta_Lung_F_ind,
                                    Delta_Spleen_F   = Delta_Spleen_F_ind,
                                    Delta_Muscle_F   = Delta_Muscle_F_ind,
                                    Delta_Adipose_F  = Delta_Adipose_F_ind,
                                    SEXBABY = sexes[i]) %>%
    mrgsim(delta = 1, end = 29094, events = expo_pb)
  
  Result_Lead <- as.data.frame(Result_Lead)
  Result_Lead <- Result_Lead[,c("time","UPbcr", "UPb","CB","CK")] 
  Result_Lead <- aggregate(Result_Lead, by = list(Result_Lead$time), mean)
  
  Pb_Urinecr <- rbind(Pb_Urinecr, Result_Lead$UPbcr)
  Pb_Urine <- rbind(Pb_Urine, Result_Lead$UPb)
  Pb_Blood <- rbind(Pb_Blood, Result_Lead$CB)
  Pb_Kidney <- rbind(Pb_Kidney, Result_Lead$CK)
  
  save(Pb_Urinecr, file = "Results/Pb_Urinecr_aggregated.RData")
  save(Pb_Urine, file = "Results/Pb_Urine_aggregated.RData")
  save(Pb_Blood, file = "Results/Pb_Blood_aggregated.RData")
  save(Pb_Kidney, file = "Results/Pb_Kidney_aggregated.RData")
  
  ## Estimation of total mercury internal concentrations
  Result_Mercury <- model_THg %>% param(Delta_BW     = Delta_BW_ind,
                                        Delta_creat      = Delta_creat_ind, 
                                        age_deb          = 0, ## Beginning of smoking (years)
                                        age_fin          = 0, ## End of smoking (years)
                                        smoke_hg         = 1, ## Quantity of mercury in one cigaret (µg)
                                        nb_cig           = 0, ## Number of cigarets smoked by days
                                        Delta_Brain_M    = Delta_Brain_M_ind,
                                        Delta_Kidney_M   = Delta_Kidney_M_ind,
                                        Delta_Liver_M    = Delta_Liver_M_ind,
                                        Delta_Pancreas_M = Delta_Pancreas_M_ind,
                                        Delta_Stomach_M  = Delta_Stomach_M_ind,
                                        Delta_Small_Intestine_M = Delta_Small_Intestine_M_ind,
                                        Delta_Large_Intestine_M = Delta_Large_Intestine_M_ind,
                                        Delta_Heart_M    = Delta_Heart_M_ind,
                                        Delta_Bone_M     = Delta_Bone_M_ind,
                                        Delta_Gonad_M    = Delta_Gonad_M_ind,
                                        Delta_Lung_M     = Delta_Lung_M_ind,
                                        Delta_Spleen_M   = Delta_Spleen_M_ind,
                                        Delta_Muscle_M   = Delta_Muscle_M_ind,
                                        Delta_Adipose_M  = Delta_Adipose_M_ind,
                                        Delta_Brain_F    = Delta_Brain_F_ind,
                                        Delta_Kidney_F   = Delta_Kidney_F_ind,
                                        Delta_Liver_F    = Delta_Liver_F_ind,
                                        Delta_Pancreas_F = Delta_Pancreas_F_ind,
                                        Delta_Stomach_F  = Delta_Stomach_F_ind,
                                        Delta_Small_Intestine_F = Delta_Small_Intestine_F_ind,
                                        Delta_Large_Intestine_F = Delta_Large_Intestine_F_ind,
                                        Delta_Heart_F    = Delta_Heart_F_ind,
                                        Delta_Bone_F     = Delta_Bone_F_ind,
                                        Delta_Gonad_F    = Delta_Gonad_F_ind,
                                        Delta_Lung_F     = Delta_Lung_F_ind,
                                        Delta_Spleen_F   = Delta_Spleen_F_ind,
                                        Delta_Muscle_F   = Delta_Muscle_F_ind,
                                        Delta_Adipose_F  = Delta_Adipose_F_ind,
                                        SEXBABY = sexes[i]) %>%
    mrgsim(delta = 1, end = 29094, events = expo_hg)
  
  Result_Mercury <- as.data.frame(Result_Mercury)
  Result_Mercury <- Result_Mercury[,c("time","CUHgcr", "CUHg","CH","CKHg")]
  Result_Mercury <- aggregate(Result_Mercury, by = list(Result_Mercury$time), mean)
  
  Hg_Urinecr <- rbind(Hg_Urinecr, Result_Mercury$CUHgcr)
  Hg_Urine <- rbind(Hg_Urine, Result_Mercury$CUHg)
  Hg_Hair <- rbind(Hg_Hair, Result_Mercury$CH)
  Hg_Kidney <- rbind(Hg_Kidney, Result_Mercury$CKHg)
  
  save(Hg_Urinecr, file = "Results/Hg_Urinecr_aggregated.RData")
  save(Hg_Urine, file = "Results/Hg_Urine_aggregated.RData")
  save(Hg_Hair, file = "Results/Hg_Hair_aggregated.RData")
  save(Hg_Kidney, file = "Results/Hg_Kidney_aggregated.RData")
  
  
  ## Estimation of internal exposures for inorganic arsenic
  Result_Arsenic <- model_As %>% param(Delta_BW         = Delta_BW_ind,
                                       Delta_creat      = Delta_creat_ind, 
                                       age_deb          = 0, ## Beginning of smoking (years)
                                       age_fin          = 0, ## End of smoking (years)
                                       smoke_As         = 1, ## Quantity of inorganic arsenic in one cigaret (µg)
                                       nb_cig           = 0, ## Number of cigaret smoked per day
                                       Delta_Brain_M    = Delta_Brain_M_ind,
                                       Delta_Kidney_M   = Delta_Kidney_M_ind,
                                       Delta_Liver_M    = Delta_Liver_M_ind,
                                       Delta_Pancreas_M = Delta_Pancreas_M_ind,
                                       Delta_Stomach_M  = Delta_Stomach_M_ind,
                                       Delta_Small_Intestine_M = Delta_Small_Intestine_M_ind,
                                       Delta_Large_Intestine_M = Delta_Large_Intestine_M_ind,
                                       Delta_Heart_M    = Delta_Heart_M_ind,
                                       Delta_Bone_M     = Delta_Bone_M_ind,
                                       Delta_Gonad_M    = Delta_Gonad_M_ind,
                                       Delta_Lung_M     = Delta_Lung_M_ind,
                                       Delta_Spleen_M   = Delta_Spleen_M_ind,
                                       Delta_Muscle_M   = Delta_Muscle_M_ind,
                                       Delta_Adipose_M  = Delta_Adipose_M_ind,
                                       Delta_Brain_F    = Delta_Brain_F_ind,
                                       Delta_Kidney_F   = Delta_Kidney_F_ind,
                                       Delta_Liver_F    = Delta_Liver_F_ind,
                                       Delta_Pancreas_F = Delta_Pancreas_F_ind,
                                       Delta_Stomach_F  = Delta_Stomach_F_ind,
                                       Delta_Small_Intestine_F = Delta_Small_Intestine_F_ind,
                                       Delta_Large_Intestine_F = Delta_Large_Intestine_F_ind,
                                       Delta_Heart_F    = Delta_Heart_F_ind,
                                       Delta_Bone_F     = Delta_Bone_F_ind,
                                       Delta_Gonad_F    = Delta_Gonad_F_ind,
                                       Delta_Lung_F     = Delta_Lung_F_ind,
                                       Delta_Spleen_F   = Delta_Spleen_F_ind,
                                       Delta_Muscle_F   = Delta_Muscle_F_ind,
                                       Delta_Adipose_F  = Delta_Adipose_F_ind,
                                       SEXBABY = sexes[i]) %>%
    mrgsim(delta = 1, end = 29094, events = expo_as)
  
  Result_Arsenic <- as.data.frame(Result_Arsenic)
  Result_Arsenic <- Result_Arsenic[,c("time","UAs", "UAscr","KAs")]
  Result_Arsenic <- aggregate(Result_Arsenic[,c("UAs","UAscr","KAs")], by = list(Result_Arsenic$time), mean)
  As_Urinecr <- rbind(As_Urinecr, Result_Arsenic$UAscr)
  As_Urine <- rbind(As_Urine, Result_Arsenic$UAs)
  As_Kidney <- rbind(As_Kidney, Result_Arsenic$KAs)
  
  
  save(As_Urinecr, file = "Results/As_Urinecr_aggregated.RData")
  save(As_Urine, file = "Results/As_Urine_aggregated.RData")
  save(As_Kidney, file = "Results/As_Kidney_aggregated.RData")
  
  print(i)
} 
