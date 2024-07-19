#### SIMULATION OF INTERNAL EXPOSURE FOR LEAD ####

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
Pb_Urinecr <- tibble()
Pb_Urine <- tibble()
Pb_Blood <- tibble()
Pb_Kidney <- tibble()

### Reading of PBK models
model_Pb <- mread("Model_Pb", file = "PBK models/Model_Pb.cpp")

### Selection of exposure trajectories to different sources of exposure
# Air contaminations are in ng/m3;
# Soil and dust contamination are in µg/g of soil/dust;
# Dietary exposure is in µg/kg/day.
traj_diet_pb <- sapply(pop_ref, function(x) x$expo_pb)
traj_soil_pb <- sapply(pop_ref, function(x) x$soil_pb)
traj_dust_pb <- sapply(pop_ref, function(x) x$dust_pb)
traj_air_pb <- sapply(pop_ref, function(x) x$air_pb/1000)

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
    if(x < 2005){traj_soil_pb[j,i] <- traj_soil_pb[j,i]*(1 - 0.005*(2005 - x)) * conso_soil[j] }
    else{traj_soil_pb[j,i] <- traj_soil_pb[j,i] * conso_soil[j]}
  }
  
  ## Extrapolation of air contamination over time
  Conta_Pb_NO <- function(x){-7.253e-03 * x^2 + 2.781e+01 * x -2.656e+04} 
  Conta_Pb_O <- function(x){-1.664e-03 * x^2 + 6.420e+00 * x -6.176e+03 } 
  Conta_Pb_E <- function(x){-8.928e-03 * x^2 + 3.461e+01 * x -3.348e+04} 
  Conta_Pb_IdF <- function(x){-5.120e-03 * x^2 + 1.969e+01 * x -1.887e+04} 
  Conta_Pb_CE <- function(x){-4.042e-03 * x^2 + 1.559e+01 * x -1.4995e+04} 
  Conta_Pb_SE <- function(x){-1.960e-03 * x^2 + 7.482e+00 * x -7.113e+03} 
  Conta_Pb_SO <- function(x){-1.254e-03 * x^2 + 4.793e+00 * x -4.563e+03} 
  Conta_Pb_C <- function(x){-1.178e-03 * x^2 + 4.493e+00 * x -4.265e+03}
  
  ## Establishment of daily exposure by dust scaled on air  with refined with time
  for(j in c(1:29120)){
    x <- as.numeric(annee_naiss[i]) + j/365
    if(x < 2020){
      
      if(region[j,i] == 0){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_NO(x)/Conta_Pb_NO(2009))}
      
      if(region[j,i] == 1){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_E(x)/Conta_Pb_E(2009))}
      
      if(region[j,i] == 2){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_IdF(x)/Conta_Pb_IdF(2009))}
      
      if(region[j,i] == 3){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_O(x)/Conta_Pb_O(2009))}
      
      if(region[j,i] == 4){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_C(x)/Conta_Pb_C(2009))}
      
      if(region[j,i] == 5){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_CE(x)/Conta_Pb_CE(2009))}
      
      if(region[j,i] == 6){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_SO(x)/Conta_Pb_SO(2009))}
      
      if(region[j,i] == 7){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_SE(x)/Conta_Pb_SE(2009))}
    }
    
    else{
      if(region[j,i] == 0){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_NO(2020)/Conta_Pb_NO(2009))}
      
      if(region[j,i] == 1){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_E(2020)/Conta_Pb_E(2009))}
      
      if(region[j,i] == 2){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_IdF(2020)/Conta_Pb_IdF(2009))}
      
      if(region[j,i] == 3){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_O(2020)/Conta_Pb_O(2009))}
      
      if(region[j,i] == 4){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_C(2020)/Conta_Pb_C(2009))}
      
      if(region[j,i] == 5){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_CE(2020)/Conta_Pb_CE(2009))}
      
      if(region[j,i] == 6){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_SO(2020)/Conta_Pb_SO(2009))}
      
      if(region[j,i] == 7){traj_dust_pb[j,i] <- traj_dust_pb[j,i] * conso_dust[j] * (Conta_Pb_SE(2020)/Conta_Pb_SE(2009))}
    }
  }
  
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
  
  print(i)
} 
