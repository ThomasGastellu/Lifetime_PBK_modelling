#### SIMULATION OF INTERNAL EXPOSURE FOR INORGANIC ARSENIC ####

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
As_Urinecr <- tibble()
As_Urine <- tibble()
As_Kidney <- tibble()

### Reading of PBK models
model_As <- mread("Model_As", file = "PBK models/Model_As.cpp")

### Selection of exposure trajectories to different sources of exposure
# Air contaminations are in ng/m3;
# Soil and dust contamination are in µg/g of soil/dust;
# Dietary exposure is in µg/kg/day.
traj_diet_as3 <- sapply(pop_ref, function(x) x$expo_asIII)
traj_diet_as5 <- sapply(pop_ref, function(x) x$expo_asV)
traj_soil_as <- sapply(pop_ref, function(x) x$soil_as)
traj_air_as <- sapply(pop_ref, function(x) x$air_as/1000000)
traj_dust_as <- sapply(pop_ref, function(x) x$dust_as)

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
      traj_soil_as[j,i] <- traj_soil_as[j,i] * conso_soil[j]
    }
    else{
      traj_soil_as[j,i] <- traj_soil_as[j,i] * conso_soil[j]
    }
  }
  
  ## Extrapolation of air contamination over time
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
      
      if(region[j,i] == 0){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_NO(x)/Conta_As_NO(2009))}
      
      if(region[j,i] == 1){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_E(x)/Conta_As_E(2009))}
      
      if(region[j,i] == 2){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_IdF(x)/Conta_As_IdF(2009))}
      
      if(region[j,i] == 3){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_O(x)/Conta_As_O(2009))}
      
      if(region[j,i] == 4){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_C(x)/Conta_As_C(2009))}
      
      if(region[j,i] == 5){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_CE(x)/Conta_As_CE(2009))}
      
      if(region[j,i] == 6){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_SO(x)/Conta_As_SO(2009))}
      
      if(region[j,i] == 7){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_SE(x)/Conta_As_SE(2009))}
    }
    
    else{
      if(region[j,i] == 0){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_NO(2020)/Conta_As_NO(2009))}
      
      if(region[j,i] == 1){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_E(2020)/Conta_As_E(2009))}
      
      if(region[j,i] == 2){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_IdF(2020)/Conta_As_IdF(2009))}
      
      if(region[j,i] == 3){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_O(2020)/Conta_As_O(2009))}
      
      if(region[j,i] == 4){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_C(2020)/Conta_As_C(2009))}
      
      if(region[j,i] == 5){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_CE(2020)/Conta_As_CE(2009))}
      
      if(region[j,i] == 6){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_SO(2020)/Conta_As_SO(2009))}
      
      if(region[j,i] == 7){traj_dust_as[j,i] <- traj_dust_as[j,i] * conso_dust[j] * (Conta_As_SE(2020)/Conta_As_SE(2009))}
    }
  }
  
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
