#### SIMULATION OF INTERNAL EXPOSURE FOR MERCURY ####
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
Hg_Hair <- tibble()
Hg_Urinecr <- tibble()
Hg_Urine <- tibble()
Hg_Kidney <- tibble()

### Reading of PBK models
model_THg <- mread("Model_THg", file = "PBK models/Model_THg.cpp")

### Selection of exposure trajectories to different sources of exposure
# Air contaminations are in ng/m3;
# Soil and dust contamination are in µg/g of soil/dust;
# Dietary exposure is in µg/kg/day.
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
      traj_soil_hg[j,i] <- traj_soil_hg[j,i] * conso_soil[j]
    }
    else{
      traj_soil_hg[j,i] <- traj_soil_hg[j,i] * conso_soil[j]
    }
  }

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
  
  print(i)
} 
