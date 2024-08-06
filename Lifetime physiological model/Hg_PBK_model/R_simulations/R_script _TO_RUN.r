### SIMULATION OF INTERNAL EXPOSURE FOR MERCURY ###
###################################################

library(mrgsolve)
library(dplyr)
library(readr)

###########################################################################################################
# <!> you will need a c compiler such as 'gcc' to compile this model <!>
# <!> you will need to install Rtools: https://cran.r-project.org/bin/windows/Rtools/ <!>
# <!> if needed, more information on https://www.youtube.com/watch?v=GxFiUEO_3zM

# Check the gcc compiler path 
path <- Sys.getenv("PATH") # check if you have gcc in the path
path
# Check if you are able to build packages (Rtools correctly installed)
pkgbuild::check_build_tools(debug = TRUE)
# if not, run the following lines in updating with your new path
# for example: 
# path <- c("C:\\rtools40\\mingw64\\bin\\gcc", path) # check where Rtools is installed on your computer
# path <- paste(path,collapse=";") 
# Sys.setenv(PATH=path)
###########################################################################################################


# Load in your R environment the exposure trajectories ####
load("Hg_PBK_model/R_simulations/Trajectories_of_exposure/POP_1.RData")
# a list called 'pop_ref' should be loaded with 250 elements, 9 objects per element of the list
# str(pop_ref)

# Selection of population parameters ####
## Gender #### (1: Male; 0: Female)
sexes <- sapply(pop_ref, function(x) x$sexe)
sexes <- as.vector(t(sexes))
sexes[sexes == 2] <- 0
sexes <- as.integer(sexes[1:10])

## Extraction of trajectories of exposure to mercury ####
# large matrix as outputs, 250 variables and 29094 observations
traj_diet_mehg <- sapply(pop_ref, function(x) x$expo_mehg) # inorganic mercury
traj_diet_ihg <- sapply(pop_ref, function(x) x$expo_hgi) # methyl mercury

# Load PBK model ####
model_PBK <- mread("Hg_PBK_model/R_simulations/PBK_model", file = "PBK_model/PBK_model_2.cpp")

# Run PBK model ####
## Create empty tables to save the results
Hg_Hair <- tibble()
Hg_Urine <- tibble()

for (i in c(1:10)) {
  ## Determination of distribution factor for individuals ####
  ### Bodyweight ####
  Delta_BW_ind <- rnorm(1, mean = 1, sd = 0.15)

  ### Creatinine excretion ####
  Delta_creat_ind <- rnorm(1, mean = 1, sd = 0.15)

  ### Variability for male ####
  Delta_Brain_M_ind <- abs(rnorm(1, mean = 1, sd = 0.05))
  Delta_Kidney_M_ind <- abs(rnorm(1, mean = 1, sd = 0.25))
  Delta_Liver_M_ind <- abs(rnorm(1, mean = 1, sd = 0.24))
  Delta_Pancreas_M_ind <- abs(rnorm(1, mean = 1, sd = 0.27))
  Delta_Stomach_M_ind <- abs(rnorm(1, mean = 1, sd = 0.31))
  Delta_Small_Intestine_M_ind <- abs(rnorm(1, mean = 1, sd = 0.12))
  Delta_Large_Intestine_M_ind <- abs(rnorm(1, mean = 1, sd = 0.20))
  Delta_Heart_M_ind <- abs(rnorm(1, mean = 1, sd = 0.19))
  Delta_Bone_M_ind <- abs(rnorm(1, mean = 1, sd = 0.01))
  Delta_Gonad_M_ind <- abs(rnorm(1, mean = 1, sd = 0.05))
  Delta_Lung_M_ind <- abs(rlnorm(1, mean = 1, sd = .33))
  Delta_Spleen_M_ind <- abs(rlnorm(1, mean = 1, sd = .38))
  Delta_Muscle_M_ind <- abs(rnorm(1, mean = 1, sd = .27))
  Delta_Adipose_M_ind <- abs(rnorm(1, mean = 1, sd = .42))

  ### Variability for female ####
  Delta_Brain_F_ind <- abs(rnorm(1, mean = 1, sd = 0.05))
  Delta_Kidney_F_ind <- abs(rnorm(1, mean = 1, sd = 0.25))
  Delta_Liver_F_ind <- abs(rnorm(1, mean = 1, sd = 0.25))
  Delta_Pancreas_F_ind <- abs(rnorm(1, mean = 1, sd = .29))
  Delta_Stomach_F_ind <- abs(rnorm(1, mean = 1, sd = .31))
  Delta_Small_Intestine_F_ind <- abs(rnorm(1, mean = 1, sd = .13))
  Delta_Large_Intestine_F_ind <- abs(rnorm(1, mean = 1, sd = .14))
  Delta_Heart_F_ind <- abs(rnorm(1, mean = 1, sd = .25))
  Delta_Bone_F_ind <- abs(rnorm(1, mean = 1, sd = .01))
  Delta_Gonad_F_ind <- abs(rnorm(1, mean = 1, sd = .05))
  Delta_Lung_F_ind <- abs(rlnorm(1, mean = 1, sd = .33))
  Delta_Spleen_F_ind <- abs(rlnorm(1, mean = 1, sd = .38))
  Delta_Muscle_F_ind <- abs(rlnorm(1, mean = 1, sd = .27))
  Delta_Adipose_F_ind <- abs(rlnorm(1, mean = 1, sd = .42))

  ## Trajectories of exposure to mercury ####
  expo_ihg <- ev(amt = (traj_diet_ihg[, i]), ii = 1, cmt = 1, addl = 0)
  expo_ihg <- as_data_set(expo_ihg)
  expo_ihg$time <- c(0:29093)

  expo_mehg <- ev(amt = (traj_diet_mehg[, i]), ii = 1, cmt = 2, addl = 0)
  expo_mehg <- as_data_set(expo_mehg)
  expo_mehg$time <- c(0:29093)

  expo_hg <- rbind(expo_ihg, expo_mehg)
  expo_hg <- expo_hg[order(expo_hg$time), ]

  # run the model
  t <- model_PBK %>%
    param(
      IND_SSKIN = 1,
      IND_FAT = 1,
      IND_BRAIN = 1,
      IND_KIDNEY = 1,
      IND_LIVER = 1,
      IND_BLOOD = 1,
      IND_HEART = 1,
      IND_MUSCLE = 1,
      IND_VSKIN = 1,
      IND_MARR = 1,
      IND_SPLEEN = 1,
      IND_PANCREAS = 1,
      IND_GONAD = 1,
      IND_LUNG = 1,
      IND_STOMACH = 1,
      IND_INTESTINE = 1,
      IND_CO = 1,
      IND_HCT = 1,
      IND_BONE = 1,
      IND_BREAST = 1,
      IND_TONGUE = 1,
      SEXBABY = sexes[i],
      Delta_Brain_F = Delta_Brain_F_ind,
      Delta_Kidney_F = Delta_Kidney_F_ind,
      Delta_Liver_F = Delta_Liver_F_ind,
      Delta_Pancreas_F = Delta_Pancreas_F_ind,
      Delta_Stomach_F = Delta_Stomach_F_ind,
      Delta_Small_Intestine_F = Delta_Small_Intestine_F_ind,
      Delta_Large_Intestine_F = Delta_Large_Intestine_F_ind,
      Delta_Heart_F = Delta_Heart_F_ind,
      Delta_Bone_F = Delta_Bone_F_ind,
      Delta_Gonad_F = Delta_Gonad_F_ind,
      Delta_Lung_F = Delta_Lung_F_ind,
      Delta_Spleen_F = Delta_Spleen_F_ind,
      Delta_Muscle_F = Delta_Muscle_F_ind,
      Delta_Adipose_F = Delta_Adipose_F_ind,
      Delta_Brain_F = Delta_Brain_F_ind,
      Delta_Kidney_F = Delta_Kidney_F_ind,
      Delta_Liver_F = Delta_Liver_F_ind,
      Delta_Pancreas_F = Delta_Pancreas_F_ind,
      Delta_Stomach_M = Delta_Stomach_M_ind,
      Delta_Small_Intestine_M = Delta_Small_Intestine_M_ind,
      Delta_Large_Intestine_M = Delta_Large_Intestine_M_ind,
      Delta_Heart_M = Delta_Heart_M_ind,
      Delta_Bone_M = Delta_Bone_M_ind,
      Delta_Gonad_M = Delta_Gonad_M_ind,
      Delta_Lung_M = Delta_Lung_M_ind,
      Delta_Spleen_M = Delta_Spleen_M_ind,
      Delta_Muscle_M = Delta_Muscle_M_ind,
      Delta_Adipose_M = Delta_Adipose_M_ind,
    ) %>%
    mrgsim(delta = 1, events = expo_hg)


  t <- as.data.frame(t)
  t <- t[, c("time", "CUHg", "CH")]
  t <- aggregate(t, by = list(t$time), mean)

  ## Combine the results ####
  Hg_Urine <- rbind(Hg_Urine, t$CUHg)
  Hg_Hair <- rbind(Hg_Hair, t$CH)
  ## Save the results ####
  save(Hg_Urine, file = "Results/Hg_Urine_aggregated.RData")
  save(Hg_Hair, file = "Results/Hg_Hair_aggregated.RData")

  # check the iteration number in the loop for
  print(i)
}

