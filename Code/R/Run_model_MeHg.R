## Run PBK model MeHg ## 

# Packages
library(mrgsolve)
library(dplyr)
library(tidyr)
library(Rcpp)
library(ggplot2)

# Number of individuals in population
nb_indiv <- 10

# Open table of exposures trajectories
load("~/Exposure_data/Expo_example_MeHg_lifetime.RData")

# Extraction of sex and transformation of female in 0
sexes <- sapply(pop, function(x) x$Sex)
sexes <- as.vector(t(sexes))
sexes[sexes==2]<-0
sexes <- as.integer(sexes)

# Extraction of MeHg exposure
traj_expo_mehg <- as.data.frame(sapply(pop, function(x) x$Expo_Mehg))

## Compilation of PBK model
model <- mread("model_MeHg_lifetime.cpp")

## Run model for the population
# Create table to save results
Results <- data.frame(Age = c(1:29095))
for(i in c(1:nb_indiv))
{ expo <- data.frame(time = c(1:29094),
                     amt = traj_expo_mehg[,i],
                     cmt = "EXPO")
  
  
  out <- model %>% param(SEXBABY = sexes[i]) %>% 
                  ev(expo) %>% mrgsim(delta=1, end=29094)
  res <- out |> slice(seq(1, n(), by = 2))
  Results <- cbind(Results, res$CH/1000)  ## Save MeHg concentration in hair (in µg/g) 
  print(i)
}

## Aggragation of results
pop_mehg_int <- data.frame(moy = apply(Results[,-1], 1, mean, na.rm=T),
                                 p05 = apply(Results[,-1], 1, quantile, 0.05, na.rm=T),
                                 med = apply(Results[,-1], 1, quantile, 0.5, na.rm=T),
                                 p95 = apply(Results[,-1], 1, quantile, 0.95, na.rm=T),
                                 age = rep(c(1:29095)/365))

## Graphical illustration of results
plot_mehg <- ggplot(pop_mehg_int, aes(x=age,y=moy)) + 
                    geom_line(size = 1, col = "orange") +
                    geom_ribbon(aes(ymin = p05, ymax = p95), fill = "orange", alpha = 0.3) + 
                    xlab("Age (in years)") + ylab("Methylmercury concentration in hair (in µg/g)")
plot_mehg
