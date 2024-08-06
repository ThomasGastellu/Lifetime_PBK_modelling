
########################################################################
# Function: Generating body weight and height based on French population
########################################################################

generate_body_data <- function() {
  # Create empty list to store results
  Body <- list()
  
  # Helper function to calculate body metrics
  calculate_metrics <- function(age_m, is_male) {
    if (is_male) {
      BW <- 3.938425 + 7.518199e-01* age_m - 2.023793e-02 * (age_m)^2 + 2.921682e-04 * (age_m)^3 - 
		2.06762e-06 * (age_m)^4 + 8.469e-09 * (age_m)^5 - 2.188427e-11 * (age_m)^6 + 
		3.699776e-14 * (age_m)^7 - 4.099077e-17 * (age_m)^8 + 2.874804e-20 * (age_m)^9 - 
		1.159732e-23 * (age_m)^10 + 2.052602e-27 * (age_m)^11
      
      HT <- 64.35 + 8.146e-01 * age_m - 7.474e-04 * (age_m)^2 - 6.322e-06 * (age_m)^3 +
        	1.903e-08 * (age_m)^4 - 1.995e-11 * (age_m)^5 + 7.297e-15 * (age_m)^6
      
      
    } else {
      BW <- 3.932403 + 6.866462e-01 * age_m - 1.949911e-02 * (age_m)^2 + 3.1311e-04 * (age_m)^3 - 
		2.466654e-06 * (age_m)^4 + 1.113217e-08 * (age_m)^5 - 3.131402e-11 * (age_m)^6 + 
		5.693737e-14 * (age_m)^7 - 6.706947e-17 * (age_m)^8 + 4.947858e-20 * (age_m)^9 - 
		2.079251e-23 * (age_m)^10 + 3.800367e-27 * (age_m)^11
      
      HT <- 57.63 + 1.083 * age_m - 3.679e-03 * (age_m)^2 + 4.633e-06 * (age_m)^3 -
        4.226e-12 * (age_m)^5 + 2.302e-15 * (age_m)^6
    }
    
    BMI <- BW / (HT / 100)^2
    
    ## Body surface area (in m?)
    S_Skin_Pendse <- exp(-3.75 + 0.42*log(HT) + 0.52*log(BW))
    
    S_Skin_Mallick <- exp(-3.75 + 0.42*log(HT) +  0.52*log(BW))

    S_Skin_Ring <- ifelse(age_m < (18 *12),
                      0.024265 * BW**0.5378 * HT**0.3964,
                      sqrt(BW * HT / 3600))
    
    S_Skin_Wu <- BW^0.5150 * HT^0.4220 * 235
   
    S_Skin_Verner <- BW^0.5150 * HT^0.4220 * 234.9
    
    S_Skin_Deepika <- 0.007184*(BW **0.425)*(HT **0.725)
    
    S_Skin_Haddad <- (BW**0.5150)*(HT**0.4220)*234.9

    
    list(BW = BW, HT = HT, BMI = BMI, S_Skin_Pendse = S_Skin_Pendse,  S_Skin_Mallick = S_Skin_Mallick, S_Skin_Ring = S_Skin_Ring, S_Skin_Wu = S_Skin_Wu, 
         S_Skin_Verner = S_Skin_Verner, S_Skin_Deepika = S_Skin_Deepika, S_Skin_Haddad = S_Skin_Haddad)
  }
  


  # Generate data for male
  age_y <- c(1:29200) / 365
  age_m <- c(1:29200) / (29200 / 960)
  age_h <- c(1:29200) * 24

  
  male_metrics <- calculate_metrics(age_m, TRUE)
  
  Body[["M"]] <- data.frame(
    age_y = age_y,
    age_m = age_m,
    age_h = age_h,
    BW = male_metrics$BW,
    HT = male_metrics$HT,
    BMI = male_metrics$BMI,
    S_Skin_Pendse =  male_metrics$S_Skin_Pendse,
    S_Skin_Mallick = male_metrics$S_Skin_Mallick,
    S_Skin_Ring = male_metrics$S_Skin_Ring,
    S_Skin_Wu = male_metrics$S_Skin_Wu,
    S_Skin_Verner = male_metrics$S_Skin_Verner
   # S_Skin_Deepika = male_metrics$S_Skin_Deepika,
   # S_Skin_Haddad = male_metrics$S_Skin_Haddad
    
  )
  
  # Generate data for female
  female_metrics <- calculate_metrics(age_m, FALSE)
  
  Body[["F"]] <- data.frame(
    age_y = age_y,
    age_m = age_m,
    age_h = age_h,
    BW = female_metrics$BW,
    HT = female_metrics$HT,
    BMI = female_metrics$BMI,
    S_Skin_Pendse =  female_metrics$S_Skin_Pendse,
    S_Skin_Mallick = female_metrics$S_Skin_Mallick,
    S_Skin_Ring = female_metrics$S_Skin_Ring,
    S_Skin_Wu = female_metrics$S_Skin_Wu,
    S_Skin_Verner = female_metrics$S_Skin_Verner
    #S_Skin_Deepika = female_metrics$S_Skin_Deepika,
    # S_Skin_Haddad = female_metrics$S_Skin_Haddad
  )
  
  # Plot body weight for male
  BW_plot_M <- ggplot(data = Body[["M"]], aes(x = age_y, y = BW)) +
    geom_line(linewidth = 2) +
    labs(title = "Body Weight Over Age (Males)", x = "Age (years)", y = "Body Weight (kg)")
  
  # Plot body weight for female
  BW_plot_F <- ggplot(data = Body[["F"]], aes(x = age_y, y = BW)) +
    geom_line(linewidth = 2) +
    labs(title = "Body Weight Over Age (Females)", x = "Age (years)", y = "Body Weight (kg)")
  
  return(list(Body = Body, BW_plot_M = BW_plot_M, BW_plot_F = BW_plot_F))
}




