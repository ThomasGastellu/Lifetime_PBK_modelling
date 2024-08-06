##############################################################
#                         BLOOD                              #
##############################################################
#Delete previous files
rm(list = ls(pattern = "^Total_Organ"))
rm(list = ls(pattern = "^combined"))
rm(list = ls(pattern = "^Model"))
rm(list = ls(pattern = "^Plot_organ"))

library(ggplot2)

# Execute the function and store the results in the list "Body"
results <- generate_body_data()
Body <- results$Body

##############################################################
# 1. List of models
##############################################################

List_of_Models <- list(
  Model_1 = "Beaudouin et al. (2010)",
  Model_2 = "Haddad et al. (2001)",
  Model_3 = "Price K. et al. (2003)", # just for males
  Model_4 = "Smith et al. (2014)",
  Model_5 = "Ring et al. (2017)",
  Model_6 = "Pendse et al. (2020)",
  Model_7 = "Mallick et al (2020)",
  Model_8 = "Sarigiannis et al. (2020)", 
  Model_9 = "Wu et al. 2015" # just for females
  #Model_10 = "Deepika et al (2021)"
  #Model_11 = "Verner et al. (2008)"# just for females
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010) model
Model_1 <- (0.0761 + (0.0289 - 0.0761)*exp(-0.592*Body[["M"]][["age_y"]]))*Body[["M"]][["BW"]]

### Haddad et al. (2001) model based on age condition
Model_2 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (-0.0623*Body[["M"]][["age_y"]]**5 + 
                     2.4425*Body[["M"]][["age_y"]]**4 - 
                     31.37*Body[["M"]][["age_y"]]**3 + 
                     149.98*Body[["M"]][["age_y"]]**2 + 
                     31.305*Body[["M"]][["age_y"]] + 393.7)/1000,
                  (-0.0623*18**5 + 2.4425*18**4 - 31.37*18**3 + 149.98*18**2 + 31.305*18 + 393.7)/1000)
                  
### Price K. et al. (2003) model based on age condition
Model_3 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (-0.0623*Body[["M"]][["age_y"]]**5 + 
                     2.4425*Body[["M"]][["age_y"]]**4 - 
                     31.37*Body[["M"]][["age_y"]]**3 + 
                     149.98*Body[["M"]][["age_y"]]**2 + 
                     31.305*Body[["M"]][["age_y"]] + 393.7)/1000,
                  (-0.0623*18**5 + 2.4425*18**4 - 31.37*18**3 + 149.98*18**2 + 31.305*18 + 393.7)/1000)
                
### Smith et al. (2014)
Model_4 <- (8.970e-02 - 3.500e-07*(Body[["M"]][["BW"]]*1000) +6.540e-13*(Body[["M"]][["BW"]]*1000)^2) * Body[["M"]][["BW"]]

### Ring et al. (2017)
Model_5 <- (3.33*Body[["M"]][["S_Skin_Ring"]] - 0.81)/1.06

## Pendse et al. (2020)
Model_6 <-  (10^(1.2082*log10(Body[["M"]][["S_Skin_Pendse"]])+3.2869))/1000

### Mallick et al. (2020)
Model_7 <-  (10^(1.2082*log10(Body[["M"]][["S_Skin_Mallick"]])+3.2869))/1000

### Sarigiannis et al. (2020)
Model_8 <-ifelse(Body[["M"]][["age_y"]] < 18,
                 (1.15e-01*Body[["M"]][["age_h"]]**1 -
                    1.10e-01*(Body[["M"]][["age_h"]]**(9.92e-01)) +
                    1.33e+02)/500,
                 (1.15e-01*(18*365*24)**1 -
                    1.10e-01*((18*365*24)**(9.92e-01)) +
                    1.33e+02)/500)


### Wu et al. (2015) - only space holder as male was not considered by Wu et al.

Model_9 <- "NA"

Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8, Model_9)
Body[["M"]]$Blood<- Model_List_M


##############################################################
# 2b. female-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010) & Brochot et al. (2019)
Model_1 <-ifelse(Body[["F"]][["age_y"]] < 1,
                 (-0.0273 * Body[["F"]][["age_y"]] + 0.0771) * Body[["F"]][["BW"]],
                 ifelse(Body[["F"]][["age_y"]] < 14.019723,
                        (3.28e-05*Body[["F"]][["age_y"]]^3 - 
                           1.21e-03*Body[["F"]][["age_y"]]^2 +
                           1.24e-02*Body[["F"]][["age_y"]] +
                           3.86e-02) *Body[["F"]][["BW"]],
                        0.065 * Body[["F"]][["BW"]]))
                 

### Haddad et al. (2001)
Model_2 <-ifelse(Body[["F"]][["age_y"]] < 18,
                 (0.0018*Body[["F"]][["age_y"]]**5 + 
                    0.0959*Body[["F"]][["age_y"]]**4 - 
                    4.4055*Body[["F"]][["age_y"]]**3 + 
                    45.442*Body[["F"]][["age_y"]]**2 + 
                    82.808*Body[["F"]][["age_y"]]+ 292.26)/1000,
                 (0.0018*18**5 +  0.0959*18**4 - 4.4055*18**3 + 45.442*18**2 + 82.808*18 + 292.26)/1000)

### Price K. et al. (2003) as a place holder as it was not applied for male subjects
Model_3 <- "NA"

### Smith et al. (2014)
Model_4 <- (8.970e-02 - 3.500e-07*(Body[["F"]][["BW"]]*1000) + 6.540e-13*(Body[["F"]][["BW"]]*1000)^2) * Body[["F"]][["BW"]]

### Ring et al. (2017)
Model_5 <- (2.66*Body[["F"]][["S_Skin_Ring"]] - 0.46)/1.06


### Pendse et al. (2020)
Model_6 <-  (10^(1.2082*log10(Body[["F"]][["S_Skin_Pendse"]] )+3.2869))/1000

### Mallick et al. (2020)
Model_7 <-  (10^(1.2082*log10(Body[["F"]][["S_Skin_Mallick"]] )+3.2869))/1000

### Sarigiannis et al. (2020)
Model_8<- ifelse(Body[["F"]][["age_y"]] < 18,
                 (1.15e-01*Body[["F"]][["age_h"]]**1 -1.10e-01*(Body[["F"]][["age_h"]]**(9.92e-01)) +1.33e+02)/500,  
                 (1.15e-01*(18*365*24)**1 -1.10e-01*((18*365*24)**(9.92e-01)) + 1.33e+02)/500)


### Wu et al. (2015)
Model_9 <- (10^(1.2082*log10(Body[["F"]][["S_Skin_Wu"]]/10000)+3.2869))/1000



Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8, Model_9)
Body[["F"]]$Blood<- Model_List_F

##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Blood"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Blood"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Blood"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Blood"]][["Model_4"]])
Body_M[16] <- unlist(Body[["M"]][["Blood"]][["Model_5"]])
Body_M[17] <- unlist(Body[["M"]][["Blood"]][["Model_6"]])
Body_M[18] <- unlist(Body[["M"]][["Blood"]][["Model_7"]])
Body_M[19] <- unlist(Body[["M"]][["Blood"]][["Model_8"]])
Body_M[20] <- unlist(Body[["M"]][["Blood"]][["Model_9"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_3", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_4", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_5", "Volume (in L)" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_6", "Volume (in L)" = Body_M[[17]]),
  data.frame(Body_M, Model = "Model_7", "Volume (in L)" = Body_M[[18]]),
  data.frame(Body_M, Model = "Model_8", "Volume (in L)" = Body_M[[19]]),
  data.frame(Body_M, Model = "Model_9", "Volume (in L)" = Body_M[[20]]) )


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Blood"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Blood"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Blood"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Blood"]][["Model_4"]])
Body_F[16] <- unlist(Body[["F"]][["Blood"]][["Model_5"]])
Body_F[17] <- unlist(Body[["F"]][["Blood"]][["Model_6"]])
Body_F[18] <- unlist(Body[["F"]][["Blood"]][["Model_7"]])
Body_F[19] <- unlist(Body[["F"]][["Blood"]][["Model_8"]])
Body_F[20] <- unlist(Body[["F"]][["Blood"]][["Model_9"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_3", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_4", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_5", "Volume (in L)" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_6", "Volume (in L)" = Body_F[[17]]),
  data.frame(Body_F, Model = "Model_7", "Volume (in L)" = Body_F[[18]]),
  data.frame(Body_F, Model = "Model_8", "Volume (in L)" = Body_F[[19]]),
  data.frame(Body_F, Model = "Model_9", "Volume (in L)" = Body_F[[20]]) )


Total_Organ <- rbind(combined_models_F, combined_models_M)

Total_Organ<- Total_Organ[, c(-12,-13,-14,-15,-16,-17,-18,-19,-20)]
colnames(Total_Organ)[14] <- "Volume (in L)"

Total_Organ <- transform(Total_Organ, `Volume (in L)` = as.numeric(`Volume (in L)`))
colnames(Total_Organ)[1] <- "Age (in years)"
colnames(Total_Organ)[2] <- "Age (in months)"
colnames(Total_Organ)[3] <- "Age (in hours)"
colnames(Total_Organ)[14] <- "Volume (in L)"
str(Total_Organ)

# Create a named vector for the labels
model_labels <- c( "Model_1" = "Beaudouin et al. (2010)",
                   "Model_2" = "Haddad et al. (2001)",
                   "Model_3" = "Price K. et al. (2003)", # just for males
                   "Model_4" = "Smith et al. (2014)",
                   "Model_5" = "Ring et al. (2017)",
                   "Model_6"= "Pendse et al. (2020)",
                   "Model_7" = "Mallick et al (2020)",
                   "Model_8" = "Sarigiannis et al. (2020)", 
                   "Model_9" = "Wu et al. (2015)" # just for females
                   )
myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Volume (in L)`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values =myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 


Plot_organ <- Plot_organ+  
  ggtitle("BLOOD") +
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.y= element_text(lineheight = 1.5,size=14),
    axis.title.x= element_text(lineheight = 1.5,size=14),
    axis.text.y= element_text(lineheight = 1.5,size=14, margin=margin(r=6),vjust =1),
    axis.text.x= element_text(lineheight = 1.5,size=14),
    legend.text=element_text(lineheight = 1.5, size=14),
    legend.title=element_text(lineheight = 1.5, size=14),
    legend.background = element_rect(
      fill = "white", 
      linewidth = 4, 
      colour = "white"
    ),
    axis.ticks = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )

Plot_organ <- Plot_organ  + 
  facet_wrap(facets = ~ Gender)+
  theme(strip.text = element_text(size=15))


# save changes of volume over age in list
LifeTimeChangesOrgans[["M"]]$Blood <- Body[["M"]]$Blood
LifeTimeChangesOrgans[["F"]]$Blood <- Body[["F"]]$Blood


