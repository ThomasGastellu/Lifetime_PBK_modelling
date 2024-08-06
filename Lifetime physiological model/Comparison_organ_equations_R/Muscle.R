##############################################################
#                         Muscle                             #
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
  #Model_6 = "Pendse et al. (2020)",
  #Model_7 = "Mallick et al (2020)",
  Model_8 = "Sarigiannis et al. (2020)", 
  #Model_9 = "Wu et al. 2015", # just for females
  #Model_10 = "Deepika et al (2021)",
  Model_11 = "Verner et al. (2008)",# just for females
  Model_12 = "Haddad et al. (2006)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- ifelse(Body[["M"]][["age_y"]] <= 24.3,
                  (0.3973+(0.201-0.3973)*exp(-0.141*Body[["M"]][["age_y"]] ))*Body[["M"]][["BW"]] ,
                  (0.3973+(0.201-0.3973)*exp(-0.141 * Body[["M"]][["age_y"]] ))* (-0.0001264 * Body[["M"]][["age_y"]] ^2 + 0.006131*Body[["M"]][["age_y"]]  + 0.926) * Body[["M"]][["BW"]] )

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["M"]][["age_y"]] <= 18,
                  (0.535*Body[["M"]][["age_y"]] **3 +    56.937*Body[["M"]][["age_y"]] **2 -     124.25*Body[["M"]][["age_y"]] + 1051.3)/1000,
                  (0.535*18**3 + 56.937*18**2 - 124.25*18 + 1051.3)/1000)

### Price K. et al. (2003)
Model_3 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (0.535*Body[["M"]][["age_y"]] **3 + 56.937*Body[["M"]][["age_y"]] **2 - 124.25*Body[["M"]][["age_y"]]  + 1051.3)/1000,
                  (0.535*18**3 + 56.937*18**2 - 124.25*18 + 1051.3)/1000)
                     
### Smith et al. (2014)
Model_4 <- (1.251e-01 + 1.458e-05*(Body[["M"]][["BW"]] *1000) - 2.927e-10*(Body[["M"]][["BW"]] *1000)^2 +  2.114e-15*(Body[["M"]][["BW"]] *1000)^3 -    5.250e-21*(Body[["M"]][["BW"]] *1000)^4) * Body[["M"]][["BW"]] 

### Ring et al. (2017)
Model_5 <- ifelse(Body[["M"]][["age_y"]] < 19,
                  (12.4/(1 + (11.0*exp(-0.45*Body[["M"]][["age_y"]] )))) + (19.7/(1 + (1*exp(-0.85*(Body[["M"]][["age_y"]] -13.7))))),
                  (12.4/(1 + (11.0*exp(-0.45*19)))) + (19.7/(1 + (1*exp(-0.85*(19-13.7))))))

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (1.26e-01*Body[["M"]][["age_h"]] **1 + 7.76e-06*(Body[["M"]][["age_h"]] **(1.76e+00)) +9.50e+02)/1000,
                  (1.26e-01*(18*365*24)**1 + 7.76e-06*((18*365*24)**(1.76e+00)) +    9.50e+02)/1000)

### Haddad et al. (2006)
Model_12 <- ifelse(Body[["M"]][["age_y"]] < 3,
                   (9.561e-02*Body[["M"]][["BW"]] +  1.601e-02*Body[["M"]][["HT"]] +  1.097e-01*Body[["M"]][["age_y"]]),
                   ifelse(Body[["M"]][["age_y"]] >= 3 & Body[["M"]][["age_y"]] < 18,   
                          (2.789e-01*Body[["M"]][["BW"]] -  6.358e-02*Body[["M"]][["HT"]] +   9.850e-01*Body[["M"]][["age_y"]]+ 2.167),
                          (2.598e-01*Body[["M"]][["BW"]] +  1.206e-01*Body[["M"]][["HT"]]-  4.300e-03*Body[["M"]][["age_y"]] - 1.110)))
                            
 
### Verner et al. (2008)
Model_11 <- "NA"

Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_8, Model_11, Model_12)
Body[["M"]]$Muscle<- Model_List_M

##############################################################
# 2b. female-specific physiological equations for models
##############################################################

###  Beaudouin et al. (2010)
Model_1 <- ifelse(Body[["F"]][["age_y"]] <= 25.90709,
                  (0.2917+(0.207-0.2917)*exp(-0.339*Body[["F"]][["age_y"]]))*Body[["F"]][["BW"]] ,
                  (0.2917+(0.207-0.2917)*exp(-0.339*Body[["F"]][["age_y"]]))*(-0.0001264 * Body[["F"]][["age_y"]] ^2 + 0.006131*Body[["F"]][["age_y"]] + 0.926)*Body[["F"]][["BW"]]  )

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (0.015*Body[["F"]][["age_y"]] **6 - 
                     0.8155*Body[["F"]][["age_y"]] **5 + 
                     15.849*Body[["F"]][["age_y"]] **4 - 
                     134.99*Body[["F"]][["age_y"]] **3 + 
                     549.43*Body[["F"]][["age_y"]] **2 - 
                     530.65*Body[["F"]][["age_y"]]  + 958.87)/1000,
                  (0.015*18**6 - 0.8155*18**5 + 15.849*18**4 - 134.99*18**3 + 549.43*18**2 - 530.65*18 + 958.87)/1000)

### Price K. et al. (2003)
Model_3 <- "NA"

### Smith et al. (2014)
Model_4 <- (1.251e-01 + 1.458e-05*(Body[["F"]][["BW"]] *1000) -  2.927e-10*(Body[["F"]][["BW"]] *1000)^2 + 2.114e-15*(Body[["F"]][["BW"]] *1000)^3 - 5.250e-21*(Body[["F"]][["BW"]] *1000)^4) * Body[["F"]][["BW"]] 

### Ring et al. (2017)
Model_5 <- ifelse(Body[["F"]][["age_y"]] < 19,
                  (7.0/(1 + (6.5*exp(-0.55*Body[["F"]][["age_y"]] )))) + (13.0/(1 + (1*exp(-0.75*(Body[["F"]][["age_y"]] -11.5))))),
                  (7.0/(1 + (6.5*exp(-0.55*19)))) + (13.0/(1 + (1*exp(-0.75*(19-11.5))))))
                  
### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (1.26e-01*Body[["F"]][["age_h"]] **1 + 7.76e-06*(Body[["F"]][["age_h"]] **(1.76e+00)) + 9.50e+02)/1000,
                  (1.26e-01*(18*365*24)**1 + 7.76e-06*((18*365*24)**(1.76e+00)) +  9.50e+02)/1000)
                    

### Verner et al. (2008)

Model_11 <- ifelse(Body[["F"]][["age_y"]] < 3,
                   (9.563e-02*Body[["F"]][["BW"]]  +
                      1.650e-02*Body[["F"]][["HT"]]  +
                      9.102e-02*Body[["F"]][["age_y"]]  -
                      1.642e-01),
                   ifelse(Body[["F"]][["age_y"]] < 18,
                          (1.629e-01*Body[["F"]][["BW"]] +
                             2.603e-02*Body[["F"]][["HT"]]  +
                             4.661e-01*Body[["F"]][["age_y"]]  -
                             3.332),
                          (6.780*(Body[["F"]][["S_Skin_Verner"]]/10000)^1.629 - 
                             1.492e-03*Body[["F"]][["age_y"]] + 3.580)))  

### Haddad et al. (2006)
S_Skin_Haddad_F <- (Body[["F"]][["BW"]] **0.5150)*(Body[["F"]][["HT"]]**0.4220)*234.9

Model_12 <- ifelse(Body[["F"]][["age_y"]] < 3,
                   (9.563e-02*Body[["F"]][["BW"]]  +
                      1.650e-02*Body[["F"]][["HT"]]  +
                      9.102e-02*Body[["F"]][["age_y"]]  - 1.642e-01),
                   ifelse(Body[["F"]][["age_y"]] >= 3 & Body[["F"]][["age_y"]] < 18,   
                          (1.629e-01*Body[["F"]][["BW"]] +
                             2.603e-02*Body[["F"]][["HT"]]  +
                             4.661e-01*Body[["F"]][["age_y"]] - 3.332),
                          (6.780*(S_Skin_Haddad_F/10000)**1.629 -
                             1.492e-03*Body[["F"]][["age_y"]]  + 3.580)))                 

Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5,Model_8, Model_11, Model_12)
Body[["F"]]$Muscle<- Model_List_F

##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Muscle"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Muscle"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Muscle"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Muscle"]][["Model_4"]])
Body_M[16] <- unlist(Body[["M"]][["Muscle"]][["Model_5"]])
Body_M[17] <- unlist(Body[["M"]][["Muscle"]][["Model_8"]])
Body_M[18] <- unlist(Body[["M"]][["Muscle"]][["Model_11"]])
Body_M[19] <- unlist(Body[["M"]][["Muscle"]][["Model_12"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_3", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_4", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_5", "Volume (in L)" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_8", "Volume (in L)" = Body_M[[17]]),
  data.frame(Body_M, Model = "Model_11", "Volume (in L)" = Body_M[[18]]),
  data.frame(Body_M, Model = "Model_12", "Volume (in L)" = Body_M[[19]]))


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Muscle"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Muscle"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Muscle"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Muscle"]][["Model_4"]])
Body_F[16] <- unlist(Body[["F"]][["Muscle"]][["Model_5"]])
Body_F[17] <- unlist(Body[["F"]][["Muscle"]][["Model_8"]])
Body_F[18] <- unlist(Body[["F"]][["Muscle"]][["Model_11"]])
Body_F[19] <- unlist(Body[["F"]][["Muscle"]][["Model_12"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_3", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_4", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_5", "Volume (in L)" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_8", "Volume (in L)" = Body_F[[17]]),
  data.frame(Body_F, Model = "Model_11", "Volume (in L)" = Body_F[[18]]),
  data.frame(Body_F, Model = "Model_12", "Volume (in L)" = Body_F[[19]]))

Total_Organ <- rbind(combined_models_F, combined_models_M)

Total_Organ<- Total_Organ[, c(-12,-13,-14,-15,-16,-17,-18,-19)]
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
                   #"Model_6"= "Pendse et al. (2020)",
                   #"Model_7" = "Mallick et al (2020)",
                   "Model_8" = "Sarigiannis et al. (2020)", 
                   #"Model_9" = "Wu et al. (2015)", # just for females
                   #"Model_10" = "Deepika et al (2021)",
                   "Model_11" = "Verner et al. (2008)",# just for females
                   "Model_12" = "Haddad et al. (2006)"
)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Volume (in L)`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values = myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 

Plot_organ <- Plot_organ+  
  ggtitle("Muscle") +
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
LifeTimeChangesOrgans[["M"]]$Muscle <- Body[["M"]]$Muscle
LifeTimeChangesOrgans[["F"]]$Muscle <- Body[["F"]]$Muscle

