##############################################################
#                         BONE                               #
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
  #Model_4 = "Smith et al. (2014)",
  Model_5 = "Ring et al. (2017)",
  #Model_6 = "Pendse et al. (2020)",
  #Model_7 = "Mallick et al (2020)",
  Model_8 = "Sarigiannis et al. (2020)" 
  #Model_9 = "Wu et al. 2015", # just for females
  #Model_10 = "Deepika et al (2021)",
  #Model_11 = "Verner et al. (2008)",# just for females
  #Model_12 = "Haddad et al. (2006)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- (0.026 + (0.043-0.026) * exp(-0.091*Body[["M"]][["age_y"]]))*Body[["M"]][["BW"]]


### Haddad et al. (2001)
Model_2 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (-0.0306*Body[["M"]][["age_y"]]**5 + 
                     0.5222*Body[["M"]][["age_y"]]**4 + 
                     9.7109*Body[["M"]][["age_y"]]**3 - 
                     197.97*Body[["M"]][["age_y"]]**2 + 
                     1089.7*Body[["M"]][["age_y"]] + 546.6)/1000,
                  (-0.0306*18**5 + 0.5222*18**4 + 9.7109*18**3 - 197.97*18**2 + 1089.7*18 + 546.6)/1000)

### Price K. et al. (2003)
Model_3 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (-0.0306*Body[["M"]][["age_y"]]**5 + 
                     0.5222*Body[["M"]][["age_y"]]**4 + 
                     9.7109*Body[["M"]][["age_y"]]**3 - 
                     197.97*Body[["M"]][["age_y"]]**2 + 
                     1089.7*Body[["M"]][["age_y"]] + 546.6)/1000,
                  (-0.0306*18**5 + 0.5222*18**4 + 9.7109*18**3 - 197.97*18**2 + 1089.7*18 + 546.6)/1000)
  

### Ring et al. (2017)
TBBMC_Ring_M <- ifelse(Body[["M"]][["age_y"]] < 50,
                     0.89983 + ((2.9901-0.89989)/(1+1*exp((14.17081-Body[["M"]][["age_y"]])/1.58179))),
                     0.89983 + ((2.9901-0.89989)/(1+1*exp((14.17081-Body[["M"]][["age_y"]])/1.58179))) - (0.0019*Body[["M"]][["age_y"]]))

Model_5 <- (TBBMC_Ring_M/0.65)/0.5

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (5.97e-02*Body[["M"]][["age_h"]]**1 +   1.26e+00*(Body[["M"]][["age_h"]]**(6.10e-01)) + 4.52e+02)/1000,
                  (5.97e-02*(18*365*24)**1 +  1.26e+00*((18*365*24)**(6.10e-01)) + 4.52e+02)/1000)
                              
                            
Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_5, Model_8)
Body[["M"]]$Bone<- Model_List_M                           
                            
##############################################################
# 2b. female-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- (0.0253 + (0.0429-0.0253) * exp(-0.0792*Body[["F"]][["age_y"]] ))*Body[["F"]][["BW"]] 

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (-2.831e-03*Body[["F"]][["age_y"]] **5 - 
                     0.1818*Body[["F"]][["age_y"]] **4 + 
                     10.685*Body[["F"]][["age_y"]] **3 - 
                     142.88*Body[["F"]][["age_y"]] **2 + 
                     782.05*Body[["F"]][["age_y"]]  + 609.64)/1000,
                  (-2.831e-03*18**5 - 0.1818*18**4 + 10.685*18**3 - 142.88*18**2 + 782.05*18 + 609.64)/1000)
                  
### Price K. et al. (2003) - Spaceholder
Model_3 <- "NA"

### Ring et al. (2017)
TBBMC_Ring_F <- ifelse(Body[["F"]][["age_y"]] < 50,
                       ((2.14976-0.74042)/(1+1*exp((12.35466-Body[["F"]][["age_y"]] )/1.35750))),
                       0.74042 + ((2.14976-0.74042)/(1+1*exp((12.35466-Body[["F"]][["age_y"]] )/1.35750))) - (0.0056*Body[["F"]][["age_y"]]))

Model_5 <- (TBBMC_Ring_F/0.65)/0.5

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (5.97e-02*Body[["F"]][["age_h"]] **1 +   1.26e+00*(Body[["F"]][["age_h"]] **(6.10e-01)) + 4.52e+02)/1000,
                  (5.97e-02*(18*365*24)**1 + 1.26e+00*((18*365*24)**(6.10e-01)) + 4.52e+02)/1000)



Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_5, Model_8)
Body[["F"]]$Bone<- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Bone"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Bone"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Bone"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Bone"]][["Model_5"]])
Body_M[16] <- unlist(Body[["M"]][["Bone"]][["Model_8"]])

Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_3", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_5", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_8", "Volume (in L)" = Body_M[[16]]))


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Bone"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Bone"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Bone"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Bone"]][["Model_5"]])
Body_F[16] <- unlist(Body[["F"]][["Bone"]][["Model_8"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_3", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_5", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_8", "Volume (in L)" = Body_F[[16]]))


Total_Organ <- rbind(combined_models_F, combined_models_M)

Total_Organ<- Total_Organ[, c(-12,-13,-14,-15,-16)]
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
                   #"Model_4" = "Smith et al. (2014)",
                   "Model_5" = "Ring et al. (2017)",
                   #"Model_6"= "Pendse et al. (2020)",
                   #"Model_7" = "Mallick et al (2020)",
                   "Model_8" = "Sarigiannis et al. (2020)" 
                   #"Model_9" = "Wu et al. (2015)", # just for females
                   #"Model_10" = "Deepika et al (2021)",
                   #"Model_11" = "Verner et al. (2008)",# just for females
                   #"Model_12" = "Haddad et al. (2006)"
)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Volume (in L)`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values = myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 

Plot_organ <- Plot_organ+  
  ggtitle("Bone") +
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
LifeTimeChangesOrgans[["M"]]$Bone <- Body[["M"]]$Bone
LifeTimeChangesOrgans[["F"]]$Bone <- Body[["F"]]$Bone
