#############################################################
#                     INTESTINE                             #
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
  #Model_5 = "Ring et al. (2017)",
  Model_6 = "Pendse et al. (2020)",
  Model_7 = "Mallick et al (2020)",
  #Model_8 = "Sarigiannis et al. (2020)", 
  Model_9 = "Wu et al. 2015" # just for females
  #Model_10 = "Deepika et al (2021)",
  #Model_11 = "Verner et al. (2008)",# just for females
  #Model_12 = "Haddad et al. (2006)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- ifelse(Body[["M"]][["age_y"]] < 16,
                  (-8.2562e-05*Body[["M"]][["age_y"]]^2 + 
                     1.3523e-03*Body[["M"]][["age_y"]] +
                     1.293e-02) * Body[["M"]][["BW"]],
                  0.014 * Body[["M"]][["BW"]])

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (-4.7817e-02*Body[["M"]][["age_y"]]**4 + 
                     1.925*Body[["M"]][["age_y"]]**3 - 
                     22.382*Body[["M"]][["age_y"]]**2 + 
                     107.09*Body[["M"]][["age_y"]] + 51.125)/1000,
                  (-4.7817e-02*18**4 + 1.925*18**3 - 22.382*18**2 + 107.09*18 + 51.125)/1000)

### Price K. et al. (2003)
Model_3 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (-4.7817e-02*Body[["M"]][["age_y"]]**4 +
                     1.925*Body[["M"]][["age_y"]]**3 - 
                     22.382*Body[["M"]][["age_y"]]**2 + 
                     107.09*Body[["M"]][["age_y"]] + 51.125)/1000,
                  (-4.7817e-02*18**4 + 1.925*18**3 - 22.382*18**2 + 107.09*18 + 51.125)/1000)

### Smith et al. (2014)
Model_4 <- (1.650e-02)*Body[["M"]][["BW"]]

### Pendse et al. (2020)
FatPendse <-ifelse(Body[["M"]][["age_y"]] < 20,
                   ((2.8975*exp(-0.129*Body[["M"]][["age_y"]]) + 0.67) * Body[["M"]][["BMI"]]+ 0.2635*Body[["M"]][["age_y"]] - 4.843)*Body[["M"]][["BW"]]/100,
                   ((-5.33798*Body[["M"]][["BMI"]] + 0.11149*Body[["M"]][["BMI"]]^2 + 0.09795*Body[["M"]][["age_y"]]+ 85.24521))*Body[["M"]][["BW"]]/100)
LBM_Pendse <- Body[["M"]][["BW"]]- FatPendse 
Model_6 <- 0.021*LBM_Pendse

### Mallick et al. (2020)
FatMallick <- ifelse(Body[["M"]][["age_y"]] < 25,
       ((1.4471*exp(-0.0761*Body[["M"]][["age_y"]]) + 0.52) * Body[["M"]][["BMI"]]-0.10124*Body[["M"]][["age_y"]]+ 5.0465)*Body[["M"]][["BW"]]/100,
       ((-6.0487*Body[["M"]][["BMI"]]+ 0.1177*Body[["M"]][["BMI"]]^2 +0.03155*Body[["M"]][["age_y"]] + 97.2025) * 0.979)*Body[["M"]][["BW"]]/100)
LBM_Mallick <- Body[["M"]][["BW"]] - FatMallick
Model_7 <- 0.021*LBM_Mallick


### Sarigiannis et al. (2020)
# 
# Model_8 <- ifelse(Body[["M"]][["age_y"]] < 18,
#                   (8.20e-02*Body[["M"]][["age_h"]]**1 +  4.41e-02*(Body[["M"]][["age_h"]]**(1.04e+00)) + 9.00e+01)/1000,
#                   (8.20e-02*(18*365*24)**1 +4.41e-02*((18*365*24)**(1.04e+00)) + 9.00e+01)/1000)

### Wu et al. (2015) - spaceholder
Model_9 <- "NA"


Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_6, Model_7, Model_9)
Body[["M"]]$Intestine<- Model_List_M


##############################################################
# 2b. female-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- ifelse(Body[["F"]][["age_y"]] < 14.453301,
                  (-7.421e-05*Body[["F"]][["age_y"]]^2 + 
                     1.276e-03*Body[["F"]][["age_y"]] +
                     1.298e-02) *Body[["F"]][["BW"]] ,
                  0.0160 * Body[["F"]][["BW"]] )

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (-0.0513*Body[["F"]][["age_y"]]**4 + 
                     2.0352*Body[["F"]][["age_y"]]**3 - 
                     23.478*Body[["F"]][["age_y"]]**2 + 
                     110.61*Body[["F"]][["age_y"]]  + 49.229)/1000,
                  (-0.0513*18**4 + 2.0352*18**3 - 23.478*18**2 + 110.61*18 + 49.229)/1000)


### Price K. et al. (2003)
Model_3 <- "NA"

### Smith et al. (2014)
Model_4<- (1.650e-02)*Body[["F"]][["BW"]] 

### Pendse et al. (2020)
FatPendse <- ifelse(Body[["F"]][["age_y"]] < 25,
                  ((1.5334*exp(-0.103*Body[["F"]][["age_y"]] ) + 0.67) * Body[["F"]][["BMI"]] +0.6276*Body[["F"]][["age_y"]] + 1.0301)*Body[["F"]][["BW"]] /100,
                  ((1.9224*Body[["F"]][["BMI"]] - 0.018517*Body[["F"]][["BMI"]]^2 +0.05537*Body[["F"]][["age_y"]]  - 0.794894))*Body[["F"]][["BW"]] /100)
LBM_Pendse <- Body[["F"]][["BW"]] - FatPendse
Model_6 <- 0.027*LBM_Pendse

### Mallick et al. (2020)
FatMallick <- ifelse(Body[["F"]][["age_y"]] < 25,
                     ((1.5334*exp(-0.103*Body[["F"]][["age_y"]] ) + 0.67) * Body[["F"]][["BMI"]] +0.6276*Body[["F"]][["age_y"]] + 1.0301)*Body[["F"]][["BW"]] /100,
                     ((1.9224*Body[["F"]][["BMI"]] - 0.018517*Body[["F"]][["BMI"]]^2 +0.05537*Body[["F"]][["age_y"]]  - 0.794894))*Body[["F"]][["BW"]] /100)
LBM_Mallick <- Body[["F"]][["BW"]] - FatMallick
Model_7 <- 0.027*LBM_Mallick

### Sarigiannis et al. (2020)
# 
# Model_8 <- ifelse(Body[["M"]][["age_y"]] < 18,
#                   (8.20e-02*Body[["F"]][["age_h"]] **1 +4.41e-02*(Body[["F"]][["age_h"]] **(1.04e+00)) +9.00e+01)/1000,
#                    (8.20e-02*(18*365*24)**1 +4.41e-02*((18*365*24)**(1.04e+00)) +9.00e+01)/1000)
                                                   

### Wu et al. (2015)
FatWu <- ifelse(Body[["F"]][["age_y"]] <= 25,
                ((1.5334*exp(-0.103*Body[["F"]][["age_y"]] )
                  + 0.67) * Body[["F"]][["BMI"]]  + 
                   0.6276 * Body[["F"]][["age_y"]]  + 1.0301)*Body[["F"]][["BW"]] /100,
                (1.9224*Body[["F"]][["BMI"]] -
                   0.018517*Body[["F"]][["BMI"]] ^2 + 
                   0.05537*Body[["F"]][["age_y"]] - 
                   0.794894) * Body[["F"]][["BW"]] /100)
LBM_Wu = Body[["F"]][["BW"]] - FatWu

Model_9 <- 0.027 * LBM_Wu

Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_6, Model_7, Model_9)
Body[["F"]]$Intestine<- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Intestine"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Intestine"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Intestine"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Intestine"]][["Model_4"]])
Body_M[16] <- unlist(Body[["M"]][["Intestine"]][["Model_6"]])
Body_M[17] <- unlist(Body[["M"]][["Intestine"]][["Model_7"]])
Body_M[18] <- unlist(Body[["M"]][["Intestine"]][["Model_9"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_3", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_4", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_6", "Volume (in L)" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_7", "Volume (in L)" = Body_M[[17]]),
  data.frame(Body_M, Model = "Model_9", "Volume (in L)" = Body_M[[18]]) )


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Intestine"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Intestine"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Intestine"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Intestine"]][["Model_4"]])
Body_F[16] <- unlist(Body[["F"]][["Intestine"]][["Model_6"]])
Body_F[17] <- unlist(Body[["F"]][["Intestine"]][["Model_7"]])
Body_F[18] <- unlist(Body[["F"]][["Intestine"]][["Model_9"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_3", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_4", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_6", "Volume (in L)" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_7", "Volume (in L)" = Body_F[[17]]),
  data.frame(Body_F, Model = "Model_9", "Volume (in L)" = Body_F[[18]]))


Total_Organ <- rbind(combined_models_F, combined_models_M)

Total_Organ<- Total_Organ[, c(-12,-13,-14,-15,-16,-17,-18)]
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
                   #"Model_5" = "Ring et al. (2017)",
                   "Model_6"= "Pendse et al. (2020)",
                   "Model_7" = "Mallick et al (2020)",
                   #"Model_8" = "Sarigiannis et al. (2020)", 
                   "Model_9" = "Wu et al. (2015)" # just for females
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
  ggtitle("Intestine") +
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
LifeTimeChangesOrgans[["M"]]$Intestine <- Body[["M"]]$Intestine
LifeTimeChangesOrgans[["F"]]$Intestine <- Body[["F"]]$Intestine

