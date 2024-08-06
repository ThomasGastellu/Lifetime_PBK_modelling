##############################################################
#                         SKIN                               #
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
Model_1 <-  ifelse(Body[["M"]][["age_y"]] < 20,
                   (-1.171e-05*Body[["M"]][["age_y"]]^3 + 
                      5.413e-04*Body[["M"]][["age_y"]]^2 -
                      6.1966e-03*Body[["M"]][["age_y"]] +
                      4.623e-02) * Body[["M"]][["BW"]],
                   0.0452 * Body[["M"]][["BW"]])

### Haddad et al. (2006)
#Body surface area
S_Skin_Haddad  <- (Body[["M"]][["BW"]]**0.5150)*(Body[["M"]][["HT"]]**0.4220)*234.9
#Dermis
V_Dermis_Haddad <-  ifelse(Body[["M"]][["age_y"]] < 10,
                           (0.664*S_Skin_Haddad/10000),
                           ifelse(Body[["M"]][["age_y"]] >= 10 & Body[["M"]][["age_y"]] < 20,
                                  (-9.356e-05 - 2.151e-05*Body[["M"]][["age_y"]] - 
                                     5.058e-01*S_Skin_Haddad/10000 + 
                                     1.134e-06*Body[["M"]][["age_y"]]^2 +
                                     0.1170*Body[["M"]][["age_y"]]*(S_Skin_Haddad/10000) - 
                                     1.673e-05*(S_Skin_Haddad/10000)^2),
                                  (1.834*(S_Skin_Haddad/10000))))
#Epidermis (in L)
V_Epidermis_Haddad <- (7.850e-02*(S_Skin_Haddad/10000)^1.049)

Model_2 <- V_Dermis_Haddad + V_Epidermis_Haddad

### Price K. et al. (2003)
Model_3 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (-0.0992*Body[["M"]][["age_y"]]**4 + 
                      4.2762*Body[["M"]][["age_y"]]**3 - 
                      62.165*Body[["M"]][["age_y"]]**2 + 
                      432.78*Body[["M"]][["age_y"]]+ 203.2)/1000,
                   (-0.0992*18**4 + 4.2762*18**3 - 62.165*18**2 + 432.78*18 + 203.2)/1000)

### Smith et al. (2014)
Model_4<- (1.030e-01 - 2.560e-06*(Body[["M"]][["BW"]]*1000) +
             3.680e-11*(Body[["M"]][["BW"]]*1000)^2 - 
             2.580e-16*(Body[["M"]][["BW"]]*1000)^3 +
             8.620e-22*(Body[["M"]][["BW"]]*1000)^4 - 
             1.100e-27*(Body[["M"]][["BW"]]*1000)^5) * Body[["M"]][["BW"]]

### Ring et al. (2017)
S_Skin_Ring<-  ifelse(Body[["M"]][["age_y"]] < 18,
                   0.024265*Body[["M"]][["BW"]]**0.5378 * Body[["M"]][["BW"]]**0.3964,
                   sqrt(Body[["M"]][["BW"]]*Body[["M"]][["HT"]]/3600))

Model_5 <- 1*exp(1.64*S_Skin_Ring - 1.93)/1.116


### Pendse et al. (2020)
# 0.15 is the average skin depth in cm
Model_6 <- ((Body[["M"]][["S_Skin_Pendse"]]*10000*0.15)/1000)

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (2.88e-01*Body[["M"]][["age_h"]]**1 +  2.71e-01*(Body[["M"]][["age_h"]]**(9.98e-01)) +2.00e+02)/25000,
                  (2.88e-01*(18*365*24)**1 + 2.71e-01*((18*365*24)**(9.98e-01)) + 2.00e+02)/25000)

### Verner et al. (2008) - Spaceholder
Model_11 <- "NA"

### Haddad et al. (2001)
Model_12 <- ifelse(Body[["M"]][["age_y"]] < 18,
                   (-0.0992*Body[["M"]][["age_y"]]**4 + 
                      4.2762*Body[["M"]][["age_y"]]**3 - 
                      62.165*Body[["M"]][["age_y"]]**2 + 
                      432.78*Body[["M"]][["age_y"]]+ 203.2)/1000,
                   (-0.0992*18**4 + 4.2762*18**3 - 62.165*18**2 + 432.78*18 + 203.2)/1000)

Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6,  Model_8, Model_11, Model_12)
Body[["M"]]$Skin<- Model_List_M


##############################################################
# 2b. female-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- ifelse(Body[["F"]][["age_y"]] < 20,
                  (-7.8882e-06*Body[["F"]][["age_y"]]^3 + 
                     4.0224e-04*Body[["F"]][["age_y"]]^2 -
                     5.2146e-03*Body[["F"]][["age_y"]] +
                     4.5605e-02) *Body[["F"]][["BW"]],
                  0.0383 * Body[["F"]][["BW"]])

### Haddad et al. (2006)
#Body surface area
S_Skin_Haddad  <- (Body[["F"]][["BW"]]**0.5150)*(Body[["F"]][["HT"]]**0.4220)*234.9
## Dermis (in L)
V_Dermis_Haddad <- ifelse(Body[["F"]][["age_y"]] < 10,
                          (0.664*S_Skin_Haddad/10000),
                          ifelse(Body[["F"]][["age_y"]] >= 10 & Body[["F"]][["age_y"]] < 20,
                                 (-9.356e-05 - 2.151e-05*Body[["F"]][["age_y"]]- 
                                    5.058e-01*S_Skin_Haddad/10000 + 
                                    1.134e-06*Body[["F"]][["age_y"]]^2 +
                                    0.1170*Body[["F"]][["age_y"]]*(S_Skin_Haddad/10000) - 
                                    1.673e-05*(S_Skin_Haddad/10000)^2),
                                    (1.834*(S_Skin_Haddad/10000))))
                                

## Epidermis (in L)
V_Epidermis_Haddad <- (7.850e-02*(S_Skin_Haddad/10000)^1.049)

Model_2 <- V_Dermis_Haddad + V_Epidermis_Haddad

### Price K. et al. (2003) - Space holder
Model_3 <- "NA"

### Smith et al. (2014)
Model_4<- (1.030e-01 - 2.560e-06*(Body[["F"]][["BW"]]*1000) +
                              3.680e-11*(Body[["F"]][["BW"]]*1000)^2 - 
                              2.580e-16*(Body[["F"]][["BW"]]*1000)^3 +
                              8.620e-22*(Body[["F"]][["BW"]]*1000)^4 - 
                              1.100e-27*(Body[["F"]][["BW"]]*1000)^5) * Body_F$BW

### Ring et al. (2017)
S_Skin_Ring <-  ifelse(Body[["F"]][["age_y"]] < 18,
                      0.024265*Body[["F"]][["BW"]]**0.5378 * Body[["F"]][["HT"]]**0.3964,
                      sqrt(Body[["F"]][["BW"]]*Body[["F"]][["HT"]]/3600))

Model_5 <- 1*exp(1.64*S_Skin_Ring - 1.93)/1.116

### Pendse et al. (2020)
# 0.15 is the average skin depth in cm
Model_6 <- ((Body[["F"]][["S_Skin_Pendse"]]*10000*0.15)/1000)

### Sarigiannis et al. (2020)
Model_8 <-  ifelse(Body[["F"]][["age_y"]] < 18,
                   (2.88e-01*Body[["F"]][["age_h"]]**1 + 2.71e-01*(Body[["F"]][["age_h"]]**(9.98e-01)) +2.00e+02)/25000,
                   (2.88e-01*(18*365*24)**1 + 2.71e-01*((18*365*24)**(9.98e-01)) + 2.00e+02)/25000)
                     
### Verner et al. (2008)
V_Dermis_Verner <- ifelse(Body[["F"]][["age_y"]] < 10,
                          0.664*(Body[["F"]][["S_Skin_Verner"]]/10000),
                          ifelse(Body[["F"]][["age_y"]] >=10 & Body[["F"]][["age_y"]] < 20,
                                 (-9.356e-05 - 2.151e-05*Body[["F"]][["age_y"]]-
                                    5.058e-01*(Body[["F"]][["S_Skin_Verner"]]/10000) + 
                                    1.134e-06*Body[["F"]][["age_y"]]^2 +
                                    0.117*Body[["F"]][["age_y"]]*(Body[["F"]][["S_Skin_Verner"]]/10000) -
                                    1.673e-05*(Body[["F"]][["S_Skin_Verner"]]/10000)^2),
                                      1.834 *Body[["F"]][["S_Skin_Verner"]]/10000))
                                 
## Epidermis (in L)                          
V_Epidermis_Verner <- 7.850e-02*(Body[["F"]][["S_Skin_Verner"]]/10000)

Model_11 <- V_Dermis_Verner + V_Epidermis_Verner


### Haddad et al. (2001)
Model_12 <- ifelse(Body[["F"]][["age_y"]] < 18,
                   (4.76622e-03*Body[["F"]][["age_y"]]**5 - 
                      0.27924*Body[["F"]][["age_y"]]**4 + 
                      6.3444*Body[["F"]][["age_y"]]**3 - 
                      70.113*Body[["F"]][["age_y"]]**2 + 
                      429.85*Body[["F"]][["age_y"]]+ 252.06)/1000,
                   (4.76622e-03*18**5 - 0.27924*18**4 + 6.3444*18**3 - 70.113*18**2 + 429.85*18 + 252.06)/1000)


Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6,  Model_8, Model_11, Model_12)
Body[["F"]]$Skin<- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Skin"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Skin"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Skin"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Skin"]][["Model_4"]])
Body_M[16] <- unlist(Body[["M"]][["Skin"]][["Model_5"]])
Body_M[17] <- unlist(Body[["M"]][["Skin"]][["Model_6"]])
Body_M[18] <- unlist(Body[["M"]][["Skin"]][["Model_8"]])
Body_M[19] <- unlist(Body[["M"]][["Skin"]][["Model_11"]])
Body_M[20] <- unlist(Body[["M"]][["Skin"]][["Model_12"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_3", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_4", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_5", "Volume (in L)" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_6", "Volume (in L)" = Body_M[[17]]),
  data.frame(Body_M, Model = "Model_8", "Volume (in L)" = Body_M[[18]]),
  data.frame(Body_M, Model = "Model_11", "Volume (in L)" = Body_M[[19]]),
  data.frame(Body_M, Model = "Model_12", "Volume (in L)" = Body_M[[20]]) )


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Skin"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Skin"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Skin"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Skin"]][["Model_4"]])
Body_F[16] <- unlist(Body[["F"]][["Skin"]][["Model_5"]])
Body_F[17] <- unlist(Body[["F"]][["Skin"]][["Model_6"]])
Body_F[18] <- unlist(Body[["F"]][["Skin"]][["Model_8"]])
Body_F[19] <- unlist(Body[["F"]][["Skin"]][["Model_11"]])
Body_F[20] <- unlist(Body[["F"]][["Skin"]][["Model_12"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_3", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_4", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_5", "Volume (in L)" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_6", "Volume (in L)" = Body_F[[17]]),
  data.frame(Body_F, Model = "Model_8", "Volume (in L)" = Body_F[[18]]),
  data.frame(Body_F, Model = "Model_11", "Volume (in L)" = Body_F[[19]]),
  data.frame(Body_F, Model = "Model_12", "Volume (in L)" = Body_F[[20]]))


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
  ggtitle("Skin") +
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
LifeTimeChangesOrgans[["M"]]$Skin <- Body[["M"]]$Skin
LifeTimeChangesOrgans[["F"]]$Skin <- Body[["F"]]$Skin

