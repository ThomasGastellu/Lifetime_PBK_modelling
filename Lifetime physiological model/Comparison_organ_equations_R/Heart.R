##############################################################
#                         HEART                              #
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
  #Model_5 = "Ring et al. (2017)",
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
Model_1 <- 0.0045 * Body[["M"]][["BW"]] 

### Haddad et al. (2001)
Model_2 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (-0.0132*Body[["M"]][["age_y"]] **4 +
                      0.5051*Body[["M"]][["age_y"]] **3 -
                      5.7113*Body[["M"]][["age_y"]] **2 + 
                      32.213*Body[["M"]][["age_y"]]  + 20.364)/1000,
                   (-0.0132*18**4 + 0.5051*18**3 - 5.7113*18**2 + 32.213*18 + 20.364)/1000)
                   

### Price K. et al. (2003)
Model_3 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (-0.0132*Body[["M"]][["age_y"]] **4 + 
                      0.5051*Body[["M"]][["age_y"]] **3 -
                      5.7113*Body[["M"]][["age_y"]] **2 + 
                      32.213*Body[["M"]][["age_y"]]  + 20.364)/1000,
                   (-0.0132*18**4 + 0.5051*18**3 - 5.7113*18**2 + 32.213*18 + 20.364)/1000)

### Sarigiannis et al. (2020)
Model_8 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (4.68e-02*Body[["M"]][["age_h"]] **1 - 3.81e-02*(Body[["M"]][["age_h"]] **(1.01e+00)) + 2.80e+01)/2000,
                   (4.68e-02*(18*365*24)**1 - 3.81e-02*((18*365*24)**(1.01e+00)) + 2.80e+01)/2000)

### Verner et al. (2008) - spaceholder
Model_11 <- "NA"
  
### Haddad et al. (2006)
Model_12 <- (1.017e-07*(Body[["M"]][["HT"]] ^0.6640 * Body[["M"]][["BW"]] ^0.3851 * 242.7)^1.420)


Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_8,  Model_11, Model_12)
Body[["M"]]$Heart<- Model_List_M


##############################################################
# 2b. female-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- 0.004167 * Body[["F"]][["BW"]]

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (4.246e-04*Body[["F"]][["age_y"]]**5 - 
                     2.97679e-02*Body[["F"]][["age_y"]]**4 +
                     0.6539*Body[["F"]][["age_y"]]**3 - 
                     5.5116*Body[["F"]][["age_y"]]**2 + 
                     28.486*Body[["F"]][["age_y"]] + 21.509)/1000,
                  (4.246e-04*18**5 - 2.97679e-02*18**4 + 0.6539*18**3 - 5.5116*18**2 + 28.486*18 + 21.509)/1000)

### Price K. et al. (2003) - spaceholder
Model_3 <- "NA"

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["F"]][["age_y"]] < 18, 
                  (4.68e-02*Body[["F"]][["age_h"]]**1 - 3.81e-02*(Body[["F"]][["age_h"]]**(1.01e+00)) + 2.80e+01)/2000,
                  (4.68e-02*(18*365*24)**1 - 3.81e-02*((18*365*24)**(1.01e+00)) + 2.80e+01)/2000)
                     
                                                  
### Verner et al. (2008)
Model_11 <- 1.017e-07 * (Body[["F"]][["HT"]]^0.6862 * Body[["F"]][["BW"]]^0.3561 * 242.7)^1.420

### Haddad et al. (2006)
Model_12<- (1.017e-07*(Body[["F"]][["HT"]]^0.6862 * Body[["F"]][["BW"]]^0.3561 * 242.7)^1.420)

Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_8, Model_11, Model_12)
Body[["F"]]$Heart<- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Heart"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Heart"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Heart"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Heart"]][["Model_8"]])
Body_M[16] <- unlist(Body[["M"]][["Heart"]][["Model_11"]])
Body_M[17] <- unlist(Body[["M"]][["Heart"]][["Model_12"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_3", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_8", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_11", "Volume (in L)" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_12", "Volume (in L)" = Body_M[[17]]) )


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Heart"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Heart"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Heart"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Heart"]][["Model_8"]])
Body_F[16] <- unlist(Body[["F"]][["Heart"]][["Model_11"]])
Body_F[17] <- unlist(Body[["F"]][["Heart"]][["Model_12"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_3", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_8", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_11", "Volume (in L)" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_12", "Volume (in L)" = Body_F[[17]]))


Total_Organ <- rbind(combined_models_F, combined_models_M)

Total_Organ<- Total_Organ[, c(-12,-13,-14,-15,-16,-17)]
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
                   #"Model_5" = "Ring et al. (2017)",
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
  ggtitle("Heart") +
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
LifeTimeChangesOrgans[["M"]]$Heart <- Body[["M"]]$Heart
LifeTimeChangesOrgans[["F"]]$Heart <- Body[["F"]]$Heart
