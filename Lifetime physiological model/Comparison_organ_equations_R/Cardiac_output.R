##############################################################
#               CARDIAC OUTPUT                               #
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
  #Model_2 = "Haddad et al. (2001)",
  Model_3 = "Price K. et al. (2003)", # just for males
  #Model_4 = "Smith et al. (2014)",
  #Model_5 = "Ring et al. (2017)",
  #Model_6 = "Pendse et al. (2020)",
  Model_7 = "Mallick et al (2020)",
 # Model_8 = "Sarigiannis et al. (2020)", 
  Model_9 = "Wu et al. 2015", # just for females
  Model_10 = "Deepika et al (2021)",
  Model_11 = "Verner et al. (2008)",# just for females
  Model_12 = "Haddad et al. (2006)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
# From 0 to 33.37yo
Model_1 <-  ifelse(Body[["M"]][["age_y"]] <=33.37,
                   (6.642+(0.6-6.642)*exp(-0.1323*Body[["M"]][["age_y"]]))*60,
                   (-0.000895*Body[["M"]][["age_y"]]^2 + 
                      0.0607*Body[["M"]][["age_y"]] +
                      5.54)*60)


### Price K. et al. (2003) (L/h)
Model_3 <-  ifelse(Body[["M"]][["age_y"]] <= 18,
                   (0.012*Body[["M"]][["age_y"]] **3 - 
                      1.2144*Body[["M"]][["age_y"]] **2 + 
                      40.324*Body[["M"]][["age_y"]]  + 44.414),
                   (0.012*18**3 - 1.2144*18**2 + 40.324*18 + 44.414))

### Mallick et al. (2020)
Model_7 <- 60 * 3.5 * Body[["M"]][["S_Skin_Mallick"]] 

### Deepika et al. (2021)
S_Skin_Deepika <- 0.007184*(Body[["M"]][["BW"]] **0.425)*(Body[["M"]][["HT"]] **0.725)
Model_10 <- 6.48370 - 1.59948*Body[["M"]][["age_y"]]  + 214.68572*S_Skin_Deepika

### Wu et al. (2015) - Spaceholder
Model_9 <-"NA"

### Verner et al. (2008) - Spaceholder
Model_11 <- "NA"
  
### Haddad et al. (2006)
Model_12<- (0.2519*Body[["M"]][["BW"]] ^0.7609)*60


Model_List_M <-cbind.data.frame(Model_1,  Model_3, Model_7, Model_9, Model_10, Model_11, Model_12)
Body[["M"]]$Cardiac_Output<- Model_List_M

##############################################################
# 2b. female-specific physiological equations for models
##############################################################

## Beaudouin et al. (2010) (in L/h)
Model_1 <- ifelse(Body[["F"]][["age_y"]] <= 16.027,
                  (7.734+(0.6-7.734)*exp(-0.09747*Body[["F"]][["age_y"]]))*60,
                  (0.000473*Body[["F"]][["age_y"]]^2 - 
                     0.0782*Body[["F"]][["age_y"]] + 7.37)*60)
### Price K. et al. (2003) - Spaceholder
Model_3 <- "NA"

### Mallick et al. (2020)
Model_7 <- 60 * 3.5 * Body[["F"]][["S_Skin_Mallick"]] 

### Wu et al. (2015)
Model_9 <- 60 * 3.5 * Body[["F"]][["S_Skin_Wu"]] /10000

### Deepika et al. (2021)
S_Skin_Deepika <- 0.007184*(Body[["F"]][["BW"]] **0.425)*(Body[["F"]][["HT"]] **0.725)
Model_10 <- 5.528076 + (- 2.834486*Body[["F"]][["age_y"]] + 
                                       0.012591*Body[["F"]][["age_y"]]**2 + 
                                       204.262351*S_Skin_Deepika + 
                                       19.274290*S_Skin_Deepika**2)

### Verner et al. (2008)
Model_11 <- 15.048*Body[["F"]][["BW"]]^0.7609

### Haddad et al. (2006)
Model_12<- (0.2508*Body[["F"]][["BW"]]^0.7815)*60

Model_List_F <-cbind.data.frame(Model_1,  Model_3, Model_7, Model_9, Model_10, Model_11, Model_12)
Body[["F"]]$Cardiac_Output<- Model_List_F



##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Cardiac_Output"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Cardiac_Output"]][["Model_3"]])
Body_M[14] <- unlist(Body[["M"]][["Cardiac_Output"]][["Model_7"]])
Body_M[15] <- unlist(Body[["M"]][["Cardiac_Output"]][["Model_9"]])
Body_M[16] <- unlist(Body[["M"]][["Cardiac_Output"]][["Model_10"]])
Body_M[17] <- unlist(Body[["M"]][["Cardiac_Output"]][["Model_11"]])
Body_M[18] <- unlist(Body[["M"]][["Cardiac_Output"]][["Model_12"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Cardiac Output in L/h" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_3", "Cardiac Output in L/h" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_7", "Cardiac Output in L/h" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_9", "Cardiac Output in L/h" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_10", "Cardiac Output in L/h" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_11", "Cardiac Output in L/h" = Body_M[[17]]),
  data.frame(Body_M, Model = "Model_12", "Cardiac Output in L/h" = Body_M[[18]]) )


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Cardiac_Output"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Cardiac_Output"]][["Model_3"]])
Body_F[14] <- unlist(Body[["F"]][["Cardiac_Output"]][["Model_7"]])
Body_F[15] <- unlist(Body[["F"]][["Cardiac_Output"]][["Model_9"]])
Body_F[16] <- unlist(Body[["F"]][["Cardiac_Output"]][["Model_10"]])
Body_F[17] <- unlist(Body[["F"]][["Cardiac_Output"]][["Model_11"]])
Body_F[18] <- unlist(Body[["F"]][["Cardiac_Output"]][["Model_12"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Cardiac Output in L/h" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_3", "Cardiac Output in L/h" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_7", "Cardiac Output in L/h" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_9", "Cardiac Output in L/h" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_10", "Cardiac Output in L/h" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_11", "Cardiac Output in L/h" = Body_F[[17]]),
  data.frame(Body_F, Model = "Model_12", "Cardiac Output in L/h" = Body_F[[18]]))


Total_Organ <- rbind(combined_models_F, combined_models_M)

Total_Organ<- Total_Organ[, c(-12,-13,-14,-15,-16,-17,-18)]
colnames(Total_Organ)[14] <- "Cardiac Output in L/h"

Total_Organ <- transform(Total_Organ, `Cardiac Output in L/h` = as.numeric(`Cardiac Output in L/h`))
colnames(Total_Organ)[1] <- "Age (in years)"
colnames(Total_Organ)[2] <- "Age (in months)"
colnames(Total_Organ)[3] <- "Age (in hours)"
colnames(Total_Organ)[14] <- "Cardiac Output in L/h"
str(Total_Organ)

# Create a named vector for the labels
model_labels <- c( "Model_1" = "Beaudouin et al. (2010)",
                   #"Model_2" = "Haddad et al. (2001)",
                   "Model_3" = "Price K. et al. (2003)", # just for males
                   #"Model_4" = "Smith et al. (2014)",
                   #"Model_5" = "Ring et al. (2017)",
                   #"Model_6"= "Pendse et al. (2020)",
                   "Model_7" = "Mallick et al (2020) / Pendse et al. (2020)",
                   #"Model_8" = "Sarigiannis et al. (2020)", 
                   "Model_9" = "Wu et al. (2015)", # just for females
                   "Model_10" = "Deepika et al (2021)",
                   "Model_11" = "Verner et al. (2008)",# just for females
                   "Model_12" = "Haddad et al. (2006)"
)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Cardiac Output in L/h`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values = myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 

Plot_organ <- Plot_organ+  
  ggtitle("Cardiac Output") +
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
LifeTimeChangesOrgans[["M"]]$Cardiac_Output <- Body[["M"]]$Cardiac_Output
LifeTimeChangesOrgans[["F"]]$Cardiac_Output <- Body[["F"]]$Cardiac_Output


