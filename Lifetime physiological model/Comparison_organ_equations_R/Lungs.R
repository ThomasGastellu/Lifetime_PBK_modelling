##############################################################
#                         LUNGS                              #
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
  Model_10 = "Deepika et al (2021)"
  #Model_11 = "Verner et al. (2008)",# just for females
  #Model_12 = "Haddad et al. (2006)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################
### Beaudouin et al. (2010)
Model_1 <- 0.0068 * Body[["M"]][["BW"]] 

### Haddad et al. (2001)
Model_2 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (-0.0346*Body[["M"]][["age_y"]]**4 +
                      1.5069*Body[["M"]][["age_y"]]**3 - 
                      20.31*Body[["M"]][["age_y"]]**2 + 
                      123.99*Body[["M"]][["age_y"]] + 59.213)/1000,
                   (-0.0346*18**4 + 1.5069*18**3 - 20.31*18**2 + 123.99*18 + 59.213)/1000)

### Price K. et al. (2003)
Model_3 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (-0.0346*Body[["M"]][["age_y"]]**4 +
                      1.5069*Body[["M"]][["age_y"]]**3 - 
                      20.31*Body[["M"]][["age_y"]]**2 + 
                      123.99*Body[["M"]][["age_y"]] + 59.213)/1000,
                   (-0.0346*18**4 + 1.5069*18**3 - 20.31*18**2 + 123.99*18 + 59.213)/1000)
                   
### Smith et al. (2014)
Model_4 <- (1.860e-02 - 4.550e-08*(Body[["M"]][["BW"]] *1000)) * Body[["M"]][["BW"]] 

### Ring et al. (2017)
condition <- which(Body[["M"]][["age_y"]] == 18)
ht_18 <- Body[["M"]][condition,"HT"]
wbw_18 <- Body[["M"]][condition,"BW"]

Model_5 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   ((29.08*Body[["M"]][["HT"]] /100 * sqrt(Body[["M"]][["BW"]] ) + 11.06) + (35.47*Body[["M"]][["HT"]] /100 * sqrt(Body[["M"]][["BW"]] ) + 5.53))/1.05/1000,
                   ((29.08*ht_18/100 * sqrt(wbw_18) + 11.06) + (35.47*ht_18/100 * sqrt(wbw_18) + 5.53))/1.05/1000)

### Pendse et al. (2020)
Model_6 <- exp(-2.092*((Body[["M"]][["HT"]] /100)^-2.1))

### Sarigiannis et al. (2020)
Model_8 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (9.74e-02*Body[["M"]][["age_h"]] **1 + 6.33e-02*(Body[["M"]][["age_h"]] **(1.03)) +   8.40e+01)/25000,
                   (9.74e-02*(18*365*24)**1 +   6.33e-02*((18*365*24)**(1.03)) + 8.40e+01)/25000)
                     
### Deepika et al. (2021)
Model_10<- -1.454e-02 + 7.269e-04*Body[["M"]][["age_y"]]  + 9.329e-06*(Body[["M"]][["age_y"]] **2) + 6.430e-03*(Body[["M"]][["BW"]] ) + 3.083e-05*(Body[["M"]][["BW"]] **2)


Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6,  Model_8, Model_10)
Body[["M"]]$Lungs<- Model_List_M


##############################################################
# 2b. female-specific physiological equations for models
##############################################################
### Beaudouin et al. (2010)
Model_1 <- 0.0070 * Body[["F"]][["BW"]] 

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (6.3e-03*Body[["F"]][["age_y"]]**5 -
                     0.3162*Body[["F"]][["age_y"]]**4 +
                     5.5896*Body[["F"]][["age_y"]]**3 -
                     42.196*Body[["F"]][["age_y"]]**2 +
                     160.79*Body[["F"]][["age_y"]] + 50.506)/1000,
                     (6.3e-03*18**5 - 0.3162*18**4 + 5.5896*18**3 - 42.196*18**2 + 160.79*18 + 50.506)/1000)

### Price K. et al. (2003) - space holder
Model_3 <- "NA"           

### Smith et al. (2014)
Model_4 <- (1.860e-02 - 4.550e-08*(Body[["F"]][["BW"]] *1000)) * Body[["F"]][["BW"]] 

### Ring et al. (2017)
condition <- which(Body[["F"]][["age_y"]]  == 18)
ht_18 <- Body[["F"]][condition,"HT"]
wbw_18 <- Body[["F"]][condition,"BW"]

Model_5 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  ((31.46*Body[["F"]][["HT"]] /100 * sqrt(Body[["F"]][["BW"]] ) + 1.43) + (35.30*Body[["F"]][["HT"]] /100 * sqrt(Body[["F"]][["BW"]]) + 1.53))/1.05/1000,
                  ((31.46*ht_18/100 * sqrt(wbw_18) + 1.43) + (35.30*ht_18/100 * sqrt(wbw_18) + 1.53))/1.05/1000)

### Pendse et al. (2020)
Model_6 <- exp(-2.092*((Body[["F"]][["HT"]] /100)^-2.1))

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (9.74e-02*Body[["F"]][["age_h"]] **1 + 6.33e-02*(Body[["F"]][["age_h"]] **(1.03)) + 8.40e+01)/25000,
                  (9.74e-02*(18*365*24)**1 +  6.33e-02*((18*365*24)**(1.03)) +  8.40e+01)/25000)
                    

### Deepika et al. (2021)
Model_10 <- -1.454e-02 + 7.269e-04*Body[["F"]][["age_y"]]  + 9.329e-06*(Body[["F"]][["age_y"]] **2) + 6.430e-03*(Body[["F"]][["BW"]] ) + 3.083e-05*(Body[["F"]][["BW"]] **2)

Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6,  Model_8, Model_10)
Body[["F"]]$Lungs<- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Lungs"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Lungs"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Lungs"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Lungs"]][["Model_4"]])
Body_M[16] <- unlist(Body[["M"]][["Lungs"]][["Model_5"]])
Body_M[17] <- unlist(Body[["M"]][["Lungs"]][["Model_6"]])
Body_M[18] <- unlist(Body[["M"]][["Lungs"]][["Model_8"]])
Body_M[19] <- unlist(Body[["M"]][["Lungs"]][["Model_10"]])

Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_3", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_4", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_5", "Volume (in L)" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_6", "Volume (in L)" = Body_M[[17]]),
  data.frame(Body_M, Model = "Model_8", "Volume (in L)" = Body_M[[18]]),
  data.frame(Body_M, Model = "Model_10", "Volume (in L)" = Body_M[[19]]) )


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Lungs"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Lungs"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Lungs"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Lungs"]][["Model_4"]])
Body_F[16] <- unlist(Body[["F"]][["Lungs"]][["Model_5"]])
Body_F[17] <- unlist(Body[["F"]][["Lungs"]][["Model_6"]])
Body_F[18] <- unlist(Body[["F"]][["Lungs"]][["Model_8"]])
Body_F[19] <- unlist(Body[["F"]][["Lungs"]][["Model_10"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_3", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_4", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_5", "Volume (in L)" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_6", "Volume (in L)" = Body_F[[17]]),
  data.frame(Body_F, Model = "Model_8", "Volume (in L)" = Body_F[[18]]),
  data.frame(Body_F, Model = "Model_10", "Volume (in L)" = Body_F[[19]]))


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
                   "Model_6"= "Pendse et al. (2020)",
                   #"Model_7" = "Mallick et al (2020)",
                   "Model_8" = "Sarigiannis et al. (2020)", 
                   #"Model_9" = "Wu et al. (2015)", # just for females
                   "Model_10" = "Deepika et al (2021)"
                   #"Model_11" = "Verner et al. (2008)",# just for females
                   #"Model_12" = "Haddad et al. (2006)"
)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Volume (in L)`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values = myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 

Plot_organ <- Plot_organ+  
  ggtitle("Lungs") +
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
LifeTimeChangesOrgans[["M"]]$Lungs <- Body[["M"]]$Lungs
LifeTimeChangesOrgans[["F"]]$Lungs <- Body[["F"]]$Lungs




