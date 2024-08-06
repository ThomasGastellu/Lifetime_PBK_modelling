##############################################################
#                        KIDNEY                              #
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
  Model_9 = "Wu et al. (2015)", # just for females
  Model_10 ="Deepika et al. (2021)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- (0.0042+(0.00767-0.0042)*exp(-0.206*Body[["M"]][["age_y"]]))*Body[["M"]][["BW"]]

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (9.737e-04*Body[["M"]][["age_y"]]**5 -
                     0.0561*Body[["M"]][["age_y"]]**4 + 
                     1.1729*Body[["M"]][["age_y"]]**3 - 
                     10.34*Body[["M"]][["age_y"]]**2 + 
                     44.604*Body[["M"]][["age_y"]]+ 28.291)/1000,
                    (9.737e-04*18**5 - 0.0561*18**4 + 1.1729*18**3 - 10.34*18**2 + 44.604*18 + 28.291)/1000)


### Price K. et al. (2003)
Model_3 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (9.737e-04*Body[["M"]][["age_y"]]**5 - 
                     0.0561*Body[["M"]][["age_y"]]**4 + 
                     1.1729*Body[["M"]][["age_y"]]**3 - 
                     10.34*Body[["M"]][["age_y"]]**2 + 
                     44.604*Body[["M"]][["age_y"]] + 28.291)/1000,
                  (9.737e-04*18**5 - 0.0561*18**4 + 1.1729*18**3 - 10.34*18**2 + 44.604*18 + 28.291)/1000)
                  

### Smith et al. (2014)
Model_4<- (7.260e-03 - 6.690e-08*(Body[["M"]][["BW"]]*1000) + 3.330e-13*(Body[["M"]][["BW"]]*1000)^2) * Body[["M"]][["BW"]]

                                    
### Ring et al. (2017)
Model_5 <- (((10.24*Body[["M"]][["HT"]]/100*sqrt(Body[["M"]][["BW"]]) + 7.85) + (9.88*Body[["M"]][["HT"]]/100*sqrt(Body[["M"]][["BW"]])+7.2))/1000)/1.05

### Pendse et al. (2020)
Model_6 <- exp(-2.306*((Body[["M"]][["HT"]]/100)^-1.93))

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (3.17e-02*Body[["M"]][["age_h"]]**1 +  1.44e-02*(Body[["M"]][["age_h"]]**(1.06e+00)) + 3.80e+01)/20000,
                  (3.17e-02*(18*365*24)**1 + 1.44e-02*((18*365*24)**(1.06e+00)) + 3.80e+01)/20000)

### Wu et al. 2021 - spaceholder
Model_9 <- "NA"

### Deepika et al. (2021)
Model_10 <- 5.668e-02 - 4.962e-04*Body[["M"]][["age_y"]]+ 3.501e-03*Body[["M"]][["BW"]]

Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_8, Model_9, Model_10)
Body[["M"]]$Kidney<- Model_List_M


##############################################################
# 2b. female-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- (0.0046+(0.00709-0.0046)*exp(-0.221*Body[["F"]][["age_y"]]))*Body[["F"]][["BW"]]

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (1.2676e-03*Body[["F"]][["age_y"]]**5 - 
                     6.6825e-02*Body[["F"]][["age_y"]]**4 +
                     1.2345*Body[["F"]][["age_y"]]**3 - 
                     9.4597*Body[["F"]][["age_y"]]**2 + 
                     39.005*Body[["F"]][["age_y"]] + 27.161)/1000,
                  (1.2676e-03*18**5 - 6.6825e-02*18**4 + 1.2345*18**3 - 9.4597*18**2 + 39.005*18 + 27.161)/1000)

### Price K. et al. (2003) - Placeholder
Model_3 <- "NA"

### Smith et al. (2014)
Model_4 <- (7.260e-03 - 6.690e-08*(Body[["F"]][["BW"]]*1000) + 3.330e-13*(Body[["F"]][["BW"]]*1000)^2) * Body[["F"]][["BW"]]
                                

### Ring et al. (2017)
Model_5<- (((10.65*Body[["F"]][["HT"]]/100*sqrt(Body[["F"]][["BW"]])+6.11) + (9.88*Body[["F"]][["HT"]]/100*sqrt(Body[["F"]][["BW"]])+6.55))/1000)/1.05

## Pendse et al. (2020)
Model_6 <- exp(-2.306*((Body[["F"]][["HT"]]/100)^-1.93))

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (3.17e-02*Body[["M"]][["age_h"]]**1 +  1.44e-02*(Body[["M"]][["age_h"]]**(1.06e+00)) + 3.80e+01)/20000,
                  (3.17e-02*(18*365*24)**1 + 1.44e-02*((18*365*24)**(1.06e+00)) + 3.80e+01)/20000)
                    
### Wu et al. (2015)
#S = ((0.02053*(Body[["F"]][["HT"]]/100) * Body[["F"]][["BW"]]^0.5 + 0.01266))/((0.0154 + 0.00204*Body[["F"]][["BW"]] + 0.0518*(Body[["F"]][["HT"]]/100)^2))
  
Model_9 <- ifelse(Body[["F"]][["age_y"]] < 12,
                  (0.02053*(Body[["F"]][["HT"]]/100) *  Body[["F"]][["BW"]]^0.5 + 0.01266),
                  (0.0154 + 0.00204*Body[["F"]][["BW"]] + 0.0518*(Body[["F"]][["HT"]]/100)^2)* ((0.02053*(Body[["F"]][["HT"]]/100) * Body[["F"]][["BW"]]^0.5 + 0.01266))/((0.0154 + 0.00204*Body[["F"]][["BW"]] + 0.0518*(Body[["F"]][["HT"]]/100)^2)))
                    

### Deepika et al. (2021)
Model_10 <- 0.0458676-0.0003957*Body[["F"]][["age_y"]]+0.0035115*Body[["F"]][["BW"]]

Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_8, Model_9, Model_10)
Body[["F"]]$Kidney <- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Kidney"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Kidney"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Kidney"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Kidney"]][["Model_4"]])
Body_M[16] <- unlist(Body[["M"]][["Kidney"]][["Model_5"]])
Body_M[17] <- unlist(Body[["M"]][["Kidney"]][["Model_6"]])
Body_M[18] <- unlist(Body[["M"]][["Kidney"]][["Model_8"]])
Body_M[19] <- unlist(Body[["M"]][["Kidney"]][["Model_9"]])
Body_M[20] <- unlist(Body[["M"]][["Kidney"]][["Model_10"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_3", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_4", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_5", "Volume (in L)" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_6", "Volume (in L)" = Body_M[[17]]),
  data.frame(Body_M, Model = "Model_8", "Volume (in L)" = Body_M[[18]]),
  data.frame(Body_M, Model = "Model_9", "Volume (in L)" = Body_M[[19]]),
  data.frame(Body_M, Model = "Model_10", "Volume (in L)" = Body_M[[20]]) )


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Kidney"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Kidney"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Kidney"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Kidney"]][["Model_4"]])
Body_F[16] <- unlist(Body[["F"]][["Kidney"]][["Model_5"]])
Body_F[17] <- unlist(Body[["F"]][["Kidney"]][["Model_6"]])
Body_F[18] <- unlist(Body[["F"]][["Kidney"]][["Model_8"]])
Body_F[19] <- unlist(Body[["F"]][["Kidney"]][["Model_9"]])
Body_F[20] <- unlist(Body[["F"]][["Kidney"]][["Model_10"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_3", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_4", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_5", "Volume (in L)" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_6", "Volume (in L)" = Body_F[[17]]),
  data.frame(Body_F, Model = "Model_8", "Volume (in L)" = Body_F[[18]]),
  data.frame(Body_F, Model = "Model_9", "Volume (in L)" = Body_F[[19]]),
  data.frame(Body_F, Model = "Model_10", "Volume (in L)" = Body_F[[20]]) )


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
                   "Model_8" = "Sarigiannis et al. (2020)", 
                   "Model_9" = "Wu et al. 2015", # just for females,
                   "Model_10" = "Deepiker et al. 2021"
)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Volume (in L)`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values = myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 


Plot_organ <- Plot_organ+  
  ggtitle("KIDNEY") +
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 20),
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



# save in List
LifeTimeChangesOrgans[["M"]]$Kidney <- Body[["M"]]$Kidney
LifeTimeChangesOrgans[["F"]]$Kidney <- Body[["F"]]$Kidney

