
##############################################################
#                        BRAIN                               #
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
  #Model_9 = "Wu et al. 2015", # just for females
  Model_10 = "Deepika et al (2021)"
  #Model_11 = "Verner et al. (2008)",# just for females
  #Model_12 = "Haddad et al. (2006)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################
## Beaudouin et al. (2010)
Model_1<- 1.45 + (0.350-1.45)*exp(-0.440*Body[["M"]][["age_y"]])


### Haddad et al. (2001)
Model_2 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   1e04*((Body[["M"]][["age_y"]]+0.213)/(6.030+6.895*Body[["M"]][["age_y"]]))/1000,
                   1e04*((18+0.213)/(6.030+6.895*18))/1000)

### Price K. et al. (2003)
Model_3 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (1e04*((Body[["M"]][["age_y"]]+0.213)/(6.030+6.895*Body[["M"]][["age_y"]])))/1000,
                   (1e04*((18+0.213)/(6.030+6.895*18)))/1000)


### Smith et al. (2014)
Model_4<- (1.216e-01 - 3.465e-06*(Body[["M"]][["BW"]]*1000) + 4.354e-11*(Body[["M"]][["BW"]]*1000)^2 - 2.463e-16*(Body[["M"]][["BW"]]*1000)^3 +5.132e-22*(Body[["M"]][["BW"]]*1000)^4) * Body[["M"]][["BW"]]
                                 
### Ring et al. (2017)
Model_5 <- ((0.425*((3.68-2.68*exp(-Body[["M"]][["age_y"]]/0.89))*(1*exp(-Body[["M"]][["age_y"]]/629)))))/1.03

## Pendse et al. (2020)
Model_6<- 10*(Body[["M"]][["age_y"]]+0.315)/(9+6.92*Body[["M"]][["age_y"]])

### Mallick et al. (2020)
Model_7 <- 10*(Body[["M"]][["age_y"]]+0.315)/(9+6.92*Body[["M"]][["age_y"]])

### Sarigiannis et al. (2020)
Model_8 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (-5.03e-02*Body[["M"]][["age_h"]]**1 +   9.07e-01*(Body[["M"]][["age_h"]]**(7.69e-01)) + 3.95e+02)/1000,
                   (-5.03e-02*(18*365*24)**1 +  9.07e-01*((18*365*24)**(7.69e-01)) + 3.95e+02)/1000)

### Deepika et al. (2021)
Model_10<- 0.218096 - 0.001590*Body[["M"]][["age_y"]] - 0.003274*Body[["M"]][["BW"]]+ 0.008626*Body[["M"]][["HT"]]

Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8,Model_10)
Body[["M"]]$Brain<- Model_List_M


##############################################################
# 2b. female-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- 1.30 + (0.347-1.30)*exp(-0.573*Body[["F"]][["age_y"]])

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  1e04*((Body[["F"]][["age_y"]] +0.226)/(6.521+7.514*Body[["F"]][["age_y"]]))/1000,
                  1e04*((18+0.226)/(6.521+7.514*18))/1000)

### Price K. et al. (2003) - Spaceholder
Model_3 <- "NA"

### Smith et al. (2014)
Model_4 <- (1.216e-01 - 3.465e-06*(Body[["F"]][["BW"]] *1000) + 4.354e-11*(Body[["F"]][["BW"]] *1000)^2 -    2.463e-16*(Body[["F"]][["BW"]] *1000)^3 + 5.132e-22*(Body[["F"]][["BW"]] *1000)^4) * Body[["F"]][["BW"]] 
             

### Ring et al. (2017)
Model_5 <- ((0.373*((3.68-2.68*exp(-Body[["F"]][["age_y"]]/0.89))*(1*exp(-Body[["F"]][["age_y"]]/629)))))/1.03

## Pendse et al. (2020)
Model_6 <- 10*(Body[["F"]][["age_y"]] +0.315)/(9+6.92*Body[["F"]][["age_y"]])

### Mallick et al. (2020)
Model_7 <- 10*(Body[["F"]][["age_y"]] +0.315)/(9+6.92*Body[["F"]][["age_y"]] )

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (-5.03e-02*Body[["F"]][["age_h"]] **1 + 9.07e-01*(Body[["F"]][["age_h"]] **(7.69e-01)) + 3.95e+02)/1000,
                  (-5.03e-02*(18*365*24)**1 + 9.07e-01*((18*365*24)**(7.69e-01)) + 3.95e+02)/1000)
                    
### Deepika et l. (2021)
Model_10 <- 0.3757397-0.0003031*Body[["F"]][["age_y"]] -0.0021962*Body[["F"]][["BW"]]  +0.0065721*Body[["F"]][["HT"]] 

Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8,Model_10)
Body[["F"]]$Brain<- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Brain"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Brain"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Brain"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Brain"]][["Model_4"]])
Body_M[16] <- unlist(Body[["M"]][["Brain"]][["Model_5"]])
Body_M[17] <- unlist(Body[["M"]][["Brain"]][["Model_6"]])
Body_M[18] <- unlist(Body[["M"]][["Brain"]][["Model_7"]])
Body_M[19] <- unlist(Body[["M"]][["Brain"]][["Model_8"]])
Body_M[20] <- unlist(Body[["M"]][["Brain"]][["Model_10"]])
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
  data.frame(Body_M, Model = "Model_10", "Volume (in L)" = Body_M[[20]]))


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Brain"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Brain"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Brain"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Brain"]][["Model_4"]])
Body_F[16] <- unlist(Body[["F"]][["Brain"]][["Model_5"]])
Body_F[17] <- unlist(Body[["F"]][["Brain"]][["Model_6"]])
Body_F[18] <- unlist(Body[["F"]][["Brain"]][["Model_7"]])
Body_F[19] <- unlist(Body[["F"]][["Brain"]][["Model_8"]])
Body_F[20] <- unlist(Body[["F"]][["Brain"]][["Model_10"]])
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
  data.frame(Body_F, Model = "Model_10", "Volume (in L)" = Body_F[[20]]))


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
  ggtitle("Brain") +
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
LifeTimeChangesOrgans[["M"]]$Brain <- Body[["M"]]$Brain
LifeTimeChangesOrgans[["F"]]$Brain <- Body[["F"]]$Brain
