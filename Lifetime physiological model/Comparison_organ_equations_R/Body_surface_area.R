##############################################################
#                 BODY SURFACE AREA                         #
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
  #Model_1 = "Beaudouin et al. (2010)",
  Model_2 = "Haddad et al. (2001)",
  #Model_3 = "Price K. et al. (2003)", # just for males
  #Model_4 = "Smith et al. (2014)",
  Model_5 = "Ring et al. (2017)",
  Model_6 = "Pendse et al. (2020)",
  Model_7 = "Mallick et al (2020)",
  #Model_8 = "Sarigiannis et al. (2020)", 
  Model_9 = "Wu et al. (2015)", # just for females
  Model_10 = "Deepika et al (2021)",
  Model_11 = "Verner et al. (2008)"# just for females
)


##############################################################
# 2a. male-specific physiological equations for models
##############################################################


### Haddad et al. (2006)
Model_2 <- (Body[["M"]][["BW"]] **0.5150)*(Body[["M"]][["HT"]] **0.4220)*234.9/10000

### Ring et al. (2017)
Model_5 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  0.024265*Body[["M"]][["BW"]] **0.5378 * Body[["M"]][["HT"]] **0.3964,
                  sqrt(Body[["M"]][["BW"]] *Body[["M"]][["HT"]] /3600))

### Pendse et al. (2020)
Model_6 <- exp(-3.75 + 0.42*log(Body[["M"]][["HT"]] ) + 0.52*log(Body[["M"]][["BW"]] ))

### Mallick et al. (2020)
Model_7 <- exp(-3.75 + 0.42*log(Body[["M"]][["HT"]]) +   0.52*log(Body[["M"]][["BW"]]))

### Wu et al. (2015)
Model_9 <- "NA"
                                  
### Deepika et al. (2021)
Model_10 <- 0.007184*(Body[["M"]][["BW"]] **0.425)*(Body[["M"]][["HT"]] **0.725)

### Verner et al. (2008)
Model_11 <- "NA"

Model_List_M <-cbind.data.frame( Model_2, Model_5, Model_6, Model_7, Model_9, Model_10, Model_11)
Body[["M"]]$BodySurfaceArea<- Model_List_M



##############################################################
# 2b. female-specific physiological equations for models
##############################################################


### Haddad et al. (2006)
Model_2 <- (Body[["M"]][["BW"]]**0.5150)*(Body[["M"]][["HT"]]**0.4220)*234.9/10000

### Ring et al. (2017)
Model_5 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  0.024265*Body[["M"]][["BW"]]**0.5378 * Body[["M"]][["HT"]]**0.3964,
                  sqrt(Body[["M"]][["BW"]]*Body[["M"]][["HT"]]/3600))

## Body surface area (in m?)
Model_6 <- exp(-3.75 + 0.42*log(Body[["M"]][["HT"]]) + 0.52*log(Body[["M"]][["BW"]]))
                                  

### Mallick et al. (2020)
Model_7<- exp(-3.75 + 0.42*log(Body[["M"]][["HT"]]) + 0.52*log(Body[["M"]][["BW"]]))

### Wu et al. (2015)
Model_9 <- Body[["M"]][["BW"]]^0.5150 * Body[["M"]][["HT"]]^0.4220 * 235/10000

### Deepika et al. (2021)
Model_10 <- 0.007184*(Body[["M"]][["BW"]]**0.425)*(Body[["M"]][["HT"]]**0.725)

### Verner et al. (2008)
Model_11 <- Body[["M"]][["BW"]]^0.5150 * Body[["M"]][["HT"]]^0.4220 * 234.9/10000

Model_List_F <-cbind.data.frame( Model_2, Model_5, Model_6, Model_7, Model_9, Model_10, Model_11)
Body[["F"]]$BodySurfaceArea<- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["BodySurfaceArea"]][["Model_2"]])
Body_M[13] <- unlist(Body[["M"]][["BodySurfaceArea"]][["Model_5"]])
Body_M[14] <- unlist(Body[["M"]][["BodySurfaceArea"]][["Model_6"]])
Body_M[15] <- unlist(Body[["M"]][["BodySurfaceArea"]][["Model_7"]])
Body_M[16] <- unlist(Body[["M"]][["BodySurfaceArea"]][["Model_9"]])
Body_M[17] <- unlist(Body[["M"]][["BodySurfaceArea"]][["Model_10"]])
Body_M[18] <- unlist(Body[["M"]][["BodySurfaceArea"]][["Model_11"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_5", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_6", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_7", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_9", "Volume (in L)" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_10", "Volume (in L)" = Body_M[[17]]),
  data.frame(Body_M, Model = "Model_11", "Volume (in L)" = Body_M[[18]]) )


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["BodySurfaceArea"]][["Model_2"]])
Body_F[13] <- unlist(Body[["F"]][["BodySurfaceArea"]][["Model_5"]])
Body_F[14] <- unlist(Body[["F"]][["BodySurfaceArea"]][["Model_6"]])
Body_F[15] <- unlist(Body[["F"]][["BodySurfaceArea"]][["Model_7"]])
Body_F[16] <- unlist(Body[["F"]][["BodySurfaceArea"]][["Model_9"]])
Body_F[17] <- unlist(Body[["F"]][["BodySurfaceArea"]][["Model_10"]])
Body_F[18] <- unlist(Body[["F"]][["BodySurfaceArea"]][["Model_11"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_5", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_6", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_7", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_9", "Volume (in L)" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_10", "Volume (in L)" = Body_F[[17]]),
  data.frame(Body_F, Model = "Model_11", "Volume (in L)" = Body_F[[18]]))


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
model_labels <- c( #"Model_1" = "Beaudouin et al. (2010)",
                   "Model_2" = "Haddad et al. (2001)",
                   #"Model_3" = "Price K. et al. (2003)", # just for males
                   #"Model_4" = "Smith et al. (2014)",
                   "Model_5" = "Ring et al. (2017)",
                   "Model_6"= "Pendse et al. (2020)",
                   "Model_7" = "Mallick et al (2020)",
                   #"Model_8" = "Sarigiannis et al. (2020)", 
                   "Model_9" = "Wu et al. (2015)", # just for females
                   "Model_10" = "Deepika et al. (2021)",
                   "Model_11" = "Verner et al. (2008)"
)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Volume (in L)`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values = myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 


Plot_organ <- Plot_organ+  
  ggtitle("BODY SURFACE AREA") +
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



# save in List
LifeTimeChangesOrgans[["M"]]$BodySurfaceArea <- Body[["M"]]$BodySurfaceArea
LifeTimeChangesOrgans[["F"]]$BodySurfaceArea <- Body[["F"]]$BodySurfaceArea
