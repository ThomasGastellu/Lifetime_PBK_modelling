##############################################################
#                         Tongue                              #
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
 # Model_1 = "Beaudouin et al. (2010)",
  Model_2 = "Haddad et al. (2001)",
 # Model_3 = "Price K. et al. (2003)", # just for males
 # Model_4 = "Smith et al. (2014)",
 # Model_5 = "Ring et al. (2017)",
 # Model_6 = "Pendse et al. (2020)",
 # Model_7 = "Mallick et al (2020)",
 # Model_8 = "Sarigiannis et al. (2020)", 
 # Model_9 = "Wu et al. 2015", # just for females
  #Model_10 = "Deepika et al (2021)",
  Model_11 = "Verner et al. (2008)"# just for females
  #Model_12 = "Haddad et al. (2006)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################

### Haddad et al. (2006)
Model_2 <- (1.190e-3*Body[["M"]][["BW"]]- 4.302e-4)

### Verner et al. (2008) -> spaceholder
Model_11 <- "NA"

Model_List_M <-cbind.data.frame(Model_2, Model_11)
Body[["M"]]$Tongue<- Model_List_M
##############################################################
# 2b. female-specific physiological equations for models
##############################################################

### Haddad et al. (2006)
Model_2 <- (1.190e-3*Body[["F"]][["BW"]] - 4.302e-4)

### Verner et al. (2008)
Model_11 <- 1.190e-03*Body[["F"]][["BW"]]- 4.302e-04


Model_List_F <-cbind.data.frame(Model_2, Model_11)
Body[["F"]]$Tongue<- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Tongue"]][["Model_2"]])
Body_M[13] <- unlist(Body[["M"]][["Tongue"]][["Model_11"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_11", "Volume (in L)" = Body_M[[13]]))

Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Tongue"]][["Model_2"]])
Body_F[13] <- unlist(Body[["F"]][["Tongue"]][["Model_11"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_11", "Volume (in L)" = Body_F[[13]]))

Total_Organ <- rbind(combined_models_F, combined_models_M)

Total_Organ<- Total_Organ[, c(-12,-13)]
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
                   #"Model_5" = "Ring et al. (2017)",
                   #"Model_6"= "Pendse et al. (2020)",
                   #"Model_7" = "Mallick et al (2020)",
                   #"Model_8" = "Sarigiannis et al. (2020)", 
                   #"Model_9" = "Wu et al. (2015)", # just for females
                   #"Model_10" = "Deepika et al (2021)",
                   "Model_11" = "Verner et al. (2008)"# just for females
                   #"Model_12" = "Haddad et al. (2006)"
)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Volume (in L)`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values = myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 

Plot_organ <- Plot_organ+  
  ggtitle("Tongue") +
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
LifeTimeChangesOrgans[["M"]]$Tongue <- Body[["M"]]$Tongue
LifeTimeChangesOrgans[["F"]]$Tongue <- Body[["F"]]$Tongue
