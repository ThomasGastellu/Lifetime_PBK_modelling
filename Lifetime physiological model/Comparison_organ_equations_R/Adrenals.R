##############################################################
#                         Adrenals                           #
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
  Model_1 = "Beaudouin et al. (2010)"
)


##############################################################
# 2a. male-specific physiological equations for models
##############################################################


#Model1: Beaudouin et al. (2010)
Model_1 <- (2.0e-04 + (1.71e-03 - 2.0e-04) * exp(-2.02*Body[["M"]][["age_y"]]))*Body[["M"]][["BW"]]


Model_List <-cbind.data.frame(Model_1)
Body[["M"]]$Adrenals<- Model_List



##############################################################
# 2b. female-specific physiological equations for models
##############################################################


#Model1: Beaudouin et al. (2010)
Model_1 <- (0.0002 + (0.00171 - 0.0002) * exp(-2.02*Body[["F"]][["age_y"]]))*Body[["F"]][["BW"]]


Model_List <-cbind.data.frame(Model_1)
Body[["F"]]$Adrenals<- Model_List

##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Adrenals"]][["Model_1"]])
Body_M[["Gender"]]  <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_1", "Volume (in L)" = Body_M[[12]]))

Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Adrenals"]][["Model_1"]])
Body_F[["Gender"]]  <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_1", "Volume (in L)" = Body_F[[12]]))

Total_Organ <- rbind(combined_models_F, combined_models_M)
Total_Organ<- Total_Organ[, c(-12)]
colnames(Total_Organ)[1] <- "Age (in years)"
colnames(Total_Organ)[2] <- "Age (in months)"
colnames(Total_Organ)[3] <- "Age (in hours)"
colnames(Total_Organ)[14] <- "Volume (in L)"

# Create a named vector for the labels
model_labels <- c("Model_1" = "Beaudouin et al. (2010)")
str(Total_Organ)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Volume (in L)`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values = myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 


Plot_organ <- Plot_organ+  
  ggtitle("Adrenals") +
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




