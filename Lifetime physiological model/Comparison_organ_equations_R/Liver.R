##############################################################
#                         LIVER                              #
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
  Model_9 = "Wu et al. 2015", # just for females
  Model_10 = "Deepika et al (2021)",
  Model_11 = "Verner et al. (2008)",# just for females
  Model_12 = "Haddad et al. (2006)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- (0.0247+(0.0409-0.0247)*exp(-0.218*Body[["M"]][["age_y"]]))*Body[["M"]][["BW"]]


### Haddad et al. (2001)
Model_2 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (0.0072*Body[["M"]][["age_y"]]**5 - 
                      0.3975*Body[["M"]][["age_y"]]**4 + 
                      7.9052*Body[["M"]][["age_y"]]**3 -
                      65.624*Body[["M"]][["age_y"]]**2 + 
                      262.02*Body[["M"]][["age_y"]] + 157.52) /1000,
                   (0.0072*18**5 - 0.3975*18**4 + 7.9052*18**3 - 65.624*18**2 + 262.02*18 + 157.52) /1000)

### Price K. et al. (2003)
Model_3 <-  ifelse(Body[["M"]][["age_y"]] < 18,
                   (0.0072*Body[["M"]][["age_y"]]**5 -
                      0.3975*Body[["M"]][["age_y"]]**4 + 
                      7.9052*Body[["M"]][["age_y"]]**3 - 
                      65.624*Body[["M"]][["age_y"]]**2 + 
                      262.02*Body[["M"]][["age_y"]] + 157.52)/1000,
                   (0.0072*18**5 -0.3975*18**4 +  7.9052*18**3 -  65.624*18**2 +   262.02*18 + 157.52)/1000)

### Smith et al. (2014)
Model_4 <- (3.939e-02 - 7.058e-07*(Body[["M"]][["BW"]]*1000) +1.155e-11*(Body[["M"]][["BW"]]*1000)^2 -8.016e-17*(Body[["M"]][["BW"]]*1000)^3 + 1.869e-22*(Body[["M"]][["BW"]]*1000)^4) * Body[["M"]][["BW"]]
              
### Ring et al. (2017)
Model_5<- (576.9*Body[["M"]][["HT"]]/100 + 8.9*Body[["M"]][["BW"]] - 159.7)/1.05/1000

### Pendse et al. (2020)
Model_6 <-  ifelse(Body[["M"]][["age_y"]] < 23,
                   0.05012*Body[["M"]][["BW"]]^0.78,
                   (1.078*Body[["M"]][["S_Skin_Pendse"]]-0.3457)*((0.05012*Body[["M"]][["BW"]]^0.78)/(1.0728*Body[["M"]][["S_Skin_Pendse"]] - 0.3457)))

### Mallick et al. (2020)
Model_7 <-  ifelse(Body[["M"]][["age_y"]] <= 22,
                   0.05012*Body[["M"]][["BW"]]^0.78,
                   ifelse(Body[["M"]][["age_y"]] == 22,
                          ((0.05012*Body[["M"]][["BW"]]^0.78)/(1.0728*Body[["M"]][["S_Skin_Mallick"]] - 0.3457)),
                          (1.078*Body[["M"]][["S_Skin_Mallick"]]-0.3457)*((0.05012*Body[["M"]][["BW"]]^0.78)/(1.0728*Body[["M"]][["S_Skin_Mallick"]] - 0.3457))))

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["M"]][["age_y"]] < 18,
                  (2.79e-03*Body[["M"]][["age_h"]]**1 + 1.10e+00*(Body[["M"]][["age_h"]]**(6.03e-01)) +  1.60e+02)/1000,
                  (2.79e-03*(18*365*24)**1 +   1.10e+00*((18*365*24)**(6.03e-01)) +  1.60e+02)/1000)

### Wu et al. (2015) -> Spaceholder
Model_9 <- "NA"

### Deepika et al. (2021)
Model_10 <- -0.0143744 - 0.0044728*Body[["M"]][["age_y"]] + 0.0264591*Body[["M"]][["BW"]]

### Verner et al. (2008) -> Spaceholder
Model_11 <- "NA"

### Haddad et al. (2006)
Model_12 <- 0.05012*Body[["M"]][["BW"]]^0.78

Model_List_M <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8, Model_9, Model_10, Model_11, Model_12)
Body[["M"]]$Liver<- Model_List_M


##############################################################
# 2b. female-specific physiological equations for models
##############################################################

### Beaudouin et al. (2010)
Model_1 <- (0.0233+(0.038-0.0233)*exp(-0.122*Body[["F"]][["age_y"]]))*Body[["F"]][["BW"]]

### Haddad et al. (2001)
Model_2 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (0.0057*Body[["F"]][["age_y"]]**5 -
                     0.3396*Body[["F"]][["age_y"]]**4 +
                     7.0134*Body[["F"]][["age_y"]]**3 -
                     59.539*Body[["F"]][["age_y"]]**2 +
                     251.9*Body[["F"]][["age_y"]] + 139.65)/1000,
                  (0.0057*18**5 - 0.3396*18**4 + 7.0134*18**3 - 59.539*18**2 + 251.9*18 + 139.65)/1000)

### Price K. et al. (2003) -> Spaceholder
Model_3 <- "NA"

### Smith et al. (2014)
Model_4 <- (3.939e-02 - 7.058e-07*(Body[["F"]][["BW"]]*1000) +
                               1.155e-11*(Body[["F"]][["BW"]]*1000)^2 - 
                               8.016e-17*(Body[["F"]][["BW"]]*1000)^3 +
                               1.869e-22*(Body[["F"]][["BW"]]*1000)^4) *Body[["F"]][["BW"]]


### Ring et al. (2017)
Model_5 <- (674.3*Body[["F"]][["HT"]]/100 + 6.5*Body[["F"]][["BW"]] - 214.4)/1.05/1000

### Pendse et al. (2020)
Model_6 <- ifelse(Body[["F"]][["age_y"]] < 23,
                  0.05012*Body[["F"]][["BW"]]^0.78,
                  (1.078*Body[["F"]][["S_Skin_Pendse"]]-0.3457)*((0.05012*Body[["F"]][["BW"]]^0.78)/(1.0728*Body[["F"]][["S_Skin_Pendse"]] - 0.3457)))


### Mallick et al. (2020)
Model_7 <- ifelse(Body[["F"]][["age_y"]] < 22,
                  0.05012*Body[["F"]][["BW"]]^0.78,
                 (1.078*Body[["F"]][["S_Skin_Mallick"]]-0.3457)* ((0.05012*Body[["F"]][["BW"]]^0.78)/(1.0728*Body[["F"]][["S_Skin_Mallick"]] - 0.3457)))

### Sarigiannis et al. (2020)¨
Model_8 <- ifelse(Body[["F"]][["age_y"]] < 18,
                  (2.79e-03*Body[["F"]][["age_h"]]**1 +  1.10e+00*(Body[["F"]][["age_h"]]**(6.03e-01)) + 1.60e+02)/1000,
                  (2.79e-03*(18*365*24)**1 + 1.10e+00*((18*365*24)**(6.03e-01)) + 1.60e+02)/1000)

### Wu et al. (2015)
Model_9 <- ifelse(Body[["F"]][["age_y"]] < 22,
                  0.05012*(Body[["F"]][["BW"]]^0.78),
                  (1.0728*(Body[["F"]][["S_Skin_Wu"]]/10000)-0.3457)*max(0.05012*(Body[["F"]][["BW"]]^0.78))/max(1.0728*(Body[["F"]][["S_Skin_Wu"]]/10000)-0.3457))

### Deepika et al. (2021)
Model_10 <- 0.0017717-0.0030113*Body[["F"]][["age_y"]]+0.0253455*Body[["F"]][["BW"]]

### Verner et al. (2008)
Model_11 <- 0.0501*Body[["F"]][["BW"]]

### Haddad et al. (2006)
Model_12<- 0.05012*Body[["F"]][["BW"]]^0.78

Model_List_F <-cbind.data.frame(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8, Model_9, Model_10, Model_11, Model_12)
Body[["F"]]$Liver<- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

Body_M <- Body[["M"]]
Body_M[12] <- unlist(Body[["M"]][["Liver"]][["Model_1"]])
Body_M[13] <- unlist(Body[["M"]][["Liver"]][["Model_2"]])
Body_M[14] <- unlist(Body[["M"]][["Liver"]][["Model_3"]])
Body_M[15] <- unlist(Body[["M"]][["Liver"]][["Model_4"]])
Body_M[16] <- unlist(Body[["M"]][["Liver"]][["Model_5"]])
Body_M[17] <- unlist(Body[["M"]][["Liver"]][["Model_6"]])
Body_M[18] <- unlist(Body[["M"]][["Liver"]][["Model_7"]])
Body_M[19] <- unlist(Body[["M"]][["Liver"]][["Model_8"]])
Body_M[20] <- unlist(Body[["M"]][["Liver"]][["Model_9"]])
Body_M[21] <- unlist(Body[["M"]][["Liver"]][["Model_10"]])
Body_M[22] <- unlist(Body[["M"]][["Liver"]][["Model_11"]])
Body_M[23] <- unlist(Body[["M"]][["Liver"]][["Model_12"]])
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
  data.frame(Body_M, Model = "Model_9", "Volume (in L)" = Body_M[[20]]),
  data.frame(Body_M, Model = "Model_10", "Volume (in L)" = Body_M[[21]]),
  data.frame(Body_M, Model = "Model_11", "Volume (in L)" = Body_M[[22]]),
  data.frame(Body_M, Model = "Model_12", "Volume (in L)" = Body_M[[23]]) )


Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Liver"]][["Model_1"]])
Body_F[13] <- unlist(Body[["F"]][["Liver"]][["Model_2"]])
Body_F[14] <- unlist(Body[["F"]][["Liver"]][["Model_3"]])
Body_F[15] <- unlist(Body[["F"]][["Liver"]][["Model_4"]])
Body_F[16] <- unlist(Body[["F"]][["Liver"]][["Model_5"]])
Body_F[17] <- unlist(Body[["F"]][["Liver"]][["Model_6"]])
Body_F[18] <- unlist(Body[["F"]][["Liver"]][["Model_7"]])
Body_F[19] <- unlist(Body[["F"]][["Liver"]][["Model_8"]])
Body_F[20] <- unlist(Body[["F"]][["Liver"]][["Model_9"]])
Body_F[21] <- unlist(Body[["F"]][["Liver"]][["Model_10"]])
Body_F[22] <- unlist(Body[["F"]][["Liver"]][["Model_11"]])
Body_F[23] <- unlist(Body[["F"]][["Liver"]][["Model_12"]])
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
  data.frame(Body_F, Model = "Model_9", "Volume (in L)" = Body_F[[20]]),
  data.frame(Body_F, Model = "Model_10", "Volume (in L)" = Body_F[[21]]),
  data.frame(Body_F, Model = "Model_11", "Volume (in L)" = Body_F[[22]]),
  data.frame(Body_F, Model = "Model_12", "Volume (in L)" = Body_F[[23]]))


Total_Organ <- rbind(combined_models_F, combined_models_M)

Total_Organ<- Total_Organ[, c(-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23)]
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
                   "Model_9" = "Wu et al. (2015)", # just for females
                   "Model_10" = "Deepika et al (2021)",
                   "Model_11" = "Verner et al. (2008)",# just for females
                   "Model_12" = "Haddad et al. (2006)"
)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73","#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#37004D","#BA4881")

Plot_organ <- ggplot(data = (Total_Organ), aes(x=`Age (in years)`, y=`Volume (in L)`, color = Model))+
  geom_line(linewidth=1.5) +
  scale_color_manual(values = myColors, labels = model_labels) 
#scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels) 

Plot_organ <- Plot_organ+  
  ggtitle("Liver") +
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
LifeTimeChangesOrgans[["M"]]$Liver <- Body[["M"]]$Liver
LifeTimeChangesOrgans[["F"]]$Liver <- Body[["F"]]$Liver
