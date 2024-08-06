#############################################################
#                        FAT                                #
##############################################################
# Delete previous files
rm(list = ls(pattern = "^Total_Organ"))
rm(list = ls(pattern = "^combined"))
rm(list = ls(pattern = "^Model"))
rm(list = ls(pattern = "^Plot_organ"))

library(ggplot2)

# Execute the function and store the results in the list "Body"
setwd("Comparison_organ_equations_R_CORRECTED/") # define the path for the associated R function code 
source("BodyParameters.R")
results <- generate_body_data()
class(results)
# head(results) # structure: results$Body$M or results$Body$F

Body <- results$Body # list of 2 elements: [1]M for male, [2]F for female
# str(Body)

##############################################################
# 1. List of models
##############################################################

List_of_Models <- list(
  # Model_1 = "Beaudouin et al. (2010)",
  Model_2 = "Haddad et al. (2001)",
  Model_3 = "Price K. et al. (2003)", # just for males
  Model_4 = "Smith et al. (2014)",
  # Model_5 = "Ring et al. (2017)",
  Model_6 = "Pendse et al. (2020)",
  Model_7 = "Mallick et al (2020)",
  Model_8 = "Sarigiannis et al. (2020)",
  Model_9 = "Wu et al. 2015", # just for females
  Model_10 = "Deepika et al (2021)",
  # Model_11 = "Verner et al. (2008)",# just for females
  Model_12 = "Haddad et al. (2006)"
)

##############################################################
# 2a. male-specific physiological equations for models
##############################################################
## Haddad et al. (2001)
Model_2 <- ifelse(Body[["M"]][["age_y"]] < 17,
  (0.0165 * Body[["M"]][["age_y"]]**5 -
    1.9784 * Body[["M"]][["age_y"]]**4 +
    51.963 * Body[["M"]][["age_y"]]**3 -
    459.39 * Body[["M"]][["age_y"]]**2 +
    1566.8 * Body[["M"]][["age_y"]] + 1004.2) / 916,
  (0.0164 * 17**5 -
    1.9784 * 17**4 +
    51.963 * 17**3 -
    459.39 * 17**2 +
    1566.8 * 17 + 1004.2) / 916
)

### Price K. et al. (2003)
Model_3 <- ifelse(Body[["M"]][["age_y"]] < 18,
  (0.0165 * Body[["M"]][["age_y"]]**5 -
    1.9784 * Body[["M"]][["age_y"]]**4 +
    51.963 * Body[["M"]][["age_y"]]**3 -
    459.38 * Body[["M"]][["age_y"]]**2 +
    1566.8 * Body[["M"]][["age_y"]] + 1004.2) / 1000,
  (0.0165 * 18**5 - 1.9784 * 18**4 + 51.963 * 18**3 - 459.38 * 18**2 + 1566.8 * 18 + 1004.2) / 1000
)


### Smith et al. (2014)
Model_4 <- (3.484e-02 + 2.803e-05 * (Body[["M"]][["BW"]] * 1000) -
  1.422e-09 * (Body[["M"]][["BW"]] * 1000)^2 +
  2.892e-14 * (Body[["M"]][["BW"]] * 1000)^3 -
  2.718e-19 * (Body[["M"]][["BW"]] * 1000)^4 +
  1.203e-24 * (Body[["M"]][["BW"]] * 1000)^5 -
  2.036e-30 * (Body[["M"]][["BW"]] * 1000)^6) * Body[["M"]][["BW"]]

#### Pendse et al. 2020
Model_6 <- ifelse(Body[["M"]][["age_y"]] < 20,
  ((2.8975 * exp(-0.129 * Body[["M"]][["age_y"]]) + 0.67) * Body[["M"]][["BMI"]] + 0.2635 * Body[["M"]][["age_y"]] - 4.843) * Body[["M"]][["BW"]] / 100,
  ((-5.33798 * Body[["M"]][["BMI"]] + 0.11149 * (Body[["M"]][["BMI"]]^2) + 0.09795 * Body[["M"]][["age_y"]] + 85.24521) * Body[["M"]][["BW"]]) / 100
)

### Mallick et al. (2020)
Model_7 <- ifelse(Body[["M"]][["age_y"]] < 25,
  ((1.4471 * exp(-0.0761 * Body[["M"]][["age_y"]]) + 0.52) * Body[["M"]][["BMI"]] - 0.10124 * Body[["M"]][["age_y"]] + 5.0465) * Body[["M"]][["BW"]] / 100,
  ((-6.0487 * Body[["M"]][["BMI"]] + 0.1177 * Body[["M"]][["BMI"]]^2 + 0.03155 * Body[["M"]][["age_y"]] + 97.2025) * 0.979) * Body[["M"]][["BW"]] / 100
)

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["M"]][["age_y"]] < 18,
  (2.54e-02 * Body[["M"]][["age_h"]]**1 + 1.88e+01 * (Body[["M"]][["age_h"]]**(5.20e-01)) + 9.06e+02) / 1000,
  (2.54e-02 * (18 * 365 * 24)**1 + 1.88e+01 * ((18 * 365 * 24)**(5.20e-01)) + 9.06e+02) / 1000
)

### Wu et al. (2015)
Model_9 <- "NA"

### Deepika et al. (2021)
Model_10 <- 1.3054356 + 0.3622685 * Body[["M"]][["age_y"]] - 0.0025165 * (Body[["M"]][["age_y"]]**2) + 0.0906119 * Body[["M"]][["BW"]] + 0.0001731 * (Body[["M"]][["BW"]]**2)



Model_List_M <- cbind.data.frame(Model_2, Model_3, Model_4, Model_6, Model_7, Model_8, Model_9, Model_10)
Body[["M"]]$Fat <- Model_List_M

head(Body$M)

head(Body$M$Fat)
##############################################################
# 2b. female-specific physiological equations for models
##############################################################


### Haddad et al. (2001)
Model_2 <- ifelse(Body[["F"]][["age_y"]] < 18,
  (0.038 * Body[["F"]][["age_y"]]**5 -
    2.6629 * Body[["F"]][["age_y"]]**4 +
    60.433 * Body[["F"]][["age_y"]]**3 -
    479.37 * Body[["F"]][["age_y"]]**2 +
    1592.3 * Body[["F"]][["age_y"]] + 912.36) / 916,
  (0.038 * 18**5 - 2.6629 * 18**4 + 60.433 * 18**3 - 479.37 * 18**2 + 1592.3 * 18 + 912.36) / 916
)

### Price K. et al. (2003) - spaceholder
Model_3 <- "NA"

### Smith et al. (2014)
Model_4 <- (9.217e-02 + 1.401e-05 * (Body[["F"]][["BW"]] * 1000) -
  6.787e-10 * (Body[["F"]][["BW"]] * 1000)^2 +
  1.540e-14 * (Body[["F"]][["BW"]] * 1000)^3 -
  1.558e-19 * (Body[["F"]][["BW"]] * 1000)^4 +
  7.249e-25 * (Body[["F"]][["BW"]] * 1000)^5 -
  1.274e-30 * (Body[["F"]][["BW"]] * 1000)^6) * Body[["F"]][["BW"]]

### Pendse et al. (2020)
Model_6 <- ifelse(Body[["F"]][["age_y"]] < 25,
  ((1.5334 * exp(-0.103 * Body[["F"]][["age_y"]]) + 0.67) * Body[["F"]][["BMI"]] + 0.6276 * Body[["F"]][["age_y"]] + 1.0301) * Body[["F"]][["BW"]] / 100,
  ((1.9224 * Body[["F"]][["BMI"]] - 0.018517 * Body[["F"]][["BMI"]]^2 + 0.05537 * Body[["F"]][["age_y"]] - 0.794894)) * Body[["F"]][["BW"]] / 100
)

### Mallick et al. (2020)
Model_7 <- ifelse(Body[["F"]][["age_y"]] < 25,
  ((1.5334 * exp(-0.103 * Body[["F"]][["age_y"]]) + 0.67) * Body[["F"]][["BMI"]] + 0.6276 * Body[["F"]][["age_y"]] + 1.0301) * Body[["F"]][["BW"]] / 100,
  ((1.9224 * Body[["F"]][["BMI"]] - 0.018517 * Body[["F"]][["BMI"]]^2 + 0.05537 * Body[["F"]][["age_y"]] - 0.794895) * 0.979) * Body[["F"]][["BW"]] / 100
)

### Sarigiannis et al. (2020)
Model_8 <- ifelse(Body[["F"]][["age_y"]] < 18,
  (2.54e-02 * Body[["F"]][["age_h"]]**1 + 1.88e+01 * (Body[["F"]][["age_h"]]**(5.20e-01)) + 9.06e+02) / 1000,
  (2.54e-02 * (18 * 365 * 24)**1 + 1.88e+01 * ((18 * 365 * 24)**(5.20e-01)) + 9.06e+02) / 1000
)

### Wu et al. (2015)
Model_9 <- ifelse(Body[["F"]][["age_y"]] <= 25,
  ((1.5334 * exp(-0.103 * Body[["F"]][["age_y"]])
    + 0.67) * Body[["F"]][["BMI"]] +
    0.6276 * Body[["F"]][["age_y"]] + 1.0301) * Body[["F"]][["BW"]] / 100,
  (1.9224 * Body[["F"]][["BMi"]] -
    0.018517 * Body[["F"]][["BMI"]]^2 +
    0.05537 * Body[["F"]][["age_y"]] -
    0.794894) * Body[["F"]][["BW"]] / 100
)

### Deepika et al. (2021)
Model_10 <- 6.132e-01 + 8.475e-02 * Body[["F"]][["age_y"]] + 8.151e-05 * Body[["F"]][["age_y"]]**2 + 1.341e-01 * Body[["F"]][["BW"]] + 2.297e-03 * (Body[["F"]][["BW"]]**2)


# Combine the output 
Model_List_F <- cbind.data.frame(Model_2, Model_3, Model_4, Model_6, Model_7, Model_8, Model_9, Model_10)
Body[["F"]]$Fat <- Model_List_F


##############################################################
# 3. Gender-specific plotting
##############################################################

# for male
Body_M <- Body[["M"]]

Body_M[12] <- unlist(Body[["M"]][["Fat"]][["Model_2"]])
Body_M[13] <- unlist(Body[["M"]][["Fat"]][["Model_3"]])
Body_M[14] <- unlist(Body[["M"]][["Fat"]][["Model_4"]])
Body_M[15] <- unlist(Body[["M"]][["Fat"]][["Model_6"]])
Body_M[16] <- unlist(Body[["M"]][["Fat"]][["Model_7"]])
Body_M[17] <- unlist(Body[["M"]][["Fat"]][["Model_8"]])
Body_M[18] <- unlist(Body[["M"]][["Fat"]][["Model_9"]])
Body_M[19] <- unlist(Body[["M"]][["Fat"]][["Model_10"]])
Body_M[["Gender"]] <- "Male"

combined_models_M <- rbind(
  data.frame(Body_M, Model = "Model_2", "Volume (in L)" = Body_M[[12]]),
  data.frame(Body_M, Model = "Model_3", "Volume (in L)" = Body_M[[13]]),
  data.frame(Body_M, Model = "Model_4", "Volume (in L)" = Body_M[[14]]),
  data.frame(Body_M, Model = "Model_6", "Volume (in L)" = Body_M[[15]]),
  data.frame(Body_M, Model = "Model_7", "Volume (in L)" = Body_M[[16]]),
  data.frame(Body_M, Model = "Model_8", "Volume (in L)" = Body_M[[17]]),
  data.frame(Body_M, Model = "Model_9", "Volume (in L)" = Body_M[[18]]),
  data.frame(Body_M, Model = "Model_10", "Volume (in L)" = Body_M[[19]])
)

# For female
Body_F <- Body[["F"]]
Body_F[12] <- unlist(Body[["F"]][["Fat"]][["Model_2"]])
Body_F[13] <- unlist(Body[["F"]][["Fat"]][["Model_3"]])
Body_F[14] <- unlist(Body[["F"]][["Fat"]][["Model_4"]])
Body_F[15] <- unlist(Body[["F"]][["Fat"]][["Model_6"]])
Body_F[16] <- unlist(Body[["F"]][["Fat"]][["Model_7"]])
Body_F[17] <- unlist(Body[["F"]][["Fat"]][["Model_8"]])
Body_F[18] <- unlist(Body[["F"]][["Fat"]][["Model_9"]])
Body_F[19] <- unlist(Body[["F"]][["Fat"]][["Model_10"]])
Body_F[["Gender"]] <- "Female"

combined_models_F <- rbind(
  data.frame(Body_F, Model = "Model_2", "Volume (in L)" = Body_F[[12]]),
  data.frame(Body_F, Model = "Model_3", "Volume (in L)" = Body_F[[13]]),
  data.frame(Body_F, Model = "Model_4", "Volume (in L)" = Body_F[[14]]),
  data.frame(Body_F, Model = "Model_6", "Volume (in L)" = Body_F[[15]]),
  data.frame(Body_F, Model = "Model_7", "Volume (in L)" = Body_F[[16]]),
  data.frame(Body_F, Model = "Model_8", "Volume (in L)" = Body_F[[17]]),
  data.frame(Body_F, Model = "Model_9", "Volume (in L)" = Body_F[[18]]),
  data.frame(Body_F, Model = "Model_10", "Volume (in L)" = Body_F[[19]])
)

# Combine male and female outuputs 
Total_Organ <- rbind(combined_models_F, combined_models_M)

Total_Organ <- Total_Organ[, c(-12, -13, -14, -15, -16, -17, -18, -19)]
colnames(Total_Organ)[14] <- "Volume (in L)"

Total_Organ <- transform(Total_Organ, `Volume (in L)` = as.numeric(`Volume (in L)`))
colnames(Total_Organ)[1] <- "Age (in years)"
colnames(Total_Organ)[2] <- "Age (in months)"
colnames(Total_Organ)[3] <- "Age (in hours)"
colnames(Total_Organ)[14] <- "Volume (in L)"
str(Total_Organ)

# Create a named vector for the labels
model_labels <- c( # "Model_1" = "Beaudouin et al. (2010)",
  "Model_2" = "Haddad et al. (2001)",
  "Model_3" = "Price K. et al. (2003)", # just for males
  "Model_4" = "Smith et al. (2014)",
  # "Model_5" = "Ring et al. (2017)",
  "Model_6" = "Pendse et al. (2020)",
  "Model_7" = "Mallick et al (2020)",
  "Model_8" = "Sarigiannis et al. (2020)",
  "Model_9" = "Wu et al. (2015)", # just for females
  "Model_10" = "Deepika et al (2021)",
  # "Model_11" = "Verner et al. (2008)",# just for females
  "Model_12" = "Haddad et al. (2006)"
)

myColors <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73", "#c00000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#37004D", "#BA4881")


# plot the outputs
Plot_organ <- ggplot(data = (Total_Organ), aes(x = `Age (in years)`, y = `Volume (in L)`, color = Model)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = myColors, labels = model_labels)
# scale_linetype_manual(values = c("solid", "dashed"), labels = model_labels)

Plot_organ <- Plot_organ +
  ggtitle("Fat") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.y = element_text(lineheight = 1.5, size = 14),
    axis.title.x = element_text(lineheight = 1.5, size = 14),
    axis.text.y = element_text(lineheight = 1.5, size = 14, margin = margin(r = 6), vjust = 1),
    axis.text.x = element_text(lineheight = 1.5, size = 14),
    legend.text = element_text(lineheight = 1.5, size = 14),
    legend.title = element_text(lineheight = 1.5, size = 14),
    legend.background = element_rect(
      fill = "white",
      linewidth = 4,
      colour = "white"
    ),
    axis.ticks = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )

Plot_organ <- Plot_organ +
  facet_wrap(facets = ~Gender) +
  theme(strip.text = element_text(size = 15))


# save changes of volume over age in list
LifeTimeChangesOrgans[["M"]]$Fat <- Body[["M"]]$Fat
LifeTimeChangesOrgans[["F"]]$Fat <- Body[["F"]]$Fat
