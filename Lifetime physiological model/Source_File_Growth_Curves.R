########################################################################
# STEP 0: Load packages
########################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # attribute the working directory at the root of the project
library(ggplot2)
library(reshape2)
library(plotly)

########################################################################
# STEP 1: Generating body weight and height based on French population
########################################################################

print("ACTION: Please set the directory path where the R file for the body parameter calculations is set:")
setwd("Comparison_organ_equations_R_CORRECTED/")
source("BodyParameters.R")

# Execute the function and store the results in the list "Body"
results <- generate_body_data() # print a result per day of life
Body <- results$Body
LifeTimeChangesOrgans <- Body

# Print the plots
print(results$BW_plot_M)
print(results$BW_plot_F)

######################################################################################
# STEP 2: Select compartments and extract R code
# <!> dynamic section for the user
# select one by one the compartment (it is not possible to have a multiple selection)
######################################################################################

print("ACTION: Please set the directory path where the R folder for the growth functions are located:")
path <- getwd() # it will be used in the 'functions' R file
print(path)

print("ACTION: Please set the directory path where the R file for the Functions is set:")
path <- getwd() # it will be used in the 'functions' R file
print(path)
source("Functions.R")

# Execute the function
# <!> dynamic section for the user
# <!> select one compartment (not possible to select multiple)
show_compartment_code()

# Memorize selection
saved_compartment <- read_selected_compartment()
if (!is.null(saved_compartment)) {
  cat("Previously selected compartment:", saved_compartment, "\n")
  selected_link <- links[[saved_compartment]]
  cat("Link to the selected source file:", selected_link, "\n")
}

cat("Previously selected compartment:", saved_compartment, "\n")
cat("Link to the selected source file:", selected_link, "\n")

########################################################################
# STEP 3: Calculation of specific organ volumes
########################################################################

# Execute specific R script
source(selected_link)

print(ggplotly(Plot_organ))
print(List_of_Models)
print(Total_Organ)


########################################################################
# STEP 4: List with all needed organs
########################################################################

cat("The following list gives you all organ volume changes over time for the selected compartments")

print(LifeTimeChangesOrgans)

# List_of_Models <- list(
#   Model_1 = "Beaudouin et al. (2010)",
#   Model_2 = "Haddad et al. (2001)",
#   Model_3 = "Price K. et al. (2003)", # just for males
#   Model_4 = "Smith et al. (2014)",
#   Model_5 = "Ring et al. (2017)",
#   Model_6 = "Pendse et al. (2020)",
#   Model_7 = "Mallick et al (2020)",
#   Model_8 = "Sarigiannis et al. (2020)", 
#   Model_9 = "Wu et al. 2015", # just for females
#   Model_10 = "Deepika et al (2021)",
#   Model_11 = "Verner et al. (2008)",# just for females
#   Model_12 = "Haddad et al. (2006)"
# )

cat("Go to line 44 if you want to select another compartment")


