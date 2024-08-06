########################################################################
# STEP 1: Generating body weight and height based on French population
########################################################################


# Define the compartments and corresponding source files

compartments <- c("Adrenals", "Blood", "Body Surface Area", "Bone", "Brain", "Breast", "Cardiac Flow", "Diaphragm", "Fat",
                  "Gonads", "HCT", "Heart", "Intestine", "Kidney", "Liver", "Lungs", "Marrow", "Muscle", "Pancreas", "Skin", "Spleen",
                  "Stomach", "Thyroid", "Tongue")

# select the directory to create the functions
setwd("..")
path_project <- getwd()
path <- paste0(path_project, "/Comparison_organ_equations_R_CORRECTED")

links <- list(
  Adrenals = file.path(path, "Adrenals.R"), 
  Blood = file.path(path, "Blood.R"), 
  "Body Surface Area" = file.path(path, "Body_surface_area.R"), 
  Bone = file.path(path, "Bone.R"), 
  Brain = file.path(path, "Brain.R"), 
  Breast = file.path(path, "Breast.R"), 
  "Cardiac Flow" = file.path(path, "Cardiac_output.R"), 
  Diaphragm = file.path(path, "Diaphragm.R"),
  Fat = file.path(path, "Fat.R"),
  Gonads = file.path(path, "Gonads.R"),
  HCT = file.path(path, "HCT.R"), 
  Heart = file.path(path, "Heart.R"),
  Intestine = file.path(path, "Intestine.R"),
  Kidney = file.path(path, "Kidney.R"), 
  Liver = file.path(path, "Liver.R"), 
  Lungs = file.path(path, "Lungs.R"),
  Marrow = file.path(path, "Marrow.R"),
  Muscle = file.path(path, "Muscle.R"),
  Pancreas = file.path(path, "Pancreas.R"), 
  Skin = file.path(path, "Skin.R"), 
  Spleen = file.path(path, "Spleen.R"), 
  Stomach = file.path(path, "Stomach.R"), 
  Thyroid = file.path(path, "Thyroid.R"), 
  Tongue = file.path(path, "Tongue.R") 
)

# Main function to handle the entire process
show_compartment_code <- function() {
  # Function to prompt the user for a yes or no answer
  get_yes_or_no <- function() {
    answers <- c("Yes", "No")
    while (TRUE) {
      selected_answer <- select.list(answers, title = "Do you want to see the code for the chosen compartment?:")
      if (selected_answer %in% c("Yes", "No")) {
        return(selected_answer)
      } else {
        cat("Please select 'Yes' or 'No'.\n")
      }
    }
  }
  
  # Prompt user to select a compartment
  selected_compartment <- select.list(compartments, title = "Choose Compartment")
  
  if (is.null(selected_compartment)) {
    cat("No compartment selected. Exiting...\n")
    return(NULL)
  }
  
  # Save the selected compartment to a file
  writeLines(selected_compartment, con = "selected_compartment.txt")
  
  # Main logic to handle the selected answer
  answer <- get_yes_or_no()
  if (answer == "Yes") {
    selected_source <- links[[selected_compartment]]
    if (!is.null(selected_source)) {
      selected_source_content <- readLines(selected_source)
      cat("Content of the selected source file:\n")
      cat(selected_source_content, sep = "\n")
      return(selected_compartment)  # Return the selected compartment
    } else {
      cat("Invalid compartment selected or source file not found.\n")
      return(NULL)
    }
  } else {
    cat("Exiting...\n")
    return(NULL)
  }
}

# Function to read the selected compartment from file
read_selected_compartment <- function() {
  if (file.exists("selected_compartment.txt")) {
    selected_compartment <- readLines("selected_compartment.txt")
    return(selected_compartment)
  } else {
    cat("No saved compartment found.\n")
    return(NULL)
  }
}



