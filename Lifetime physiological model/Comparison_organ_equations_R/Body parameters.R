
print("Set your working directory")

res_dir = "Growth_Function"
dir.create(res_dir)
dir.create(paste(res_dir,"/Growth_results",sep =""))

library(ggplot2)


########################################################################
# STEP 1: Generating body weight and height based on French population
########################################################################

generate_body_data <- function() {
  # Create empty list to store results
  Body <- list()
  
  # Helper function to calculate body metrics
  calculate_metrics <- function(age_m, is_male) {
    if (is_male) {
      BW <- 8.917 - 3.499e-02 * age_m + 4.152e-03 * (age_m)^2 - 1.917e-05 * (age_m)^3 +
        3.621e-08 * (age_m)^4 - 3.126e-11 * (age_m)^5 + 1.019e-14 * (age_m)^6
      
      HT <- 64.35 + 8.146e-01 * age_m - 7.474e-04 * (age_m)^2 - 6.322e-06 * (age_m)^3 +
        1.903e-08 * (age_m)^4 - 1.995e-11 * (age_m)^5 + 7.297e-15 * (age_m)^6
    } else {
      BW <- 6.564 + 1.193e-01 * age_m + 2.412e-03 * (age_m)^2 - 1.357e-05 * (age_m)^3 +
        2.832e-08 * (age_m)^4 - 2.630e-11 * (age_m)^5 + 9.097e-15 * (age_m)^6
      
      HT <- 57.63 + 1.083 * age_m - 3.679e-03 * (age_m)^2 + 4.633e-06 * (age_m)^3 -
        4.226e-12 * (age_m)^5 + 2.302e-15 * (age_m)^6
    }
    
    BMI <- BW / (HT / 100)^2
    list(BW = BW, HT = HT, BMI = BMI)
  }
  
  # Generate data for male
  age_y <- c(1:29200) / 365
  age_m <- c(1:29200) / (29200 / 960)
  age_h <- c(1:29200) * 24
  male_metrics <- calculate_metrics(age_m, TRUE)
  
  Body[["M"]] <- data.frame(
    age_y = age_y,
    age_m = age_m,
    age_h = age_h,
    BW = male_metrics$BW,
    HT = male_metrics$HT,
    BMI = male_metrics$BMI
  )
  
  # Generate data for female
  female_metrics <- calculate_metrics(age_m, FALSE)
  
  Body[["F"]] <- data.frame(
    age_y = age_y,
    age_m = age_m,
    age_h = age_h,
    BW = female_metrics$BW,
    HT = female_metrics$HT,
    BMI = female_metrics$BMI
  )
  
  # Plot body weight for male
  BW_plot_M <- ggplot(data = Body[["M"]], aes(x = age_y, y = BW)) +
    geom_line(linewidth = 2) +
    labs(title = "Body Weight Over Age (Males)", x = "Age (years)", y = "Body Weight (kg)")
  
  # Plot body weight for female
  BW_plot_F <- ggplot(data = Body[["F"]], aes(x = age_y, y = BW)) +
    geom_line(linewidth = 2) +
    labs(title = "Body Weight Over Age (Females)", x = "Age (years)", y = "Body Weight (kg)")
  
  return(list(Body = Body, BW_plot_M = BW_plot_M, BW_plot_F = BW_plot_F))
}





# Function to prompt the user for a yes or no answer
get_yes_or_no <- function() {
  # Define the available answers
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

# Function to select a compartment and show the corresponding source code if the answer is 'Yes'
show_compartment_code <- function() {
  # Use the selected compartment to retrieve the corresponding source file link
  selected_source <- links[[selected_compartment]]
  if (!is.null(selected_source)) {
    selected_source <- readLines(selected_source)
    # Print the content of the selected source file
    cat("Content of the selected source file:", "\n")
    # Print selected source
    cat(selected_source, sep = "\n")
  } else {
    cat("Invalid compartment selected or source file not found.\n")
  }
}